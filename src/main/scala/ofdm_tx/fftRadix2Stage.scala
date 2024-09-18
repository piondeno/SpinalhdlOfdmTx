package ofdm_tx

import ofdm_tx.ButterflyDataIf.initNullFunc
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._

import scala.math._

case class ComplexDataType(inputDataWidth : Int) extends Bundle{
  val I = SInt(inputDataWidth bits)
  val Q = SInt(inputDataWidth bits)
}

object ComplexDataType{
  def initRegFunc(x : ComplexDataType) : Unit ={
    x.I.setAsReg() init(0)
    x.Q.setAsReg() init(0)
  }

  def setValueAsZero(x : ComplexDataType) : Unit ={
    x.I := 0
    x.Q := 0
  }
}

case class ButterflyDataIf(inputDataWidth : Int) extends Bundle{
  val aData = ComplexDataType(inputDataWidth)
  val bData = ComplexDataType(inputDataWidth)
}

object ButterflyDataIf{
  def initRegFunc(x : ButterflyDataIf) : Unit ={
    x.aData.I init(0)
    x.aData.Q init(0)
    x.bData.I init (0)
    x.bData.Q init (0)
  }

  // do not thing for initialization is not needed
  def initNullFunc(x: ButterflyDataIf): Unit = {
    x.aData.I := 0
    x.aData.Q := 0
    x.bData.I := 0
    x.bData.Q := 0
  }
}

case class FftDataOutIf(inputDataWidth : Int) extends Bundle{
  val data = ButterflyDataIf(inputDataWidth)
  val valid = Bool()
}

// sampleCount is the number of FFT points
// forFFT = true for FFT
// forFFT = false for IFFT
case class FftCoeeRom(resolutionWidth: Int, sampleCount: Int, forFFT : Boolean) extends Component {
  require(isPow2(sampleCount))
  val io = new Bundle {
    val dataOut = out(ComplexDataType(resolutionWidth))
    val addr = in UInt(log2Up(sampleCount/2) bits)
  }

  println(" case class sineRom "+" io.addr.getWidth : "+io.addr.getWidth)

  // Calculate values for the lookup table
  def sinTable = for(sampleIndex <- 0 until (sampleCount/2)) yield {
    //sample from 1/2*PI ~ 3/2*PI for FFT
    val sinValueFFT = Math.sin(2 * Math.PI * sampleIndex / sampleCount+ Math.PI)
    val sinValueIFFT = Math.sin(2 * Math.PI * sampleIndex / sampleCount)
    if(forFFT){
      S((sinValueFFT * (1<<(GlobalDefine().rotationFactorWidth-2))).toInt,GlobalDefine().rotationFactorWidth bits)
    }else{
      S((sinValueIFFT * (1<<(GlobalDefine().rotationFactorWidth-2))).toInt,GlobalDefine().rotationFactorWidth bits)
    }
  }

  def cosTable = for (sampleIndex <- 0 until (sampleCount / 2)) yield {
    //sample from 1/2*PI ~ 3/2*PI
    val cosValue = Math.sin(2 * Math.PI * sampleIndex / sampleCount + Math.PI/2)
    S((cosValue * (1 << (GlobalDefine().rotationFactorWidth - 2))).toInt, GlobalDefine().rotationFactorWidth bits)
  }

  val sinRom =  Mem(SInt(resolutionWidth bits),initialContent = sinTable)
  val cosRom =  Mem(SInt(resolutionWidth bits),initialContent = cosTable)

  io.dataOut.Q := sinRom.readAsync(io.addr)
  io.dataOut.I := cosRom.readAsync(io.addr)
}

//Because in final stage, the rotation coee always 1,
//So it does not need multiplier
case class Butterfly2ForFinalStage(inputDataWidth : Int) extends Component{
  val io = new Bundle {
    val dataInA = in(ComplexDataType(inputDataWidth))
    val dataInB = in(ComplexDataType(inputDataWidth))
    val dataOutA = out(ComplexDataType(inputDataWidth + 1))
    val dataOutB = out(ComplexDataType(inputDataWidth + 1))
  }
  def validOutputDelay : Int = 2

  //Butterfly2ForFinalStage
  /*
  * ------------
  *    \  /
  *     \/
  *     /\
  *    /  \
  *   /    \
  * ---(-1)-----
  * Because pipeline, get the result @ second clock
 */

  val dataInA_Reg = RegNext(io.dataInA)
  val dataInB_Reg = RegNext(io.dataInB)

  var butterFlyLen = dataInA_Reg.I.getWidth + 1
  val butterFlyA = Reg(ComplexDataType(butterFlyLen))
  val butterFlyB = Reg(ComplexDataType(butterFlyLen))
  butterFlyA.I := dataInA_Reg.I +^ dataInB_Reg.I
  butterFlyA.Q := dataInA_Reg.Q +^ dataInB_Reg.Q
  butterFlyB.I := dataInA_Reg.I -^ dataInB_Reg.I
  butterFlyB.Q := dataInA_Reg.Q -^ dataInB_Reg.Q

  io.dataOutA := butterFlyA
  io.dataOutB := butterFlyB
}

case class Butterfly2(inputDataWidth : Int, rotationFactorWidth : Int) extends Component{
  val io = new Bundle{
    val dataInA = in(ComplexDataType(inputDataWidth))
    val dataInB = in(ComplexDataType(inputDataWidth))
    val rotationFactor = in(ComplexDataType(rotationFactorWidth))
    val dataOutA = out(ComplexDataType(inputDataWidth+1))
    val dataOutB = out(ComplexDataType(inputDataWidth+1))
  }

  def validOutputDelay : Int = 4

  val dataInA_Reg = RegNext(io.dataInA)
  val dataInB_Reg = RegNext(io.dataInB)

  val rotationFactorReg1 = RegNext(io.rotationFactor)
  val rotationFactorReg2 = RegNext(rotationFactorReg1)

  //butterfly
  /*
  *  -----------------------
  *    \  /
  *     \/
  *     /\
  *    /  \
  *   /    \
  * ---(-1)-----(W^(n/N))---
  * Because pipeline, get the result @ 4'th clock
  * Whole design is base on Xilinx DSP48 structure
  *
  * For maintain the consistence of A and B path
  * The max amplitude of rotation factor will be B"0100...00" : rotationFactorWidth bits
  * (W^(n/N)) is rotation factor
  */
  var butterFlyLen = dataInA_Reg.I.getWidth + 1
  val butterFlyA = Reg(ComplexDataType(butterFlyLen))
  val butterFlyB = Reg(ComplexDataType(butterFlyLen))
  butterFlyA.I := dataInA_Reg.I +^ dataInB_Reg.I
  butterFlyA.Q := dataInA_Reg.Q +^ dataInB_Reg.Q
  butterFlyB.I := dataInA_Reg.I -^ dataInB_Reg.I
  butterFlyB.Q := dataInA_Reg.Q -^ dataInB_Reg.Q

  //butterfly second stage, multiply rotation factor
  // (a+jb)*(c+jd) = (ac-bd) + j(bc+ad)
  var multiplyLen = butterFlyA.I.getWidth + io.rotationFactor.I.getWidth
  val multiplyA = Reg(ComplexDataType(butterFlyA.I.getWidth))
  val multiplyB_ac = Reg(SInt(multiplyLen bits)) //init(0) // If initial is needed, unmark
  val multiplyB_bd = Reg(SInt(multiplyLen bits)) //init(0) // If initial is needed, unmark
  val multiplyB_bc = Reg(SInt(multiplyLen bits)) //init(0) // If initial is needed, unmark
  val multiplyB_ad = Reg(SInt(multiplyLen bits)) //init(0) // If initial is needed, unmark
  //List(butterFlyA,butterFlyB,multiplyA).foreach(elem => elem.I init(0)) // If initial is needed, unmark
  //List(butterFlyA,butterFlyB,multiplyA).foreach(elem => elem.Q init(0)) // If initial is needed, unmark
  // (a+jb)*(c+jd) = (ac-bd)+j(ad+bc)
  multiplyB_ac := butterFlyB.I * rotationFactorReg2.I
  multiplyB_bd := butterFlyB.Q * rotationFactorReg2.Q
  multiplyB_bc := butterFlyB.Q * rotationFactorReg2.I
  multiplyB_ad := butterFlyB.I * rotationFactorReg2.Q
  multiplyA.I := butterFlyA.I
  multiplyA.Q := butterFlyA.Q

  //result register base DSP48 structure
  val resultRegA = RegNext(multiplyA)
  val resultRegB = Reg(ComplexDataType(multiplyB_ac.getBitsWidth))
  resultRegB.I := multiplyB_ac - multiplyB_bd //(ac-bd)
  resultRegB.Q := multiplyB_bc + multiplyB_ad // j(ad+bc)

  //output
  io.dataOutA := resultRegA
  var msbBitLoc = resultRegB.I.getWidth-1
  //val test = B(resultRegB.I((msbBitLoc-2) downto (msbBitLoc-2-inputDataWidth)))
  //println(test)

  //assign and remove unused bits
  //in current state, there is no round operation. Maybe round operation is needed for accuracy.
  io.dataOutB.I := B(resultRegB.I((msbBitLoc-2) downto (msbBitLoc-2-inputDataWidth))).asSInt
  io.dataOutB.Q := B(resultRegB.Q((msbBitLoc-2) downto (msbBitLoc-2-inputDataWidth))).asSInt
}

//modify the interface of input and output of Butterfly2()
case class Butterfly2WithCsgFftDataIf(inputDataWidth : Int, rotationFactorWidth : Int) extends Component{
  val io = new Bundle {
    val dataIn = in(ButterflyDataIf(inputDataWidth))
    val rotationFactor = in(ComplexDataType(rotationFactorWidth))
    val dataOut = out(ButterflyDataIf(inputDataWidth + 1))
  }

  val butterfly2Inst = Butterfly2(inputDataWidth, rotationFactorWidth)
  butterfly2Inst.io.dataInA := io.dataIn.aData
  butterfly2Inst.io.dataInB := io.dataIn.bData
  butterfly2Inst.io.rotationFactor := io.rotationFactor
  io.dataOut.aData := butterfly2Inst.io.dataOutA
  io.dataOut.bData := butterfly2Inst.io.dataOutB

  def validOutputDelay : Int = butterfly2Inst.validOutputDelay
}

//It only use 3 multipliers to implement complex multiplier
//refer to this web : https://fpga.eetrend.com/files-eetrend-xilinx/download/201304/3892-7815-500mhzgaoxingnengshuzixinhaochulisheji.pdf
//全流水的复数乘法
//this design is base on DSP48, ASIC needs modify.
//The output is rounding output
case class Butterfly2LessMultiplier(inputDataWidth : Int, rotationFactorWidth : Int) extends Component {
  val io = new Bundle {
    val dataIn = in(ButterflyDataIf(inputDataWidth))
    val rotationFactor = in(ComplexDataType(rotationFactorWidth))
    val dataOut = out(ButterflyDataIf(inputDataWidth + 1))
  }

  def validOutputDelay : Int = 8

  val dataInReg = RegNext(io.dataIn)
  dataInReg.aData.I init(0)
  dataInReg.aData.Q init(0)
  dataInReg.bData.I init (0)
  dataInReg.bData.Q init (0)

  //Rotation Coee delay control to meet the delay of butterfly
  val rotationFactorReg = RegNext(io.rotationFactor)
  rotationFactorReg.I init(0)
  rotationFactorReg.Q init(0)

  //Butterfly operation
  val upperPathReg = Reg(ComplexDataType(inputDataWidth + 1))
  upperPathReg.I init(0)
  upperPathReg.Q init(0)

  upperPathReg.I := dataInReg.aData.I +^ dataInReg.bData.I
  upperPathReg.Q := dataInReg.aData.Q +^ dataInReg.bData.Q
  val lowerPath_I = dataInReg.aData.I -^ dataInReg.bData.I
  val lowerPath_Q = dataInReg.aData.Q -^ dataInReg.bData.Q

  var upperPathDelayCtrlNum = validOutputDelay - 2
  val upperPathDelayReg = Vec.fill(upperPathDelayCtrlNum)(Reg(cloneOf(upperPathReg)))
  for (index <- 0 until upperPathDelayReg.length) {
    upperPathDelayReg(index).I init(0)
    upperPathDelayReg(index).Q init(0)
  }
  for(index <- 1 until upperPathDelayReg.length){
    upperPathDelayReg(index) := upperPathDelayReg(index-1)
  }
  upperPathDelayReg(0) := upperPathReg

  //Complex Multiplier (A+jB)*(C+jD)
  //P1 = C*(A+B)
  val preAddP1RegA = RegNext(lowerPath_I) init(0)
  val preAddP1RegB = RegNext(lowerPath_Q) init(0)
  val preAddP1RegC = RegNext(rotationFactorReg.I) init(0)
  val preMulP1RegAB = RegNext(preAddP1RegA +^ preAddP1RegB) init(0)
  val preMulP1RegC = RegNext(preAddP1RegC) init(0)
  val postMulP1RegP1 = RegNext( preMulP1RegAB * preMulP1RegC ) init(0)//DSP48 M Reg
  val pOutRegP1 = RegNext(postMulP1RegP1) init(0)//DSP48 P Reg, it will output to C Reg of other two DSP48

  //Re = P1 - B*(C+D) , Im = P1 + A*(D-C) , implement Z^-2 by registers/SRL
  val delayRegA = Vec.fill(2)(Reg(cloneOf(lowerPath_I)))
  val delayRegB = Vec.fill(2)(Reg(cloneOf(lowerPath_Q)))
  val delayRegC = Vec.fill(2)(Reg(cloneOf(rotationFactorReg.I)))
  val delayRegD = Vec.fill(2)(Reg(cloneOf(rotationFactorReg.Q)))
  for(index <- 0 until delayRegA.length){
    delayRegA(index) init(0)
    delayRegB(index) init(0)
    delayRegC(index) init(0)
    delayRegD(index) init(0)
  }
  delayRegA(0) := lowerPath_I
  delayRegB(0) := lowerPath_Q
  delayRegC(0) := rotationFactorReg.I
  delayRegD(0) := rotationFactorReg.Q
  delayRegA(1) := delayRegA(0)
  delayRegB(1) := delayRegB(0)
  delayRegC(1) := delayRegC(0)
  delayRegD(1) := delayRegD(0)

  val preAddReRegC = RegNext(delayRegC(1)) init(0)
  val preAddReRegD = RegNext(delayRegD(1)) init(0)
  val preAddReRegB = RegNext(delayRegB(1)) init(0)
  val preMulReCD = RegNext(preAddReRegC +^ preAddReRegD) init(0)
  val preMulReB  = RegNext(preAddReRegB) init(0)
  val postMulRe = RegNext(preMulReCD * preMulReB) init(0)
  val cReP1Reg = RegNext(pOutRegP1) init(0)
  val pReReg = RegNext(cReP1Reg -^ postMulRe) init(0)
  var roundValue = B(pReReg.getWidth bits, (rotationFactorWidth-3) -> true, default -> false).asSInt
  val pReRoundReg = RegNext(roundValue + pReReg)  // For rounding

  val preAddImRegC = RegNext(delayRegC(1)) init(0)
  val preAddImRegD = RegNext(delayRegD(1)) init(0)
  val preAddImRegA = RegNext(delayRegA(1)) init(0)
  val preMulImCD = RegNext(preAddImRegD - preAddImRegC) init(0)
  val preMulImA  = RegNext(preAddImRegA) init(0)
  val postMulIm = RegNext(preMulImA * preMulImCD) init(0)
  val cImP1Reg = RegNext(pOutRegP1) init(0)
  val pImReg = RegNext(cImP1Reg +^ postMulIm) init(0)
  val pImRoundReg = RegNext(roundValue + pImReg) // For rounding

    //output assign
  io.dataOut.aData := upperPathDelayReg(upperPathDelayReg.length-1)
  //io.dataOutB.Q := B(resultRegB.Q((msbBitLoc-2) downto (msbBitLoc-2-inputDataWidth))).asSInt
  io.dataOut.bData.I := B(pReRoundReg( (rotationFactorWidth-2+inputDataWidth) downto (rotationFactorWidth-2) )).asSInt
  io.dataOut.bData.Q := B(pImRoundReg( (rotationFactorWidth-2+inputDataWidth) downto (rotationFactorWidth-2) )).asSInt
}
//    |-------|
//    |       |
//    |       |
//    |       |
//    | Reg   |
//    |       |
//    |       |
//    |       |
//    |-------|

case class FftDataIF(fftPoints : Int, inputDataWidth : Int) extends Bundle {
  val data = Vec.fill(fftPoints)(ComplexDataType(inputDataWidth))
}

// constant geometry structure FFT
// numStageCombine : used to indicate how many radix-2 stages be combined in this module
// for example, 64-points FFT, there are six radix-x stages in this FFT
// if numStageCombine is 3, it will need 2 CsgFftRadix2Stage() to implement this FFT
// statePlan will be 0 or 1
// statePlan = 0 : this CsgFftRadix2Stage() will combine 1st,2nd,3rd radix-2 stage
// statePlan = 1 : this CsgFftRadix2Stage() will combine 4th,5th,6th radix-2 stage
// numStageCombined must >= 2 for current design



case class LoopFifo[T <: Data](val dataType: HardType[T], depth : Int, initFunc: T => Unit) extends Component{
  val io = new Bundle{
    val pushData = in(dataType)
    val push = in(Bool())
    val popData = out(dataType)
    val pop = in(Bool())
    val overflow = out(Bool())
    val underflow = out(Bool())
  }

  io.overflow.setAsReg() init(False)
  io.underflow.setAsReg() init(False)

  var popAddrBitWidth = log2Up(depth+1)
  var popAddrInit = pow(2,popAddrBitWidth).toInt - 1
  val popAddr = Reg(UInt(log2Up(depth+1) bits)) init(popAddrInit)
  val plusOne = popAddr + 1
  val minusOne = popAddr - 1

  val condition = (io.push,io.pop).asBits
  switch(condition){
    is(B"10"){  //push
      when(plusOne === depth){
        io.overflow := True
      } otherwise{
        popAddr := plusOne
      }
    }
    is(B"01"){  //pop
      when(popAddr === popAddrInit){
        io.underflow := True
      } otherwise{
        popAddr := minusOne
      }
    }
  }

  val fifo = Reg(Vec.fill(depth)(cloneOf(io.pushData)))
  fifo.foreach(initFunc)

  io.popData := fifo(popAddr)

  when(io.push){
    fifo(0) := io.pushData
    for(index <- 1 until fifo.length){
      fifo(index) := fifo(index-1)
    }
  }
}


// 參考文件 16-points CGS FFT 使用buffer的時序分析 in my 雲端硬碟，法3
// 因為送到fft的資料很快 必須dataIn使用Stream來調節速度
// 但CsgFftRadix2Stage一旦計算完成 就要立即的資料輸出 所以後方的電路必須確保處於能夠即時接收來自CsgFftRadix2Stage的資料
// 且接收過程不能有停頓，雖然CsgFftRadix2Stage的dataOut界面為Stream，但ready信號不起作用。
// dataOut界面為Stream，只是為了方便多級CsgFftRadix2Stage的信號連接。
// ex : 64 points FFT will need log2(64)=6 stages to complete operation.
// stagePlan : 使用多個大階段實現FFT運算，stagePlan用來表示是這些大階段的第幾個
// numStageCombined : 表示有多少個fft stages被整合到這個大階段中
case class CsgFftRadix2Stage(fftPoints : Int, inputDataWidth : Int, rotationFactorWidth : Int, stagePlan : Int, numStageCombined : Int, forFFT : Boolean) extends Component{
  require(isPow2(fftPoints))
  var fftPower = log2Up(fftPoints)
  require(((stagePlan+1)*numStageCombined) <= fftPower)
  require(numStageCombined >= 2)

  val io = new Bundle{
      val dataIn = slave(Stream(ButterflyDataIf(inputDataWidth)))
      val dataOut = master(Stream(ButterflyDataIf(inputDataWidth+numStageCombined)))
  }

  //finalStage Check
  var finalStageCheck = ((stagePlan * numStageCombined + numStageCombined) == fftPower)
  println("finalStageCheck : " + finalStageCheck)

  //default input signal declare here to avoid errors

  //If there are multi-stages be combined in this module, it is used to control the coee ROM
  val csgProgress = Reg(B(1, numStageCombined - 1 bits)) //this signal will be used to mask addr signal of coee ROM

  //expand dataIn width
  val dataInExpand = ButterflyDataIf(inputDataWidth+numStageCombined-1)
  dataInExpand := io.dataIn.payload.resized

  //instantiate butterfly and rotation coee ROM
  val butterflyInput = ButterflyDataIf(inputDataWidth+numStageCombined-1) //used to mux input as io.input or loop data
  //val butterfly2Inst = Butterfly2WithCsgFftDataIf((inputDataWidth+numStageCombined-1), rotationFactorWidth)
  val butterfly2Inst = Butterfly2LessMultiplier((inputDataWidth+numStageCombined-1), rotationFactorWidth)
  var sampleCount = (fftPoints / (pow(2,numStageCombined*stagePlan))).toInt
  println("sampleCount : "+sampleCount)
  val fftCoeeRomInst = FftCoeeRom(rotationFactorWidth, sampleCount, forFFT)
  val fftCoeeRomInstAddr = cloneOf(fftCoeeRomInst.io.addr)
  butterfly2Inst.io.dataIn := butterflyInput
  butterfly2Inst.io.rotationFactor := fftCoeeRomInst.io.dataOut
  fftCoeeRomInst.io.addr := fftCoeeRomInstAddr

  //the val be used in fsm
  val validTiming = False
  val fsmCnt = Reg(UInt((fftPower - 1) bits)) init (0)
  var dumpWidth = fftCoeeRomInstAddr.getWidth - csgProgress.getWidth
  var dumpBits = Bits(dumpWidth bits)
  dumpBits := (dumpBits.bitsRange -> true)
  val fftCoeeRomInstAddrMask = (dumpBits, csgProgress).asBits.asUInt
  fftCoeeRomInstAddr := ((fsmCnt >> (stagePlan*numStageCombined)) & fftCoeeRomInstAddrMask)

  //instantiate internal buffer
  println("butterfly2Inst.io.dataOut.aData.getBitsWidth : "+butterfly2Inst.io.dataOut.aData.getBitsWidth)
  var delayChainLen = (fftPoints / 4).toInt
  var loopFifoLen = delayChainLen - ceil(butterfly2Inst.validOutputDelay/2).toInt
  println("delayChainLen : "+delayChainLen)
  println("loopFifoLen : "+loopFifoLen)
  val delayChain = Reg(Vec.fill(delayChainLen)(cloneOf(butterfly2Inst.io.dataOut)))
  val loopFifoEven, loopFifoOdd = LoopFifo(cloneOf(butterfly2Inst.io.dataOut), loopFifoLen , ButterflyDataIf.initRegFunc)
  loopFifoEven.io.pushData.aData := delayChain(delayChain.length-1).aData
  loopFifoEven.io.pushData.bData := butterfly2Inst.io.dataOut.aData
  loopFifoOdd.io.pushData.aData := delayChain(delayChain.length-1).bData
  loopFifoOdd.io.pushData.bData := butterfly2Inst.io.dataOut.bData

  // butterfly pipeline delay control
  var butterflyDelayValue = butterfly2Inst.validOutputDelay
  println("butterflyDelayValue : " + butterflyDelayValue)

  val fftAddr_Reg = Reg(Vec.fill(butterflyDelayValue)(cloneOf(fsmCnt)))
  val validTimingReg = Reg(Vec.fill(butterflyDelayValue)(Bool())) //pipeline timing control, delay chain
  val outputEnable = Reg(Bool()) init(False)
  for(index <- 0 until validTimingReg.length){
    validTimingReg(index) init(False)
  }

  validTimingReg(0) := validTiming
  fftAddr_Reg(0) := fsmCnt
  for(index <- 1 until butterflyDelayValue){
    validTimingReg(index) := validTimingReg(index-1)
    fftAddr_Reg(index) := fftAddr_Reg(index-1)
  }

  when(validTimingReg(validTimingReg.length-1)){
    delayChain(0) := butterfly2Inst.io.dataOut
    for(index <- 1 until delayChainLen){
      delayChain(index) := delayChain(index-1)
    }
  }

  val pushCond = validTimingReg(validTimingReg.length-1) && fftAddr_Reg(fftAddr_Reg.length-1).msb
  if (finalStageCheck) {
    loopFifoEven.io.push := pushCond && (~outputEnable)
    loopFifoOdd.io.push := pushCond && (~outputEnable)
  }
  else {
    loopFifoEven.io.push := pushCond
    loopFifoOdd.io.push := pushCond
  }

//  loopFifoEven.io.push := False
//  loopFifoOdd.io.push := False
//  when(validTimingReg(validTimingReg.length-1) && fftAddr_Reg(fftAddr_Reg.length-1).msb){
//    if(finalStageCheck){
//      loopFifoEven.io.push := True && (~outputEnable)
//      loopFifoOdd.io.push := True && (~outputEnable)
//    }
//    else{
//      loopFifoEven.io.push := True
//      loopFifoOdd.io.push := True
//    }
//  }

  val fsm = new StateMachine{
    val stateInput : State = new State with EntryPoint{
      onEntry{
        csgProgress := (default -> true)
        fsmCnt := 0
      }
      whenIsActive{
        validTiming := io.dataIn.fire
        when(validTiming){
          when(fsmCnt === (fftPoints / 2 - 1)) {
            csgProgress := csgProgress |<< 1
            when(csgProgress(csgProgress.getWidth - 2)) {
              goto(stateLoop)
            } otherwise {
              goto(stateOutput)
            }
          }
        }
      }
    }

    val stateLoop : State = new State{
      whenIsActive {
        validTiming := True

        when(fsmCnt === (fftPoints / 2 - 1)) {
          csgProgress := csgProgress |<< 1

          when(csgProgress(csgProgress.getWidth - 2)) {
            goto(stateLoop)
          } otherwise {
            goto(stateOutput)
          }
        }
      }
    }

    val stateOutput : State = new State{
      whenIsActive{
        validTiming := True
        when(fsmCnt === (fftPoints / 2 - 1)){
          goto(stateInput)
        }
      }
    }
  }

  //
  io.dataIn.ready := Mux(fsm.isActive(fsm.stateInput),True,False)

  //announce signals
  val loopFifoPopDataMux = cloneOf(loopFifoEven.io.popData)
  val loopFifoPopDataAddr = UInt(fsmCnt.getBitsWidth bits)


  //val outputEnable = Reg(Bool()) init(False)
  io.dataOut.valid := outputEnable

  val zeroOut = ButterflyDataIf(inputDataWidth+numStageCombined)
  initNullFunc(zeroOut)

  if(finalStageCheck){ //It will output signals without ready signal confirm. Beware...
    io.dataOut.payload := Mux(io.dataOut.valid, butterfly2Inst.io.dataOut, zeroOut)
    loopFifoPopDataAddr := fsmCnt

    when(fsm.isActive(fsm.stateOutput) && (fftAddr_Reg(fftAddr_Reg.length-1)===(fftPoints / 2 - 1)) && validTimingReg(validTimingReg.length-1) ){
      outputEnable := True
    } elsewhen((fftAddr_Reg(fftAddr_Reg.length-1)===(fftPoints / 2 - 1)) && validTimingReg(validTimingReg.length-1)){
      outputEnable := False
    }
  } else{
    val outputCnt = Reg(UInt(fsmCnt.getBitsWidth bits)) init(0)
    outputCnt.setName("outputCnt")
    println("fsmCnt : "+fsmCnt)
    io.dataOut.payload := Mux(io.dataOut.valid,loopFifoPopDataMux,zeroOut)

    loopFifoPopDataAddr := outputEnable ? outputCnt | fsmCnt

    when(outputEnable === False){
      outputCnt := 0
    } otherwise{
      outputCnt := outputCnt+1
    }

    when(fsm.isActive(fsm.stateOutput) && (fsmCnt === (fftPoints / 2 - 1)) && validTiming) {
      outputEnable := True
    } elsewhen (outputCnt === (fftPoints / 2 - 1)) {
      outputEnable := False
    }
  }

  //control the output of loopFifo
  when(validTiming) {
    fsmCnt := fsmCnt + 1
  }

  //val loopFifoPopDataMux = cloneOf(loopFifoEven.io.popData)
  val loopFifoPopDataMuxCtrl = loopFifoPopDataAddr.lsb && (fsm.isActive(fsm.stateOutput) | fsm.isActive(fsm.stateLoop) | outputEnable)
  loopFifoPopDataMux := (loopFifoPopDataMuxCtrl) ? loopFifoOdd.io.popData | loopFifoEven.io.popData
  println("loopFifoPopDataMux : " + loopFifoPopDataMux)
  loopFifoOdd.io.pop := False
  loopFifoEven.io.pop := False
  when(fsm.isActive(fsm.stateLoop) | fsm.isActive(fsm.stateOutput)) {
    loopFifoEven.io.pop := loopFifoPopDataAddr.lsb ? False | True
    loopFifoOdd.io.pop := loopFifoPopDataAddr.lsb ? True | False
  } otherwise{
    if(finalStageCheck){
      loopFifoEven.io.pop := False
      loopFifoOdd.io.pop := False
    } else{
      loopFifoEven.io.pop := (loopFifoPopDataAddr.lsb ? False | True) & outputEnable
      loopFifoOdd.io.pop := (loopFifoPopDataAddr.lsb ? True | False) & outputEnable
    }
  }

  val ioInputResized = cloneOf(butterflyInput)
  val loopFifoPopResized = cloneOf(butterflyInput)
  ioInputResized := io.dataIn.payload.resized
  loopFifoPopResized := loopFifoPopDataMux.resized
  butterflyInput := (fsm.isActive(fsm.stateInput)) ? ioInputResized | loopFifoPopResized
}

// CSG : Constant Structure Geometry
// forFFT = true, for FFT
// forFFT = false, for IFFT


// The output of FftByCsgRadix2 is not in order,X0 X1 X2 X3 ...., but inverse addr.
// for example as 64 points, the output is X0, X32, X16, X48,....
case class FftByCsgRadix2(fftPoints : Int, inputDataWidth : Int , rotationFactorWidth : Int, forFFT : Boolean) extends Component{
  var fftPower = log2Up(fftPoints)

  val io = new Bundle {
    val dataIn = slave(Stream(ButterflyDataIf(inputDataWidth)))
    val dataOut = out(FftDataOutIf(inputDataWidth))
    val giTypeIn = in(UInt(GlobalDefine().giTypeWidth bits))
    val giTypeOut = out(UInt(GlobalDefine().giTypeWidth bits))
  }
  // To plan numStageCombined for each combined stages
  //var stageCombinePlan = Array(3,3) //拆成兩個大階段，每大階段有3個fft stages
  var stageCombinePlan = Array(6) //只使用一個大階段完成fft運算

  println("stageCombinePlan.sum: "+stageCombinePlan.sum)
  require(stageCombinePlan.sum == fftPower)
  val fftStages = for(stagePlan <- 0 until stageCombinePlan.length) yield{
    if( stageCombinePlan == 0){
      CsgFftRadix2Stage(fftPoints, inputDataWidth, 18, stagePlan, stageCombinePlan(stagePlan), forFFT)
    }
    else{
      var previousTotalStageNum = 0
      for(index <- 0 until stagePlan){
        previousTotalStageNum = previousTotalStageNum + stageCombinePlan(index)
        println("stagePlan , previousTotalStageNum : "+stagePlan+previousTotalStageNum)
      }
      CsgFftRadix2Stage(fftPoints, inputDataWidth+previousTotalStageNum, 18, stagePlan, stageCombinePlan(stagePlan), forFFT)
    }
  }

  for(index <- 0 until fftStages.length-1){
    fftStages(index+1).io.dataIn <> fftStages(index).io.dataOut
  }
  fftStages(0).io.dataIn <> io.dataIn
  //io.dataOut <> fftStage(fftStage.length-1).io.dataOut

  //Shrink the output datawidth
  //According the simulation on pycharm for 64 points FFT, the max abs output value needs 20 bits(already include sign bit) when input data width is 16 bits.
  var validOutputWidth = inputDataWidth
  if(fftPoints == 64){
    validOutputWidth = inputDataWidth + 4
    println("validoutputWidth : "+validOutputWidth)
  }
  require(fftStages(fftStages.length-1).io.dataOut.aData.I.getBitsWidth >=  validOutputWidth)
  println("fftStage(fftStages.length - 1).io.dataOut.data.aData.I((validoutputWidth - 1) downto (validoutputWidth - 1 - (inputDataWidth - 1)))"+fftStages(fftStages.length - 1).io.dataOut.aData.I((validOutputWidth - 1) downto (validOutputWidth - 1 - (inputDataWidth - 1))))

  io.dataOut.data.aData.I := fftStages(fftStages.length - 1).io.dataOut.aData.I((validOutputWidth - 1) downto (validOutputWidth - 1 - (inputDataWidth - 1)))
  io.dataOut.data.aData.Q := fftStages(fftStages.length - 1).io.dataOut.aData.Q((validOutputWidth - 1) downto (validOutputWidth - 1 - (inputDataWidth - 1)))
  io.dataOut.data.bData.I := fftStages(fftStages.length - 1).io.dataOut.bData.I((validOutputWidth - 1) downto (validOutputWidth - 1 - (inputDataWidth - 1)))
  io.dataOut.data.bData.Q := fftStages(fftStages.length - 1).io.dataOut.bData.Q((validOutputWidth - 1) downto (validOutputWidth - 1 - (inputDataWidth - 1)))
  io.dataOut.valid := fftStages(fftStages.length - 1).io.dataOut.valid

  //symbolTypeIn and symbolTypeOut control
  val giTypeInRegAddr = Reg(UInt(1 bits))
  val giTypeInReg = Vec.fill(2)(Reg(cloneOf(io.giTypeIn)))
  io.giTypeOut := giTypeInReg(giTypeInRegAddr)
  when(io.dataIn.fire.rise()){
    when(io.giTypeIn === GlobalDefine().ltfGiType){
      for(index <- 0 until giTypeInReg.length){
        giTypeInReg(index) := GlobalDefine().ltfGiType
      }
      giTypeInRegAddr := 0
    } otherwise{
      giTypeInReg(0) := io.giTypeIn
      giTypeInReg(1) := giTypeInReg(0)
      giTypeInRegAddr := ~giTypeInRegAddr
    }
  }
  when(io.dataOut.valid.fall()){
    giTypeInRegAddr := ~giTypeInRegAddr
  }

}

case class FftRadix2Stage(fftPoints : Int, inputDataWidth : Int, rotationFactorWidth : Int, stagePlan : Int) extends Component{
  require(isPow2(fftPoints))
  var fftPower = log2Up(fftPoints)
  require(stagePlan < fftPower)

  val io = new Bundle{
    val dataIn = slave(Stream(FftDataIF(fftPoints, inputDataWidth)))
    val dataOut = master(Stream(FftDataIF(fftPoints, inputDataWidth+1)))
  }

  val dataReg = Reg(FftDataIF(fftPoints, inputDataWidth+1))

  //set default output value
  io.dataIn.ready := False
  io.dataOut.valid := False

  //stateMachine
  var finalStageCheck = (stagePlan == (fftPower-1))

  val butterfly2Inst = (!finalStageCheck) generate Butterfly2(inputDataWidth, GlobalDefine().rotationFactorWidth)
  val butterfly2ForFinalStageInst = (finalStageCheck) generate Butterfly2ForFinalStage(inputDataWidth)
  val fftCoeeRomInst = (!finalStageCheck) generate FftCoeeRom(rotationFactorWidth, fftPoints >> stagePlan, forFFT = false)

  var butterflyDelayValue = 2
  if(finalStageCheck){
    butterflyDelayValue = butterfly2ForFinalStageInst.validOutputDelay
  }else{
    butterflyDelayValue = butterfly2Inst.validOutputDelay
  }

  val fftAddrA_Reg = Reg(Vec.fill(butterflyDelayValue)(UInt(fftPower bits)))
  val fftAddrB_Reg = Reg(Vec.fill(butterflyDelayValue)(UInt(fftPower bits)))
  val validTimingReg = Reg(Vec.fill(butterflyDelayValue)(Bool()))

  val fsm = new StateMachine{
    val cnt = Reg(UInt(fftPower-1 bits)) init(0)
    val validTiming = False

    val stateIdle : State = new State with EntryPoint {
       whenIsActive{
       when(io.dataIn.valid){
         io.dataIn.ready := True

         //load data
         for (index <- 0 until fftPoints ){
           dataReg.data(index).I := B(io.dataIn.data(index).I(inputDataWidth - 1), io.dataIn.data(index).I).asSInt
           dataReg.data(index).Q := B(io.dataIn.data(index).Q(inputDataWidth - 1), io.dataIn.data(index).Q).asSInt
         }

         goto(stateFFT)
       }
      }
    }

    val stateFFT : State = new State{
      onEntry(cnt := 0)
      whenIsActive{
       validTiming := True
       cnt := cnt + 1
       when(cnt === (fftPoints/2 - 1)){
         goto(stateWaitPipeline)
       }
      }
    }

    val stateWaitPipeline : State = new State{
      whenIsActive{
        io.dataOut.valid := !validTimingReg(butterflyDelayValue-1)
        when(io.dataOut.fire){
          goto(stateIdle)
        }
      }
    }

  } // end of stateMachine of fsm

  // addr control
  val fftAddrA = UInt(fftPower bits) simPublic()
  val fftAddrB = UInt(fftPower bits) simPublic()

  fftAddrA_Reg(0) := fftAddrA
  fftAddrB_Reg(0) := fftAddrB
  validTimingReg(0) := fsm.validTiming
  validTimingReg(0) init(False)

  for(index <- 1 until butterflyDelayValue){
    fftAddrA_Reg(index) := fftAddrA_Reg(index-1)
    fftAddrB_Reg(index) := fftAddrB_Reg(index-1)
    validTimingReg(index) := validTimingReg(index-1)
    validTimingReg(index) init(False)
  }

  val ctx = WhenBuilder()
  var fsmCntMsbBitLoc = fsm.cnt.getWidth - 1
  ctx.when(Bool(stagePlan == 0)){
    fftAddrA := B( False , fsm.cnt).asUInt
    fftAddrB := B( True  , fsm.cnt).asUInt
  }
  ctx.elsewhen(Bool(finalStageCheck)) {
    fftAddrA := B(fsm.cnt , False).asUInt
    fftAddrB := B(fsm.cnt ,  True).asUInt
  }
  ctx.otherwise{
    fftAddrA := B(fsm.cnt(fsmCntMsbBitLoc downto (fsmCntMsbBitLoc+1-stagePlan)), False ,fsm.cnt((fsmCntMsbBitLoc-stagePlan) downto 0)).asUInt
    fftAddrB := B(fsm.cnt(fsmCntMsbBitLoc downto (fsmCntMsbBitLoc+1-stagePlan)), True  ,fsm.cnt((fsmCntMsbBitLoc-stagePlan) downto 0)).asUInt
  }

  // add Butterfly2 and rotation coefficient table connection
  if(finalStageCheck){
    butterfly2ForFinalStageInst.io.dataInA := dataReg.data(fftAddrA).resized
    butterfly2ForFinalStageInst.io.dataInB := dataReg.data(fftAddrB).resized
    // save data back to dataReg
    when(validTimingReg(butterflyDelayValue - 1)) {
      dataReg.data(fftAddrA_Reg(butterflyDelayValue - 1)) := butterfly2ForFinalStageInst.io.dataOutA
      dataReg.data(fftAddrB_Reg(butterflyDelayValue - 1)) := butterfly2ForFinalStageInst.io.dataOutB
    }
  }else{
    butterfly2Inst.io.dataInA := dataReg.data(fftAddrA).resized
    butterfly2Inst.io.dataInB := dataReg.data(fftAddrB).resized
    butterfly2Inst.io.rotationFactor := fftCoeeRomInst.io.dataOut
    fftCoeeRomInst.io.addr := fsm.cnt.resized
    // save data back to dataReg
    when(validTimingReg(butterflyDelayValue - 1)) {
      dataReg.data(fftAddrA_Reg(butterflyDelayValue - 1)) := butterfly2Inst.io.dataOutA
      dataReg.data(fftAddrB_Reg(butterflyDelayValue - 1)) := butterfly2Inst.io.dataOutB
    }
  }

  println(" statePlan : "+ stagePlan)
  println("fftPoints>>statePlan : "+(fftPoints>>stagePlan))

  // for avoid null assign for out signals
  io.dataOut.data := dataReg.data
}


/*
*  以輸入位寬16 bit為列，BPSK最大值為2^14=16384，經過64點fft，資料會需要增加位寬5 bits，
*  在加上所需的sign bit，大致上輸出需要位寬為20 bits。
*  目前輸出位寬為22 bits，所以除了sign bit，MSB的部份可以捨棄2位元。
* */
case class FftByRadix2(fftPoints : Int, inputDataWidth : Int , rotationFactorWidth : Int) extends Component{
  var fftPower = log2Up(fftPoints)

  val io = new Bundle{
    val dataIn = slave(Stream(FftDataIF(fftPoints, inputDataWidth)))
    val dataOut = master(Stream(FftDataIF(fftPoints, inputDataWidth+fftPower)))
  }

  val fftStage = for(stagePlan <- 0 until  fftPower) yield{
    FftRadix2Stage(64 , inputDataWidth+stagePlan , rotationFactorWidth ,stagePlan)
  }

  for(stagePlan <- 1 until  fftPower) {
    fftStage(stagePlan).io.dataIn <> fftStage(stagePlan-1).io.dataOut
  }
  fftStage(0).io.dataIn <> io.dataIn
  io.dataOut <> fftStage(fftPower-1).io.dataOut
}

object FftRadix2Stage {
  def main(args: Array[String]) {
    SpinalVerilog(FftRadix2Stage(64,16,18,0)) //Or SpinalVerilog
  }
}

object fftRadix2StageSim {
  def main(args: Array[String]) {
    if(false){
        SimConfig.withWave.doSim(FftRadix2Stage(64,16,18,5)){dut =>
          dut.clockDomain.forkStimulus(period = 10)
          dut.io.dataOut.ready #= false
          dut.clockDomain.assertReset()
          dut.clockDomain.waitSampling(10)
          dut.clockDomain.deassertReset()
          dut.clockDomain.waitSampling(10)
          dut.io.dataIn.valid #= true
          dut.clockDomain.waitSampling(1)
          while(dut.io.dataIn.ready.toBoolean == false){
            dut.clockDomain.waitSampling(1)
          }
          dut.io.dataIn.valid #= false
          dut.clockDomain.waitSampling(1)
          while (dut.io.dataOut.valid.toBoolean == false) {
            println("dut.fftAddrA.toInt : " , dut.fftAddrA.toInt)
            println("dut.fftAddrB.toInt : " , dut.fftAddrB.toInt)
            dut.clockDomain.waitSampling(1)
          }
          dut.io.dataOut.ready #= true
          dut.clockDomain.waitSampling(1)
          dut.io.dataOut.ready #= false
          dut.clockDomain.waitSampling(50)
        }
    }

    else if(false){
      SimConfig.withWave.doSim(CsgFftRadix2Stage(64, 16, 18, 1, 3, false)) { dut =>
        dut.io.dataIn.aData.I #= 100
        dut.io.dataIn.aData.Q #= 200
        dut.io.dataIn.bData.I #= 300
        dut.io.dataIn.bData.Q #= 400
        dut.clockDomain.forkStimulus(period = 10)
        dut.clockDomain.assertReset()
        dut.clockDomain.waitSampling(10)
        dut.clockDomain.deassertReset()
        dut.clockDomain.waitSampling(10)
        dut.io.dataIn.valid #= true
        dut.clockDomain.waitSampling(32)
        dut.io.dataIn.valid #= false
        dut.clockDomain.waitSampling(100)
      }
    }

    else if(true){
      import scala.io.Source
      import java.math.BigDecimal

      var File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/dataInRe.txt", "UTF-8")
      var lines = File.getLines().toArray
      File.close()
      val dataInRe = for (line <- lines) yield {
        println(line)
        var result = new BigDecimal(line)
        result.longValue()
      }

      File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/dataInIm.txt", "UTF-8")
      lines = File.getLines().toArray
      File.close()
      val dataInIm = for (line <- lines) yield {
        println(line)
        var result = new BigDecimal(line)
        result.longValue()
      }

      File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/dataOutRe.txt", "UTF-8")
      lines = File.getLines().toArray
      File.close()
      val dataOutRe = for (line <- lines) yield {
        println(line)
        var result = new BigDecimal(line)
        result.longValue()
      }

      File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/dataOutIm.txt", "UTF-8")
      lines = File.getLines().toArray
      File.close()
      val dataOutIm = for (line <- lines) yield {
        println(line)
        var result = new BigDecimal(line)
        result.longValue()
      }

      println("dataIn---------------------")
      for(index <- 0 until dataInRe.length){
        if(dataInIm(index) < 0){
          println(index + " : " + dataInRe(index) + dataInIm(index)+ "j")
        } else{
          println(index + " : " + dataInRe(index) + "+" + dataInIm(index)+ "j")
        }

      }
      println("---------------------------")

      println("dataOut---------------------")

      var scale = 16
      for (index <- 0 until dataOutRe.length) {
        if(dataOutIm(index) < 0){
          println(index + " : " + dataOutRe(index)/scale  + dataOutIm(index)/scale+ "j")
        } else{
          println(index + " : " + dataOutRe(index)/scale + "+" + dataOutIm(index)/scale+ "j")
        }

      }
      println("---------------------------")


      SimConfig.withWave.doSim(FftByCsgRadix2(64,16,18, false)) { dut =>

        dut.clockDomain.forkStimulus(period = 10)
        dut.clockDomain.assertReset()
        dut.clockDomain.waitSampling(10)
        dut.clockDomain.deassertReset()
        dut.clockDomain.waitSampling(10)

        dut.io.dataIn.valid #= true
        for(index <- 0 until 32){
          dut.io.dataIn.aData.I #= dataInRe(index)
          dut.io.dataIn.aData.Q #= dataInIm(index)
          dut.io.dataIn.bData.I #= dataInRe(index+32)
          dut.io.dataIn.bData.Q #= dataInIm(index+32)
          dut.clockDomain.waitSampling(1)
        }
        dut.io.dataIn.valid #= false

        dut.clockDomain.waitSampling(300)
      }
    }

    else if(false){
      import scala.io.Source
      import java.math.BigDecimal

      var File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/dataInRe.txt" , "UTF-8" )
      var lines = File.getLines().toArray
      File.close()
      val dataInRe = for(line <- lines) yield {
        println(line)
        var result = new BigDecimal(line)
        result.longValue()
      }

      File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/dataInIm.txt", "UTF-8")
      lines = File.getLines().toArray
      File.close()
      val dataInIm = for (line <- lines) yield {
        println(line)
        var result = new BigDecimal(line)
        result.longValue()
      }

      File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/dataOutRe.txt", "UTF-8")
      lines = File.getLines().toArray
      File.close()
      val dataOutRe = for (line <- lines) yield {
        println(line)
        var result = new BigDecimal(line)
        result.longValue()
      }

      File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/dataOutIm.txt", "UTF-8")
      lines = File.getLines().toArray
      File.close()
      val dataOutIm = for (line <- lines) yield {
        println(line)
        var result = new BigDecimal(line)
        result.longValue()
      }

      println("dataInRe : " + dataInRe)
      println("dataInIm : " + dataInIm)
      println("dataOutRe : " + dataOutRe)
      println("dataOutIm : " + dataOutIm)

      SimConfig.withWave.doSim(FftByRadix2(64, 16 , 18)){dut =>
        //Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.dataOut.ready #= false

        //assign input
        for(index <- 0 until 64){
          dut.io.dataIn.data(index).I #= dataInRe(index)
          dut.io.dataIn.data(index).Q #= dataInIm(index)
        }

        dut.clockDomain.assertReset()
        dut.clockDomain.waitSampling(10)
        dut.clockDomain.deassertReset()
        dut.clockDomain.waitSampling(10)

        //send valid signal
        dut.io.dataIn.valid #= true
        dut.clockDomain.waitSampling(1)
        while (dut.io.dataIn.ready.toBoolean == false) {
          dut.clockDomain.waitSampling(1)
        }
        dut.io.dataIn.valid #= false

        dut.clockDomain.waitSampling(1000)
      }
    }

    else if(false){
      SimConfig.withWave.doSim(Butterfly2LessMultiplier(16, 18)) { dut =>
        //Fork a process to generate the reset and the clock on the dut
        //val period = (1e12/1000000.toDouble).toLong //1M
        dut.clockDomain.forkStimulus(2)
        dut.clockDomain.assertReset()
        dut.io.dataIn.aData.I #= 200
        dut.io.dataIn.aData.Q #= 400
        dut.io.dataIn.bData.I #= 800
        dut.io.dataIn.bData.Q #= 400
        dut.io.rotationFactor.I #= pow(2,16).toInt
        dut.io.rotationFactor.Q #= 0
        dut.clockDomain.waitSampling(10)
        dut.clockDomain.deassertReset()
        dut.clockDomain.waitSampling(100)
      }

    }

    else if(false){
        SimConfig.withWave.doSim(Butterfly2(16,18)){dut =>
          //Fork a process to generate the reset and the clock on the dut
          var addr = 0
          dut.clockDomain.assertReset()
          dut.io.dataInA.I #= 400
          dut.io.dataInA.Q #= 300
          dut.io.dataInB.I #= 200
          dut.io.dataInB.Q #= 100
          dut.io.rotationFactor.I #= pow(2,16).toInt
          dut.io.rotationFactor.Q #= 0

          dut.clockDomain.forkStimulus(period = 10)
          dut.clockDomain.deassertReset()
          dut.clockDomain.waitSampling(10)
          for (addr <- 0 to 1024) {
            if((dut.io.dataInA.I.toInt+100) < pow(2,15)){
              dut.io.dataInA.I #= (dut.io.dataInA.I.toInt+100)
            }else{
              dut.io.dataInA.I #= -32000
            }
            if ((dut.io.dataInA.Q.toInt + 200) < pow(2, 15)) {
              dut.io.dataInA.Q #= (dut.io.dataInA.Q.toInt + 200)
            } else {
              dut.io.dataInA.Q #= -32000

            }
            if ((dut.io.dataInB.I.toInt + 300) < pow(2, 15)) {
              dut.io.dataInB.I #= (dut.io.dataInB.I.toInt + 300)
            } else {
              dut.io.dataInB.I #= -32000
            }
            if ((dut.io.dataInB.Q.toInt + 400) < pow(2, 15)) {
              dut.io.dataInB.Q #= (dut.io.dataInB.Q.toInt + 400)
            } else {
              dut.io.dataInB.Q #= -32000
              dut.io.rotationFactor.I #= 0
              dut.io.rotationFactor.Q #= (pow(2,16)* -1).toInt
            }

            println(addr + " pathA I : " + dut.io.dataInA.I.toInt)
            println(addr + " pathA Q : " + dut.io.dataInA.Q.toInt)
            println(addr + " pathB I : " + dut.io.dataInB.I.toInt)
            println(addr + " pathB Q : " + dut.io.dataInB.Q.toInt)

            dut.clockDomain.waitSampling(1)
          }
          dut.clockDomain.waitSampling(1000)
        }
    }

    else if(false){
      SimConfig.withWave.doSim(Butterfly2ForFinalStage(16)) { dut =>
        //Fork a process to generate the reset and the clock on the dut
        var addr = 0
        dut.clockDomain.forkStimulus(period = 10)
        dut.clockDomain.assertReset()
        dut.io.dataInA.I #= 400
        dut.io.dataInA.Q #= 300
        dut.io.dataInB.I #= 200
        dut.io.dataInB.Q #= 100

        dut.clockDomain.waitSampling(10)
        dut.clockDomain.deassertReset()
        dut.clockDomain.waitSampling(10)
        for (addr <- 0 to 1024) {
          if ((dut.io.dataInA.I.toInt + 100) < pow(2, 15)) {
            dut.io.dataInA.I #= (dut.io.dataInA.I.toInt + 100)
          } else {
            dut.io.dataInA.I #= -32000
          }
          if ((dut.io.dataInA.Q.toInt + 200) < pow(2, 15)) {
            dut.io.dataInA.Q #= (dut.io.dataInA.Q.toInt + 200)
          } else {
            dut.io.dataInA.Q #= -32000

          }
          if ((dut.io.dataInB.I.toInt + 300) < pow(2, 15)) {
            dut.io.dataInB.I #= (dut.io.dataInB.I.toInt + 300)
          } else {
            dut.io.dataInB.I #= -32000
          }
          if ((dut.io.dataInB.Q.toInt + 400) < pow(2, 15)) {
            dut.io.dataInB.Q #= (dut.io.dataInB.Q.toInt + 400)
          } else {
            dut.io.dataInB.Q #= -32000
          }

          println(addr + " pathA I : " + dut.io.dataInA.I.toInt)
          println(addr + " pathA Q : " + dut.io.dataInA.Q.toInt)
          println(addr + " pathB I : " + dut.io.dataInB.I.toInt)
          println(addr + " pathB Q : " + dut.io.dataInB.Q.toInt)

          dut.clockDomain.waitSampling(1)
        }
        dut.clockDomain.waitSampling(1000)
      }
    }

    else if(false){
        SimConfig.withWave.doSim(FftCoeeRom(GlobalDefine().rotationFactorWidth,64, forFFT = false)){dut =>
          //Fork a process to generate the reset and the clock on the dut
          var addr = 0
          dut.clockDomain.assertReset()
          dut.io.addr #= addr
          dut.clockDomain.forkStimulus(period = 10)
          dut.clockDomain.deassertReset()
          dut.clockDomain.waitSampling(10)
          for(addr <- 0 to 1024 ){
            dut.io.addr #= (addr & 31)
            dut.clockDomain.waitSampling(1)
          }

          dut.clockDomain.waitSampling(1000)
        }
    }

  }
}

