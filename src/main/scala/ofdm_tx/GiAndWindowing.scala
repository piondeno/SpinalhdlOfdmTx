package ofdm_tx

import ofdm_tx.ComplexDataType.{initRegFunc, setValueAsZero}
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._

/*
* The function of GpAndWindowing() is described as below
* First. The output of IFFT is reverse-addr-bits-order. Reorder the output of IFFT.
* Second. GP
* Third. Windowing
* The multiplier is implementd by shifter and adder, please refer to Windowing Coee in my google driver
* */

case class GiAndWindowing() extends Component {
  val io = new Bundle {
    val dataIn = in(FftDataOutIf(GlobalDefine().modulationDataOutWidth))
    val newPackage = in(Bool())
    val shortGp = in(Bool())
    val dataOut = out(FftDataOutIf(GlobalDefine().modulationDataOutWidth))
  }

  val fftPoints = GlobalDefine().fftPoints
  val giLen = 16 // 64/4=16 for normal GI, short GI only 8 points
  val windowLen = 5
  val fsmCnt = Reg(UInt(log2Up(fftPoints + giLen + windowLen) bits)) init(0)

  // 0.5 - 0.5*cos(36 degree) = 0.0955 ~= 1/8 + 1/16 +1/32
  // 0.5 - 0.5*cos(72 degree) ~= 0.096
  require((windowLen == 4) || (windowLen == 5) || (windowLen == 6))

  //  //order the output of FFT
  val memPreHalf = Mem(ComplexDataType(GlobalDefine().modulationDataOutWidth ), wordCount = (fftPoints/2))
  val memPostHalf = Mem(ComplexDataType(GlobalDefine().modulationDataOutWidth), wordCount = (fftPoints/2))
  val writeAddr = UInt(log2Up(fftPoints/2) bits)

  writeAddr :=  fsmCnt.resize(writeAddr.getWidth).reversed
  memPreHalf.write(
    enable = io.dataIn.valid,
    address = writeAddr,
    data = io.dataIn.data.aData
  )
  memPostHalf.write(
    enable = io.dataIn.valid,
    address = writeAddr,
    data = io.dataIn.data.bData
  )
  val memPreHalfOut = memPreHalf.readAsync(fsmCnt.resized)
  val memPostHalfOut = memPostHalf.readAsync(fsmCnt.resized)
  val reorderOut = cloneOf(io.dataIn.data.bData)
  val reorderOutZero = cloneOf(io.dataIn.data.bData)
  val reorderOutValid = Bool()
  val reorderOutMuxCondition = fsmCnt(writeAddr.getWidth)
  val reorderOutMux = Mux(reorderOutMuxCondition,memPostHalfOut,memPreHalfOut)
  setValueAsZero(reorderOutZero)
  reorderOut := Mux(reorderOutValid,reorderOutMux,reorderOutZero)

  val fsm = new StateMachine{
    val stateWrite : State = new State with EntryPoint {
      onEntry(fsmCnt := 0)
      whenIsActive{
        when(io.dataIn.valid){
          fsmCnt := fsmCnt + 1
          when(fsmCnt === (fftPoints/2 -1)){
            goto(stateOutputGiWindowOverlap)
          }
        }
      }
    }

    val stateOutputGiWindowOverlap = new State{
      val endTiming = Mux(io.shortGp, U(fftPoints - 8 + windowLen -1, fsmCnt.getWidth bits), U(fftPoints - 16 + windowLen -1, fsmCnt.getWidth bits) )

      onEntry{
        when(io.shortGp){
          fsmCnt := fftPoints - 8
        }otherwise{
          fsmCnt := fftPoints - 16
        }
      }
      whenIsActive{
        reorderOutValid := True
        fsmCnt := fsmCnt + 1
        when(fsmCnt === endTiming){
          goto(stateOutputGi)
        }
      }
    }

    val stateOutputGi = new State{
      whenIsActive{
        reorderOutValid := True
        fsmCnt := fsmCnt + 1
        when(fsmCnt === (fftPoints-1)){
          goto(stateOutputData)
        }
      }
    }

    val stateOutputData = new State {
      onEntry{
        fsmCnt := 0
      }
      whenIsActive {
        reorderOutValid := True
        fsmCnt := fsmCnt + 1
        when(fsmCnt === (fftPoints - 1)) {
          goto(stateOutputWindowing)
        }
      }
    }

    val stateOutputWindowing = new State{
      onEntry{
        fsmCnt := 0
      }
      whenIsActive{
        reorderOutValid := True
        fsmCnt := fsmCnt + 1
        when(fsmCnt === (windowLen - 1)) {
          goto(stateWrite)
        }
      }
    }
  }

  reorderOutValid := Mux(fsm.isActive(fsm.stateWrite), False, True)

  //implement windowing, use delay chain to implement.
  val windowingReg = Vec.fill(windowLen)(cloneOf(reorderOut))
  val windowingZero = cloneOf(reorderOut)
  val windowingMuxInput = cloneOf(reorderOut)
  val windowingRegEn = False
  val windowAdderOut = cloneOf(reorderOut)
  setValueAsZero(windowingZero)
  windowingMuxInput := Mux(fsm.isActive(fsm.stateOutputWindowing), windowAdderOut, windowingZero)

  when(windowingRegEn){
    for(index <- 1 until windowLen){
      windowingReg(index) := windowingReg(index-1)
    }
    windowingReg(0) := windowingMuxInput
  }

  windowingReg.foreach(initRegFunc)
  when(io.newPackage){
    windowingReg.foreach(setValueAsZero)
  }otherwise{
    when(fsm.isActive(fsm.stateOutputGiWindowOverlap) || fsm.isActive(fsm.stateOutputWindowing)){
      windowingRegEn := True
    }
  }

  // The overlap timing of GI and Windowing
  val windowingCnt = Reg(UInt(windowLen bits)) init(0)
  val timingOverlapGiWindow = fsm.isActive(fsm.stateOutputGiWindowOverlap)
  val dataInDivBy2_I = reorderOut.I |>> 1
  val dataInDivBy2_Q = reorderOut.Q |>> 1
  val dataInDivBy4_I = reorderOut.I >> 2
  val dataInDivBy4_Q = reorderOut.Q >> 2
  val dataInDivBy4_I2SC = (~dataInDivBy4_I + 1)
  val dataInDivBy4_Q2SC = (~dataInDivBy4_Q + 1)
  val dataInDivBy16_I = reorderOut.I >> 4
  val dataInDivBy16_Q = reorderOut.Q >> 4
  val dataInDivBy16_I2SC = (~dataInDivBy16_I + 1)
  val dataInDivBy16_Q2SC = (~dataInDivBy16_Q + 1)

  val windowAdderInputZero = cloneOf(reorderOut)
  setValueAsZero(windowAdderInputZero)
  val windowAdderInput = ButterflyDataIf(reorderOut.I.getBitsWidth)
  val overlapOutput = cloneOf(reorderOut)
  val finalOutput = Reg(cloneOf(reorderOut))
  println("windowAdderInput.aData.I + windowAdderInput.bData.I len = " + (windowAdderInput.aData.I + windowAdderInput.bData.I).getWidth)
  windowAdderOut.I := windowAdderInput.aData.I + windowAdderInput.bData.I
  windowAdderOut.Q := windowAdderInput.aData.Q + windowAdderInput.bData.Q

  when(fsm.isActive(fsm.stateOutputGiWindowOverlap) || fsm.isActive(fsm.stateOutputWindowing)){
    windowingCnt := windowingCnt + 1
  } otherwise{
    windowingCnt := 0
  }

  overlapOutput.I := windowAdderOut.I + windowingReg(windowingReg.length-1).I
  overlapOutput.Q := windowAdderOut.Q + windowingReg(windowingReg.length-1).Q

  when(fsm.isActive(fsm.stateOutputGiWindowOverlap)) {
    finalOutput := overlapOutput
  } elsewhen(fsm.isActive(fsm.stateOutputGi) || fsm.isActive(fsm.stateOutputData)){
    finalOutput := reorderOut
  } otherwise{
    finalOutput := windowAdderInputZero
  }

  when(fsm.isActive(fsm.stateOutputGiWindowOverlap)){
    switch(windowingCnt){
      is(0){
        windowAdderInput.aData := windowAdderInputZero
        windowAdderInput.bData.I := dataInDivBy16_I.resized
        windowAdderInput.bData.Q := dataInDivBy16_Q.resized
      }
      is(1) {
        windowAdderInput.aData := windowAdderInputZero
        windowAdderInput.bData.I := dataInDivBy4_I.resized
        windowAdderInput.bData.Q := dataInDivBy4_Q.resized
      }
      is(2) {
        windowAdderInput.aData := windowAdderInputZero
        windowAdderInput.bData.I := dataInDivBy2_I.resized
        windowAdderInput.bData.Q := dataInDivBy2_Q.resized
      }
      is(3) {
        windowAdderInput.aData := reorderOut
        windowAdderInput.bData.I := dataInDivBy4_I2SC.resized
        windowAdderInput.bData.Q := dataInDivBy4_Q2SC.resized
      }
      default{
        windowAdderInput.aData := reorderOut
        windowAdderInput.bData.I := dataInDivBy16_I2SC.resized
        windowAdderInput.bData.Q := dataInDivBy16_Q2SC.resized
      }
    }
  } elsewhen(fsm.isActive(fsm.stateOutputWindowing)){
    switch(windowingCnt) {
      is(4) {
        windowAdderInput.aData := windowAdderInputZero
        windowAdderInput.bData.I := dataInDivBy16_I.resized
        windowAdderInput.bData.Q := dataInDivBy16_Q.resized
      }
      is(3) {
        windowAdderInput.aData := windowAdderInputZero
        windowAdderInput.bData.I := dataInDivBy4_I.resized
        windowAdderInput.bData.Q := dataInDivBy4_Q.resized
      }
      is(2) {
        windowAdderInput.aData := windowAdderInputZero
        windowAdderInput.bData.I := dataInDivBy2_I.resized
        windowAdderInput.bData.Q := dataInDivBy2_Q.resized
      }
      is(1) {
        windowAdderInput.aData := reorderOut
        windowAdderInput.bData.I := dataInDivBy4_I2SC.resized
        windowAdderInput.bData.Q := dataInDivBy4_Q2SC.resized
      }
      default {
        windowAdderInput.aData := reorderOut
        windowAdderInput.bData.I := dataInDivBy16_I2SC.resized
        windowAdderInput.bData.Q := dataInDivBy16_Q2SC.resized
      }
    }
  } otherwise {
    windowAdderInput.aData := windowAdderInputZero
    windowAdderInput.bData := windowAdderInputZero
  }


}

case class reorderIfftAndAddGiDataOutIF(width : Int) extends Bundle{
  val data = ComplexDataType(width)
  val valid = Bool()
}

case class ReorderIfftAndAddGi() extends Component {
  val io = new Bundle {
    val dataIn = in(FftDataOutIf(GlobalDefine().modulationDataOutWidth))
    val giTypeIn = in(UInt(GlobalDefine().giTypeWidth bits))
    val dataOut = master(Flow(ComplexDataType(GlobalDefine().modulationDataOutWidth)))
  }

  // giTypeIn
  val giTypeInReg = Reg(cloneOf(io.giTypeIn)) init(GlobalDefine().ltfGiType)
  when(io.dataIn.valid.rise()){
    giTypeInReg := io.giTypeIn
  }

  val fftPoints = GlobalDefine().fftPoints
  val fsmCnt = Reg(UInt(log2Up(fftPoints) bits)) init(0)

  //  //order the output of FFT
  val memPreHalf = Mem(ComplexDataType(GlobalDefine().modulationDataOutWidth ), wordCount = (fftPoints/2))
  val memPostHalf = Mem(ComplexDataType(GlobalDefine().modulationDataOutWidth), wordCount = (fftPoints/2))
  val writeAddr = UInt(log2Up(fftPoints/2) bits)

  writeAddr :=  fsmCnt.resize(writeAddr.getWidth).reversed
  memPreHalf.write(
    enable = io.dataIn.valid,
    address = writeAddr,
    data = io.dataIn.data.aData
  )
  memPostHalf.write(
    enable = io.dataIn.valid,
    address = writeAddr,
    data = io.dataIn.data.bData
  )
  val memPreHalfOut = memPreHalf.readAsync(fsmCnt.resized)
  val memPostHalfOut = memPostHalf.readAsync(fsmCnt.resized)
  val reorderOut = cloneOf(io.dataIn.data.bData)
  val reorderOutZero = cloneOf(io.dataIn.data.bData)
  val reorderOutValid = False
  val reorderOutMuxCondition = fsmCnt(writeAddr.getWidth)
  val reorderOutMux = Mux(reorderOutMuxCondition,memPostHalfOut,memPreHalfOut)
  setValueAsZero(reorderOutZero)
  reorderOut := Mux(reorderOutValid,reorderOutMux,reorderOutZero)

  val giInitVal = cloneOf(fsmCnt)
  switch(giTypeInReg){
    is(GlobalDefine().ltfGiType){
      giInitVal := fftPoints - 32
    }
    is(GlobalDefine().shortGiType){
      giInitVal := fftPoints - 8
    }
    default{
      giInitVal := fftPoints - 16
    }
  }

  val fsm = new StateMachine{
    val stateWrite : State = new State with EntryPoint {
      onEntry(fsmCnt := 0)
      whenIsActive{
        when(io.dataIn.valid){
          fsmCnt := fsmCnt + 1
          when(fsmCnt === (fftPoints/2 -1)){
            goto(stateOutputGi)
          }
        }
      }
    }

    val stateOutputGi = new State{
      onEntry{
        fsmCnt := giInitVal
      }
      whenIsActive{
        reorderOutValid := True
        fsmCnt := fsmCnt + 1
        when(fsmCnt === (fftPoints-1)){
          goto(stateOutputData)
        }
      }
    }

    val stateOutputData = new State {
      whenIsActive {
        reorderOutValid := True
        fsmCnt := fsmCnt + 1
        when(fsmCnt === (fftPoints - 1)) {
          when(giTypeInReg === GlobalDefine().ltfGiType){
            goto(stateOutputDataRepeat)
          } otherwise{
            goto(stateWrite)
          }
        }
      }
    }

    val stateOutputDataRepeat = new State {
      whenIsActive {
        reorderOutValid := True
        fsmCnt := fsmCnt + 1
        when(fsmCnt === (fftPoints - 1)) {
          goto(stateWrite)
        }
      }
    }

  }

  io.dataOut.valid := reorderOutValid
  io.dataOut.payload := reorderOut
}

