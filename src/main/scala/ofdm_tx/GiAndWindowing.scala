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
  val windowLen = 4
  val fsmCnt = Reg(UInt(log2Up(fftPoints + giLen + windowLen) bits)) init(0)

  // 0.5 - 0.5*cos(36 degree) = 0.0955 ~= 1/8 + 1/16 +1/32
  // 0.5 - 0.5*cos(72 degree) ~= 0.096
  require((windowLen % 2) == 0)

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
  val giOut = cloneOf(io.dataIn.data.bData)
  val giOutZero = cloneOf(io.dataIn.data.bData)
  val giOutValid = False
  val giOutMuxCondition = fsmCnt(writeAddr.getWidth)
  val giOutMux = Mux(giOutMuxCondition,memPostHalfOut,memPreHalfOut)
  setValueAsZero(giOutZero)
  giOut := Mux(giOutValid,giOutMux,giOutZero)

  val fsm = new StateMachine{
    val stateWrite : State = new State with EntryPoint {
      onEntry(fsmCnt := 0)
      whenIsActive{
        when(io.dataIn.valid){
          fsmCnt := fsmCnt + 1
          when(fsmCnt === (fftPoints/2 -1)){
            goto(stateOutputGP)
          }
        }
      }
    }

    val stateOutputGP = new State{
      onEntry{
        when(io.shortGp){
          fsmCnt := fftPoints - 8
        }otherwise{
          fsmCnt := fftPoints - 16
        }
      }
      whenIsActive{
        giOutValid := True
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
        giOutValid := True
        fsmCnt := fsmCnt + 1
        when(fsmCnt === (fftPoints - 1)) {
          goto(stateWrite)
        }
      }
    }
  }
}

/*
case class GpAndWindowing() extends Component{
  val io = new Bundle{
    val dataIn = in(FftDataOutIf(GlobalDefine().modulationDataOutWidth))
    val newPackage = in(Bool())
    val shortGp = in(Bool())
    val dataOut = out(FftDataOutIf(GlobalDefine().modulationDataOutWidth))
  }

  val fftPoints = GlobalDefine().fftPoints
  val gpLen = 16 // 64/4=16 for normal GP, short GP only 8 points
  val windowLen = 4
  val fsmCnt = Reg(UInt(log2Up(fftPoints+gpLen+windowLen) bits))

  // 0.5 - 0.5*cos(36 degree) = 0.0955 ~= 1/8 + 1/16 +1/32
  // 0.5 - 0.5*cos(72 degree) ~= 0.096
  require((windowLen%2) == 0)

  //state machine
  val fsm = new StateMachine{

    val statePreHalfWindow : State = new State with EntryPoint {
      onEntry( fsmCnt := 0)
      whenIsActive{
        when(io.dataIn.valid){
          fsmCnt := fsmCnt + 1
        }
        when( fsmCnt === (windowLen-1) ){
          goto(stateGpDataIn)
        }
      }
    }

    val stateGpDataIn = new State{
      whenIsActive{
        when(io.dataIn.valid) {
          fsmCnt := fsmCnt + 1
        }
        when(fsmCnt === (windowLen + gpLen - 1)) {
          goto(stateDataOut)
        }
      }
    }

    val stateDataOut = new State{
      whenIsActive{
        when(io.dataIn.valid) {
          fsmCnt := fsmCnt + 1
        }
        when(fsmCnt === (fftPoints - 1)) {
          goto(stateGpDataOut)
        }
      }
    }

    val stateGpDataOut = new State{
      whenIsActive{
        fsmCnt := fsmCnt + 1
        when(fsmCnt === (fftPoints+gpLen-1)){
          goto(statePreHalfWindow)
        }
      }
    }
  }

  val gpReg = Vec.fill(gpLen + windowLen)(cloneOf(io.dataIn.data))
  gpReg.foreach(initRegFunc)
  when(io.newPackage) { //clean content for first symbol
    for (index <- 0 until gpReg.length) {
      gpReg(index).I := 0
      gpReg(index).Q := 0
    }
  } elsewhen (io.dataIn.valid && (fsmCnt < (gpLen + windowLen))) {
    gpReg(0) := io.dataIn.data
    for (index <- 1 until gpReg.length) {
      gpReg(index) := gpReg(index - 1)
    }
  } elsewhen(fsm.isActive(fsm.stateGpDataOut)) {
    gpReg(0).I := 0
    gpReg(0).Q := 0
    for (index <- 1 until gpReg.length) {
      gpReg(index) := gpReg(index - 1)
    }
  }

  // Windowing
  val gpOutValid = fsm.isActive(fsm.stateGpDataOut)
  val dataInReg = RegNext(io.dataIn.data)
  val currentSymbolAdderIn_I = Vec.fill(windowLen)(cloneOf(io.dataIn.data.I))
  val currentSymbolAdderIn_Q = Vec.fill(windowLen)(cloneOf(io.dataIn.data.I))
  val currentSymbolAdderOut = cloneOf(io.dataIn.data)
  val previousSymbolAdderIn_I = Vec.fill(windowLen)(cloneOf(io.dataIn.data.I))
  val previousSymbolAdderIn_Q = Vec.fill(windowLen)(cloneOf(io.dataIn.data.I))
  val previousSymbolAdderOut = cloneOf(io.dataIn.data)
  val windowingOut = Reg(cloneOf(io.dataIn.data))

  if(windowLen == 4){ //currentSymbol control
    val currentSymbolMiddleAdder_I = Vec.fill(windowLen/2)(Reg(cloneOf(io.dataIn.data.I)))
    val currentSymbolMiddleAdder_Q = Vec.fill(windowLen/2)(Reg(cloneOf(io.dataIn.data.I)))
    val previousSymbolMiddleAdder_I = Vec.fill(windowLen/2)(Reg(cloneOf(io.dataIn.data.I)))
    val previousSymbolMiddleAdder_Q = Vec.fill(windowLen/2)(Reg(cloneOf(io.dataIn.data.I)))

    for(index <- 0 until currentSymbolMiddleAdder_I.length){
      currentSymbolMiddleAdder_I(index) := currentSymbolAdderIn_I(index*(windowLen/2)) + currentSymbolAdderIn_I(index*(windowLen/2)+1)
      currentSymbolMiddleAdder_Q(index) := currentSymbolAdderIn_Q(index*(windowLen/2)) + currentSymbolAdderIn_Q(index*(windowLen/2)+1)
      previousSymbolMiddleAdder_I(index) := previousSymbolAdderIn_I(index * (windowLen / 2)) + previousSymbolAdderIn_I(index * (windowLen / 2) + 1)
      previousSymbolMiddleAdder_Q(index) := previousSymbolAdderIn_Q(index * (windowLen / 2)) + previousSymbolAdderIn_Q(index * (windowLen / 2) + 1)
    }

    currentSymbolAdderOut.I := currentSymbolMiddleAdder_I(0) + currentSymbolMiddleAdder_I(1)
    currentSymbolAdderOut.Q := currentSymbolMiddleAdder_Q(0) + currentSymbolMiddleAdder_Q(1)
    previousSymbolAdderOut.I := previousSymbolMiddleAdder_I(0) + previousSymbolMiddleAdder_I(1)
    previousSymbolAdderOut.Q := previousSymbolMiddleAdder_Q(0) + previousSymbolMiddleAdder_Q(1)
    windowingOut.I := currentSymbolAdderOut.I + previousSymbolAdderOut.I
    windowingOut.Q := currentSymbolAdderOut.Q + previousSymbolAdderOut.Q

    when(fsm.isActive(fsm.statePreHalfWindow)){
      switch(fsmCnt(1 downto 0)){
        is(0){ //0.0955 for currentSymbol, 0.9045 for previousSymbol
          currentSymbolAdderIn_I(0) := io.dataIn.data.I |>> 5 // divid by 32
          currentSymbolAdderIn_Q(0) := io.dataIn.data.Q |>> 5
          currentSymbolAdderIn_I(1) := io.dataIn.data.I |>> 4 // divid by 16
          currentSymbolAdderIn_Q(1) := io.dataIn.data.Q |>> 4
          currentSymbolAdderIn_I(2) := 0
          currentSymbolAdderIn_Q(2) := 0
          currentSymbolAdderIn_I(3) := 0
          currentSymbolAdderIn_Q(3) := 0

          previousSymbolAdderIn_I(0) := gpReg(gpReg.length-1).I |>> 5 // divid by 32
          previousSymbolAdderIn_Q(0) := gpReg(gpReg.length-1).Q |>> 5
          previousSymbolAdderIn_I(1) := gpReg(gpReg.length-1).I |>> 3 // divid by 8
          previousSymbolAdderIn_Q(1) := gpReg(gpReg.length-1).Q |>> 3
          previousSymbolAdderIn_I(2) := gpReg(gpReg.length-1).I |>> 1 // divid by 2
          previousSymbolAdderIn_Q(2) := gpReg(gpReg.length-1).Q |>> 1
          previousSymbolAdderIn_I(3) := gpReg(gpReg.length-1).I |>> 2 // divid by 4
          previousSymbolAdderIn_Q(3) := gpReg(gpReg.length-1).Q |>> 2
        }
        is(1){ //0.3455 for currentSymbol, 0.6545 for previousSymbol
          currentSymbolAdderIn_I(0) := io.dataIn.data.I |>> 5 // divid by 32
          currentSymbolAdderIn_Q(0) := io.dataIn.data.Q |>> 5
          currentSymbolAdderIn_I(1) := io.dataIn.data.I |>> 4 // divid by 16
          currentSymbolAdderIn_Q(1) := io.dataIn.data.Q |>> 4
          currentSymbolAdderIn_I(2) := io.dataIn.data.I |>> 2 // divid by 4
          currentSymbolAdderIn_Q(2) := io.dataIn.data.Q |>> 2
          currentSymbolAdderIn_I(3) := 0
          currentSymbolAdderIn_Q(3) := 0

          previousSymbolAdderIn_I(0) := gpReg(gpReg.length-1).I |>> 5 // divid by 32
          previousSymbolAdderIn_Q(0) := gpReg(gpReg.length-1).Q |>> 5
          previousSymbolAdderIn_I(1) := gpReg(gpReg.length-1).I |>> 3 // divid by 8
          previousSymbolAdderIn_Q(1) := gpReg(gpReg.length-1).Q |>> 3
          previousSymbolAdderIn_I(2) := gpReg(gpReg.length-1).I |>> 1 // divid by 2
          previousSymbolAdderIn_Q(2) := gpReg(gpReg.length-1).Q |>> 1
          previousSymbolAdderIn_I(3) := 0
          previousSymbolAdderIn_Q(3) := 0
        }
        is(2) { //0.6545 for currentSymbol, 0.3455 for previousSymbol
          currentSymbolAdderIn_I(0) := io.dataIn.data.I |>> 5 // divid by 32
          currentSymbolAdderIn_Q(0) := io.dataIn.data.Q |>> 5
          currentSymbolAdderIn_I(1) := io.dataIn.data.I |>> 3 // divid by 8
          currentSymbolAdderIn_Q(1) := io.dataIn.data.Q |>> 3
          currentSymbolAdderIn_I(2) := io.dataIn.data.I |>> 1 // divid by 2
          currentSymbolAdderIn_Q(2) := io.dataIn.data.Q |>> 1
          currentSymbolAdderIn_I(3) := 0
          currentSymbolAdderIn_Q(3) := 0

          previousSymbolAdderIn_I(0) := gpReg(gpReg.length-1).I |>> 5 // divid by 32
          previousSymbolAdderIn_Q(0) := gpReg(gpReg.length-1).Q |>> 5
          previousSymbolAdderIn_I(1) := gpReg(gpReg.length-1).I |>> 4 // divid by 16
          previousSymbolAdderIn_Q(1) := gpReg(gpReg.length-1).Q |>> 4
          previousSymbolAdderIn_I(2) := gpReg(gpReg.length-1).I |>> 2 // divid by 4
          previousSymbolAdderIn_Q(2) := gpReg(gpReg.length-1).Q |>> 2
          previousSymbolAdderIn_I(3) := 0
          previousSymbolAdderIn_Q(3) := 0
        }
        default{ //0.9045 for currentSymbol, 0.0955 for previousSymbol
          currentSymbolAdderIn_I(0) := io.dataIn.data.I |>> 5 // divid by 32
          currentSymbolAdderIn_Q(0) := io.dataIn.data.Q |>> 5
          currentSymbolAdderIn_I(1) := io.dataIn.data.I |>> 3 // divid by 8
          currentSymbolAdderIn_Q(1) := io.dataIn.data.Q |>> 3
          currentSymbolAdderIn_I(2) := io.dataIn.data.I |>> 1 // divid by 2
          currentSymbolAdderIn_Q(2) := io.dataIn.data.Q |>> 1
          currentSymbolAdderIn_I(3) := io.dataIn.data.I |>> 2 // divid by 4
          currentSymbolAdderIn_Q(3) := io.dataIn.data.Q |>> 2

          previousSymbolAdderIn_I(0) := gpReg(gpReg.length-1).I |>> 5 // divid by 32
          previousSymbolAdderIn_Q(0) := gpReg(gpReg.length-1).Q |>> 5
          previousSymbolAdderIn_I(1) := gpReg(gpReg.length-1).I |>> 4 // divid by 16
          previousSymbolAdderIn_Q(1) := gpReg(gpReg.length-1).Q |>> 4
          previousSymbolAdderIn_I(2) := 0
          previousSymbolAdderIn_Q(2) := 0
          previousSymbolAdderIn_I(3) := 0
          previousSymbolAdderIn_Q(3) := 0
        }
      }
    } otherwise{
      when(gpOutValid){
        (currentSymbolAdderIn_Q(0),currentSymbolAdderIn_I(0)) := gpReg(gpReg.length-1)
      } otherwise {
        (currentSymbolAdderIn_Q(0),currentSymbolAdderIn_I(0)) := io.dataIn.data
      }
      for(index <- 1 until windowLen){
        currentSymbolAdderIn_I(index) := 0
        currentSymbolAdderIn_Q(index) := 0
      }
      for (index <- 0 until windowLen) {
        previousSymbolAdderIn_I(index) := 0
        previousSymbolAdderIn_Q(index) := 0
      }
    }

    //output control
    val validReg = Vec.fill(windowLen/2)(RegInit(False))
    validReg(0) := io.dataIn.valid || gpOutValid
    validReg(1) := validReg(0)

    when(validReg(1)){
      io.dataOut.data := windowingOut
    } otherwise{
      io.dataOut.data.I := 0
      io.dataOut.data.Q := 0
    }
    io.dataOut.valid := validReg(1)
  }

}
*/