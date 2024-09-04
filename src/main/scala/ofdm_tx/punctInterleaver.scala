package ofdm_tx

import spinal.core.{IntToBuilder, _}
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._

case class PunctInterleaverDataOutIF() extends Bundle{
  val data = Bits(GlobalDefine().modulatonDataInWidth bits)
  val nBPSC = UInt(GlobalDefine().nBPSCWidth bits)
  val symbolType = UInt(GlobalDefine().symbolTypeWidth bits)
}

case class PunctInterleaver() extends Component{
  val io = new Bundle{
    val dataIn = slave(Stream(ConvolutionDataOutIf()))
    val dataOut = master(Stream(PunctInterleaverDataOutIF()))
  }
  val punInReady = False

  // state machine
  val fsm = new StateMachine{
    var cntInitValueNot = U(0,GlobalDefine().nDBPSWidth bits)
    val cnt = RegInit(~cntInitValueNot)
    val nDBPS = RegInit(U(24,GlobalDefine().nDBPSWidth bits))
    val nCBPS = RegInit(U(48,GlobalDefine().nCBPSWidth bits))
    val nBPSC = RegInit(U(1,GlobalDefine().nBPSCWidth bits))
    val symbolTypeReg = RegInit(U(GlobalDefine().legacySignalSymbol,GlobalDefine().symbolTypeWidth bits))
    //val mcsRateReg = RegInit(B("1011").resize(GlobalDefine().mcsRateWidth))

    val interleaverRowAddr = Reg(UInt(log2Up(GlobalDefine().nInterleaverSingleRamNum / GlobalDefine().modulatonDataInWidth) bits))
    val interleaverColAddr = Reg(UInt(log2Up(GlobalDefine().interleaverSingleRamDepth) bits))

    val cntCBPS = Reg(UInt(log2Up(288) bits))

    io.dataIn.ready := (cnt < nDBPS) && punInReady

    val stateIdle : State = new State with EntryPoint {
      whenIsActive(
        when(io.dataIn.valid) {
          goto(stateInterleave)
          cnt := 0
          punctCnt := 0
          cntCBPS := 0
          interleaverRowAddr := 0
          interleaverColAddr := 0
          symbolTypeReg := io.dataIn.symbolType

          switch(io.dataIn.mcsRate) {
            is(B("1011").resize(GlobalDefine().mcsRateWidth)) {
              nDBPS := 24
              nCBPS := 48
              nBPSC := 1
              codeRateReg := GlobalDefine().codeRate_1_2
            }
            is(B("1111").resize(GlobalDefine().mcsRateWidth)) {
              nDBPS := 36
              nCBPS := 48
              nBPSC := 1
              codeRateReg := GlobalDefine().codeRate_3_4
            }
            is(B("1010").resize(GlobalDefine().mcsRateWidth)) {
              nDBPS := 48
              nCBPS := 96
              nBPSC := 2
              codeRateReg := GlobalDefine().codeRate_1_2
            }
            is(B("1110").resize(GlobalDefine().mcsRateWidth)) {
              nDBPS := 72
              nCBPS := 96
              nBPSC := 2
              codeRateReg := GlobalDefine().codeRate_3_4
            }
            is(B("1001").resize(GlobalDefine().mcsRateWidth)) {
              nDBPS := 96
              nCBPS := 192
              nBPSC := 4
              codeRateReg := GlobalDefine().codeRate_1_2
            }
            is(B("1101").resize(GlobalDefine().mcsRateWidth)) {
              nDBPS := 144
              nCBPS := 192
              nBPSC := 4
              codeRateReg := GlobalDefine().codeRate_3_4
            }
            is(B("1000").resize(GlobalDefine().mcsRateWidth)) {
              nDBPS := 192
              nCBPS := 288
              nBPSC := 6
              codeRateReg := GlobalDefine().codeRate_2_3
            }
            is(B("1100").resize(GlobalDefine().mcsRateWidth)) {
              nDBPS := 216
              nCBPS := 288
              nBPSC := 6
              codeRateReg := GlobalDefine().codeRate_3_4
            }
            default {
              nDBPS := 24
              nCBPS := 48
              nBPSC := 1
              codeRateReg := GlobalDefine().codeRate_1_2
            }
          }
        }otherwise{
          cntCBPS := B(log2Up(288) bits, default -> True).asUInt
        }
      )
    }
    val stateInterleave : State = new State{
      whenIsActive {
        when(io.dataIn.fire){
          cnt := cnt + 1
        }

        when(cntCBPS < nCBPS){
          cntCBPS := cntCBPS +1
        }

        when(cnt === nDBPS) {
          goto(stateReadyToModulation)
          cntCBPS := 0
        }
      }
    }
    val stateReadyToModulation : State = new State{
      whenIsActive {
        for(index <- 0 to (GlobalDefine().nInterleaverSingleRamNum-1)) {
          interleaverRamEn(index) := False
          interleaverRamAddr(index) := interleaverColAddr
        }

        when(io.dataOut.ready){
          when(interleaverRowAddr === (GlobalDefine().nInterleaverSingleRamNum / GlobalDefine().modulatonDataInWidth - 1)) {
            interleaverRowAddr := 0
            interleaverColAddr := interleaverColAddr + 1
          } otherwise {
            interleaverRowAddr := interleaverRowAddr + 1
          }
        }

        when((interleaverRowAddr === 2) && (interleaverColAddr === (GlobalDefine().interleaverSingleRamDepth-1))){
          goto(stateIdle)
        }
      }
    }
  }

  // puncture cnt control
  val codeRateReg = RegInit(U(0,GlobalDefine().codeRateWidth bits))
  val punctInCntBound = UInt(GlobalDefine().punctCntWidth bits) // repeat puncture after inputs
  val punctOutCntBound = UInt(GlobalDefine().punctCntWidth bits)

  switch(codeRateReg){
    //is(GlobalDefine().codeRate_1_2){punctOutCntBound := 2 ; punctInCntBound := 1 }
    is(GlobalDefine().codeRate_2_3){punctOutCntBound := 6 ; punctInCntBound := 4 }
    is(GlobalDefine().codeRate_3_4){punctOutCntBound := 4 ; punctInCntBound := 3 }
    is(GlobalDefine().codeRate_5_6){punctOutCntBound := 6 ; punctInCntBound := 5 }
    default{punctOutCntBound := 2 ; punctInCntBound := 1 }
  }


  // puncture implement
  val punctCnt = RegInit(U(0,GlobalDefine().codeRateWidth bits))

  val punctAReg = Reg(Bits(3 bits))
  val punctBReg = Reg(Bits(3 bits))
  val punctOut = False

  when(fsm.isActive(fsm.stateInterleave)) {
    var preAdd = punctCnt+1
    when(preAdd === punctOutCntBound){
      punctCnt := 0
    }otherwise{
      punctCnt := preAdd
    }

    when(io.dataIn.fire){
      //var temp = (punctAReg(1 downto 0),B"1")
      punctAReg := (punctAReg(1 downto 0),io.dataIn.data(0)).asBits
      punctBReg := (punctBReg(1 downto 0),io.dataIn.data(1)).asBits
    }

    switch(codeRateReg) {
      //is(GlobalDefine().codeRate_1_2){punctOutCntBound := 2 ; punctInCntBound := 1 }
      is(GlobalDefine().codeRate_2_3) {
        punctOut := io.dataIn.data(0)
      }
      is(GlobalDefine().codeRate_3_4) {
        punctOut := io.dataIn.data(0)
      }
      is(GlobalDefine().codeRate_5_6) {
        punctOut := io.dataIn.data(0)
      }
      default { //GlobalDefine().codeRate_1_2
        when(punctCnt===0){
          punctOut := io.dataIn.data(0)
        } otherwise{
          punctOut := punctBReg(0)
        }
      }
    }
  }

  when(io.dataIn.valid && (punctCnt < punctInCntBound)){
    punInReady := True
  }

  // state to implement interleaver
  val interleaverRamAddr = Array.fill(GlobalDefine().nInterleaverSingleRamNum)(UInt(log2Up(GlobalDefine().interleaverSingleRamDepth) bits))
  val interleaverRamEn = Array.fill(GlobalDefine().nInterleaverSingleRamNum)(Bool())
  val interleaverRamRead = Bits(GlobalDefine().nInterleaverSingleRamNum bits)
  val interleaverRam = Array.fill(GlobalDefine().nInterleaverSingleRamNum)(Mem(Bool(),GlobalDefine().interleaverSingleRamDepth).init (Seq(False, False, False, False, False, False, False, False,False, False, False, False, False, False, False, False)))
  val interleaverOut = Array.fill(GlobalDefine().nInterleaverSingleRamNum / GlobalDefine().modulatonDataInWidth)(Bits(GlobalDefine().modulatonDataInWidth bits))
  val toModulaton = Bits(GlobalDefine().modulatonDataInWidth bits)

  var index = 0
  for(index <- 0 to (GlobalDefine().nInterleaverSingleRamNum-1)){ //set default value
    interleaverRamAddr(index) := 0
    interleaverRamEn(index) := False
    interleaverRam(index).write(
      enable = interleaverRamEn(index),
      address = interleaverRamAddr(index),
      data    = punctOut
    )
    interleaverRamRead(index) := interleaverRam(index).readAsync(
      address = interleaverRamAddr(index)
    )
  }

  println(GlobalDefine().nInterleaverSingleRamNum/GlobalDefine().modulatonDataInWidth-1)
  for(index <- 0 to (GlobalDefine().nInterleaverSingleRamNum/GlobalDefine().modulatonDataInWidth-1)){
    println((5+index*GlobalDefine().modulatonDataInWidth))
    interleaverOut(index) := interleaverRamRead((5+index*GlobalDefine().modulatonDataInWidth) downto (0+index*GlobalDefine().modulatonDataInWidth)).asBits
  }

  when(fsm.isActive(fsm.stateInterleave)) {
    switch(fsm.nCBPS) {
      is(96) {
      }
      is(192) {
      }
      is(288) {
      }
      default { //48
        //disable all unused ram
        for(index <- 0 to (GlobalDefine().nInterleaverSingleRamNum-1)){
          if((index%GlobalDefine().modulatonDataInWidth) != 0){
            interleaverRamEn(index) := False
          }
          interleaverRamAddr(index) := fsm.cntCBPS(3 downto 0)
        }
        switch(fsm.cntCBPS(5 downto 4).asBits){
          is(B"00"){interleaverRamEn(0) := True}
          is(B"01"){interleaverRamEn(6) := True}
          default{interleaverRamEn(12) := True}
        }
      }
    }
  }

  switch(fsm.interleaverRowAddr){
    is(0){toModulaton := interleaverOut(0)}
    is(1){toModulaton := interleaverOut(1)}
    default{toModulaton := interleaverOut(2)}
  }

  io.dataOut.data := toModulaton
  io.dataOut.valid := fsm.isActive(fsm.stateReadyToModulation)
  io.dataOut.nBPSC := fsm.nBPSC
  io.dataOut.symbolType := fsm.symbolTypeReg
}

object punctInterleaverSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(PunctInterleaver()){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.assertReset()

      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.deassertReset()



      dut.clockDomain.waitSampling(1000)
    }
  }
}
