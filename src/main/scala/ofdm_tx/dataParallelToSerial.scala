package ofdm_tx

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

case class DataParallelToSerialOutIF() extends Bundle{
  val data = Bool()
  val symbolType = UInt(GlobalDefine().symbolTypeWidth bits)
  val mcsRATE = Bits(GlobalDefine().mcsRateWidth bits)
}

case class DataParallelToSerial(DataInWidth : Int) extends Component{
  val io = new Bundle{
    val trig = in Bool()
    val symbolType = in UInt(GlobalDefine().symbolTypeWidth bits)
    val mcsRate = in Bits(GlobalDefine().mcsRateWidth bits)
    val parallelIn = in Bits(DataInWidth bits)
    val numMinusOneBitsTranslateThisTime = in UInt(GlobalDefine().parallelBusDataInWidth bits)
    val inIdle = out(Bool())
    val serialOut = master(Stream(DataParallelToSerialOutIF()))
  }
  //noIoPrefix()

  println("@DataParallelToSerial(), symbolTypeWidth:"+GlobalDefine().symbolTypeWidth)

  val scrambleReg = RegInit(B"1111111")
  val dataInReg = RegInit(B(0 , DataInWidth bits))
  val isLegacyDataSymbol = (io.symbolType === GlobalDefine().legacyDataSymbol)
  val isHtDataSymbol = (io.symbolType === GlobalDefine().htDataSymbol)

//  // control numUncodedBitsPerSymbol
//  def maxNumUncodedBitsPerSymbol : Int = 260 // @802.11n MCS index = 7, 64QAM, Coding Rate = 5/6
//  val numUncodedBitsPerSymbol = UInt(log2Up(maxNumUncodedBitsPerSymbol) bits)
//  val uncodedBitsPerSymbolCntReg = Reg(cloneOf(numUncodedBitsPerSymbol)) init(0)
//  when(isHtDataSymbol){
//    switch(io.mcsRate){
//      is(0){
//        numUncodedBitsPerSymbol := 25 // 0~25
//      }
//      is(1){
//        numUncodedBitsPerSymbol := 51
//      }
//      is(2) {
//        numUncodedBitsPerSymbol := 77
//      }
//      is(3) {
//        numUncodedBitsPerSymbol := 103
//      }
//      is(4) {
//        numUncodedBitsPerSymbol := 155
//      }
//      is(5) {
//        numUncodedBitsPerSymbol := 207
//      }
//      is(6) {
//        numUncodedBitsPerSymbol := 233
//      }
//      default{
//        numUncodedBitsPerSymbol := maxNumUncodedBitsPerSymbol-1
//      }
//    }
//  } otherwise{
//    switch(io.mcsRate) {
//      is(B"1101".reversed) {
//        numUncodedBitsPerSymbol := 23 // 0~23, 24 bits
//      }
//      is(B"1111".reversed) {
//        numUncodedBitsPerSymbol := 35
//      }
//      is(B"0101".reversed) {
//        numUncodedBitsPerSymbol := 47
//      }
//      is(B"0111".reversed) {
//        numUncodedBitsPerSymbol := 71
//      }
//      is(B"1001".reversed) {
//        numUncodedBitsPerSymbol := 95
//      }
//      is(B"1011".reversed) {
//        numUncodedBitsPerSymbol := 143
//      }
//      is(B"0001".reversed) {
//        numUncodedBitsPerSymbol := 191
//      }
//      default {
//        numUncodedBitsPerSymbol := 215
//      }
//    }
//  }


  val isDataSymbol = isLegacyDataSymbol || isHtDataSymbol

  val cntReg = Reg(cloneOf(io.numMinusOneBitsTranslateThisTime)) init(0)
  val fsm = new StateMachine {
    val stateIdle : State = new State with EntryPoint {
      whenIsActive{
        when(io.trig){
          goto(stateTranslating)
          cntReg := 0
          dataInReg := io.parallelIn
        }
      }
    }

    val stateTranslating = new State{
      whenIsActive{
        when(io.serialOut.fire){
          cntReg := cntReg + 1
          when(cntReg === io.numMinusOneBitsTranslateThisTime){
            goto(stateIdle)
          } otherwise{
            dataInReg := dataInReg |>> 1 // Logical shift right
          }
        }
      }
    }
  }

  io.inIdle := fsm.isActive(fsm.stateIdle)
  io.serialOut.valid := fsm.isActive(fsm.stateTranslating)

  val scrambleOut = (scrambleReg(3)^scrambleReg(6))
  when(isDataSymbol){
    io.serialOut.data := dataInReg(0) ^ scrambleOut
  }otherwise {
    io.serialOut.data := dataInReg(0)
  }

  when(fsm.isActive(fsm.stateTranslating)){
    when(isDataSymbol) {
      when(io.serialOut.fire) {
        scrambleReg := (scrambleReg(5 downto 0), scrambleOut).asBits
      }
    } otherwise {
      scrambleReg := B"1111111"
    }
  }

  io.serialOut.symbolType := io.symbolType
  io.serialOut.mcsRATE := io.mcsRate
}

object DataParallelToSerialSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(DataParallelToSerial(64)){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.assertReset()
      dut.io.symbolType #= GlobalDefine().legacySignalSymbol
      dut.io.serialOut.ready #= false
      dut.io.trig #= false
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.deassertReset()

      dut.clockDomain.waitSampling(10)
      dut.io.trig #= true
      dut.clockDomain.waitSampling(1)
      dut.io.trig #= false
      dut.clockDomain.waitSampling(5)
      dut.io.serialOut.ready #= true
      dut.clockDomain.waitSampling(10)
      dut.io.serialOut.ready #= false
      dut.clockDomain.waitSampling(6)
      dut.io.serialOut.ready #= true
      dut.clockDomain.waitSampling(80)
      dut.io.serialOut.ready #= false
      dut.io.symbolType #= GlobalDefine().legacyDataSymbol
      dut.io.trig #= true
      dut.clockDomain.waitSampling(1)
      dut.io.trig #= false
      dut.clockDomain.waitSampling(5)
      dut.io.serialOut.ready #= true
      dut.clockDomain.waitSampling(10)
      dut.io.serialOut.ready #= false
      dut.clockDomain.waitSampling(6)
      dut.io.serialOut.ready #= true
      dut.clockDomain.waitSampling(80)
      dut.io.serialOut.ready #= false
      dut.clockDomain.waitSampling(1000)
    }
  }
}
