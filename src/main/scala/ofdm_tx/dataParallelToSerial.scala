package ofdm_tx

import spinal.core._
import spinal.lib._
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
    val bitsLen = in UInt(log2Up(DataInWidth+1) bits)
    val parallelIn = in Bits(DataInWidth bits)
    val serialOut = master(Stream(DataParallelToSerialOutIF()))
  }
  //noIoPrefix()

  println("symbolTypeWidth:"+GlobalDefine().symbolTypeWidth)
  val symbolTypeReg = RegNextWhen(io.symbolType,io.trig) init(GlobalDefine().legacySignalSymbol)
  val mcsRateReg = RegNextWhen(io.mcsRate,io.trig) init(B("1011").resize(GlobalDefine().mcsRateWidth))

  val scrambleReg = RegInit(B"1111111")
  val dataInReg = RegInit(B(0 , DataInWidth bits))
  val cnt = RegInit( U(DataInWidth , log2Up(DataInWidth+1) bits) )
  val bitsLenReg = RegInit( U(DataInWidth , log2Up(DataInWidth+1) bits) )


  val isDataSymbol = (symbolTypeReg === GlobalDefine().dataSymbol)

  when(io.trig){
    cnt := 0
    dataInReg := io.parallelIn
    bitsLenReg := io.bitsLen
  }.elsewhen(io.serialOut.fire && (cnt < bitsLenReg)){
    cnt := cnt + 1
    dataInReg := dataInReg.rotateRight(1)
  }

  io.serialOut.valid := False
  when(cnt < bitsLenReg){
    io.serialOut.valid := True
  }
//  io.serialOut.payload := dataInReg(0)

  val scrambleOut = (scrambleReg(3)^scrambleReg(6))
  when(isDataSymbol){
    io.serialOut.data := dataInReg(0) ^ scrambleOut
  }otherwise {
    io.serialOut.data := dataInReg(0)
  }


  when(isDataSymbol){
    when(io.serialOut.fire){
      scrambleReg := (scrambleReg(5 downto 0),scrambleReg(3)^scrambleReg(6)).asBits
    }
  }otherwise{
    scrambleReg := B"1111111"
  }

  io.serialOut.symbolType := symbolTypeReg
  io.serialOut.mcsRATE := mcsRateReg
}

object DataParallelToSerialSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(DataParallelToSerial(64)){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.assertReset()
      dut.io.symbolType #= GlobalDefine().legacySignalSymbol
      dut.io.serialOut.ready #= false
      dut.io.trig #= false
      dut.io.bitsLen #= 30
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
      dut.io.bitsLen #= 45
      dut.io.symbolType #= GlobalDefine().dataSymbol
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
