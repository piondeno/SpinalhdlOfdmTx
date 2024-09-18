package ofdm_tx

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._
import scala.io.Source
import java.math.BigDecimal

case class LegacyShortTrainRom() extends Component{
  val io = new Bundle{
    val addr = in(UInt(log2Up(16) bits))
    val dataOut = out(ComplexDataType(GlobalDefine().modulationDataOutWidth))
  }

  var File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/shortTrainingOutRe.txt", "UTF-8")
  var lines = File.getLines().toArray
  File.close()
  println("shartTrainingOutRe.txt")
  val trainingSeqRe = for (line <- lines) yield {
    println(line)
    var result = new BigDecimal(line)
    ((result.longValue()+8)/16).toInt
    //先前用ifft計算，得知64點ifft的輸出，最高會使用到20bit（包含sign bit）
    //所以這裡會除上16，使量化相同，皆輸出16bits，+8是用來四捨五入
  }


  switch(io.addr){
    for(index <- 0 until 16){
      is(index){
        io.dataOut.I := trainingSeqRe(index)
        if(index < 8){
          io.dataOut.Q := trainingSeqRe(index+8)
        } else{
          io.dataOut.Q := trainingSeqRe(index-8)
        }
      }
    }
  }
}

case class LegacyLongTrainSignalGen() extends Component{
  val io = new Bundle{
    val trig = in(Bool())
    val dataOut = master(Stream(ButterflyDataIf(GlobalDefine().modulationDataOutWidth)))
  }

  val bpskPosAmplitude = S(GlobalDefine().amplitude_BPSK, GlobalDefine().modulationDataOutWidth bits)
  val bpskNegAmplitude = S(GlobalDefine().amplitude_BPSK * -1, GlobalDefine().modulationDataOutWidth bits)

  // because BPSK
  io.dataOut.aData.Q := 0
  io.dataOut.bData.Q := 0

  // LTF sequence
  /* About posSubcarrierSeq
  * index 0 will map to subcarrier 0
  * subcarrier 0 is DC
  * subcarrier 27~31 is guard band and useless
  * */
  val posSubcarrierSeq = (B"01001101_01000001_10010101_11100000").reversed

  /* About negSubcarrierSeq
  * index 0 will map to subcarrier -32
  * subcarrier -32~-27 is guard band and useless
  * */
  val negSubcarrierSeq = (B"00000011_00110101_11111001_10101111").reversed

  val outputValid = Reg(Bool()) init(False)
  val cnt = Reg(UInt(log2Up(32) bits)) init(0)
  when(io.trig){
    outputValid := True
  } elsewhen(cnt === 31){
    outputValid := False
  }

  when(io.dataOut.fire){
    cnt := cnt + 1
  }

  // dataOut assignment
  io.dataOut.valid := outputValid

  val dcCond = (cnt === 0)
  val atNegGuardBandCond = (cnt < 6)
  val atPosGuardBandCond = (cnt > 26)
  val conCombine = Bits(3 bits)
  conCombine := B(atPosGuardBandCond, atNegGuardBandCond, dcCond)
  switch(conCombine){
    is(B"011"){
      io.dataOut.aData.I := 0
      io.dataOut.bData.I := 0
    }
    is(B"010"){
      io.dataOut.aData.I := Mux(posSubcarrierSeq(cnt), bpskPosAmplitude, bpskNegAmplitude)
      io.dataOut.bData.I := 0
    }
    is(B"100"){
      io.dataOut.aData.I := 0
      io.dataOut.bData.I := Mux(negSubcarrierSeq(cnt), bpskPosAmplitude, bpskNegAmplitude)
    }
    default{
      io.dataOut.aData.I := Mux(posSubcarrierSeq(cnt), bpskPosAmplitude, bpskNegAmplitude)
      io.dataOut.bData.I := Mux(negSubcarrierSeq(cnt), bpskPosAmplitude, bpskNegAmplitude)
    }
  }
}

case class LegacyLongTrainRom() extends Component{
  val io = new Bundle{
    val addr = in(UInt(log2Up(64) bits))
    val dataOut = out(ComplexDataType(GlobalDefine().modulationDataOutWidth))
  }

  var File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/longTrainingOutRe.txt", "UTF-8")
  var lines = File.getLines().toArray
  File.close()
  println("longTrainingOutRe.txt")
  val trainingSeqRe = for (line <- lines) yield {
    println(line)
    var result = new BigDecimal(line)
    result.longValue()
  }

  File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/longTrainingOutIm.txt", "UTF-8")
  lines = File.getLines().toArray
  File.close()
  println("longTrainingOutIm.txt")
  val trainingSeqIm = for (line <- lines) yield {
    println(line)
    var result = new BigDecimal(line)
    result.longValue()
  }

  switch(io.addr){
    for(index <- 0 until GlobalDefine().fftPoints){
      is(index){
        io.dataOut.I := ((trainingSeqRe(index)+8)/16).toInt
        io.dataOut.Q := ((trainingSeqIm(index)+8)/16).toInt
      }
    }
  }

}

object legacyShortTrainRom {
  def main(args: Array[String]) {
    SpinalVerilog(LegacyShortTrainRom()) //Or SpinalVerilog
  }
}
