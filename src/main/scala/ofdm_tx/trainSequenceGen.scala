package ofdm_tx

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._
import scala.io.Source
import java.math.BigDecimal

case class ShortTrainFieldRom() extends Component{
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

/*
* To make top as clean as possibile
 */
case class ShortTrainFieldGen() extends Component{
  val io = new Bundle{
    val trig = in(Bool())
    val numRepeatRequire = in(UInt(log2Up(10) bits))
    val dataOut = master(Flow(ComplexDataType(GlobalDefine().modulationDataOutWidth)))
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

  def samplesPerEachRepeat = 16 //STF will repeat again every 16 samples
  val dataOutValidReg = Reg(Bool()) init(False)
  val repeatCnt = Reg(cloneOf(io.numRepeatRequire)) init(0)
  val addr = Reg(UInt(log2Up(16) bits)) init(0)
  when(io.trig){
    dataOutValidReg := True
  } elsewhen((addr===(samplesPerEachRepeat-1)) && (repeatCnt === (io.numRepeatRequire))){
    dataOutValidReg := False
  }

  when(dataOutValidReg){
    addr := addr + 1

    when(addr === (samplesPerEachRepeat-1)){
      repeatCnt := repeatCnt + 1
    }
  } otherwise{
    addr := 0
    repeatCnt := 0
  }

  io.dataOut.valid := dataOutValidReg

  switch(addr){
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

//save a lot of resource vs implement by rom
case class LongTrainSignalGen() extends Component{
  val io = new Bundle{
    val trig = in(Bool())
    val isHtLtf = in(Bool())
    val giType = out(UInt(GlobalDefine().giTypeWidth bits))
    val dataOut = master(Stream(ButterflyDataIf(GlobalDefine().modulationDataOutWidth)))
  }

  val bpskPosAmplitude = S(GlobalDefine().amplitude_BPSK, GlobalDefine().modulationDataOutWidth bits)
  val bpskNegAmplitude = S(GlobalDefine().amplitude_BPSK * -1, GlobalDefine().modulationDataOutWidth bits)

  // because BPSK
  io.dataOut.aData.Q := 0
  io.dataOut.bData.Q := 0

  val legacyPosSubcarrierSeq  = (B"01001101_01000001_10010101_11100000").reversed
  val legacyNegSubcarrierSeq  = (B"00000011_00110101_11111001_10101111").reversed
  val htPosSubcarrierSeq      = (B"01001101_01000001_10010101_11100000").reversed //same as legacyPosSubcarrierSeq
  val htNegSubcarrierSeq      = (B"00001111_00110101_11111001_10101111").reversed
  val isHtLtfReg      = Reg(Bool())

  when(io.trig){
    isHtLtfReg := io.isHtLtf
  }

  val ltfSeqSel = B(isHtLtfReg)

  val outputValid = Reg(Bool()) init (False)
  val cnt = Reg(UInt(log2Up(32) bits)) init (0)
  val negGuardBandRequire = cloneOf(cnt)
  val posGuardBandRequire = cloneOf(cnt)

  // LTF sequence
  /* About posSubcarrierSeq will mapping to subcarrier form 0 to 31
  * index 0 will map to subcarrier 0
  * subcarrier 0 is DC
  * subcarrier 27~31 is guard band and useless for Legacy
  * subcarrier 29~31 is guard band and useless for HT
  * */
  val posSubcarrierSeq = cloneOf(legacyPosSubcarrierSeq)

  /* About negSubcarrierSeq will mapping to subcarrier form -32 to -1
  * index 0 will map to subcarrier -32
  * subcarrier -32~-27 is guard band and useless
  * subcarrier -32~-29 is guard band and useless
  * */
  val negSubcarrierSeq = cloneOf(legacyPosSubcarrierSeq)

  switch(ltfSeqSel){
    is(B"0"){ //legacyLtf
      posSubcarrierSeq := legacyPosSubcarrierSeq
      negSubcarrierSeq := legacyNegSubcarrierSeq
      posGuardBandRequire := 26
      negGuardBandRequire := 6
      io.giType := GlobalDefine().ltfGiType
    }
    default{ //htLtf
      posSubcarrierSeq := htPosSubcarrierSeq
      negSubcarrierSeq := htNegSubcarrierSeq
      posGuardBandRequire := 28
      negGuardBandRequire := 4
      io.giType := GlobalDefine().normalGiType
    }
  }

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
  val atNegGuardBandCond = (cnt < negGuardBandRequire)
  val atPosGuardBandCond = (cnt > posGuardBandRequire)
  val condCombine = Bits(3 bits)
  condCombine := B(atPosGuardBandCond, atNegGuardBandCond, dcCond)

  // Be careful, the output data IF is ButterflyDataIf
  // posSubcarrierSeq will be sent out by upper path
  // negSubcarrierSeq will be sent out by lower path
  switch(condCombine){
    is(B"011"){ //@ DC
      io.dataOut.aData.I := 0
      io.dataOut.bData.I := 0
    }
    is(B"010"){ //@ NegGuardBandCond
      io.dataOut.aData.I := Mux(posSubcarrierSeq(cnt), bpskPosAmplitude, bpskNegAmplitude)
      io.dataOut.bData.I := 0
    }
    is(B"100"){ //@ PosGuardBandCond
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
    SpinalVerilog(ShortTrainFieldRom()) //Or SpinalVerilog
  }
}
