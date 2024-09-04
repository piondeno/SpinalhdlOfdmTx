package ofdm_tx

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._
import scala.io.Source
import java.math.BigDecimal

case class legacyShortTrainRom() extends Component{
  val io = new Bundle{
    val addr = in(UInt(log2Up(16) bits))
    val dataOut = out(ComplexDataType(GlobalDefine().modulationDataOutWidth))
  }

  var File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/shartTrainingOutRe.txt", "UTF-8")
  var lines = File.getLines().toArray
  File.close()
  val trainingSeqRe = for (line <- lines) yield {
    println(line)
    var result = new BigDecimal(line)
    result.longValue()
  }

  File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/shartTrainingOutIm.txt", "UTF-8")
  lines = File.getLines().toArray
  File.close()
  val trainingSeqIm = for (line <- lines) yield {
    println(line)
    var result = new BigDecimal(line)
    result.longValue()
  }

  switch(io.addr){
    for(index <- 0 until 16){
      is(index){
        io.dataOut.I := ((trainingSeqRe(index)+8)/16).toInt
        io.dataOut.Q := ((trainingSeqIm(index)+8)/16).toInt
      }
    }
  }

}

case class legacyLongTrainRom() extends Component{
  val io = new Bundle{
    val addr = in(UInt(log2Up(64) bits))
    val dataOut = out(ComplexDataType(GlobalDefine().modulationDataOutWidth))
  }

  var File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/longTrainingOutRe.txt", "UTF-8")
  var lines = File.getLines().toArray
  File.close()
  val trainingSeqRe = for (line <- lines) yield {
    println(line)
    var result = new BigDecimal(line)
    result.longValue()
  }

  File = Source.fromFile("/home/datakey/myProject/opensdr/python/pythonProject/longTrainingOutIm.txt", "UTF-8")
  lines = File.getLines().toArray
  File.close()
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
    SpinalVerilog(legacyShortTrainRom()) //Or SpinalVerilog
  }
}
