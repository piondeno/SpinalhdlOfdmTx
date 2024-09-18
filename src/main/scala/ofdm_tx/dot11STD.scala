package ofdm_tx
import spinal.core._
import spinal.lib._
import scala.math.pow

case class Dot11STD() extends Enumeration{
  val dot11L     = B(0, 3 bits)
  val dot11N     = B(1, 3 bits)
  val dot11AC    = B(2, 3 bits)
  val dot11AX    = B(3, 3 bits)
}

case class GlobalDefine(){
  def fftPoints : Int = 64
  def mcsRateWidth : Int= 5
  def nDBPSWidth : Int=log2Up(260) //MCS index=7, 64-QAM, 5/6 codeRate,
  def nBPSCWidth : Int=log2Up(6) //64QAM -> 6bits
  def nCBPSWidth : Int=log2Up(312) //MCS index=7, 64-QAM, 5/6 codeRate,
  def codeRateWidth : Int=2
  def punctCntWidth : Int=log2Up(6) //puncture rate 5/6
  def interleaverSingleRamDepth : Int=16
  def nInterleaverSingleRamNum : Int=18 // 288(CBPSmax)/16 = 18

  def codeRate_1_2 : Int = 0
  def codeRate_2_3 : Int = 1
  def codeRate_3_4 : Int = 2
  def codeRate_5_6 : Int = 3

  def amplitude_BPSK : Int = pow(2,14).toInt
  def amplitude_QPSK : Int = (amplitude_BPSK * 0.7071).toInt
  def amplitude_16QAM : Int = (amplitude_BPSK * 0.31627766).toInt
  def amplitude_64QAM : Int = (amplitude_BPSK * 0.1543).toInt

  def modulatonDataInWidth : Int = 6  // 2^6 =64 QAM
  def modulationDataOutWidth : Int = 16


  //sybmol type
  def legacySignalSymbol    : Int = 0
  def htSignalSymbol        : Int = 1
  def dataSymbol            : Int = 2
  def symbolTypeWidth       : Int = log2Up(dataSymbol+1)

  //giType
  def ltfGiType : Int = 0 // 1.6 us
  def normalGiType : Int = 1 // 0.8 us
  def shortGiType : Int = 2 // 0.4 us
  def giTypeWidth : Int = log2Up(shortGiType+1)

  def rotationFactorWidth : Int = 18 // base on DSP48 structure
}
