package ofdm_tx

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._

case class ModulatorDataInIF() extends Bundle{
  val data = Bits(GlobalDefine().modulatonDataInWidth bits)
  val nBPSC = UInt(GlobalDefine().nBPSCWidth bits)
}

case class Modulation() extends Component{
  val io = new Bundle{
    val dataIn = in(ModulatorDataInIF())
    val dataOut = out(ComplexDataType(GlobalDefine().modulationDataOutWidth))
  }
  //noIoPrefix()

  var ampBPSK = GlobalDefine().amplitude_BPSK
  var ampQPSK = GlobalDefine().amplitude_QPSK
  var amp16QAM = GlobalDefine().amplitude_16QAM
  var amp64QAM = GlobalDefine().amplitude_64QAM

  switch(io.dataIn.nBPSC){
    default{
      switch(io.dataIn.data(0)){
        is(False){
          io.dataOut.I := ampBPSK * -1 ; io.dataOut.Q := 0
        }
        is(True){
          io.dataOut.I := ampBPSK *  1 ; io.dataOut.Q := 0
        }
      }
    }
    is(2){
      switch(io.dataIn.data(1 downto 0)) {
        is(B"00") {
          io.dataOut.I := ampQPSK * -1; io.dataOut.Q := ampQPSK * -1
        }
        is(B"01") {
          io.dataOut.I := ampQPSK * -1; io.dataOut.Q := ampQPSK *  1
        }
        is(B"10") {
          io.dataOut.I := ampQPSK *  1; io.dataOut.Q := ampQPSK * -1
        }
        is(B"11") {
          io.dataOut.I := ampQPSK *  1; io.dataOut.Q := ampQPSK *  1
        }
      }
    }
    is(4){
      switch(io.dataIn.data(3 downto 0)) {
        is(B"0000"){
          io.dataOut.I := amp16QAM * -3; io.dataOut.Q := amp16QAM * -3
        }
        is(B"0001") {
          io.dataOut.I := amp16QAM * -3; io.dataOut.Q := amp16QAM * -1
        }
        is(B"0010") {
          io.dataOut.I := amp16QAM * -3; io.dataOut.Q := amp16QAM *  3
        }
        is(B"0011") {
          io.dataOut.I := amp16QAM * -3; io.dataOut.Q := amp16QAM *  1
        }
        is(B"0100") {
          io.dataOut.I := amp16QAM * -1; io.dataOut.Q := amp16QAM * -3
        }
        is(B"0101") {
          io.dataOut.I := amp16QAM * -1; io.dataOut.Q := amp16QAM * -1
        }
        is(B"0110") {
          io.dataOut.I := amp16QAM * -1; io.dataOut.Q := amp16QAM *  3
        }
        is(B"0111") {
          io.dataOut.I := amp16QAM * -1; io.dataOut.Q := amp16QAM *  1
        }
        is(B"1000") {
          io.dataOut.I := amp16QAM *  3; io.dataOut.Q := amp16QAM * -3
        }
        is(B"1001") {
          io.dataOut.I := amp16QAM *  3; io.dataOut.Q := amp16QAM * -1
        }
        is(B"1010") {
          io.dataOut.I := amp16QAM *  3; io.dataOut.Q := amp16QAM *  3
        }
        is(B"1011") {
          io.dataOut.I := amp16QAM *  3; io.dataOut.Q := amp16QAM *  1
        }
        is(B"1100") {
          io.dataOut.I := amp16QAM *  1; io.dataOut.Q := amp16QAM * -3
        }
        is(B"1101") {
          io.dataOut.I := amp16QAM *  1; io.dataOut.Q := amp16QAM * -1
        }
        is(B"1110") {
          io.dataOut.I := amp16QAM *  1; io.dataOut.Q := amp16QAM *  3
        }
        is(B"1111") {
          io.dataOut.I := amp16QAM *  1; io.dataOut.Q := amp16QAM *  1
        }
      }
    }
    is(6){
      switch(io.dataIn.data(5 downto 0)){
        is(B"000000"){
          io.dataOut.I := amp64QAM * -7; io.dataOut.Q := amp64QAM * -7
        }
        is(B"000001") {
          io.dataOut.I := amp64QAM * -7; io.dataOut.Q := amp64QAM * -5
        }
        is(B"000010") {
          io.dataOut.I := amp64QAM * -7; io.dataOut.Q := amp64QAM * -1
        }
        is(B"000011") {
          io.dataOut.I := amp64QAM * -7; io.dataOut.Q := amp64QAM * -3
        }
        is(B"000100") {
          io.dataOut.I := amp64QAM * -7; io.dataOut.Q := amp64QAM *  7
        }
        is(B"000101") {
          io.dataOut.I := amp64QAM * -7; io.dataOut.Q := amp64QAM *  5
        }
        is(B"000110") {
          io.dataOut.I := amp64QAM * -7; io.dataOut.Q := amp64QAM *  1
        }
        is(B"000111") {
          io.dataOut.I := amp64QAM * -7; io.dataOut.Q := amp64QAM *  3
        }
        is(B"001000") {
          io.dataOut.I := amp64QAM * -5; io.dataOut.Q := amp64QAM * -7
        }
        is(B"001001") {
          io.dataOut.I := amp64QAM * -5; io.dataOut.Q := amp64QAM * -5
        }
        is(B"001010") {
          io.dataOut.I := amp64QAM * -5; io.dataOut.Q := amp64QAM * -1
        }
        is(B"001011") {
          io.dataOut.I := amp64QAM * -5; io.dataOut.Q := amp64QAM * -3
        }
        is(B"001100") {
          io.dataOut.I := amp64QAM * -5; io.dataOut.Q := amp64QAM *  7
        }
        is(B"001101") {
          io.dataOut.I := amp64QAM * -5; io.dataOut.Q := amp64QAM *  5
        }
        is(B"001110") {
          io.dataOut.I := amp64QAM * -5; io.dataOut.Q := amp64QAM *  1
        }
        is(B"001111") {
          io.dataOut.I := amp64QAM * -5; io.dataOut.Q := amp64QAM *  3
        }
        is(B"010000") {
          io.dataOut.I := amp64QAM * -1; io.dataOut.Q := amp64QAM * -7
        }
        is(B"010001") {
          io.dataOut.I := amp64QAM * -1; io.dataOut.Q := amp64QAM * -5
        }
        is(B"010010") {
          io.dataOut.I := amp64QAM * -1; io.dataOut.Q := amp64QAM * -1
        }
        is(B"010011") {
          io.dataOut.I := amp64QAM * -1; io.dataOut.Q := amp64QAM * -3
        }
        is(B"010100") {
          io.dataOut.I := amp64QAM * -1; io.dataOut.Q := amp64QAM *  7
        }
        is(B"010101") {
          io.dataOut.I := amp64QAM * -1; io.dataOut.Q := amp64QAM *  5
        }
        is(B"010110") {
          io.dataOut.I := amp64QAM * -1; io.dataOut.Q := amp64QAM *  1
        }
        is(B"010111") {
          io.dataOut.I := amp64QAM * -1; io.dataOut.Q := amp64QAM *  3
        }
        is(B"011000") {
          io.dataOut.I := amp64QAM * -3; io.dataOut.Q := amp64QAM * -7
        }
        is(B"011001") {
          io.dataOut.I := amp64QAM * -3; io.dataOut.Q := amp64QAM * -5
        }
        is(B"011010") {
          io.dataOut.I := amp64QAM * -3; io.dataOut.Q := amp64QAM * -1
        }
        is(B"011011") {
          io.dataOut.I := amp64QAM * -3; io.dataOut.Q := amp64QAM * -3
        }
        is(B"011100") {
          io.dataOut.I := amp64QAM * -3; io.dataOut.Q := amp64QAM *  7
        }
        is(B"011101") {
          io.dataOut.I := amp64QAM * -3; io.dataOut.Q := amp64QAM *  5
        }
        is(B"011110") {
          io.dataOut.I := amp64QAM * -3; io.dataOut.Q := amp64QAM *  1
        }
        is(B"011111") {
          io.dataOut.I := amp64QAM * -3; io.dataOut.Q := amp64QAM *  3
        }
        is(B"100000") {
          io.dataOut.I := amp64QAM *  7; io.dataOut.Q := amp64QAM * -7
        }
        is(B"100001") {
          io.dataOut.I := amp64QAM *  7; io.dataOut.Q := amp64QAM * -5
        }
        is(B"100010") {
          io.dataOut.I := amp64QAM *  7; io.dataOut.Q := amp64QAM * -1
        }
        is(B"100011") {
          io.dataOut.I := amp64QAM *  7; io.dataOut.Q := amp64QAM * -3
        }
        is(B"100100") {
          io.dataOut.I := amp64QAM *  7; io.dataOut.Q := amp64QAM *  7
        }
        is(B"100101") {
          io.dataOut.I := amp64QAM *  7; io.dataOut.Q := amp64QAM *  5
        }
        is(B"100110") {
          io.dataOut.I := amp64QAM *  7; io.dataOut.Q := amp64QAM *  1
        }
        is(B"100111") {
          io.dataOut.I := amp64QAM *  7; io.dataOut.Q := amp64QAM *  3
        }
        is(B"101000") {
          io.dataOut.I := amp64QAM *  5; io.dataOut.Q := amp64QAM * -7
        }
        is(B"101001") {
          io.dataOut.I := amp64QAM *  5; io.dataOut.Q := amp64QAM * -5
        }
        is(B"101010") {
          io.dataOut.I := amp64QAM *  5; io.dataOut.Q := amp64QAM * -1
        }
        is(B"101011") {
          io.dataOut.I := amp64QAM *  5; io.dataOut.Q := amp64QAM * -3
        }
        is(B"101100") {
          io.dataOut.I := amp64QAM *  5; io.dataOut.Q := amp64QAM *  7
        }
        is(B"101101") {
          io.dataOut.I := amp64QAM *  5; io.dataOut.Q := amp64QAM *  5
        }
        is(B"101110") {
          io.dataOut.I := amp64QAM *  5; io.dataOut.Q := amp64QAM *  1
        }
        is(B"101111") {
          io.dataOut.I := amp64QAM *  5; io.dataOut.Q := amp64QAM *  3
        }
        is(B"110000") {
          io.dataOut.I := amp64QAM *  1; io.dataOut.Q := amp64QAM * -7
        }
        is(B"110001") {
          io.dataOut.I := amp64QAM *  1; io.dataOut.Q := amp64QAM * -5
        }
        is(B"110010") {
          io.dataOut.I := amp64QAM *  1; io.dataOut.Q := amp64QAM * -1
        }
        is(B"110011") {
          io.dataOut.I := amp64QAM *  1; io.dataOut.Q := amp64QAM * -3
        }
        is(B"110100") {
          io.dataOut.I := amp64QAM *  1; io.dataOut.Q := amp64QAM *  7
        }
        is(B"110101") {
          io.dataOut.I := amp64QAM *  1; io.dataOut.Q := amp64QAM *  5
        }
        is(B"110110") {
          io.dataOut.I := amp64QAM *  1; io.dataOut.Q := amp64QAM *  1
        }
        is(B"110111") {
          io.dataOut.I := amp64QAM *  1; io.dataOut.Q := amp64QAM *  3
        }
        is(B"111000") {
          io.dataOut.I := amp64QAM *  3; io.dataOut.Q := amp64QAM * -7
        }
        is(B"111001") {
          io.dataOut.I := amp64QAM *  3; io.dataOut.Q := amp64QAM * -5
        }
        is(B"111010") {
          io.dataOut.I := amp64QAM *  3; io.dataOut.Q := amp64QAM * -1
        }
        is(B"111011") {
          io.dataOut.I := amp64QAM *  3; io.dataOut.Q := amp64QAM * -3
        }
        is(B"111100") {
          io.dataOut.I := amp64QAM *  3; io.dataOut.Q := amp64QAM *  7
        }
        is(B"111101") {
          io.dataOut.I := amp64QAM *  3; io.dataOut.Q := amp64QAM *  5
        }
        is(B"111110") {
          io.dataOut.I := amp64QAM *  3; io.dataOut.Q := amp64QAM *  1
        }
        is(B"111111") {
          io.dataOut.I := amp64QAM *  3; io.dataOut.Q := amp64QAM *  3
        }
      }
    }
  }
}
