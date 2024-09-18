package ofdm_tx

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

case class OfdmTx(DataInWidth : Int, DacWidth : Int) extends Component{
  val io = new Bundle{
    val dataIn = slave(Stream(Bits(DataInWidth bits)))
    val dataOut = master(Flow(ComplexDataType(GlobalDefine().modulationDataOutWidth)))
  }
  //noIoPrefix()

  def pktTypePos : Int= 24

  val dataInFire = io.dataIn.ready && io.dataIn.valid
  val dataInReg = RegNextWhen(io.dataIn.payload,dataInFire)

  // State Machine control the flow ...
  val currentSymbolMcsRate = Bits(GlobalDefine().mcsRateWidth bits)
  val mcsRATE = B((B"01011"),GlobalDefine().mcsRateWidth bits)
  val symbolType = U(GlobalDefine().legacySignalSymbol,GlobalDefine().symbolTypeWidth bits)
  val pktType = RegInit(False)
  val p2sLen = U(64,log2Up(DataInWidth+1) bits)
  val isDataSymbol = RegInit(False) // only data symble needs to be scrambled

  val mainFSM = new StateMachine{
    io.dataIn.ready := False
    currentSymbolMcsRate := mcsRATE

    val stateIdle : State = new State with EntryPoint {
      whenIsActive{
        when(io.dataIn.valid){
          goto(stateSTF)
        }
      }
    }

    val stateSTF : State = new State{
      whenIsActive {
        goto(stateLTF)
      }
    }

    val stateLTF : State = new State{
      whenIsActive(
        when(io.dataIn.valid){
          goto(stateLegacySig)
          io.dataIn.ready := True
          p2sLen := 24
          symbolType := GlobalDefine().legacySignalSymbol
          isDataSymbol := False

          switch(io.dataIn.payload(3 downto 0)){
            is(B"1011"){mcsRATE := B"01011"}//  6 Mbps
            is(B"1111"){mcsRATE := B"01111"}//  9 Mbps
            is(B"1010"){mcsRATE := B"01010"}// 12 Mbps
            is(B"1110"){mcsRATE := B"01110"}// 18 Mbps
            is(B"1001"){mcsRATE := B"01001"}// 24 Mbps
            is(B"1101"){mcsRATE := B"01101"}// 36 Mbps
            is(B"1000"){mcsRATE := B"01000"}// 48 Mbps
            is(B"1100"){mcsRATE := B"01100"}// 54 Mbps
            default{    mcsRATE := B"01011"}//  6 Mbps
          }
          pktType := io.dataIn.payload(pktTypePos)
        }
      )
    }
    val stateLegacySig : State = new State {
      whenIsActive(
        currentSymbolMcsRate := B("1011").resize(GlobalDefine().mcsRateWidth)
      )
    }
  }

  // short train field timing control, STF
  val stfOutputValid = Reg(Bool()) init(False)
  val stfTimeCnt = Reg(UInt(log2Up(10) bits)) init(0)
  val stfAddr = Reg(UInt(log2Up(16) bits)) init(0)
  val stfTimeCntCond = (stfTimeCnt===9)
  val stfAddrCond = (stfAddr===15)

  when(mainFSM.isActive(mainFSM.stateSTF)){
    stfOutputValid := True
  } elsewhen(stfAddrCond && stfTimeCntCond){
    stfOutputValid := False
  }

  when(stfOutputValid){
    stfAddr := stfAddr + 1
  } otherwise{
    stfAddr := 0
  }

  when(~stfOutputValid){
    stfTimeCnt := 0
  } elsewhen(stfAddrCond){
    stfTimeCnt := stfTimeCnt+1
  }

  val stfDataOut = Flow(ComplexDataType(GlobalDefine().modulationDataOutWidth))
  val legacyShortTrainRomModule = LegacyShortTrainRom()
  legacyShortTrainRomModule.io.addr := stfAddr
  stfDataOut.valid := stfOutputValid
  stfDataOut.payload := legacyShortTrainRomModule.io.dataOut

  // Receive the data and parallel to serial
  val p2sModule = DataParallelToSerial(DataInWidth)
  p2sModule.io.parallelIn := io.dataIn.payload
  p2sModule.io.mcsRate := currentSymbolMcsRate
  p2sModule.io.trig := io.dataIn.ready
  p2sModule.io.bitsLen := p2sLen
  p2sModule.io.symbolType := symbolType

  // Convolution
  val convModule = Convolution()
  p2sModule.io.serialOut <> convModule.io.serialIn

  // Puncture and Interleave
  val punctInterleaveModule = PunctInterleaver()
  punctInterleaveModule.io.dataIn <> convModule.io.serialOut


  //pilot and zero insert, modulator is in the pilotInsertModule
  val pilotInsertModule = PilotInsert()
  pilotInsertModule.io.dataIn <> punctInterleaveModule.io.dataOut
  //pilotInsertModule.io.dataOut.ready := pilotInsertModule.io.dataOut.valid

  //LTF gen
  val legacyLongTrainSignalGenModule = LegacyLongTrainSignalGen()
  legacyLongTrainSignalGenModule.io.trig := mainFSM.isActive(mainFSM.stateLTF)

  //instantiate IFFT
  val ifftModule = FftByCsgRadix2(64, GlobalDefine().modulationDataOutWidth, GlobalDefine().rotationFactorWidth, forFFT = false)
  //ifftModule.io.dataIn <> pilotInsertModule.io.dataOut
  when(legacyLongTrainSignalGenModule.io.dataOut.valid){
    ifftModule.io.dataIn <> legacyLongTrainSignalGenModule.io.dataOut
    ifftModule.io.giTypeIn := GlobalDefine().ltfGiType
    pilotInsertModule.io.dataOut.ready := False
  } otherwise{
    ifftModule.io.dataIn <> pilotInsertModule.io.dataOut
    ifftModule.io.giTypeIn := GlobalDefine().normalGiType
    legacyLongTrainSignalGenModule.io.dataOut.ready := False
  }

  //instantiate GpAndWindowing
  val reorderIfftAndAddGiModule = ReorderIfftAndAddGi()
  reorderIfftAndAddGiModule.io.dataIn <> ifftModule.io.dataOut
  reorderIfftAndAddGiModule.io.giTypeIn := ifftModule.io.giTypeOut

  //io.dataOut assignment
  io.dataOut := Mux(stfOutputValid, stfDataOut, reorderIfftAndAddGiModule.io.dataOut)
}


object OfdmTx {
  def main(args: Array[String]) {
    SpinalVerilog(OfdmTx(64,16)) //Or SpinalVerilog
  }
}

object OfdmTxSim {
  def main(args: Array[String]) {
    import scala.io.Source
    import java.math.BigDecimal

    var File = Source.fromFile("/home/datakey/tools/VexRiscV/VexRiscv-master/src/main/scala/ofdm_tx/ht_tx_intf_mem_mcs7_gi1_aggr0_byte8176.mem", "UTF-8")
    var lines = File.getLines().toArray
    File.close()
    val dataInRe = for (line <- lines) yield {
      println(line)
    }

    val spinalConfig = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(10 MHz))
    SimConfig
      .withConfig(spinalConfig)
      .withWave
      .compile(OfdmTx(64,16))
      .doSim{dut =>
        //Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.assertReset()
        dut.clockDomain.forkStimulus(10 )

        //dut.io.dataIn.payload #= BigInt("000000000100564B", 16)
        dut.io.dataIn.payload #= BigInt(lines(0), 16)
        dut.io.dataIn.valid #= true

        dut.clockDomain.deassertReset()

        var index = 0
        while(index < lines.length){
          if(dut.io.dataIn.ready.toBoolean == true){
            index = index + 1
          }
          println("In simulation, index :"+index+"   --- dut.io.dataIn.ready :"+dut.io.dataIn.ready.toBoolean)
          dut.io.dataIn.payload #= BigInt(lines(index), 16)
          dut.clockDomain.waitSampling(1)
        }

        dut.clockDomain.waitSampling(10)
        //dut.convModule.io.serialOut.ready #= true
        dut.clockDomain.waitSampling(1000)
        println("Simulation done")
        //println(GlobalDefine().amplitude_64QAM)
        simSuccess()

    }
  }
}