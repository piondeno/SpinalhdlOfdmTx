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

  def pktTypePos : Int= 24 //in the testbench, the 24'th bit is usded to indicate package type

  val numSymbolTransmitted = Reg(UInt(log2Up(10) bits)) init(0)

  // buffer the io.dataIn
  val dataInReg = RegNextWhen(io.dataIn.payload,io.dataIn.fire)

  // short train field timing control, STF
  val stfDataOut = Flow(ComplexDataType(GlobalDefine().modulationDataOutWidth))
  val stfGenArea = new Area {
    val shortTrainFieldGenModule = ShortTrainFieldGen()
    val trig = False
    val nunRepeatRequre = cloneOf(shortTrainFieldGenModule.io.numRepeatRequire)  //legacy stf needs repeat 10 times
    nunRepeatRequre := 9

    shortTrainFieldGenModule.io.trig := trig
    shortTrainFieldGenModule.io.numRepeatRequire := nunRepeatRequre

    def setTrig() : Unit ={
      trig := True
    }
    def assignNumRepeatRequreAssign(num : UInt) ={
      nunRepeatRequre := num
    }
  }
  stfDataOut := stfGenArea.shortTrainFieldGenModule.io.dataOut


  // State Machine control the flow ...
  val ofdmTxReady = Bool()
  val currentSymbolMcsRate = Bits(GlobalDefine().mcsRateWidth bits)
  val mcsRateReg = Reg(Bits(GlobalDefine().mcsRateWidth bits)) init(B"1011")
  val numPlcpDataBytesLen = Reg(UInt(GlobalDefine().plcpDataBytesLenWidth bits))
  val symbolType = U(GlobalDefine().legacySignalSymbol,GlobalDefine().symbolTypeWidth bits)
  val pktTypeReg = RegInit(False)
  val numMinusOneBitsTranslateThisTime = UInt(GlobalDefine().parallelBusDataInWidth bits)
  numMinusOneBitsTranslateThisTime := 23

  val mainFSM = new StateMachine{
    setEncoding(binaryOneHot) // 比較複雜的狀態機，使用onehot code，可節省邏輯電路
    io.dataIn.ready := False
    currentSymbolMcsRate := mcsRateReg

    val stateIdle : State = new State with EntryPoint {
      whenIsActive{
        when(io.dataIn.valid){
          stfGenArea.setTrig()
          goto(stateLegacyLTF)
        }
      }
    }
    val stateLegacyLTF : State = new State{
      whenIsActive {
        when(io.dataIn.valid && ofdmTxReady) {
          goto(stateLegacySig)
          io.dataIn.ready := True
          mcsRateReg := io.dataIn.payload(3 downto 0)
          numPlcpDataBytesLen := io.dataIn.payload(16 downto 5).resized
          pktTypeReg := io.dataIn.payload(pktTypePos) // it used to verify legacy or HT
        }
      }
    }
    val stateLegacySig : State = new State {
      onEntry{
      }
      whenIsActive {
        currentSymbolMcsRate := B("1011")
        symbolType := GlobalDefine().legacySignalSymbol

        when(io.dataIn.valid && ofdmTxReady) {
          io.dataIn.ready := True
          when(pktTypeReg){
            // because only implement one antenna and 20 MHz channel
            // igore bit4 ~ bit6 of MCS field
            mcsRateReg := io.dataIn.payload(3 downto 0)
            numPlcpDataBytesLen := io.dataIn.payload(23 downto 8)
            goto(stateHtSig)
          } otherwise{
            goto(stateLegacyData)
          }
        }
      }
    }
    val stateLegacyData : State = new State{
      whenIsActive{
      }
    }
    val stateHtSig: State = new State {
      whenIsActive {
        currentSymbolMcsRate := B("1011")
        symbolType := GlobalDefine().htSignalSymbol
        numMinusOneBitsTranslateThisTime := 48-1

        when(io.dataIn.valid && ofdmTxReady) {
          //io.dataIn.ready := True
          goto(stateHtStf)
        }
      }
    }
    val stateHtStf: State = new State {
      whenIsActive {
        stfGenArea.assignNumRepeatRequreAssign(4)  //for saving resource, numRepeatRequre is combination logic, need to be keeped
        when((numSymbolTransmitted === 5).rise()){
          stfGenArea.setTrig()
        }
        when(stfDataOut.valid.fall()){
          goto(stateHtLTF)
        }
      }
    }
    val stateHtLTF: State = new State {
      whenIsActive {
        goto(stateHtData)
      }
    }
    val stateHtData : State = new State{

    }
  }

  // control numSymbolTransmitted
  when(mainFSM.isActive(mainFSM.stateIdle)){
    numSymbolTransmitted := 0
  } elsewhen(io.dataOut.valid.fall()){
    numSymbolTransmitted := numSymbolTransmitted + 1
  }


  // Receive the data and parallel to serial
  val p2sModule = DataParallelToSerial(DataInWidth)
  p2sModule.io.parallelIn := io.dataIn.payload
  p2sModule.io.mcsRate := currentSymbolMcsRate
  p2sModule.io.numMinusOneBitsTranslateThisTime := numMinusOneBitsTranslateThisTime
  p2sModule.io.trig := io.dataIn.ready
  p2sModule.io.symbolType := symbolType
  ofdmTxReady := p2sModule.io.inIdle

  // Convolution
  val convModule = Convolution()
  p2sModule.io.serialOut <> convModule.io.serialIn

  // Puncture and Interleave
  val punctInterleaveModule = PunctInterleaver()
  punctInterleaveModule.io.dataIn <> convModule.io.serialOut

  //pilot and zero insert, modulator is in the pilotInsertModule
  //val pilotInsertModule = PilotInsert()
  val pilotInsertModule = PilotInsertV2()
  pilotInsertModule.io.dataIn <> punctInterleaveModule.io.dataOut
  //pilotInsertModule.io.dataOut.ready := pilotInsertModule.io.dataOut.valid

  //LTF gen
  val ltfGenModule = LongTrainSignalGen()
  ltfGenModule.io.trig := mainFSM.isActive(mainFSM.stateLegacyLTF) || mainFSM.isActive(mainFSM.stateHtLTF)
  ltfGenModule.io.isHtLtf := mainFSM.isActive(mainFSM.stateHtLTF)

  //instantiate IFFT
  val ifftModule = FftByCsgRadix2(64, GlobalDefine().modulationDataOutWidth, GlobalDefine().rotationFactorWidth, forFFT = false)
  //ifftModule.io.dataIn <> pilotInsertModule.io.dataOut
  when(ltfGenModule.io.dataOut.valid){
    ifftModule.io.dataIn <> ltfGenModule.io.dataOut
    ifftModule.io.giTypeIn := ltfGenModule.io.giType
    pilotInsertModule.io.dataOut.ready := False
  } otherwise{
    ifftModule.io.dataIn <> pilotInsertModule.io.dataOut
    ifftModule.io.giTypeIn := GlobalDefine().normalGiType
    ltfGenModule.io.dataOut.ready := False
  }

  //instantiate GpAndWindowing
  val reorderIfftAndAddGiModule = ReorderIfftAndAddGi()
  reorderIfftAndAddGiModule.io.dataIn <> ifftModule.io.dataOut
  reorderIfftAndAddGiModule.io.giTypeIn := ifftModule.io.giTypeOut

  //io.dataOut assignment
  io.dataOut := Mux(stfDataOut.valid, stfDataOut, reorderIfftAndAddGiModule.io.dataOut)
}


object OfdmTx {
  def main(args: Array[String]) {
    SpinalConfig(
      //nameWhenByFile = false,
      genLineComments = true
    ).generateVerilog(OfdmTx(64,16))
    //SpinalVerilog(OfdmTx(64,16)) //Or SpinalVerilog
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

    val spinalConfig = SpinalConfig(
      nameWhenByFile = false,
      rtlHeader = "By Ryan",
      headerWithDate = true,
      genLineComments = true,
      defaultClockDomainFrequency = FixedFrequency(10 MHz))
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
        //while(index < lines.length){
        while(index < 2){
          if(dut.io.dataIn.ready.toBoolean == true){
            index = index + 1
          }
          println("In simulation, index :"+index+"   --- dut.io.dataIn.ready :"+dut.io.dataIn.ready.toBoolean)
          dut.io.dataIn.payload #= BigInt(lines(index), 16)
          dut.clockDomain.waitSampling(1)
        }

        dut.clockDomain.waitSampling(10)
        //dut.convModule.io.serialOut.ready #= true
        dut.clockDomain.waitSampling(1500)
        println("Simulation done")
        //println(GlobalDefine().amplitude_64QAM)
        simSuccess()

    }
  }
}