package ofdm_tx

import ofdm_tx.ButterflyDataIf
import ofdm_tx.ComplexDataType.setValueAsZero
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._

/*
* 參考google driver上的pilotLocation 試算表文件
* PilotInsert()的設計理念為先將對應到data index 0~23(對應到負頻的sub carrier) 與pilot
* 先經過modulator，將其輸出緩存到記憶體中，並拉高輸出的valid信號。
* 等到之後的ifft電路備妥(輸出端的ready信號備妥)，並將data index 24~47的資料與先前緩存的資料
* 平行的輸出到ifft電路。
*
* 這樣實現的好處是，構造簡單，速度快。但卻因為緩存了24筆調變後的資料(16 bits*24*2, I and Q)，有點浪費資源。
* 以目前實現的結果來看，nBPSC=1，code rate＝1/2，須使用192 clock。(以80MHz來算，大約2.4us，還有128clock的餘裕。)
*/
case class PilotInsert() extends Component{
  val io = new Bundle{
    val dataIn = slave(Stream(PunctInterleaverDataOutIF()))
    val dataOut = master(Stream(ButterflyDataIf(GlobalDefine().modulationDataOutWidth) ))
  }

  def cntWidth : Int =  6

  //instantiate modulation
  val modulator = Modulation()
  val modulatorInputMux = cloneOf(modulator.io.dataIn.data)
  val modulatorNbpscMux = cloneOf(modulator.io.dataIn.nBPSC)
  val modulatorOutCtrl = cloneOf(modulator.io.dataOut)
  modulator.io.dataIn.data := modulatorInputMux
  modulator.io.dataIn.nBPSC := modulatorNbpscMux
  when(io.dataIn.payload.symbolType === GlobalDefine().htSignalSymbol){
    // shift 90 degree for HT signal symbol
    modulatorOutCtrl.I := modulator.io.dataOut.Q
    modulatorOutCtrl.Q := modulator.io.dataOut.I
  } otherwise{
    modulatorOutCtrl := modulator.io.dataOut
  }

  //input data buffer for data 0 ~ 23 and two Pilot Tone, 24+2=26
  val pilotInsertCnt = Reg(UInt(cntWidth bits)) init(0)
  val dataInReg = Vec.fill(26)(Reg( ComplexDataType(GlobalDefine().modulationDataOutWidth) ))
  val pushEnable = Bool() // when true, data push into dataInReg
  val pilotTiming = Bool()   // the time for insert pilot
  val inputPreHalfValidTiming = io.dataIn.fire || pilotTiming

  // 57=63-6
  // 請參考 google driver 的 pilotLocation檔案內的pilotInsertCnt欄位
  // 前26點 {data(24) + pilot(2)}不用考慮dataOut的ready信號，這些資料會先填充到dataInReg
  // 後32點就必須根據等待dataOut的ready信號
  when(pilotInsertCnt < 26){
    when(inputPreHalfValidTiming){
      pilotInsertCnt := pilotInsertCnt + 1
    }
  } otherwise{
    when(io.dataOut.fire){
      when(pilotInsertCnt === 57){
        pilotInsertCnt := 0
      } otherwise{
        pilotInsertCnt := pilotInsertCnt + 1
      }
    }
  }

  when(pushEnable ){
    for (index <- 1 until dataInReg.length) {
      dataInReg(index) := dataInReg(index - 1)
    }
    dataInReg(0) := modulatorOutCtrl
  }

  //record the symbolType
  val isHtDataSymbol = io.dataIn.symbolType === GlobalDefine().htDataSymbol
  val isLegacyDataSymbol = io.dataIn.symbolType === GlobalDefine().legacyDataSymbol
  val isDataSymbol = isHtDataSymbol || isLegacyDataSymbol
  val scrambleReg = RegInit(B"1111111")
  val scrambleIn = (scrambleReg(3)^scrambleReg(6))
  when(io.dataIn.fire && (pilotInsertCnt === 0)){
    when(isDataSymbol){
      //only data symbol needs to scramble the pilot
      scrambleReg := (scrambleReg(5 downto 0),scrambleIn).asBits
    } otherwise{
      scrambleReg := B"1111111"
    }
  }

  // pilotInsertCnt : 0 ~ 25, input preHalf data into dataInReg
  // pilotInsertCnt : 26 ~ 31, lower path only output null tone.
  // pilotInsertCnt : 32 ~ 63, output lower path data from dataInReg
  pushEnable := ((pilotInsertCnt < 26) && inputPreHalfValidTiming) || ((pilotInsertCnt >31) && io.dataOut.fire ) // index : 26 ~ 32, lower path only output null tone.
  pilotTiming := (pilotInsertCnt === 5) || (pilotInsertCnt === 19) || (pilotInsertCnt === 33) || (pilotInsertCnt === 47)

  when(pilotTiming) {
    when(pilotInsertCnt === 47) {
      modulatorInputMux := Mux(scrambleIn, B(1, GlobalDefine().modulatonDataInWidth bits) , B(0, GlobalDefine().modulatonDataInWidth bits))
      modulatorNbpscMux := 1
    } otherwise {
      modulatorInputMux := Mux(scrambleIn, B(0, GlobalDefine().modulatonDataInWidth bits) , B(1, GlobalDefine().modulatonDataInWidth bits))
      modulatorNbpscMux := 1
    }
  } otherwise {
    modulatorInputMux := io.dataIn.data
    modulatorNbpscMux := io.dataIn.nBPSC
  }

  //io.dataIn.ready control
  //pilotInsertCnt==26 is DC
  val dataInReadyCtrl = False
  when(pilotInsertCnt < 26){
    dataInReadyCtrl := True
  } elsewhen((pilotInsertCnt > 26) && (pilotInsertCnt < 53)){
    dataInReadyCtrl := True
  }
  io.dataIn.ready := dataInReadyCtrl && (~pilotTiming)

  //io.dataOut.valid control
  io.dataOut.valid := Mux((pilotInsertCnt>25),True,False)

  //io.dataOut control
  io.dataOut.aData.I := 0
  io.dataOut.aData.Q := 0
  io.dataOut.bData.I := 0
  io.dataOut.bData.Q := 0

  when(pilotInsertCnt > 31){
    io.dataOut.bData := dataInReg(dataInReg.length-1)
  }
  when((pilotInsertCnt > 26) && (pilotInsertCnt < 53) ){
    io.dataOut.aData := modulatorOutCtrl
  }
}

/*
* PilotInsertV2()
* 為了解決緩存所帶來的資源浪費
* 只緩存data index 0-23的調變前的原始資料(6 bits X 24)
* 利用乒乓切換modulator的input，會稍微增加電路的複雜度與些微的延遲(<24 clock)
*/
case class PilotInsertV2() extends Component{
  val io = new Bundle{
    val dataIn = slave(Stream(PunctInterleaverDataOutIF()))
    val dataOut = master(Stream(ButterflyDataIf(GlobalDefine().modulationDataOutWidth) ))
  }

  def cntWidth : Int =  5 //Max 31
  val amplitude_BPSK = GlobalDefine().amplitude_BPSK
  val preHalfDataLen = 24

  //default value setting
  io.dataOut.valid := False
  io.dataIn.ready := False
  val symbolTypeReg = Reg(cloneOf(io.dataIn.symbolType)) init(GlobalDefine().legacySignalSymbol)

  // scramble pilot for data Symbol
  val scrambleReg = RegInit(B"1111111")
  val scrambleIn = (scrambleReg(3) ^ scrambleReg(6))
  val pilotBpsk_43_57_7 = cloneOf(io.dataOut.aData) // pilot @ -21, -7, 7 subcarrier
  val pilotBpsk_21 = cloneOf(io.dataOut.aData) // pilot @ 21 subcarrier
  val isHtDataSymbol = io.dataIn.symbolType === GlobalDefine().htDataSymbol
  val isLegacyDataSymbol = io.dataIn.symbolType === GlobalDefine().legacyDataSymbol
  val symbolIsData = isHtDataSymbol || isLegacyDataSymbol
  val symbolIsHtSig = io.dataIn.symbolType === GlobalDefine().htSignalSymbol
  val pilotBpskCond = B(scrambleIn, symbolIsHtSig)
  val pilotScramblerEn = False
  when(symbolIsData) { //only data symbol needs to scramble the pilot
    when(pilotScramblerEn) {
      scrambleReg := (scrambleReg(5 downto 0), scrambleIn).asBits
    }
  } otherwise {
    scrambleReg := B"1111111" // set default value
  }

  switch(pilotBpskCond){
    is(B"00"){ // scrambleIn = 0,  symbolIsHtSig = 0
      pilotBpsk_43_57_7.I := amplitude_BPSK
      pilotBpsk_43_57_7.Q := 0
      pilotBpsk_21.I := amplitude_BPSK * -1
      pilotBpsk_21.Q := 0
    }
    is(B"01"){ // scrambleIn = 0,  symbolIsHtSig = 1
      pilotBpsk_43_57_7.I := 0
      pilotBpsk_43_57_7.Q := amplitude_BPSK
      pilotBpsk_21.I := 0
      pilotBpsk_21.Q := amplitude_BPSK * -1
    }
    is(B"10"){ // scrambleIn = 1,  symbolIsHtSig = 0
      pilotBpsk_43_57_7.I := amplitude_BPSK * -1
      pilotBpsk_43_57_7.Q := 0
      pilotBpsk_21.I := amplitude_BPSK
      pilotBpsk_21.Q := 0
    }
    default{ // scrambleIn = 1,  symbolIsHtSig = 1
      pilotBpsk_43_57_7.I := 0
      pilotBpsk_43_57_7.Q := amplitude_BPSK * -1
      pilotBpsk_21.I := 0
      pilotBpsk_21.Q := amplitude_BPSK
    }
  }

  //instantiate modulation
  val toggleSwitchReg = Reg(Bool()) init(False)
  val modulator = Modulation()
  val modulatorOutCtrl = cloneOf(modulator.io.dataOut)
  val modulatorOutCtrlReg = RegNext(modulatorOutCtrl)

  modulator.io.dataIn.nBPSC := io.dataIn.nBPSC
  when(symbolTypeReg === GlobalDefine().htSignalSymbol){
    // shift 90 degree for HT signal symbol
    modulatorOutCtrl.I := modulator.io.dataOut.Q
    modulatorOutCtrl.Q := modulator.io.dataOut.I
  } otherwise{
    modulatorOutCtrl := modulator.io.dataOut
  }

  when((toggleSwitchReg === False ) && io.dataIn.fire){
    modulatorOutCtrlReg := modulatorOutCtrl
  }

  //input data buffer for data 0 ~ 23
  val pilotInsertCnt = Reg(UInt(cntWidth bits)) init(0)
  val dataInReg = Vec.fill(24)(Reg( cloneOf(io.dataIn.data) ))
  val dataInRegInMux = cloneOf(io.dataIn.data)
  val dataInRegPush = False
  when(dataInRegPush){
    for(index <- 1 until dataInReg.length){
      dataInReg(index) := dataInReg(index-1)
    }
    dataInReg(0) := dataInRegInMux
  }

  // state machine
  // Upper means the upper path of butterfly
  // Lower means the lower path of butterfly
  // Null Tone means output Zero
  val complexDataZero = cloneOf(io.dataOut.aData)
  setValueAsZero(complexDataZero)
  io.dataOut.aData := complexDataZero
  io.dataOut.bData := complexDataZero

  val fsm = new StateMachine{
    val stateReadPreHalfData : State = new State with EntryPoint {
      onEntry{
        pilotInsertCnt := 0
        toggleSwitchReg := False
      }
      whenIsActive{
        io.dataIn.ready := True
        when(io.dataIn.fire){
          pilotInsertCnt := pilotInsertCnt + 1
          dataInRegPush := True
          when(pilotInsertCnt === (preHalfDataLen-1)){
            goto(stateOutputDC)
          }
        }
      }
    }

    val stateOutputDC : State = new State{
      onEntry {
        pilotInsertCnt := 0
        symbolTypeReg := io.dataIn.symbolType
      }
      whenIsActive{
        io.dataOut.valid := True
        when(io.dataOut.fire){
          goto(stateOutputUpperWithNullTone)
        }
      }
    }

    val stateOutputUpperWithNullTone : State = new State{
      onEntry {
        pilotInsertCnt := 0
      }
      whenIsActive{
        io.dataOut.valid := io.dataIn.valid
        io.dataIn.ready := io.dataOut.fire

        // contorl the io.dataOut
        io.dataOut.aData := modulatorOutCtrl
        io.dataOut.bData := complexDataZero

        when(io.dataOut.fire){
          pilotInsertCnt := pilotInsertCnt + 1
          when(pilotInsertCnt === 4){
            goto(stateOutputUpperAndLower)
          }
        }
      }
    }

    val stateOutputUpperAndLower : State = new State{
      onEntry(toggleSwitchReg := False)
      whenIsActive{
        // mux input to modulator
        io.dataOut.valid := toggleSwitchReg && io.dataIn.valid
        io.dataIn.ready := io.dataOut.fire

        // contorl the io.dataOut
        io.dataOut.aData := modulatorOutCtrlReg
        io.dataOut.bData := modulatorOutCtrl

        when(toggleSwitchReg === False){
          toggleSwitchReg := True
        } elsewhen(io.dataOut.fire){
          toggleSwitchReg := False
        }
        dataInRegPush := io.dataOut.fire

        when(io.dataOut.fire){
          pilotInsertCnt := pilotInsertCnt + 1
          when(pilotInsertCnt === 5){
            goto(stateOutputPosPilotWithLower)
          } elsewhen( pilotInsertCnt === 19 ){
            goto(stateOutputNegPilotWithLower)
          } elsewhen( (pilotInsertCnt === 9) || (pilotInsertCnt === 23)){
            goto(stateOutputUpperWithPosPilot)
          } elsewhen( pilotInsertCnt === 25 ){
            goto(stateOutputNullToneWithLower)
          }
        }
      }
    }

    val stateOutputPosPilotWithLower : State = new State{
      onEntry(toggleSwitchReg := True)
      whenIsActive{
        io.dataOut.valid := True
        dataInRegPush := io.dataOut.fire

        // contorl the io.dataOut
        io.dataOut.aData := pilotBpsk_43_57_7
        io.dataOut.bData := modulatorOutCtrl

        when(io.dataOut.fire){
          pilotInsertCnt := pilotInsertCnt + 1
          goto(stateOutputUpperAndLower)
        }
      }
    }

    val stateOutputNegPilotWithLower : State = new State{
      onEntry(toggleSwitchReg := True)
      whenIsActive {
        io.dataOut.valid := True
        dataInRegPush := io.dataOut.fire

        // contorl the io.dataOut
        io.dataOut.aData := pilotBpsk_21
        io.dataOut.bData := modulatorOutCtrl

        when(io.dataOut.fire) {
          pilotInsertCnt := pilotInsertCnt + 1
          goto(stateOutputUpperAndLower)
        }
      }
    }

    val stateOutputUpperWithPosPilot : State = new State{
      whenIsActive {
        io.dataOut.valid := io.dataIn.valid
        io.dataIn.ready := io.dataOut.fire

        // contorl the io.dataOut
        io.dataOut.aData := modulatorOutCtrl
        io.dataOut.bData := pilotBpsk_43_57_7

        when(io.dataOut.fire) {
          pilotInsertCnt := pilotInsertCnt + 1
          goto(stateOutputUpperAndLower)
        }
      }
    }

    val stateOutputNullToneWithLower : State = new State{
      onEntry(toggleSwitchReg := True)
      whenIsActive {
        io.dataOut.valid := True
        dataInRegPush := io.dataOut.fire

        // contorl the io.dataOut
        io.dataOut.aData := complexDataZero
        io.dataOut.bData := modulatorOutCtrl

        when(io.dataOut.fire) {
          pilotInsertCnt := pilotInsertCnt + 1
          when(pilotInsertCnt === 30){
            goto(stateReadPreHalfData)
            pilotScramblerEn := True
          }
        }
      }
    }

  }

  // mux the input for dataInReg
  when(fsm.isActive(fsm.stateReadPreHalfData)){
    dataInRegInMux := io.dataIn.data
  } otherwise{
    dataInRegInMux := 0
  }

  // control the input of modulator
  modulator.io.dataIn.data := Mux(toggleSwitchReg, dataInReg(dataInReg.length-1), io.dataIn.data)


}

object PilotInsert {
  def main(args: Array[String]) {
    SpinalVerilog(PilotInsert()) //Or SpinalVerilog
  }
}
