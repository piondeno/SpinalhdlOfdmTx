package ofdm_tx

import spinal.core._
import spinal.lib._
//import spinal.lib.fsm._
//import spinal.core.sim._

case class ConvolutionDataOutIf() extends Bundle{
  val data = Bits(2 bits)
  val mcsRate = Bits(GlobalDefine().mcsRateWidth bits)
  val symbolType = UInt(GlobalDefine().symbolTypeWidth bits)
}

case class Convolution() extends Component{
  val io = new Bundle{
    val serialIn = slave(Stream(DataParallelToSerialOutIF()))
    val serialOut = master(Stream(ConvolutionDataOutIf()))
  }

  def convRegLen : Int = 6
  io.serialOut.symbolType := io.serialIn.symbolType

  //when(io.serialIn.valid===)

  //val punctPlanReg = RegNextWhen(io.punctPlan, io.serialIn.valid===False)
  val convReg = RegInit(B(0,convRegLen bits))
  when(io.serialOut.fire && io.serialIn.valid){
    convReg := (convReg(convRegLen-2 downto 0),io.serialIn.data).asBits
  }

  val convOutA,convOutB =Bool()
  convOutA := io.serialIn.data ^ convReg(1) ^ convReg(2) ^ convReg(4) ^ convReg(5)
  convOutB := io.serialIn.data ^ convReg(0) ^ convReg(1) ^ convReg(2) ^ convReg(5)
  io.serialOut.data := (convOutB,convOutA).asBits
  //io.serialOut.data := (convOutA,convOutB).asBits

  io.serialOut.valid := io.serialIn.valid
  io.serialOut.mcsRate := io.serialIn.mcsRATE
  io.serialIn.ready := io.serialOut.ready
}
