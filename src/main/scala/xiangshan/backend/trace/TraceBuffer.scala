package xiangshan.backend.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}
import xiangshan.{HasXSParameter, XSCoreParamsKey}

class TraceBuffer(implicit val p: Parameters) extends Module
  with TraceConfig
  with HasCircularQueuePtrHelper {

  val io = IO(new Bundle {
    val in = new Bundle{
      val fromEncoder = Input(new FromEncoder)
      val fromRob     = Flipped(new TraceBundle(false, CommitWidth, IretireWidthCompressed))
    }

    val out = new Bundle { // output groups to pcMem
      val blockCommit = Output(Bool())
      val groups = new TraceBundle(false, GroupNum, IretireWidthCompressed)
    }
  })

  // buffer: rob commit info compressed
  val traceTrap    = Reg(new TraceTrap)
  val traceEntries = Reg(Vec(CommitWidth, ValidIO(new TraceBlock(false, IretireWidthCompressed))))
  traceTrap := io.in.fromRob.trap

  val blockCommit = RegInit(false.B) // to rob

  /**
   * 1. compress, update blocks
   */
  val inValidVec = VecInit(io.in.fromRob.blocks.map(_.valid))
  val inTypeIsNotNoneVec = VecInit(io.in.fromRob.blocks.map(_.bits.tracePipe.itype.isNotNone))
  val needPcVec = Wire(Vec(CommitWidth, Bool()))
  for(i <- 0 until CommitWidth) {
    val rightHasValid = if(i == CommitWidth - 1) false.B  else (inValidVec.asUInt(CommitWidth-1, i+1).orR)
    needPcVec(i) := inValidVec(i) & (inTypeIsNotNoneVec(i) || !rightHasValid) & !blockCommit
  }

  val blocksUpdate = WireInit(io.in.fromRob.blocks)
  for(i <- 1 until CommitWidth){
    when(!needPcVec(i-1)){
      blocksUpdate(i).bits.tracePipe.iretire := blocksUpdate(i - 1).bits.tracePipe.iretire + io.in.fromRob.blocks(i).bits.tracePipe.iretire
      blocksUpdate(i).bits.ftqOffset.get := blocksUpdate(i - 1).bits.ftqOffset.get
      blocksUpdate(i).bits.ftqIdx.get := blocksUpdate(i - 1).bits.ftqIdx.get
     }
  }

  /**
   * 2. enq to traceEntries
   */
  val countVec = VecInit((0 until CommitWidth).map(i => PopCount(needPcVec.asUInt(i, 0))))
  val numNeedPc = countVec(CommitWidth-1)

  val enqPtr = RegInit(TracePtr(false.B, 0.U))
  val deqPtr = RegInit(TracePtr(false.B, 0.U))
  val deqPtrPre = RegNext(deqPtr)
  val enqPtrNext = WireInit(enqPtr)
  val deqPtrNext = WireInit(deqPtr)
  enqPtr := enqPtrNext
  deqPtr := deqPtrNext
  val numValidEntries = distanceBetween(enqPtrNext, deqPtrNext)
  blockCommit := (numValidEntries > 0.U) && (io.in.fromEncoder.enable) &&(!io.in.fromEncoder.stall)

  enqPtrNext := enqPtr + numNeedPc
  deqPtrNext := Mux(deqPtr + GroupNum.U > enqPtrNext, enqPtrNext, deqPtr + GroupNum.U)

  val traceIdxVec = VecInit(countVec.map(count => (enqPtr + count - 1.U).value))

  for(i <- 0 until CommitWidth){
    when(needPcVec(i)){
      traceEntries(traceIdxVec(i)) := blocksUpdate(i)
    }
  }

  /**
   * 2. buffer out
   */
  val blockOut = WireInit(0.U.asTypeOf(io.out.groups))
  blockOut.trap := traceTrap
  for(i <- 0 until GroupNum) {
    when(deqPtrPre + i.U < enqPtr) {
      blockOut.blocks(i) := traceEntries((deqPtrPre + i.U).value)
    } .otherwise {
      blockOut.blocks(i).valid := false.B
    }
  }

  dontTouch(countVec)
  dontTouch(numNeedPc)
  dontTouch(traceIdxVec)

  io.out.blockCommit := blockCommit
  io.out.groups := blockOut

}

class TracePtr(entries: Int) extends CircularQueuePtr[TracePtr](
  entries
) with HasCircularQueuePtrHelper {

  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).CommitWidth)

  def lineHeadPtr(implicit p: Parameters): TracePtr = {
    val CommitWidth = p(XSCoreParamsKey).CommitWidth
    val out = Wire(new TracePtr)
    out.flag := this.flag
    out.value := Cat(this.value(this.PTR_WIDTH-1, log2Up(CommitWidth)), 0.U(log2Up(CommitWidth).W))
    out
  }

}

object TracePtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): TracePtr = {
    val ptr = Wire(new TracePtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}