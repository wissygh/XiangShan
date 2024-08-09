package xiangshan.backend.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.HasXSParameter
import xiangshan.frontend.{BrType, FtqPtr, PreDecodeInfo}

trait TraceConfig extends HasXSParameter {
  implicit val p: Parameters
  def CauseWidth         = XLEN //64
  def TvalWidth          = XLEN
  def PrivWidth          = 3
  def IaddrWidth         = XLEN

  def ItypeWidth             = 4
  def IretireWidthInPipe     = log2Up(RenameWidth * 2)
  def IretireWidthCompressed = log2Up(RenameWidth * CommitWidth * 2)
  def IlastsizeWidth         = 1
  def TraceGroupNum          = 3 // Width to Encoder
  def HasEncoder             = false
  def TraceEnable            = true
}

class TraceTrap(implicit val p: Parameters) extends Bundle with TraceConfig {
  val cause = UInt(CauseWidth.W)
  val tval  = UInt(TvalWidth.W)
  val priv  = new PrivEnum
}

class TracePipe(iretireWidth: Int)(implicit val p: Parameters) extends Bundle with TraceConfig {
  val itype     = new ItypeEnum
  val iretire   = UInt(iretireWidth.W)
  val ilastsize = new IlastsizeEnum
}

class TraceBlock(hasIaddr: Boolean, iretireWidth: Int)(implicit val p: Parameters) extends Bundle with TraceConfig {
  val iaddr     = if (hasIaddr)   Some(UInt(IaddrWidth.W))                else None
  val ftqIdx    = if (!hasIaddr)  Some(new FtqPtr)                        else None
  val ftqOffset = if (!hasIaddr)  Some( UInt(log2Up(PredictWidth).W))     else None
  val tracePipe = new TracePipe(iretireWidth)
}

class TraceBundle(hasIaddr: Boolean, blockSize: Int, iretireWidth: Int)(implicit val p: Parameters) extends Bundle with TraceConfig {
  val trap = Output(new TraceTrap)
  val blocks = Vec(blockSize, ValidIO(new TraceBlock(hasIaddr, iretireWidth)))
}

class FromEncoder extends Bundle {
  val enable = Bool()
  val stall  = Bool()
}

class TraceCoreInterface(implicit val p: Parameters) extends Bundle with TraceConfig {
  val fromEncoder = Input(new FromEncoder)
  val toEncoder = new TraceBundle(true, TraceGroupNum, IretireWidthCompressed)
}

class ItypeEnum extends Bundle {
  val value                = UInt(4.W)
  def None                 = 0.U
  def Exception            = 1.U    //rob
  def Interrupt            = 2.U    //rob
  def ExpIntReturn         = 3.U    //rename
  def NonTaken             = 4.U    //commit
  def Taken                = 5.U    //commit
  def UninferableJump      = 6.U    //It's reserved(I think the code must not be adopted) when width of itype is 4.
  def reserved             = 7.U    //reserved
  def UninferableCall      = 8.U    //rename
  def InferableCall        = 9.U    //rename
  def UninferableTailCall  = 10.U   //rename
  def InferableTailCall    = 11.U   //rename
  def CoRoutineSwap        = 12.U   //rename
  def FunctionReturn       = 13.U   //rename
  def OtherUninferableJump = 14.U   //rename
  def OtherInferableJump   = 15.U   //rename

  // Assuming the branchType is taken here, it will be correctly modified after writeBack.
  def Branch = 5.U

  def clear(): Unit = {
    this.value := None
  }

  def jumpTypeGen(brType: UInt, rd: OpRegType, rs: OpRegType): Unit = {

    val isEqualRdRs = rd === rs
    val isJal       = brType === BrType.jal
    val isJalr      = brType === BrType.jalr
    val isBranch    = brType === BrType.branch

    // push to RAS when rd is link, pop from RAS when rs is link
    def isUninferableCall      = isJalr && rd.isLink && (!rs.isLink || rs.isLink && isEqualRdRs)  //8   push
    def isInferableCall        = isJal && rd.isLink                                               //9   push
    def isUninferableTailCall  = isJalr && rd.isX0 && !rs.isLink                                  //10  no op
    def isInferableTailCall    = isJal && rd.isX0                                                 //11  no op
    def isCoRoutineSwap        = (isJalr && rd.isLink && rs.isLink && !isEqualRdRs)               //12  pop then push
    def isFunctionReturn       = (isJalr && !rd.isLink && rs.isLink)                              //13  pop
    def isOtherUninferableJump = (isJalr && !rd.isLink && !rd.isX0 && !rs.isLink)                 //14  no op
    def isOtherInferableJump   = isJal && !rd.isLink && !rd.isX0                                  //15  no op

    val jumpType = Mux1H(
      Seq(
        isBranch,
        isUninferableCall,
        isInferableCall,
        isUninferableTailCall,
        isInferableTailCall,
        isCoRoutineSwap,
        isFunctionReturn,
        isOtherUninferableJump,
        isOtherInferableJump,
      ),
      Seq(
        Branch,
        UninferableCall,
        InferableCall,
        UninferableTailCall,
        InferableTailCall,
        CoRoutineSwap,
        FunctionReturn,
        OtherUninferableJump,
        OtherInferableJump,
      )
    )
    this.value := Mux(isBranch || isJal || isJalr, jumpType, 0.U)
  }

  // supportSijump
  def isTrap = Seq(Exception, Interrupt).map(_ === this.value).reduce(_ || _)

  def isNotNone = this.value =/= None

  def isBranchType = this.value === Branch

  // supportSijump
  def isUninferable = Seq(UninferableCall, UninferableTailCall, CoRoutineSwap,
    UninferableTailCall, OtherUninferableJump).map(_ === this.value).reduce(_ || _)
}

object ItypeEnum extends ItypeEnum{
  def apply = new ItypeEnum
}

class IlastsizeEnum extends Bundle {
  val value = UInt(1.W)
  def HalfWord = 0.U
  def Word     = 1.U
}

object IlastsizeEnum extends IlastsizeEnum {
  def apply()   = new IlastsizeEnum
}

class PrivEnum extends Bundle {
  val value = UInt(3.W)
  def HU = 0.U
  def HS = 1.U
  def M  = 3.U
  def D  = 4.U
  def VU = 5.U
  def VS = 6.U
}

object PrivEnum extends PrivEnum {
  def apply()   = new PrivEnum
}


class OpRegType extends Bundle {
  val value = UInt(3.W)
  def isX0   = this.value === 0.U
  def isX1   = this.value === 1.U
  def isX5   = this.value === 5.U
  def isLink = Seq(isX1, isX5).map(_ === this.value).reduce(_ || _)
}