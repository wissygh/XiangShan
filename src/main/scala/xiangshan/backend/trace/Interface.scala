package xiangshan.backend.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.HasXSParameter
import xiangshan.frontend.{BrType, PreDecodeInfo}

trait TraceConfig extends HasXSParameter {
  implicit val p: Parameters
  def causeWidth = XLEN //64
  def tvalWidth = XLEN
  def privWidth = 3

  def blockNum = CommitWidth
  def itypeWidth = 4
  def iaddrWidth = VAddrBits
  def iretireWidth = log2Up(RenameWidth * 2)
}

class TraceTrap(implicit val p: Parameters) extends Bundle with TraceConfig {
  val cause = UInt(causeWidth.W)
  val tval  = UInt(tvalWidth.W)
  val priv  = new PrivEnum
}

class TracePipe(implicit val p: Parameters) extends Bundle with TraceConfig {
  val itype     = new ItypeEnum
  val iretire   = UInt(iretireWidth.W)
  val ilastsize = new IlastsizeEnum
}

class TraceBlock(implicit val p: Parameters) extends Bundle with TraceConfig {
  val iaddr     = UInt(iaddrWidth.W)
  val tracePipe = new TracePipe
}

class Interface(implicit val p: Parameters) extends Bundle with TraceConfig {
  val fromEncoder = Input(new Bundle {
  })

  val toEncoder = Output(new Bundle {
    val trap = new TraceTrap
    val blocks = Vec(blockNum, ValidIO(new TraceBlock))
  })
}

class ItypeEnum extends Bundle {
  val value                = UInt(4.W)
  def None                 = 0.U
  def Exception            = 1.U    //rob
  def Interrupt            = 2.U    //rob
  def ExpIntReturn         = 3.U    //rename
  def NonTaken             = 4.U    //commit
  def Taken                = 5.U    //commit
  def UninferableJump      = 6.U    //reserved
  def UninferableCall      = 8.U    //rename
  def InferableCall        = 9.U    //rename
  def UninferableTailCall  = 10.U   //rename
  def InferableTailCall    = 11.U   //rename
  def CoRoutineSwap        = 12.U   //rename
  def isFunctionReturn     = 13.U   //rename
  def OtherUninferableJump = 14.U   //rename
  def OtherInferableJump   = 15.U   //rename

  def clear(): Unit = {
    this.value := None
  }

  def jumpTypeGen(brType: UInt, rd: OpRegType, rs: OpRegType): ItypeEnum = {

    val isEqualRdRs = rd === rs
    val isJal       = brType === BrType.jal
    val isJalr      = brType === BrType.jalr

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
        isUninferableCall,
        isInferableCall,
        isUninferableTailCall,
        isInferableTailCall,
        isCoRoutineSwap,
        isFunctionReturn,
        isOtherUninferableJump,
        isOtherInferableJump,
      ),
      (8 to 15).map(i => i.U)
    )
    Mux(isJal || isJalr, jumpType, 0.U).asTypeOf(new ItypeEnum)
  }

  // supportSijump
  def isTrap = Seq(Exception, Interrupt).map(_ === this.value).reduce(_ || _)

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


class OpRegType extends Bundle {
  val value = UInt(3.W)
  def isX0   = this.value === 0.U
  def isX1   = this.value === 1.U
  def isX5   = this.value === 5.U
  def isLink = Seq(isX1, isX5).map(_ === this.value).reduce(_ || _)
}