package xiangshan.backend.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils.NamedUInt
import xiangshan._
import xiangshan.frontend.{BrType, FtqPtr, PreDecodeInfo}
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import top.{ArgParser, BaseConfig, DefaultConfig, Generator, MinimalConfig}
import xiangshan.backend.fu.NewCSR.NewCSR
import xiangshan.backend.fu.NewCSR.NewCSRMain.args

class TraceTrap(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val cause = UInt(CauseWidth.W)
  val tval  = UInt(TvalWidth.W)
  val priv  = Priv()
}

class TracePipe(iretireWidth: Int)(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val itype     = Itype()
  val iretire   = UInt(iretireWidth.W)
  val ilastsize = Ilastsize()
}

class TraceBlock(hasIaddr: Boolean, iretireWidth: Int)(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val iaddr     = if (hasIaddr)   Some(UInt(IaddrWidth.W))                else None
  val ftqIdx    = if (!hasIaddr)  Some(new FtqPtr)                        else None
  val ftqOffset = if (!hasIaddr)  Some(UInt(log2Up(PredictWidth).W))      else None
  val tracePipe = new TracePipe(iretireWidth)
}

class TraceBundle(hasIaddr: Boolean, blockSize: Int, iretireWidth: Int)(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val trap = Output(new TraceTrap)
  val blocks = Vec(blockSize, ValidIO(new TraceBlock(hasIaddr, iretireWidth)))
}

class FromEncoder extends Bundle {
  val enable = Bool()
  val stall  = Bool()
}

class TraceCoreInterface(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val fromEncoder = Input(new Bundle {
    val enable = Bool()
    val stall  = Bool()
  })
  val toEncoder   = Output(new Bundle {
    val cause     = UInt(CauseWidth.W)
    val tval      = UInt(TvalWidth.W)
    val priv      = UInt(PrivWidth.W)
    val iaddr     = UInt((TraceGroupNum * IaddrWidth).W)
    val itype     = UInt((TraceGroupNum * ItypeWidth).W)
    val iretire   = UInt((TraceGroupNum * IretireWidthCompressed).W)
    val ilastsize = UInt((TraceGroupNum * IlastsizeWidth).W)
  })
}

object Itype extends NamedUInt(4) {
  def None                 = 0.U
  def Exception            = 1.U    //rob
  def Interrupt            = 2.U    //rob
  def ExpIntReturn         = 3.U    //rename
  def NonTaken             = 4.U    //commit
  def Taken                = 5.U    //commit
  def UninferableJump      = 6.U    //It's reserved when width of itype is 4.
  def reserved             = 7.U    //reserved
  def UninferableCall      = 8.U    //rename
  def InferableCall        = 9.U    //rename
  def UninferableTailCall  = 10.U   //rename
  def InferableTailCall    = 11.U   //rename
  def CoRoutineSwap        = 12.U   //rename
  def FunctionReturn       = 13.U   //rename
  def OtherUninferableJump = 14.U   //rename
  def OtherInferableJump   = 15.U   //rename

  // Assuming the branchType is NonTaken here, it will be correctly modified after writeBack.
  def Branch = NonTaken

  def jumpTypeGen(brType: UInt, rd: OpRegType, rs: OpRegType): UInt = {

    val isEqualRdRs = rd === rs
    val isJal       = brType === BrType.jal
    val isJalr      = brType === BrType.jalr
    val isBranch    = brType === BrType.branch

    // push to RAS when rd is link, pop from RAS when rs is link
    def isUninferableCall      = isJalr && rd.isLink && (!rs.isLink || rs.isLink && isEqualRdRs)  //8   push
    def isInferableCall        = isJal && rd.isLink                                               //9   push
    def isUninferableTailCall  = isJalr && rd.isX0 && !rs.isLink                                  //10  no op
    def isInferableTailCall    = isJal && rd.isX0                                                 //11  no op
    def isCoRoutineSwap        = isJalr && rd.isLink && rs.isLink && !isEqualRdRs                 //12  pop then push
    def isFunctionReturn       = isJalr && !rd.isLink && rs.isLink                                //13  pop
    def isOtherUninferableJump = isJalr && !rd.isLink && !rd.isX0 && !rs.isLink                   //14  no op
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

    Mux(isBranch || isJal || isJalr, jumpType, 0.U)
  }

  def isTrap(itype: UInt) = Seq(Exception, Interrupt).map(_ === itype).reduce(_ || _)

  def isNotNone(itype: UInt) = itype =/= None

  def isBranchType(itype: UInt) = itype === Branch

  // supportSijump
  def isUninferable(itype: UInt) = Seq(UninferableCall, UninferableTailCall, CoRoutineSwap,
    UninferableTailCall, OtherUninferableJump).map(_ === itype).reduce(_ || _)
}

object Ilastsize extends NamedUInt(1) {
  def HalfWord = 0.U
  def Word     = 1.U
}

object Priv extends NamedUInt(3) {
  def HU = 0.U
  def HS = 1.U
  def M  = 3.U
  def D  = 4.U
  def VU = 5.U
  def VS = 6.U
}

class OpRegType extends Bundle {
  val value = UInt(6.W)
  def isX0   = this.value === 0.U
  def isX1   = this.value === 1.U
  def isX5   = this.value === 5.U
  def isLink = Seq(isX1, isX5).reduce(_ || _)
}

/*
class ItypeGen extends Module {
  val io = IO(new Bundle {
    val brType = Input(UInt(2.W))
    val rd     = Input(UInt(6.W))
    val rs     = Input(UInt(6.W))
    val itype  = Output(UInt(4.W))
    val rsIslink = Output(Bool())
    val rsIs0 = Output(Bool())
    val rdIslink = Output(Bool())
    val rdIs0 = Output(Bool())
    val sel = Output(UInt(9.W))
  })

  val rd = io.rd.asTypeOf(new OpRegType)
  val rs = io.rs.asTypeOf(new OpRegType)
  val brType = io.brType

  val isEqualRdRs = rd === rs
  val isJal       = brType === BrType.jal
  val isJalr      = brType === BrType.jalr
  val isBranch    = brType === BrType.branch

  // push to RAS when rd is link, pop from RAS when rs is link
  def isUninferableCall      = isJalr && rd.isLink && (!rs.isLink || rs.isLink && isEqualRdRs)  //8   push
  def isInferableCall        = isJal && rd.isLink                                               //9   push
  def isUninferableTailCall  = isJalr && rd.isX0 && !rs.isLink                                  //10  no op
  def isInferableTailCall    = isJal && rd.isX0                                                 //11  no op
  def isCoRoutineSwap        = isJalr && rd.isLink && rs.isLink && !isEqualRdRs                 //12  pop then push
  def isFunctionReturn       = isJalr && !rd.isLink && rs.isLink                                //13  pop
  def isOtherUninferableJump = isJalr && !rd.isLink && !rd.isX0 && !rs.isLink                   //14  no op
  def isOtherInferableJump   = isJal && !rd.isLink && !rd.isX0                                  //15  no op

  val sel = Seq(
    isBranch,
    isUninferableCall,
    isInferableCall,
    isUninferableTailCall,
    isInferableTailCall,
    isCoRoutineSwap,
    isFunctionReturn,
    isOtherUninferableJump,
    isOtherInferableJump,
  )
  import Itype._
  val typesel = Seq(
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
  val jumpType = Mux1H(sel, typesel)

  io.itype := Mux(isBranch || isJal || isJalr, jumpType, 0.U)
  io.rsIslink := rs.isLink
  io.rsIs0 := rs.isX0
  io.rdIslink := rd.isLink
  io.rdIs0 := rd.isX0
  io.sel := VecInit(sel).asUInt
}

class TypeGen extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val defaultConfig = (new MinimalConfig()).alterPartial({
    case XSCoreParamsKey => XSCoreParameters()
  })

  println("test start")

  behavior of "TypeGen"
  it should "run" in {
    test(new ItypeGen).withAnnotations(Seq(VerilatorBackendAnnotation)) {
      m: ItypeGen =>
        m.io.brType.poke("h3".U)
        m.io.rd.poke("h2".U)
        m.io.rs.poke("h2".U)

        println("in.brtype: " + m.io.brType.peek().litValue.toString(16))
        println("in.rd: " + m.io.rd.peek().litValue.toString(2))
        println("in.rs: " + m.io.rs.peek().litValue.toString(2))

        println("out.itype: " + m.io.itype.peek().litValue.toString(16))
        println("out.rsislink: " + m.io.rsIslink.peek().litValue.toString(2))
        println("out.rsis0: " + m.io.rsIs0.peek().litValue.toString(2))
        println("out.rdislink: " + m.io.rdIslink.peek().litValue.toString(2))
        println("out.rdis0: " + m.io.rdIs0.peek().litValue.toString(2))
        println("out.sel: " + m.io.sel.peek().litValue.toString(9))
    }
    println("test done")
  }
}
 */