package device

import chisel3._
import chisel3.experimental._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.HasXSParameter
import xiangshan.backend.trace._

class ENCODER (
   GroupNum:       Int = 3,     // GroupNum
   IAddrWidth:     Int = 64,    // XLEN
   NumInstRetires: Int = 6 * 8, // rename * commit
   IlastSizeWidth: Int = 1,
   ItypeWidth:     Int = 4,
   CauseWidth:     Int = 64,    // XLEN
   PrivWidth:      Int = 3,
   // optional
   ContextWidth:   Int = 32,
   CtypeWidth:     Int = 32,
   ImpdefWidth:    Int = 32,
   StatusWidth:    Int = 32,
)(implicit val p: Parameters) extends Module with HasXSParameter {

  val encoderWapper = Module(new ust_ss_bosc_eval_m (
    GroupNum       = this.GroupNum,
    IAddrWidth     = this.IAddrWidth,
    NumInstRetires = this.NumInstRetires,
    IlastSizeWidth = this.IlastSizeWidth,
    ItypeWidth     = this.ItypeWidth,
    CauseWidth     = this.CauseWidth,
    PrivWidth      = this.PrivWidth,
    ContextWidth   = this.ContextWidth,
    CtypeWidth     = this.CtypeWidth,
    ImpdefWidth    = this.ImpdefWidth,
    StatusWidth    = this.StatusWidth,
  ))

  val io = IO(Flipped(new TraceCoreInterface))
  encoderWapper.ust.clk_udb_ip := clock
  encoderWapper.ust.rst_udb_ip := reset

  // ctrl
  encoderWapper.ust.ete_mult_retire_rv.halted_ip := 0.U
  io.fromEncoder.stall  := encoderWapper.ust.ete_mult_retire_rv.stall_op
  io.fromEncoder.enable := encoderWapper.ust.ete_mult_retire_rv.i_enable_op

  // trap
  encoderWapper.ust.ete_mult_retire_rv_i.ecause_ip    := io.toEncoder.cause
  encoderWapper.ust.ete_mult_retire_rv_i.tval_ip      := io.toEncoder.tval
  encoderWapper.ust.ete_mult_retire_rv_i.privilege_ip := io.toEncoder.priv

  // block
  encoderWapper.ust.ete_mult_retire_rv_i.address_ip  := io.toEncoder.iaddr
  encoderWapper.ust.ete_mult_retire_rv_i.retire_ip   := io.toEncoder.iretire
  encoderWapper.ust.ete_mult_retire_rv_i.lastsize_ip := io.toEncoder.ilastsize
  encoderWapper.ust.ete_mult_retire_rv_i.type_ip     := io.toEncoder.itype

  // optional
  encoderWapper.ust.ete_mult_retire_rv_i.sijump_ip  := 0.U
  encoderWapper.ust.ete_mult_retire_rv_i.context_ip := 0.U
  encoderWapper.ust.ete_mult_retire_rv_i.ctype_ip   := 0.U
  encoderWapper.ust.ete_mult_retire_rv_i.impdef_ip  := 0.U
  encoderWapper.ust.ete_mult_retire_rv_i.status_ip  := 0.U
}

class ust_ss_bosc_eval_m (
  GroupNum:       Int = 3,
  IAddrWidth:     Int = 64,
  NumInstRetires: Int = 6 * 8,
  IlastSizeWidth: Int = 1,
  ItypeWidth:     Int = 4,
  CauseWidth:     Int = 64,
  PrivWidth:      Int = 3,
  ContextWidth:   Int = 32,
  CtypeWidth:     Int = 2,
  ImpdefWidth:    Int = 1,
  StatusWidth:    Int = 4,
) extends BlackBox(Map(
  "ete_mult_retire_iblocks_p"             -> GroupNum,

  "ete_mult_retire_iaddress_width_p"      -> IAddrWidth,
  "ete_mult_retire_iretires_p"            -> NumInstRetires,
  "ete_mult_retire_lastsize_width_p"      -> IlastSizeWidth,
  "ete_mult_retire_itype_width_p"         -> ItypeWidth,

  "ete_mult_retire_ecause_width_p"        -> CauseWidth,
  "ete_mult_retire_privilege_width_p"     -> PrivWidth,
  // option
  "ete_mult_retire_context_width_p"       -> ContextWidth,
  "ete_mult_retire_ctype_width_p"         -> CtypeWidth,
  "ete_mult_retire_impdef_width_p"        -> ImpdefWidth,
  "ete_mult_retire_status_width_p"        -> StatusWidth,


)) with HasBlackBoxResource with HasBlackBoxPath {

  val ust = IO(new Bundle {
    val clk_udb_ip = Input(Clock())
    val rst_udb_ip = Input(Reset())


    val ete_mult_retire_rv_i = Input(new Bundle {
      val address_ip   = UInt((GroupNum * IAddrWidth).W)
      val retire_ip    = UInt((GroupNum * log2Up(NumInstRetires)).W)
      val lastsize_ip  = UInt((GroupNum * IlastSizeWidth).W)
      val type_ip      = UInt((GroupNum * ItypeWidth).W)

      val ecause_ip    = UInt(CauseWidth.W)
      val tval_ip      = UInt(IAddrWidth.W)
      val privilege_ip = UInt(PrivWidth.W)

      // optional
      val sijump_ip  = UInt(GroupNum.W)
      val uiret_ip   = UInt(GroupNum.W)
      val context_ip = UInt(ContextWidth.W)
      val ctype_ip   = UInt(CtypeWidth.W)
      val trigger_ip = UInt((GroupNum + 2).W)
      val impdef_ip  = UInt(ImpdefWidth.W)
      val status_ip  = UInt(StatusWidth.W)
    })

    val ete_mult_retire_rv = new Bundle {
      val halted_ip   = Input(Bool())
      val stall_op    = Output(Bool())
      val i_enable_op = Output(Bool())
    }

  })
  addResource("/ust_ss_bosc_envl_m.sv")
}
