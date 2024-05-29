package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.SignExt
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig.{VaddrMaxWidth, XLEN}
import xiangshan.backend.fu.NewCSR._


class TrapEntryMEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase  {

  val mstatus   = ValidIO((new MstatusBundle ).addInEvent(_.MPV, _.MPP, _.GVA, _.MPIE, _.MIE))
  val mepc      = ValidIO((new Epc           ).addInEvent(_.epc))
  val mcause    = ValidIO((new CauseBundle   ).addInEvent(_.Interrupt, _.ExceptionCode))
  val mtval     = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val mtval2    = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val mtinst    = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val tcontrol  = ValidIO((new TcontrolBundle).addInEvent(_.MPTE, _.MTE))
  val targetPc  = ValidIO(UInt(VaddrMaxWidth.W))

  def getBundleByName(name: String): Valid[CSRBundle] = {
    name match {
      case "mstatus"  => this.mstatus
      case "mepc"     => this.mepc
      case "mcause"   => this.mcause
      case "mtval"    => this.mtval
      case "mtval2"   => this.mtval2
      case "mtinst"   => this.mtinst
      case "tcontrol" => this.tcontrol
    }
  }
}

class TrapEntryMEventModule(implicit val p: Parameters) extends Module with CSREventBase {
  val in = IO(new TrapEntryEventInput)
  val out = IO(new TrapEntryMEventOutput)

  private val current = in
  private val iMode = current.iMode
  private val dMode = current.dMode
  private val satp  = current.satp
  private val vsatp = current.vsatp
  private val hgatp = current.hgatp

  private val highPrioTrapNO = in.causeNO.ExceptionCode.asUInt
  private val isException = !in.causeNO.Interrupt.asBool
  private val isInterrupt = in.causeNO.Interrupt.asBool

  private val trapPC = genTrapVA(
    iMode,
    satp,
    vsatp,
    hgatp,
    in.trapPc,
  )

  private val trapMemVA = genTrapVA(
    dMode,
    satp,
    vsatp,
    hgatp,
    in.memExceptionVAddr,
  )

  private val trapMemGPA = SignExt(in.memExceptionGPAddr, XLEN)

  private val fetchIsVirt = iMode.isVirtual
  private val memIsVirt   = dMode.isVirtual

  private val isFetchExcp    = isException && Seq(/*EX_IAM, */ EX_IAF, EX_IPF).map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isMemExcp      = isException && Seq(EX_LAM, EX_LAF, EX_SAM, EX_SAF, EX_LPF, EX_SPF).map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isBpExcp       = isException && EX_BP.U === highPrioTrapNO
  private val fetchCrossPage = in.isCrossPageIPF

  private val isGuestExcp    = isException && Seq(EX_IGPF, EX_LGPF, EX_SGPF).map(_.U === highPrioTrapNO).reduce(_ || _)
  // Software breakpoint exceptions are permitted to write either 0 or the pc to xtval
  // We fill pc here
  private val tvalFillPc       = isFetchExcp && !fetchCrossPage || isBpExcp
  private val tvalFillPcPlus2  = isFetchExcp && fetchCrossPage
  private val tvalFillMemVaddr = isMemExcp
  private val tvalFillGVA      = isGuestExcp ||
    (isFetchExcp || isBpExcp) && fetchIsVirt ||
    isMemExcp && memIsVirt

  private val tval = Mux1H(Seq(
    (tvalFillPc                     ) -> trapPC,
    (tvalFillPcPlus2                ) -> (trapPC + 2.U),
    (tvalFillMemVaddr && !memIsVirt ) -> trapMemVA,
    (tvalFillMemVaddr &&  memIsVirt ) -> trapMemVA,
    (isGuestExcp                    ) -> trapMemVA,
  ))

  private val tval2 = Mux(isGuestExcp, trapMemGPA, 0.U)

  out := DontCare

  out.privState.valid := valid
  out.mstatus  .valid := valid
  out.mepc     .valid := valid
  out.mcause   .valid := valid
  out.mtval    .valid := valid
  out.mtval2   .valid := valid
  out.tcontrol .valid := valid
  out.targetPc .valid := valid

  out.privState.bits            := PrivState.ModeM
  out.mstatus.bits.MPV          := current.privState.V
  out.mstatus.bits.MPP          := current.privState.PRVM
  out.mstatus.bits.GVA          := tvalFillGVA
  out.mstatus.bits.MPIE         := current.mstatus.MIE
  out.mstatus.bits.MIE          := 0.U
  out.mepc.bits.epc             := trapPC(VaddrMaxWidth - 1, 1)
  out.mcause.bits.Interrupt     := isInterrupt
  out.mcause.bits.ExceptionCode := highPrioTrapNO
  out.mtval.bits.ALL            := tval
  out.mtval2.bits.ALL           := tval2
  out.mtinst.bits.ALL           := 0.U
  out.tcontrol.bits.MPTE        := in.tcontrol.MTE
  out.tcontrol.bits.MTE         := 0.U
  out.targetPc.bits             := in.pcFromXtvec

  dontTouch(isGuestExcp)
  dontTouch(tvalFillGVA)
}

trait TrapEntryMEventSinkBundle { self: CSRModule[_] =>
  val trapToM = IO(Flipped(new TrapEntryMEventOutput))

  private val updateBundle: ValidIO[CSRBundle] = trapToM.getBundleByName(self.modName.toLowerCase())

  (reg.asInstanceOf[CSRBundle].getFields zip updateBundle.bits.getFields).foreach { case (sink, source) =>
    if (updateBundle.bits.eventFields.contains(source)) {
      when(updateBundle.valid) {
        sink := source
      }
    }
  }
}
