package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import xiangshan.backend.fu.NewCSR.CSRBundles.{Counteren, PrivState}
import freechips.rocketchip.rocket.CSRs

class CSRPermitModule extends Module {
  val io = IO(new CSRPermitIO)

  private val (ren, wen, addr, privState, debugMode) = (
    io.in.csrAccess.ren,
    io.in.csrAccess.wen,
    io.in.csrAccess.addr,
    io.in.privState,
    io.in.debugMode
  )

  private val csrAccess = WireInit(ren || wen)

  private val (mret, sret, wfi) = (
    io.in.mret,
    io.in.sret,
    io.in.wfi,
  )

  private val (tsr, vtsr) = (
    io.in.status.tsr,
    io.in.status.vtsr,
  )

  private val (tw, vtw) = (
    io.in.status.tw,
    io.in.status.vtw
  )

  private val (tvm, vtvm) = (
    io.in.status.tvm,
    io.in.status.vtvm,
  )

  private val csrIsCustom = io.in.csrIsCustom

  private val (mcounteren, hcounteren, scounteren) = (
    io.in.status.mcounteren,
    io.in.status.hcounteren,
    io.in.status.scounteren,
  )

  private val csrIsRO = addr(11, 10) === "b11".U
  private val csrIsUnpriv = addr(9, 8) === "b00".U
  private val csrIsHPM = addr >= CSRs.cycle.U && addr <= CSRs.hpmcounter31.U
  private val counterAddr = addr(4, 0) // 32 counters

  private val accessTable = TruthTable(Seq(
    //       V PRVM ADDR
    BitPat("b0__00___00") -> BitPat.Y(), // HU access U
    BitPat("b1__00___00") -> BitPat.Y(), // VU access U
    BitPat("b0__01___00") -> BitPat.Y(), // HS access U
    BitPat("b0__01___01") -> BitPat.Y(), // HS access S
    BitPat("b1__01___00") -> BitPat.Y(), // VS access U
    BitPat("b1__01___01") -> BitPat.Y(), // VS access S
    BitPat("b0__11___00") -> BitPat.Y(), // M  access HU
    BitPat("b0__11___01") -> BitPat.Y(), // M  access HS
    BitPat("b0__11___11") -> BitPat.Y(), // M  access M
  ), BitPat.N())

  private val isDebugReg   = addr(11, 4) === "h7b".U
  private val isTriggerReg = addr(11, 4) === "h7a".U

  private val regularPrivilegeLegal = chisel3.util.experimental.decode.decoder(
    privState.V.asUInt ## privState.PRVM.asUInt ## addr(9, 8),
    accessTable
  ).asBool

  private val privilegeLegal = MuxCase(
    regularPrivilegeLegal,
    Seq(
      isDebugReg   -> debugMode,
      isTriggerReg -> (debugMode || privState.isModeM),
    )
  )

  private val rwIllegal = csrIsRO && wen

  private val csrAccessIllegal = (!privilegeLegal || rwIllegal)

  private val mretIllegal = !privState.isModeM

  private val sretIllegal = sret && (
    privState.isModeHS && tsr || privState.isModeVS && vtsr || privState.isModeHUorVU
  )

  private val wfi_EX_II = wfi && (!privState.isModeM && tw)
  private val wfi_EX_VI = wfi && (privState.isModeVS && vtw && !tw || privState.isModeVU && !tw)

  private val rwSatp_EX_II = csrAccess && privState.isModeHS &&  tvm && (addr === CSRs.satp.U || addr === CSRs.hgatp.U)
  private val rwSatp_EX_VI = csrAccess && privState.isModeVS && vtvm && (addr === CSRs.satp.U)

  private val rwCustom_EX_II = csrAccess && privState.isModeVS && csrIsCustom

  private val accessHPM = ren && csrIsHPM
  private val accessHPM_EX_II = accessHPM && (
    !mcounteren(counterAddr) ||
    privState.isModeHU && scounteren(counterAddr)
  )
  private val accessHPM_EX_VI = accessHPM && mcounteren(counterAddr) && (
    privState.isModeVS && !hcounteren(counterAddr) ||
    privState.isModeVU && (!hcounteren(counterAddr) || !scounteren(counterAddr))
  )

  io.out.illegal := csrAccess && csrAccessIllegal || mret && mretIllegal || sret && sretIllegal

  // Todo: check correct
  io.out.EX_II := io.out.illegal && !privState.isVirtual || wfi_EX_II || rwSatp_EX_II || accessHPM_EX_II || rwCustom_EX_II
  io.out.EX_VI := io.out.illegal &&  privState.isVirtual || wfi_EX_VI || rwSatp_EX_VI || accessHPM_EX_VI

  io.out.hasLegalWen := io.in.csrAccess.wen && !csrAccessIllegal
  io.out.hasLegalMret := mret && !mretIllegal
  io.out.hasLegalSret := sret && !sretIllegal
  io.out.hasLegalWfi := wfi && !wfi_EX_II && !wfi_EX_VI
}

class CSRPermitIO extends Bundle {
  val in = Input(new Bundle {
    val csrAccess = new Bundle {
      val ren = Bool()
      val wen = Bool()
      val addr = UInt(12.W)
    }
    val privState = new PrivState
    val debugMode = Bool()
    val mret = Bool()
    val sret = Bool()
    val wfi = Bool()
    val csrIsCustom = Bool()
    val status = new Bundle {
      // Trap SRET
      val tsr = Bool()
      // Virtual Trap SRET
      val vtsr = Bool()
      // Timeout Wait
      val tw = Bool()
      // Virtual Timeout Wait
      val vtw = Bool()
      // Trap Virtual Memory
      val tvm = Bool()
      // Virtual Trap Virtual Memory
      val vtvm = Bool()
      // Machine level counter enable, access PMC from the level less than M will trap EX_II
      val mcounteren = UInt(32.W)
      // Hypervisor level counter enable.
      // Accessing PMC from VS/VU level will trap EX_VI, if m[x]=1 && h[x]=0
      val hcounteren = UInt(32.W)
      // Supervisor level counter enable.
      // Accessing PMC from **HU level** will trap EX_II, if s[x]=0
      // Accessing PMC from **VU level** will trap EX_VI, if m[x]=1 && h[x]=1 && s[x]=0
      val scounteren = UInt(32.W)
    }
  })

  val out = Output(new Bundle {
    val hasLegalWen = Bool()
    val hasLegalMret = Bool()
    val hasLegalSret = Bool()
    val hasLegalWfi = Bool()
    // Todo: split illegal into EX_II and EX_VI
    val illegal = Bool()
    val EX_II = Bool()
    val EX_VI = Bool()
  })
}
