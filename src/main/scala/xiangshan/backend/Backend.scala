package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.ZeroExt
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, IssueQueueIQWakeUpBundle, MemExuInput, MemExuOutput}
import xiangshan.backend.ctrlblock.CtrlBlock
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath.{BypassNetwork, DataPath, NewPipelineConnect, WbDataPath}
import xiangshan.backend.exu.ExuBlock
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType}
import xiangshan.backend.fu.{FenceIO, FenceToSbuffer, FuConfig, PerfCounterIO}
import xiangshan.backend.issue.Scheduler
import xiangshan.backend.rob.RobLsqIO
import xiangshan.frontend.{FtqPtr, FtqRead}
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}

class Backend(val params: BackendParams)(implicit p: Parameters) extends LazyModule
  with HasXSParameter {

  println(params.iqWakeUpParams)

  for ((schdCfg, i) <- params.allSchdParams.zipWithIndex) {
    schdCfg.bindBackendParam(params)
  }

  for ((iqCfg, i) <- params.allIssueParams.zipWithIndex) {
    iqCfg.bindBackendParam(params)
  }

  for ((exuCfg, i) <- params.allExuParams.zipWithIndex) {
    exuCfg.updateIQWakeUpConfigs(params.iqWakeUpParams)
    exuCfg.updateExuIdx(i)
    exuCfg.bindBackendParam(params)
  }

  println("[Backend] ExuConfigs:")
  for (exuCfg <- params.allExuParams) {
    val fuConfigs = exuCfg.fuConfigs
    val wbPortConfigs = exuCfg.wbPortConfigs
    val immType = exuCfg.immType

    println("[Backend]   " +
      s"${exuCfg.name}: " +
      s"${fuConfigs.map(_.name).mkString("fu(s): {", ",", "}")}, " +
      s"${wbPortConfigs.mkString("wb: {", ",", "}")}, " +
      s"${immType.map(SelImm.mkString(_)).mkString("imm: {", ",", "}")}, " +
      s"latMax(${exuCfg.latencyValMax}), ${exuCfg.fuLatancySet.mkString("lat: {", ",", "}")}, "
    )
    require(
      wbPortConfigs.collectFirst { case x: IntWB => x }.nonEmpty ==
        fuConfigs.map(_.writeIntRf).reduce(_ || _),
      "int wb port has no priority"
    )
    require(
      wbPortConfigs.collectFirst { case x: VfWB => x }.nonEmpty ==
        fuConfigs.map(x => x.writeFpRf || x.writeVecRf).reduce(_ || _),
      "vec wb port has no priority"
    )
  }

  for (cfg <- FuConfig.allConfigs) {
    println(s"[Backend] $cfg")
  }

  val ctrlBlock = LazyModule(new CtrlBlock(params))
  val intScheduler = params.intSchdParams.map(x => LazyModule(new Scheduler(x)))
  val vfScheduler = params.vfSchdParams.map(x => LazyModule(new Scheduler(x)))
  val memScheduler = params.memSchdParams.map(x => LazyModule(new Scheduler(x)))
  val dataPath = LazyModule(new DataPath(params))
  val intExuBlock = params.intSchdParams.map(x => LazyModule(new ExuBlock(x)))
  val vfExuBlock = params.vfSchdParams.map(x => LazyModule(new ExuBlock(x)))

  lazy val module = new BackendImp(this)
}

class BackendImp(override val wrapper: Backend)(implicit p: Parameters) extends LazyModuleImp(wrapper)
  with HasXSParameter {
  implicit private val params = wrapper.params
  val io = IO(new BackendIO()(p, wrapper.params))

  private val ctrlBlock = wrapper.ctrlBlock.module
  private val intScheduler = wrapper.intScheduler.get.module
  private val vfScheduler = wrapper.vfScheduler.get.module
  private val memScheduler = wrapper.memScheduler.get.module
  private val dataPath = wrapper.dataPath.module
  private val intExuBlock = wrapper.intExuBlock.get.module
  private val vfExuBlock = wrapper.vfExuBlock.get.module
  private val bypassNetwork = Module(new BypassNetwork)
  private val wbDataPath = Module(new WbDataPath(params))

  private val iqWakeUpMappedBundle: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] = (
    intScheduler.io.toSchedulers.wakeupVec ++
      vfScheduler.io.toSchedulers.wakeupVec ++
      memScheduler.io.toSchedulers.wakeupVec
    ).map(x => (x.bits.exuIdx, x)).toMap

  println(s"[Backend] iq wake up keys: ${iqWakeUpMappedBundle.keys}")

  private val (intRespWrite, vfRespWrite, memRespWrite) = (intScheduler.io.toWbFuBusyTable.intFuBusyTableWrite,
    vfScheduler.io.toWbFuBusyTable.intFuBusyTableWrite,
    memScheduler.io.toWbFuBusyTable.intFuBusyTableWrite)
  private val (intRespRead, vfRespRead, memRespRead) = (intScheduler.io.fromWbFuBusyTable.fuBusyTableRead,
    vfScheduler.io.fromWbFuBusyTable.fuBusyTableRead,
    memScheduler.io.fromWbFuBusyTable.fuBusyTableRead)
  private val intAllRespWrite = (intRespWrite ++ vfRespWrite ++ memRespWrite).flatten
  private val intAllRespRead = (intRespRead ++ vfRespRead ++ memRespRead).flatten.map(_.intWbBusyTable)
  private val intAllWbConflictFlag = dataPath.io.wbConfictRead.flatten.flatten.map(_.intConflict)

  private val (vfIntRespWrite, vfVfRespWrite, vfMemRespWrite) = (intScheduler.io.toWbFuBusyTable.vfFuBusyTableWrite,
    vfScheduler.io.toWbFuBusyTable.vfFuBusyTableWrite,
    memScheduler.io.toWbFuBusyTable.vfFuBusyTableWrite)

  private val vfAllRespWrite = (vfIntRespWrite ++ vfVfRespWrite ++ vfMemRespWrite).flatten
  private val vfAllRespRead = (intRespRead ++ vfRespRead ++ memRespRead).flatten.map(_.vfWbBusyTable)
  private val vfAllWbConflictFlag = dataPath.io.wbConfictRead.flatten.flatten.map(_.vfConflict)

  wbDataPath.io.fromIntExu.flatten.filter(x => x.bits.params.writeIntRf)
  private val allExuParams = params.allExuParams
  private val intRespWriteWithParams = intAllRespWrite.zip(allExuParams)
  println(s"[intRespWriteWithParams] is ${intRespWriteWithParams}")
  intRespWriteWithParams.foreach { case (l, r) =>
    println(s"FuBusyTableWriteBundle is ${l}, ExeUnitParams is ${r}")
  }
  private val vfRespWriteWithParams = vfAllRespWrite.zip(allExuParams)

  private val intWBAllFuGroup = params.getIntWBExeGroup.map { case (groupId, exeUnit) => (groupId, exeUnit.flatMap(_.fuConfigs)) }
  private val intWBFuGroup = params.getIntWBExeGroup.map { case (groupId, exeUnit) => (groupId, exeUnit.flatMap(_.fuConfigs).filter(_.writeIntRf)) }
  private val intLatencyCertains = intWBAllFuGroup.map { case (k, v) => (k, v.map(_.latency.latencyVal.nonEmpty).reduce(_ && _)) }
  private val intWBFuLatencyMap = intLatencyCertains.map { case (k, latencyCertain) =>
    if (latencyCertain) Some(intWBFuGroup(k).map(y => (y.fuType, y.latency.latencyVal.get)))
    else None
  }.toSeq
  private val intWBFuLatencyValMax = intWBFuLatencyMap.map(latencyMap => latencyMap.map(x => x.map(_._2).max))

  private val vfWBAllFuGroup = params.getVfWBExeGroup.map { case (groupId, exeUnit) => (groupId, exeUnit.flatMap(_.fuConfigs)) }
  private val vfWBFuGroup = params.getVfWBExeGroup.map { case (groupId, exeUnit) => (groupId, exeUnit.flatMap(_.fuConfigs).filter(x => x.writeFpRf || x.writeVecRf)) }
  private val vfLatencyCertains = vfWBAllFuGroup.map { case (k, v) => (k, v.map(_.latency.latencyVal.nonEmpty).reduce(_ && _)) }
  val vfWBFuLatencyMap = vfLatencyCertains.map { case (k, latencyCertain) =>
    if (latencyCertain) Some(vfWBFuGroup(k).map(y => (y.fuType, y.latency.latencyVal.get)))
    else None
  }.toSeq
  private val vfWBFuLatencyValMax = vfWBFuLatencyMap.map(latencyMap => latencyMap.map(x => x.map(_._2).max))

  private val intWBFuBusyTable = intWBFuLatencyValMax.map { case y => if (y.getOrElse(0) > 0) Some(Reg(UInt(y.getOrElse(1).W))) else None }
  println(s"[intWBFuBusyTable] is ${intWBFuBusyTable.map(x => x)}")
  private val vfWBFuBusyTable = vfWBFuLatencyValMax.map { case y => if (y.getOrElse(0) > 0) Some(Reg(UInt(y.getOrElse(1).W))) else None }
  println(s"[vfWBFuBusyTable] is ${vfWBFuBusyTable.map(x => x)}")

  private val intWBPortConflictFlag = intWBFuLatencyValMax.map { case y => if (y.getOrElse(0) > 0) Some(Reg(Bool())) else None }
  private val vfWBPortConflictFlag = vfWBFuLatencyValMax.map { case y => if (y.getOrElse(0) > 0) Some(Reg(Bool())) else None }

  intWBPortConflictFlag.foreach(x => if (x.isDefined) dontTouch((x.get)))
  vfWBPortConflictFlag.foreach(x => if (x.isDefined) dontTouch((x.get)))


  intWBFuBusyTable.map(x => x.map(dontTouch(_)))
  vfWBFuBusyTable.map(x => x.map(dontTouch(_)))


  private val intWBFuBusyTableWithPort = intWBFuBusyTable.zip(intWBFuGroup.map(_._1))
  private val intWBPortConflictFlagWithPort = intWBPortConflictFlag.zip(intWBFuGroup.map(_._1))
  // intWBFuBusyTable write
  intWBFuBusyTableWithPort.zip(intWBPortConflictFlag).zip(intWBFuLatencyValMax).foreach {
    case (((busyTable, wbPort), wbPortConflictFlag), maxLatency) =>
      if (busyTable.nonEmpty) {
        val maskWidth = maxLatency.getOrElse(0)
        val defaultMask = ((1 << maskWidth) - 1).U
        val deqWbFuBusyTableValue = intRespWriteWithParams.zipWithIndex.filter { case ((r, p), idx) =>
          (p.wbPortConfigs.collectFirst { case x: IntWB => x.port }.getOrElse(-1)) == wbPort
        }.map { case ((r, p), idx) =>
          val resps = Seq(r.deqResp, r.og0Resp, r.og1Resp)
          Mux(
            resps(0).valid && resps(0).bits.respType === RSFeedbackType.issueSuccess,
            VecInit((0 until maxLatency.getOrElse(0) + 1).map { case latency =>
              val latencyNumFuType = p.writeIntFuConfigs.filter(_.latency.latencyVal.getOrElse(-1) == latency).map(_.fuType)
              val isLatencyNum = Cat(latencyNumFuType.map(futype => resps(0).bits.fuType === futype.U)).asUInt().orR() // The latency of the deqResp inst is Num
              isLatencyNum
            }
            ).asUInt,
            0.U
          )
        }
        //        deqWbFuBusyTableValue.foreach(x => dontTouch(x))
        val deqIsLatencyNumMask = (deqWbFuBusyTableValue.reduce(_ | _) >> 1).asUInt
        wbPortConflictFlag.get := deqWbFuBusyTableValue.reduce(_ & _).orR

        val og0IsLatencyNumMask = WireInit(defaultMask)
        og0IsLatencyNumMask := intRespWriteWithParams.zipWithIndex.map { case ((r, p), idx) =>
          val resps = Seq(r.deqResp, r.og0Resp, r.og1Resp)
          val matchI = (p.wbPortConfigs.collectFirst { case x: IntWB => x.port }.getOrElse(-1)) == wbPort
          if (matchI) {
            Mux(
              resps(1).valid && resps(1).bits.respType === RSFeedbackType.rfArbitFail,
              (~(Cat(
                VecInit((0 until maxLatency.getOrElse(0)).map { case num =>
                  val latencyNumFuType = p.writeIntFuConfigs.filter(_.latency.latencyVal.getOrElse(-1) == num + 1).map(_.fuType)
                  val isLatencyNum = Cat(latencyNumFuType.map(futype => resps(1).bits.fuType === futype.U)).asUInt().orR() // The latency of the deqResp inst is Num
                  isLatencyNum
                }
                ).asUInt, 0.U(1.W)
              ))).asUInt,
              defaultMask
            )
          } else defaultMask
        }.reduce(_ & _)
        val og1IsLatencyNumMask = WireInit(defaultMask)
        og1IsLatencyNumMask := intRespWriteWithParams.zipWithIndex.map { case ((r, p), idx) =>
          val resps = Seq(r.deqResp, r.og0Resp, r.og1Resp)
          val matchI = (p.wbPortConfigs.collectFirst { case x: IntWB => x.port }.getOrElse(-1)) == wbPort
          if (matchI && resps.length == 3) {
            Mux(
              resps(2).valid && resps(2).bits.respType === RSFeedbackType.fuBusy,
              (~(Cat(
                VecInit((0 until maxLatency.getOrElse(0)).map { case num =>
                  val latencyNumFuType = p.writeIntFuConfigs.filter(_.latency.latencyVal.getOrElse(-1) == num + 1).map(_.fuType)
                  val isLatencyNum = Cat(latencyNumFuType.map(futype => resps(2).bits.fuType === futype.U)).asUInt().orR() // The latency of the deqResp inst is Num
                  isLatencyNum
                }
                ).asUInt, 0.U(2.W)
              ))).asUInt,
              defaultMask
            )
          } else defaultMask
        }.reduce(_ & _)
        dontTouch(deqIsLatencyNumMask)
        dontTouch(og0IsLatencyNumMask)
        dontTouch(og1IsLatencyNumMask)
        busyTable.get := ((busyTable.get >> 1.U).asUInt() | deqIsLatencyNumMask) & og0IsLatencyNumMask.asUInt() & og1IsLatencyNumMask.asUInt()
      }
  }
  // intWBFuBusyTable read
  for (i <- 0 until intAllRespRead.size) {
    if (intAllRespRead(i).isDefined) {
      intAllRespRead(i).get := intWBFuBusyTableWithPort.map { case (busyTable, wbPort) =>
        val matchI = (allExuParams(i).wbPortConfigs.collectFirst { case x: IntWB => x.port }.getOrElse(-1)) == wbPort
        if (busyTable.nonEmpty && matchI) {
          busyTable.get.asTypeOf(intAllRespRead(i).get)
        } else {
          0.U.asTypeOf(intAllRespRead(i).get)
        }
      }.reduce(_ | _)
    }

    if (intAllWbConflictFlag(i).isDefined) {
      intAllWbConflictFlag(i).get := intWBPortConflictFlagWithPort.map { case (conflictFlag, wbPort) =>
        val matchI = (allExuParams(i).wbPortConfigs.collectFirst { case x: IntWB => x.port }.getOrElse(-1)) == wbPort
        if (conflictFlag.nonEmpty && matchI) {
          conflictFlag.get
        } else false.B
      }.reduce(_ | _)
    }
  }

  private val vfWBFuBusyTableWithPort = vfWBFuBusyTable.zip(vfWBFuGroup.map(_._1))
  private val vfWBPortConflictFlagWithPort = vfWBPortConflictFlag.zip(vfWBFuGroup.map(_._1))
  // vfWBFuBusyTable write
  vfWBFuBusyTableWithPort.zip(vfWBPortConflictFlag).zip(vfWBFuLatencyValMax).foreach {
    case (((busyTable, wbPort), wbPortConflictFlag), maxLatency) =>
      if (busyTable.nonEmpty) {
        val maskWidth = maxLatency.getOrElse(0)
        val defaultMask = ((1 << maskWidth) - 1).U
        val deqWbFuBusyTableValue = vfRespWriteWithParams.zipWithIndex.filter { case ((_, p), _) =>
          (p.wbPortConfigs.collectFirst { case x: VfWB => x.port }.getOrElse(-1)) == wbPort
        }.map { case ((r, p), _) =>
          val resps = Seq(r.deqResp, r.og0Resp, r.og1Resp)
          Mux(
            resps(0).valid && resps(0).bits.respType === RSFeedbackType.issueSuccess,
            VecInit((0 until maxLatency.getOrElse(0) + 1).map { case latency =>
              val latencyNumFuType = p.writeVfFuConfigs.filter(_.latency.latencyVal.getOrElse(-1) == latency).map(_.fuType)
              val isLatencyNum = Cat(latencyNumFuType.map(futype => resps(0).bits.fuType === futype.U)).asUInt.orR // The latency of the deqResp inst is Num
              isLatencyNum
            }
            ).asUInt,
            0.U
          )
        }
        val deqIsLatencyNumMask = (deqWbFuBusyTableValue.reduce(_ | _) >> 1).asUInt
        wbPortConflictFlag.get := deqWbFuBusyTableValue.reduce(_ & _).orR

        val og0IsLatencyNumMask = WireInit(defaultMask)
        og0IsLatencyNumMask := vfRespWriteWithParams.zipWithIndex.map { case ((r, p), idx) =>
          val resps = Seq(r.deqResp, r.og0Resp, r.og1Resp)
          val matchI = (p.wbPortConfigs.collectFirst { case x: VfWB => x.port }.getOrElse(-1)) == wbPort
          if (matchI) {
            Mux(
              resps(1).valid && resps(1).bits.respType === RSFeedbackType.rfArbitFail,
              (~(Cat(
                VecInit((0 until maxLatency.getOrElse(0)).map { case num =>
                  val latencyNumFuType = p.writeVfFuConfigs.filter(_.latency.latencyVal.getOrElse(-1) == num + 1).map(_.fuType)
                  val isLatencyNum = Cat(latencyNumFuType.map(futype => resps(1).bits.fuType === futype.U)).asUInt.orR // The latency of the deqResp inst is Num
                  isLatencyNum
                }
                ).asUInt, 0.U(1.W)
              ))).asUInt,
              defaultMask
            )
          } else defaultMask
        }.reduce(_ & _)
        val og1IsLatencyNumMask = WireInit(defaultMask)
        og1IsLatencyNumMask := vfRespWriteWithParams.zipWithIndex.map { case ((r, p), idx) =>
          val resps = Seq(r.deqResp, r.og0Resp, r.og1Resp)

          val matchI = (p.wbPortConfigs.collectFirst { case x: VfWB => x.port }.getOrElse(-1)) == wbPort
          if (matchI && resps.length == 3) {
            Mux(
              resps(2).valid && resps(2).bits.respType === RSFeedbackType.fuBusy,
              (~(Cat(
                VecInit((0 until maxLatency.getOrElse(0)).map { case num =>
                  val latencyNumFuType = p.writeVfFuConfigs.filter(_.latency.latencyVal.getOrElse(-1) == num + 1).map(_.fuType)
                  val isLatencyNum = Cat(latencyNumFuType.map(futype => resps(2).bits.fuType === futype.U)).asUInt.orR // The latency of the deqResp inst is Num
                  isLatencyNum
                }
                ).asUInt, 0.U(2.W)
              ))).asUInt,
              defaultMask
            )
          } else defaultMask
        }.reduce(_ & _)
        dontTouch(deqIsLatencyNumMask)
        dontTouch(og0IsLatencyNumMask)
        dontTouch(og1IsLatencyNumMask)
        busyTable.get := ((busyTable.get >> 1.U).asUInt | deqIsLatencyNumMask) & og0IsLatencyNumMask.asUInt & og1IsLatencyNumMask.asUInt
      }
  }

  // vfWBFuBusyTable read
  for (i <- 0 until vfAllRespRead.size) {
    if (vfAllRespRead(i).isDefined) {
      vfAllRespRead(i).get := vfWBFuBusyTableWithPort.map { case (busyTable, wbPort) =>
        val matchI = (allExuParams(i).wbPortConfigs.collectFirst { case x: VfWB => x.port }.getOrElse(-1)) == wbPort
        if (busyTable.nonEmpty && matchI) {
          busyTable.get.asTypeOf(vfAllRespRead(i).get)
        } else {
          0.U.asTypeOf(vfAllRespRead(i).get)
        }
      }.reduce(_ | _)
    }

    if (vfAllWbConflictFlag(i).isDefined) {
      vfAllWbConflictFlag(i).get := vfWBPortConflictFlagWithPort.map { case (conflictFlag, wbPort) =>
        val matchI = (allExuParams(i).wbPortConfigs.collectFirst { case x: VfWB => x.port }.getOrElse(-1)) == wbPort
        if (conflictFlag.nonEmpty && matchI) {
          conflictFlag.get
        } else false.B
      }.reduce(_ | _)
    }
  }

  private val vconfig = dataPath.io.vconfigReadPort.data
  private val og0CancelVec: Vec[Bool] = VecInit(dataPath.io.toIQCancelVec.map(_("OG0")))
  private val og1CancelVec: Vec[Bool] = VecInit(dataPath.io.toIQCancelVec.map(_("OG1")))
  dontTouch(og0CancelVec)
  dontTouch(og1CancelVec)

  ctrlBlock.io.fromTop.hartId := io.fromTop.hartId
  ctrlBlock.io.frontend <> io.frontend
  ctrlBlock.io.fromWB.wbData <> wbDataPath.io.toCtrlBlock.writeback
  ctrlBlock.io.fromMem.stIn <> io.mem.stIn
  ctrlBlock.io.fromMem.violation <> io.mem.memoryViolation
  ctrlBlock.io.csrCtrl <> intExuBlock.io.csrio.get.customCtrl
  ctrlBlock.io.robio.csr.intrBitSet := intExuBlock.io.csrio.get.interrupt
  ctrlBlock.io.robio.csr.trapTarget := intExuBlock.io.csrio.get.trapTarget
  ctrlBlock.io.robio.csr.isXRet := intExuBlock.io.csrio.get.isXRet
  ctrlBlock.io.robio.csr.wfiEvent := intExuBlock.io.csrio.get.wfi_event
  ctrlBlock.io.robio.lsq <> io.mem.robLsqIO
  ctrlBlock.io.fromDataPath.vtype := vconfig(7, 0).asTypeOf(new VType)

  intScheduler.io.fromTop.hartId := io.fromTop.hartId
  intScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  intScheduler.io.fromCtrlBlock.pcVec := ctrlBlock.io.toIssueBlock.pcVec
  intScheduler.io.fromCtrlBlock.targetVec := ctrlBlock.io.toIssueBlock.targetVec
  intScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  intScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.intUops
  intScheduler.io.intWriteBack := wbDataPath.io.toIntPreg
  intScheduler.io.vfWriteBack := 0.U.asTypeOf(intScheduler.io.vfWriteBack)
  intScheduler.io.fromDataPath.resp := dataPath.io.toIntIQ
  intScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  intScheduler.io.fromDataPath.cancel.foreach(x => x.cancelVec := dataPath.io.toIQCancelVec(x.exuIdx).cancelVec)

  memScheduler.io.fromTop.hartId := io.fromTop.hartId
  memScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  memScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  memScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.memUops
  memScheduler.io.intWriteBack := wbDataPath.io.toIntPreg
  memScheduler.io.vfWriteBack := wbDataPath.io.toVfPreg
  memScheduler.io.fromMem.get.scommit := io.mem.sqDeq
  memScheduler.io.fromMem.get.lcommit := io.mem.lqDeq
  memScheduler.io.fromMem.get.sqCancelCnt := io.mem.sqCancelCnt
  memScheduler.io.fromMem.get.lqCancelCnt := io.mem.lqCancelCnt
  memScheduler.io.fromMem.get.stIssuePtr := io.mem.stIssuePtr
  memScheduler.io.fromMem.get.memWaitUpdateReq.staIssue.zip(io.mem.stIn).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits.uop := 0.U.asTypeOf(sink.bits.uop)
    sink.bits.uop.robIdx := source.bits.robIdx
  }
  memScheduler.io.fromDataPath.resp := dataPath.io.toMemIQ
  memScheduler.io.fromMem.get.ldaFeedback := io.mem.ldaIqFeedback
  memScheduler.io.fromMem.get.staFeedback := io.mem.staIqFeedback
  memScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  memScheduler.io.fromDataPath.cancel.foreach(x => x.cancelVec := dataPath.io.toIQCancelVec(x.exuIdx).cancelVec)

  vfScheduler.io.fromTop.hartId := io.fromTop.hartId
  vfScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  vfScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  vfScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.vfUops
  vfScheduler.io.intWriteBack := 0.U.asTypeOf(vfScheduler.io.intWriteBack)
  vfScheduler.io.vfWriteBack := wbDataPath.io.toVfPreg
  vfScheduler.io.fromDataPath.resp := dataPath.io.toVfIQ
  vfScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  vfScheduler.io.fromDataPath.cancel.foreach(x => x.cancelVec := dataPath.io.toIQCancelVec(x.exuIdx).cancelVec)

  dataPath.io.flush := ctrlBlock.io.toDataPath.flush
  dataPath.io.vconfigReadPort.addr := ctrlBlock.io.toDataPath.vtypeAddr

  for (i <- 0 until dataPath.io.fromIntIQ.length) {
    for (j <- 0 until dataPath.io.fromIntIQ(i).length) {
      NewPipelineConnect(
        intScheduler.io.toDataPath(i)(j), dataPath.io.fromIntIQ(i)(j), dataPath.io.fromIntIQ(i)(j).valid,
        dontTouch(intScheduler.io.toDataPath(i)(j).bits.common.robIdx.needFlush(ctrlBlock.io.toDataPath.flush) | intScheduler.io.toDataPath(i)(j).bits.common.needCancel(og0CancelVec, og1CancelVec)),
        Option("intScheduler2DataPathPipe")
      )
    }
  }

  for (i <- 0 until dataPath.io.fromVfIQ.length) {
    for (j <- 0 until dataPath.io.fromVfIQ(i).length) {
      NewPipelineConnect(
        vfScheduler.io.toDataPath(i)(j), dataPath.io.fromVfIQ(i)(j), dataPath.io.fromVfIQ(i)(j).valid,
        dontTouch(vfScheduler.io.toDataPath(i)(j).bits.common.robIdx.needFlush(ctrlBlock.io.toDataPath.flush) | vfScheduler.io.toDataPath(i)(j).bits.common.needCancel(og0CancelVec, og1CancelVec)),
        Option("vfScheduler2DataPathPipe")
      )
    }
  }

  for (i <- 0 until dataPath.io.fromMemIQ.length) {
    for (j <- 0 until dataPath.io.fromMemIQ(i).length) {
      NewPipelineConnect(
        memScheduler.io.toDataPath(i)(j), dataPath.io.fromMemIQ(i)(j), dataPath.io.fromMemIQ(i)(j).valid,
        dontTouch(memScheduler.io.toDataPath(i)(j).bits.common.robIdx.needFlush(ctrlBlock.io.toDataPath.flush) | memScheduler.io.toDataPath(i)(j).bits.common.needCancel(og0CancelVec, og1CancelVec)),
        Option("memScheduler2DataPathPipe")
      )
    }
  }

  println(s"[Backend] wbDataPath.io.toIntPreg: ${wbDataPath.io.toIntPreg.size}, dataPath.io.fromIntWb: ${dataPath.io.fromIntWb.size}")
  println(s"[Backend] wbDataPath.io.toVfPreg: ${wbDataPath.io.toVfPreg.size}, dataPath.io.fromFpWb: ${dataPath.io.fromVfWb.size}")
  dataPath.io.fromIntWb := wbDataPath.io.toIntPreg
  dataPath.io.fromVfWb := wbDataPath.io.toVfPreg
  dataPath.io.debugIntRat := ctrlBlock.io.debug_int_rat
  dataPath.io.debugFpRat := ctrlBlock.io.debug_fp_rat
  dataPath.io.debugVecRat := ctrlBlock.io.debug_vec_rat
  dataPath.io.debugVconfigRat := ctrlBlock.io.debug_vconfig_rat

  bypassNetwork.io.fromDataPath.int <> dataPath.io.toIntExu
  bypassNetwork.io.fromDataPath.vf <> dataPath.io.toFpExu
  bypassNetwork.io.fromDataPath.mem <> dataPath.io.toMemExu
  bypassNetwork.io.fromExus.connectExuOutput(_.int)(intExuBlock.io.out)
  bypassNetwork.io.fromExus.connectExuOutput(_.vf)(vfExuBlock.io.out)
  bypassNetwork.io.fromExus.mem.flatten.zip(io.mem.writeBack).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits.pdest := source.bits.uop.pdest
    sink.bits.data := source.bits.data
  }

  intExuBlock.io.flush := ctrlBlock.io.toExuBlock.flush
  for (i <- 0 until intExuBlock.io.in.length) {
    for (j <- 0 until intExuBlock.io.in(i).length) {
      NewPipelineConnect(
        bypassNetwork.io.toExus.int(i)(j), intExuBlock.io.in(i)(j), intExuBlock.io.in(i)(j).fire,
        Mux(
          bypassNetwork.io.toExus.int(i)(j).fire,
          bypassNetwork.io.toExus.int(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush),
          intExuBlock.io.in(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush)
        )
      )
    }
  }

  private val csrio = intExuBlock.io.csrio.get
  csrio.hartId := io.fromTop.hartId
  csrio.perf.retiredInstr <> ctrlBlock.io.robio.csr.perfinfo.retiredInstr
  csrio.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  csrio.perf.perfEventsCtrl <> ctrlBlock.getPerf
  csrio.fpu.fflags := ctrlBlock.io.robio.csr.fflags
  csrio.fpu.isIllegal := false.B // Todo: remove it
  csrio.fpu.dirty_fs := ctrlBlock.io.robio.csr.dirty_fs
  csrio.vpu <> 0.U.asTypeOf(csrio.vpu) // Todo

  val debugVconfig = dataPath.io.debugVconfig.asTypeOf(new VConfig)
  val debugVtype = VType.toVtypeStruct(debugVconfig.vtype).asUInt
  val debugVl = debugVconfig.vl
  csrio.vpu.set_vxsat := ctrlBlock.io.robio.csr.vxsat
  csrio.vpu.set_vstart.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vstart.bits := 0.U
  csrio.vpu.set_vtype.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vtype.bits := ZeroExt(debugVtype, XLEN)
  csrio.vpu.set_vl.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vl.bits := ZeroExt(debugVl, XLEN)
  csrio.exception := ctrlBlock.io.robio.exception
  csrio.memExceptionVAddr := io.mem.exceptionVAddr
  csrio.externalInterrupt := io.fromTop.externalInterrupt
  csrio.distributedUpdate(0) := io.mem.csrDistributedUpdate
  csrio.distributedUpdate(1) := io.frontendCsrDistributedUpdate
  csrio.perf <> io.perf
  private val fenceio = intExuBlock.io.fenceio.get
  fenceio.disableSfence := csrio.disableSfence
  io.fenceio <> fenceio

  vfExuBlock.io.flush := ctrlBlock.io.toExuBlock.flush
  for (i <- 0 until vfExuBlock.io.in.size) {
    for (j <- 0 until vfExuBlock.io.in(i).size) {
      NewPipelineConnect(
        bypassNetwork.io.toExus.vf(i)(j), vfExuBlock.io.in(i)(j), vfExuBlock.io.in(i)(j).fire,
        Mux(
          bypassNetwork.io.toExus.vf(i)(j).fire,
          bypassNetwork.io.toExus.vf(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush),
          vfExuBlock.io.in(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush)
        )
      )
    }
  }
  vfExuBlock.io.frm.foreach(_ := csrio.fpu.frm)

  wbDataPath.io.flush := ctrlBlock.io.redirect
  wbDataPath.io.fromTop.hartId := io.fromTop.hartId
  wbDataPath.io.fromIntExu <> intExuBlock.io.out
  wbDataPath.io.fromVfExu <> vfExuBlock.io.out
  wbDataPath.io.fromMemExu.flatten.zip(io.mem.writeBack).foreach { case (sink, source) =>
    sink.valid := source.valid
    source.ready := sink.ready
    sink.bits.data := source.bits.data
    sink.bits.pdest := source.bits.uop.pdest
    sink.bits.robIdx := source.bits.uop.robIdx
    sink.bits.intWen.foreach(_ := source.bits.uop.rfWen)
    sink.bits.fpWen.foreach(_ := source.bits.uop.fpWen)
    sink.bits.vecWen.foreach(_ := source.bits.uop.vecWen)
    sink.bits.exceptionVec.foreach(_ := source.bits.uop.exceptionVec)
    sink.bits.flushPipe.foreach(_ := source.bits.uop.flushPipe)
    sink.bits.replay.foreach(_ := source.bits.uop.replayInst)
    sink.bits.debug := source.bits.debug
    sink.bits.debugInfo := 0.U.asTypeOf(sink.bits.debugInfo)
    sink.bits.lqIdx.foreach(_ := source.bits.uop.lqIdx)
    sink.bits.sqIdx.foreach(_ := source.bits.uop.sqIdx)
    sink.bits.ftqIdx.foreach(_ := source.bits.uop.ftqPtr)
    sink.bits.ftqOffset.foreach(_ := source.bits.uop.ftqOffset)
  }

  // to mem
  private val toMem = Wire(bypassNetwork.io.toExus.mem.cloneType)
  for (i <- toMem.indices) {
    for (j <- toMem(i).indices) {
      NewPipelineConnect(
        bypassNetwork.io.toExus.mem(i)(j), toMem(i)(j), toMem(i)(j).fire,
        Mux(
          bypassNetwork.io.toExus.mem(i)(j).fire,
          bypassNetwork.io.toExus.mem(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush),
          toMem(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush)
        )
      )
    }
  }

  io.mem.redirect := ctrlBlock.io.redirect
  io.mem.issueUops.zip(toMem.flatten).foreach { case (sink, source) =>
    sink.valid := source.valid
    source.ready := sink.ready
    sink.bits.iqIdx := source.bits.iqIdx
    sink.bits.isFirstIssue := source.bits.isFirstIssue
    sink.bits.uop := 0.U.asTypeOf(sink.bits.uop)
    sink.bits.src := 0.U.asTypeOf(sink.bits.src)
    sink.bits.src.zip(source.bits.src).foreach { case (l, r) => l := r }
    sink.bits.uop.fuType := source.bits.fuType
    sink.bits.uop.fuOpType := source.bits.fuOpType
    sink.bits.uop.imm := source.bits.imm
    sink.bits.uop.robIdx := source.bits.robIdx
    sink.bits.uop.pdest := source.bits.pdest
    sink.bits.uop.rfWen := source.bits.rfWen.getOrElse(false.B)
    sink.bits.uop.fpWen := source.bits.fpWen.getOrElse(false.B)
    sink.bits.uop.vecWen := source.bits.vecWen.getOrElse(false.B)
    sink.bits.uop.flushPipe := source.bits.flushPipe.getOrElse(false.B)
    sink.bits.uop.pc := source.bits.pc.getOrElse(0.U)
    sink.bits.uop.lqIdx := source.bits.lqIdx.getOrElse(0.U.asTypeOf(new LqPtr))
    sink.bits.uop.sqIdx := source.bits.sqIdx.getOrElse(0.U.asTypeOf(new SqPtr))
    sink.bits.uop.ftqPtr := source.bits.ftqIdx.getOrElse(0.U.asTypeOf(new FtqPtr))
    sink.bits.uop.ftqOffset := source.bits.ftqOffset.getOrElse(0.U)
  }
  io.mem.loadFastMatch := memScheduler.io.toMem.get.loadFastMatch.map(_.fastMatch)
  io.mem.loadFastImm := memScheduler.io.toMem.get.loadFastMatch.map(_.fastImm)
  io.mem.tlbCsr := csrio.tlb
  io.mem.csrCtrl := csrio.customCtrl
  io.mem.sfence := fenceio.sfence
  io.mem.isStoreException := CommitType.lsInstIsStore(ctrlBlock.io.robio.exception.bits.commitType)
  require(io.mem.loadPcRead.size == params.LduCnt)
  io.mem.loadPcRead.zipWithIndex.foreach { case (loadPcRead, i) =>
    loadPcRead.data := ctrlBlock.io.memLdPcRead(i).data
    ctrlBlock.io.memLdPcRead(i).ptr := loadPcRead.ptr
    ctrlBlock.io.memLdPcRead(i).offset := loadPcRead.offset
  }
  // mem io
  io.mem.lsqEnqIO <> memScheduler.io.memIO.get.lsqEnqIO
  io.mem.robLsqIO <> ctrlBlock.io.robio.lsq
  io.mem.toSbuffer <> fenceio.sbuffer

  io.frontendSfence := fenceio.sfence
  io.frontendTlbCsr := csrio.tlb
  io.frontendCsrCtrl := csrio.customCtrl

  io.tlb <> csrio.tlb

  io.csrCustomCtrl := csrio.customCtrl

  dontTouch(memScheduler.io)
  dontTouch(io.mem)
  dontTouch(dataPath.io.toMemExu)
  dontTouch(wbDataPath.io.fromMemExu)
}

class BackendMemIO(implicit p: Parameters, params: BackendParams) extends XSBundle {
  // params alias
  private val LoadQueueSize = VirtualLoadQueueSize
  // In/Out // Todo: split it into one-direction bundle
  val lsqEnqIO = Flipped(new LsqEnqIO)
  val robLsqIO = new RobLsqIO
  val toSbuffer = new FenceToSbuffer
  val ldaIqFeedback = Vec(params.LduCnt, Flipped(new MemRSFeedbackIO))
  val staIqFeedback = Vec(params.StaCnt, Flipped(new MemRSFeedbackIO))
  val loadPcRead = Vec(params.LduCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))

  // Input
  val writeBack = MixedVec(Seq.fill(params.LduCnt + params.StaCnt * 2)(Flipped(DecoupledIO(new MemExuOutput()))) ++ Seq.fill(params.VlduCnt)(Flipped(DecoupledIO(new MemExuOutput(true)))))

  val s3_delayed_load_error = Input(Vec(LoadPipelineWidth, Bool()))
  val stIn = Input(Vec(params.StaCnt, ValidIO(new DynInst())))
  val memoryViolation = Flipped(ValidIO(new Redirect))
  val exceptionVAddr = Input(UInt(VAddrBits.W))
  val sqDeq = Input(UInt(log2Ceil(EnsbufferWidth + 1).W))
  val lqDeq = Input(UInt(log2Up(CommitWidth + 1).W))

  val lqCancelCnt = Input(UInt(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))

  val otherFastWakeup = Flipped(Vec(params.LduCnt + 2 * params.StaCnt, ValidIO(new DynInst)))
  val stIssuePtr = Input(new SqPtr())

  val csrDistributedUpdate = Flipped(new DistributedCSRUpdateReq)

  // Output
  val redirect = ValidIO(new Redirect) // rob flush MemBlock
  val issueUops = MixedVec(Seq.fill(params.LduCnt + params.StaCnt * 2)(DecoupledIO(new MemExuInput())) ++ Seq.fill(params.VlduCnt)(DecoupledIO(new MemExuInput(true))))
  val loadFastMatch = Vec(params.LduCnt, Output(UInt(params.LduCnt.W)))
  val loadFastImm = Vec(params.LduCnt, Output(UInt(12.W))) // Imm_I

  val tlbCsr = Output(new TlbCsrBundle)
  val csrCtrl = Output(new CustomCSRCtrlIO)
  val sfence = Output(new SfenceBundle)
  val isStoreException = Output(Bool())
}

class BackendIO(implicit p: Parameters, params: BackendParams) extends XSBundle {
  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
    val externalInterrupt = new ExternalInterruptIO
  }

  val toTop = new Bundle {
    val cpuHalted = Output(Bool())
  }

  val fenceio = new FenceIO
  // Todo: merge these bundles into BackendFrontendIO
  val frontend = Flipped(new FrontendToCtrlIO)
  val frontendSfence = Output(new SfenceBundle)
  val frontendCsrCtrl = Output(new CustomCSRCtrlIO)
  val frontendTlbCsr = Output(new TlbCsrBundle)
  // distributed csr write
  val frontendCsrDistributedUpdate = Flipped(new DistributedCSRUpdateReq)

  val mem = new BackendMemIO

  val perf = Input(new PerfCounterIO)

  val tlb = Output(new TlbCsrBundle)

  val csrCustomCtrl = Output(new CustomCSRCtrlIO)
}
