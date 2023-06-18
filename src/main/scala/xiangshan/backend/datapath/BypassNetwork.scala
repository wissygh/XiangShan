package xiangshan.backend.datapath

import chisel3._
import chisel3.util._

class BypassNetworkIO() extends Bundle {

}

class BypassNetwork extends Module {
  val io = IO(new BypassNetworkIO)
}
