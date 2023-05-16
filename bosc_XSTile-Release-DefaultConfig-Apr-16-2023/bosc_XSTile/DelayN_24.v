module DelayN_24(
  input         clock,
  input  [15:0] io_in_satp_asid,
  input  [43:0] io_in_satp_ppn,
  input         io_in_satp_changed,
  output [15:0] io_out_satp_asid,
  output [43:0] io_out_satp_ppn,
  output        io_out_satp_changed
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [63:0] _RAND_1;
  reg [31:0] _RAND_2;
`endif // RANDOMIZE_REG_INIT
  reg [15:0] out_satp_asid; // @[Hold.scala 90:18]
  reg [43:0] out_satp_ppn; // @[Hold.scala 90:18]
  reg  out_satp_changed; // @[Hold.scala 90:18]
  assign io_out_satp_asid = out_satp_asid; // @[Hold.scala 92:10]
  assign io_out_satp_ppn = out_satp_ppn; // @[Hold.scala 92:10]
  assign io_out_satp_changed = out_satp_changed; // @[Hold.scala 92:10]
  always @(posedge clock) begin
    out_satp_asid <= io_in_satp_asid; // @[Hold.scala 90:18]
    out_satp_ppn <= io_in_satp_ppn; // @[Hold.scala 90:18]
    out_satp_changed <= io_in_satp_changed; // @[Hold.scala 90:18]
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  out_satp_asid = _RAND_0[15:0];
  _RAND_1 = {2{`RANDOM}};
  out_satp_ppn = _RAND_1[43:0];
  _RAND_2 = {1{`RANDOM}};
  out_satp_changed = _RAND_2[0:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
