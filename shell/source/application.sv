module application #(
)(
  input  [31:0] AXIL_araddr,
  input  [2:0]  AXIL_arprot,
  output        AXIL_arready,
  input         AXIL_arvalid,

  input  [31:0] AXIL_awaddr,
  input  [2:0]  AXIL_awprot,
  output        AXIL_awready,
  input         AXIL_awvalid,

  input         AXIL_bready,
  output [1:0]  AXIL_bresp,
  output        AXIL_bvalid,

  output [31:0] AXIL_rdata,
  input         AXIL_rready,
  output [1:0]  AXIL_rresp,
  output        AXIL_rvalid,

  input  [31:0] AXIL_wdata,
  output        AXIL_wready,
  input  [3:0]  AXIL_wstrb,
  input         AXIL_wvalid,

  output [63:0] AXIS_C2H_tdata,
  output [7:0]  AXIS_C2H_tkeep,
  output        AXIS_C2H_tlast,
  input         AXIS_C2H_tready,
  output        AXIS_C2H_tvalid,

  input  [63:0] AXIS_H2C_tdata,
  input  [7:0]  AXIS_H2C_tkeep,
  input         AXIS_H2C_tlast,
  output        AXIS_H2C_tready,
  input         AXIS_H2C_tvalid,

  input         AXI_clock,
  input         AXI_reset_n
);

  logic [63:0]  C2H_tdata;
  logic [7:0]   C2H_tkeep;
  logic         C2H_tlast;
  logic         C2H_tready;
  logic         C2H_tvalid;

  always_ff @(posedge AXI_clock) begin
    if (~AXI_reset_n) begin
      C2H_tdata  <= '0;
      C2H_tkeep  <= '0;
      C2H_tlast  <= '0;
      C2H_tready <= '0;
      C2H_tvalid <= '0;
    end else begin
      C2H_tdata  <= AXIS_H2C_tdata;
      C2H_tkeep  <= AXIS_H2C_tkeep;
      C2H_tlast  <= AXIS_H2C_tlast;
      C2H_tready <= AXIS_H2C_tready;
      C2H_tvalid <= AXIS_H2C_tvalid;
    end
  end

  assign AXIS_C2H_tdata  = C2H_tdata;
  assign AXIS_C2H_tkeep  = C2H_tkeep;
  assign AXIS_C2H_tlast  = C2H_tlast;
  assign AXIS_C2H_tready = C2H_tready;
  assign AXIS_C2H_tvalid = C2H_tvalid;

endmodule