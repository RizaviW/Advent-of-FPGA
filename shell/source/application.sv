module application #(
  parameter integer AXIL_DATA_WIDTH     = 32,
  parameter integer AXIL_ADDRESS_WIDTH  = 4
)(
  // AXI4-Lite Slave Interface
  input  logic [AXIL_ADDRESS_WIDTH-1:0]  AXIL_araddr,
  input  logic [2:0]                     AXIL_arprot,
  output logic                           AXIL_arready,
  input  logic                           AXIL_arvalid,

  input  logic [AXIL_ADDRESS_WIDTH-1:0]  AXIL_awaddr,
  input  logic [2:0]                     AXIL_awprot,
  output logic                           AXIL_awready,
  input  logic                           AXIL_awvalid,

  input  logic                           AXIL_bready,
  output logic [1:0]                     AXIL_bresp,
  output logic                           AXIL_bvalid,

  output logic  [AXIL_DATA_WIDTH-1:0]    AXIL_rdata,
  input  logic                           AXIL_rready,
  output logic [1:0]                     AXIL_rresp,
  output logic                           AXIL_rvalid,

  input  logic [AXIL_DATA_WIDTH-1:0]     AXIL_wdata,
  output logic                           AXIL_wready,
  input  logic [(AXIL_DATA_WIDTH/8)-1:0] AXIL_wstrb,
  input  logic                           AXIL_wvalid,

  // AXI4-Stream Interface (Loopback C2H <-> H2C)
  output logic [63:0]                    AXIS_C2H_tdata,
  output logic [7:0]                     AXIS_C2H_tkeep,
  output logic                           AXIS_C2H_tlast,
  input  logic                           AXIS_C2H_tready, // Input from host
  output logic                           AXIS_C2H_tvalid,

  input  logic [63:0]                    AXIS_H2C_tdata,
  input  logic [7:0]                     AXIS_H2C_tkeep,
  input  logic                           AXIS_H2C_tlast,
  output logic                           AXIS_H2C_tready, // Output to host
  input  logic                           AXIS_H2C_tvalid,

  // System Signals
  input  logic                           AXI_clock,
  input  logic                           AXI_reset_n
);

  // --------------------------------------------------------------------
  // AXI-Stream Loopback Logic
  // --------------------------------------------------------------------

  // Data flows from H2C (Host to Card) -> C2H (Card to Host)
  assign AXIS_C2H_tdata  = AXIS_H2C_tdata;
  assign AXIS_C2H_tkeep  = AXIS_H2C_tkeep;
  assign AXIS_C2H_tlast  = AXIS_H2C_tlast;
  assign AXIS_C2H_tvalid = AXIS_H2C_tvalid;
  assign AXIS_H2C_tready = AXIS_C2H_tready;

endmodule
