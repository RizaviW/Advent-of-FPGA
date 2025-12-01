module shell #(
)(
  input  logic          system_clock_p,
  input  logic          system_clock_n,
  input  logic          system_reset_n,
  input  logic          PCIe_RX_n,
  input  logic          PCIe_RX_p,
  output logic          PCIe_TX_n,
  output logic          PCIe_TX_p
);

  logic [31:0]          AXIL_araddr;
  logic [2:0]           AXIL_arprot;
  logic                 AXIL_arready;
  logic                 AXIL_arvalid;
  logic [31:0]          AXIL_awaddr;
  logic [2:0]           AXIL_awprot;
  logic                 AXIL_awready;
  logic                 AXIL_awvalid;
  logic                 AXIL_bready;
  logic [1:0]           AXIL_bresp;
  logic                 AXIL_bvalid;
  logic [31:0]          AXIL_rdata;
  logic                 AXIL_rready;
  logic [1:0]           AXIL_rresp;
  logic                 AXIL_rvalid;
  logic [31:0]          AXIL_wdata;
  logic                 AXIL_wready;
  logic [3:0]           AXIL_wstrb;
  logic                 AXIL_wvalid;
  logic [63:0]          AXIS_C2H_tdata;
  logic [7:0]           AXIS_C2H_tkeep;
  logic                 AXIS_C2H_tlast;
  logic                 AXIS_C2H_tready;
  logic                 AXIS_C2H_tvalid;
  logic [63:0]          AXIS_H2C_tdata;
  logic [7:0]           AXIS_H2C_tkeep;
  logic                 AXIS_H2C_tlast;
  logic                 AXIS_H2C_tready;
  logic                 AXIS_H2C_tvalid;
  logic                 AXI_clock;
  logic                 AXI_reset_n;
  logic                 system_clock;
  logic                 system_reset;

  IBUFDS_GTE2 clock_buffer (.O(system_clock), .I(system_clock_p), .IB(system_clock_n), .CEB(1'b0), .ODIV2());
  IBUF        reset_buffer (.O(system_reset), .I(system_reset_n));

  application #(
  ) application (
    .AXIL_araddr        (AXIL_araddr),
    .AXIL_arprot        (AXIL_arprot),
    .AXIL_arready       (AXIL_arready),
    .AXIL_arvalid       (AXIL_arvalid),
    .AXIL_awaddr        (AXIL_awaddr),
    .AXIL_awprot        (AXIL_awprot),
    .AXIL_awready       (AXIL_awready),
    .AXIL_awvalid       (AXIL_awvalid),
    .AXIL_bready        (AXIL_bready),
    .AXIL_bresp         (AXIL_bresp),
    .AXIL_bvalid        (AXIL_bvalid),
    .AXIL_rdata         (AXIL_rdata),
    .AXIL_rready        (AXIL_rready),
    .AXIL_rresp         (AXIL_rresp),
    .AXIL_rvalid        (AXIL_rvalid),
    .AXIL_wdata         (AXIL_wdata),
    .AXIL_wready        (AXIL_wready),
    .AXIL_wstrb         (AXIL_wstrb),
    .AXIL_wvalid        (AXIL_wvalid),
    .AXIS_C2H_tdata     (AXIS_C2H_tdata),
    .AXIS_C2H_tkeep     (AXIS_C2H_tkeep),
    .AXIS_C2H_tlast     (AXIS_C2H_tlast),
    .AXIS_C2H_tready    (AXIS_C2H_tready),
    .AXIS_C2H_tvalid    (AXIS_C2H_tvalid),
    .AXIS_H2C_tdata     (AXIS_H2C_tdata),
    .AXIS_H2C_tkeep     (AXIS_H2C_tkeep),
    .AXIS_H2C_tlast     (AXIS_H2C_tlast),
    .AXIS_H2C_tready    (AXIS_H2C_tready),
    .AXIS_H2C_tvalid    (AXIS_H2C_tvalid),
    .AXI_clock          (AXI_clock),
    .AXI_reset_n        (AXI_reset_n)
  );

  system #(
  ) system (
    .AXIL_araddr        (AXIL_araddr),
    .AXIL_arprot        (AXIL_arprot),
    .AXIL_arready       (AXIL_arready),
    .AXIL_arvalid       (AXIL_arvalid),
    .AXIL_awaddr        (AXIL_awaddr),
    .AXIL_awprot        (AXIL_awprot),
    .AXIL_awready       (AXIL_awready),
    .AXIL_awvalid       (AXIL_awvalid),
    .AXIL_bready        (AXIL_bready),
    .AXIL_bresp         (AXIL_bresp),
    .AXIL_bvalid        (AXIL_bvalid),
    .AXIL_rdata         (AXIL_rdata),
    .AXIL_rready        (AXIL_rready),
    .AXIL_rresp         (AXIL_rresp),
    .AXIL_rvalid        (AXIL_rvalid),
    .AXIL_wdata         (AXIL_wdata),
    .AXIL_wready        (AXIL_wready),
    .AXIL_wstrb         (AXIL_wstrb),
    .AXIL_wvalid        (AXIL_wvalid),
    .AXIS_C2H_tdata     (AXIS_C2H_tdata),
    .AXIS_C2H_tkeep     (AXIS_C2H_tkeep),
    .AXIS_C2H_tlast     (AXIS_C2H_tlast),
    .AXIS_C2H_tready    (AXIS_C2H_tready),
    .AXIS_C2H_tvalid    (AXIS_C2H_tvalid),
    .AXIS_H2C_tdata     (AXIS_H2C_tdata),
    .AXIS_H2C_tkeep     (AXIS_H2C_tkeep),
    .AXIS_H2C_tlast     (AXIS_H2C_tlast),
    .AXIS_H2C_tready    (AXIS_H2C_tready),
    .AXIS_H2C_tvalid    (AXIS_H2C_tvalid),
    .AXI_clock          (AXI_clock),
    .AXI_reset_n        (AXI_reset_n),
    .PCIe_rxn           (PCIe_RX_n),
    .PCIe_rxp           (PCIe_RX_p),
    .PCIe_txn           (PCIe_TX_n),
    .PCIe_txp           (PCIe_TX_p),
    .system_clock       (system_clock),
    .system_reset       (system_reset)
  );

endmodule
