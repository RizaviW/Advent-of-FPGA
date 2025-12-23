module shell #(
)(
  input  logic                   system_clock_p,
  input  logic                   system_clock_n,
  input  logic                   system_reset_n,
  input  logic                   PCIe_RX_n,
  input  logic                   PCIe_RX_p,
  output logic                   PCIe_TX_n,
  output logic                   PCIe_TX_p
);

  // --------------------------------------------------------------------
  // Parameters & Constants
  // --------------------------------------------------------------------

  // AXI4-Lite data width
  localparam integer AXIL_DATA_WIDTH    = 32;
  // AXI4-Lite address width
  localparam integer AXIL_ADDRESS_WIDTH = 18;
  // Calculate the LSB of the address to align to data width (e.g., 32-bit = 4 bytes = 2 bits shift)
  localparam integer ADDRESS_LSB        = $clog2(AXIL_DATA_WIDTH/8);
  // The address width for the RAM (word addressable)
  localparam integer RAM_ADDRESS_WIDTH  = AXIL_ADDRESS_WIDTH - ADDRESS_LSB;

  // --------------------------------------------------------------------
  // Internal signals
  // --------------------------------------------------------------------

  logic [31:0]                   AXIL_araddr;
  logic [2:0]                    AXIL_arprot;
  logic                          AXIL_arready;
  logic                          AXIL_arvalid;
  logic [31:0]                   AXIL_awaddr;
  logic [2:0]                    AXIL_awprot;
  logic                          AXIL_awready;
  logic                          AXIL_awvalid;
  logic                          AXIL_bready;
  logic [1:0]                    AXIL_bresp;
  logic                          AXIL_bvalid;
  logic [31:0]                   AXIL_rdata;
  logic                          AXIL_rready;
  logic [1:0]                    AXIL_rresp;
  logic                          AXIL_rvalid;
  logic [31:0]                   AXIL_wdata;
  logic                          AXIL_wready;
  logic [3:0]                    AXIL_wstrb;
  logic                          AXIL_wvalid;
  logic [63:0]                   AXIS_C2H_tdata;
  logic [7:0]                    AXIS_C2H_tkeep;
  logic                          AXIS_C2H_tlast;
  logic                          AXIS_C2H_tready;
  logic                          AXIS_C2H_tvalid;
  logic [63:0]                   AXIS_H2C_tdata;
  logic [7:0]                    AXIS_H2C_tkeep;
  logic                          AXIS_H2C_tlast;
  logic                          AXIS_H2C_tready;
  logic                          AXIS_H2C_tvalid;
  logic                          AXI_clock;
  logic                          AXI_reset_n;
  logic                          system_clock;
  logic                          system_reset;

  logic                          CSR_write_enable;
  logic [AXIL_DATA_WIDTH-1:0]    CSR_write_data;
  logic [RAM_ADDRESS_WIDTH-1:0]  CSR_write_address;
  logic                          CSR_read_enable;
  logic [AXIL_DATA_WIDTH-1:0]    CSR_read_data;
  logic [RAM_ADDRESS_WIDTH-1:0]  CSR_read_address;

  logic                          write_enable;
  logic [AXIL_DATA_WIDTH-1:0]    write_data;
  logic [RAM_ADDRESS_WIDTH-1:0]  write_address;
  logic                          read_enable;
  logic [AXIL_DATA_WIDTH-1:0]    read_data;
  logic [RAM_ADDRESS_WIDTH-1:0]  read_address;
  logic                          read_pending;

  logic                          araddr_valid;
  logic [AXIL_ADDRESS_WIDTH-1:0] araddr_latched;
  logic                          wdata_valid;
  logic [AXIL_DATA_WIDTH-1:0]    wdata_latched;
  logic                          awaddr_valid;
  logic [AXIL_ADDRESS_WIDTH-1:0] awaddr_latched;

  // --------------------------------------------------------------------
  // Instantiations
  // --------------------------------------------------------------------

  IBUFDS_GTE2 clock_buffer (.O(system_clock), .I(system_clock_p), .IB(system_clock_n), .CEB(1'b0), .ODIV2());
  IBUF        reset_buffer (.O(system_reset), .I(system_reset_n));

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

  application #(
    .CSR_DATA_WIDTH     (AXIL_DATA_WIDTH),
    .CSR_ADDRESS_WIDTH  (RAM_ADDRESS_WIDTH)
  ) application (
    .clock              (AXI_clock),
    .reset_n            (AXI_reset_n),
    .CSR_write_enable   (CSR_write_enable),
    .CSR_write_data     (CSR_write_data),
    .CSR_write_address  (CSR_write_address),
    .CSR_read_enable    (CSR_read_enable),
    .CSR_read_data      (CSR_read_data),
    .CSR_read_address   (CSR_read_address),
    .AXIS_C2H_tdata     (AXIS_C2H_tdata),
    .AXIS_C2H_tkeep     (AXIS_C2H_tkeep),
    .AXIS_C2H_tlast     (AXIS_C2H_tlast),
    .AXIS_C2H_tready    (AXIS_C2H_tready),
    .AXIS_C2H_tvalid    (AXIS_C2H_tvalid),
    .AXIS_H2C_tdata     (AXIS_H2C_tdata),
    .AXIS_H2C_tkeep     (AXIS_H2C_tkeep),
    .AXIS_H2C_tlast     (AXIS_H2C_tlast),
    .AXIS_H2C_tready    (AXIS_H2C_tready),
    .AXIS_H2C_tvalid    (AXIS_H2C_tvalid)
  );

  DPRAM #(
    .DATA_WIDTH         (AXIL_DATA_WIDTH),
    .ADDRESS_WIDTH      (RAM_ADDRESS_WIDTH)
  ) generic_DPRAM (
    // --- PORT A (Application) ---
    .clka               (AXI_clock),
    .rsta               (~AXI_reset_n),
    .ena                (CSR_write_enable | CSR_read_enable),
    .wea                (CSR_write_enable),
    .addra              (CSR_write_enable ? CSR_write_address : CSR_read_address),
    .dina               (CSR_write_data),
    .douta              (CSR_read_data),

    // --- PORT B (AXI-Lite Access) ---
    .clkb               (AXI_clock),
    .rstb               (~AXI_reset_n),
    .enb                (write_enable | read_enable),
    .web                (write_enable),
    .addrb              (write_enable ? write_address : read_address),
    .dinb               (write_data),
    .doutb              (read_data)
  );

  // --------------------------------------------------------------------
  // AXI Lite Writes
  // --------------------------------------------------------------------

  always_ff @(posedge AXI_clock) begin
    if (~AXI_reset_n) begin
      wdata_valid    <= '0;
      wdata_latched  <= '0;
      awaddr_valid   <= '0;
      awaddr_latched <= '0;
      AXIL_bvalid    <= '0;
    end else begin
      // Latch Write Data
      if (AXIL_wvalid && AXIL_wready) begin
        wdata_valid   <= 1'b1;
        wdata_latched <= AXIL_wdata;
      end
      
      // Latch Write Address
      if (AXIL_awvalid && AXIL_awready) begin
        awaddr_valid   <= 1'b1;
        awaddr_latched <= AXIL_awaddr;
      end
      
      // Execute Write
      if (write_enable) begin
        AXIL_bvalid  <= 1'b1;
        awaddr_valid <= 1'b0;
        wdata_valid  <= 1'b0;
      end
      
      // Handle Response Handshake
      if (AXIL_bvalid && AXIL_bready) begin
        AXIL_bvalid <= 1'b0;
      end
    end
  end

  assign AXIL_awready = ~awaddr_valid & ~AXIL_bvalid;
  assign AXIL_wready  = ~wdata_valid  & ~AXIL_bvalid;
  assign AXIL_bresp   = '0;

  // Trigger write when we have both address and data latched
  assign write_enable  = awaddr_valid & wdata_valid & ~AXIL_bvalid;
  assign write_data    = wdata_latched;
  // Convert byte address to word address
  assign write_address = awaddr_latched[AXIL_ADDRESS_WIDTH-1 : ADDRESS_LSB];

  // --------------------------------------------------------------------
  // AXI Lite Reads
  // --------------------------------------------------------------------

  always_ff @(posedge AXI_clock) begin
    if (~AXI_reset_n) begin
      araddr_valid   <= '0;
      araddr_latched <= '0;
      AXIL_rvalid    <= '0;
      read_pending   <= '0;
    end else begin
      // Latch Read Address
      if (AXIL_arvalid && AXIL_arready) begin
        araddr_valid   <= 1'b1;
        araddr_latched <= AXIL_araddr;
      end
      
      // Execute Read
      if (read_enable) begin
        araddr_valid <= 1'b0;
        read_pending <= 1'b1;
      end
      
      // Wait
      if (read_pending) begin
        AXIL_rvalid  <= 1'b1;
        read_pending <= 1'b0;
      end
      
      // Handle Response Handshake
      if (AXIL_rvalid && AXIL_rready) begin
        AXIL_rvalid <= '0;
      end
    end
  end

  assign AXIL_arready = ~araddr_valid & ~AXIL_rvalid;
  assign AXIL_rresp   = '0;
  assign AXIL_rdata   = read_data;

  // Trigger read when we have an address and we aren't currently writing
  assign read_enable  = araddr_valid & ~AXIL_rvalid & ~write_enable;
  // Convert byte address to word address
  assign read_address = araddr_latched[AXIL_ADDRESS_WIDTH-1 : ADDRESS_LSB];

endmodule
