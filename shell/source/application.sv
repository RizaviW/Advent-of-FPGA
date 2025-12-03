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

// -----------------------------------------------------------------------------
// Internal signals
// -----------------------------------------------------------------------------

  logic                                   data_pending;

  logic                                   write_enable;
  logic                                   write_select;
  logic  [AXIL_DATA_WIDTH-1:0]            write_data;
  logic  [KEY_BITS-1:0]                   write_address;
  logic  [DESTINATION_BITS-1:0]           destination_read;
  logic  [SOURCE_BITS-1:0]                source_read;

  logic                                   read_enable;
  logic                                   read_select;
  logic  [KEY_BITS-1:0]                   read_address;
  logic                                   araddr_valid;
  logic  [AXIL_ADDRESS_WIDTH-1:0]         araddr_latched;
  logic                                   read_pending;

  logic                                   wdata_valid;
  logic  [AXIL_DATA_WIDTH-1:0]            wdata_latched;
  logic                                   awaddr_valid;
  logic  [AXIL_ADDRESS_WIDTH-1:0]         awaddr_latched;

// -----------------------------------------------------------------------------
// DPRAMs
// -----------------------------------------------------------------------------

  DPRAM #(
    .DATA_WIDTH     (DESTINATION_BITS),
    .ADDRESS_WIDTH  (KEY_BITS)
  ) destination_DPRAM (
    .clka           (transmit_axis.aclk),
    .clkb           (configuration_axil.aclk),
    .rsta           (~transmit_axis.aresetn),
    .rstb           (~configuration_axil.aresetn),
    .addra          (transmit_axis.tdata[KEY_BITS+:KEY_BITS]),
    .addrb          (write_enable ? write_address : read_address),
    .ena            (transmit_axis.tvalid & transmit_axis.tready),
    .enb            (write_enable ? write_select : (read_enable & read_select)),
    .wea            ('0),
    .web            (write_select & write_enable),
    .dina           ('0),
    .dinb           (write_data[DESTINATION_BITS-1:0]),
    .douta          (destination_data),
    .doutb          (destination_read)
  );

  DPRAM #(
    .DATA_WIDTH     (SOURCE_BITS),
    .ADDRESS_WIDTH  (KEY_BITS)
  ) source_DPRAM (
    .clka           (transmit_axis.aclk),
    .clkb           (configuration_axil.aclk),
    .rsta           (~transmit_axis.aresetn),
    .rstb           (~configuration_axil.aresetn),
    .addra          (transmit_axis.tdata[0+:KEY_BITS]),
    .addrb          (write_enable ? write_address : read_address),
    .ena            (transmit_axis.tvalid & transmit_axis.tready),
    .enb            (write_enable ? (~write_select) : (read_enable & ~read_select)),
    .wea            ('0),
    .web            (~write_select & write_enable),
    .dina           ('0),
    .dinb           (write_data[SOURCE_BITS-1:0]),
    .douta          (source_data),
    .doutb          (source_read)
  );

// -----------------------------------------------------------------------------
// AXI Lite writes
// -----------------------------------------------------------------------------

  always_ff @(posedge configuration_axil.aclk) begin
    if (~configuration_axil.aresetn) begin
      wdata_valid               <= '0;
      wdata_latched             <= '0;
      awaddr_valid              <= '0;
      awaddr_latched            <= '0;
      configuration_axil.bvalid <= '0;
    end else begin
      if (configuration_axil.wvalid && configuration_axil.wready) begin
        wdata_valid    <= 1'b1;
        wdata_latched  <= configuration_axil.wdata;
      end
      if (configuration_axil.awvalid && configuration_axil.awready) begin
        awaddr_valid   <= 1'b1;
        awaddr_latched <= configuration_axil.awaddr;
      end
      if (write_enable) begin
        configuration_axil.bvalid <= 1'b1;
        awaddr_valid   <= 1'b0;
        wdata_valid    <= 1'b0;
      end
      if (configuration_axil.bvalid && configuration_axil.bready) begin
        configuration_axil.bvalid <= 1'b0;
      end
    end
  end

  assign configuration_axil.awready = ~awaddr_valid & ~configuration_axil.bvalid;
  assign configuration_axil.wready  = ~wdata_valid  & ~configuration_axil.bvalid;
  assign configuration_axil.bresp   = '0;
  assign configuration_axil.buser   = '0;

  assign write_enable  = awaddr_valid & wdata_valid & ~configuration_axil.bvalid;
  assign write_select  = awaddr_latched[KEY_BITS+3];
  assign write_data    = wdata_latched;
  assign write_address = awaddr_latched[KEY_BITS+2:3];

// -----------------------------------------------------------------------------
// AXI Lite reads
// -----------------------------------------------------------------------------

  always_ff @(posedge configuration_axil.aclk) begin
    if (~configuration_axil.aresetn) begin
      araddr_valid              <= '0;
      araddr_latched            <= '0;
      configuration_axil.rvalid <= '0;
      read_pending              <= '0;
    end else begin
      if (configuration_axil.arvalid && configuration_axil.arready) begin
        araddr_valid   <= 1'b1;
        araddr_latched <= configuration_axil.araddr;
      end
      if (read_enable) begin
        araddr_valid   <= 1'b0;
        read_pending   <= 1'b1;
      end
      if (read_pending) begin
        configuration_axil.rvalid <= 1'b1;
        read_pending              <= 1'b0;
      end
      if (configuration_axil.rvalid && configuration_axil.rready) begin
        configuration_axil.rvalid <= '0;
      end
    end
  end

  assign configuration_axil.arready = ~araddr_valid & ~configuration_axil.rvalid;
  assign configuration_axil.rdata   = read_select ? {{(AXIL_DATA_WIDTH-DESTINATION_BITS){1'b0}}, destination_read} : {{(AXIL_DATA_WIDTH-SOURCE_BITS){1'b0}}, source_read};
  assign configuration_axil.ruser   = '0;
  assign configuration_axil.rresp   = '0;

  assign read_enable  = araddr_valid & ~configuration_axil.rvalid & ~write_enable;
  assign read_select  = araddr_latched[KEY_BITS+3];
  assign read_address = araddr_latched[KEY_BITS+2:3];

endmodule
