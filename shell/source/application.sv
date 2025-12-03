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

  output logic [AXIL_DATA_WIDTH-1:0]     AXIL_rdata,
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
  // Parameters & Constants
  // --------------------------------------------------------------------
  
  // Calculate the LSB of the address to align to data width (e.g., 32-bit = 4 bytes = 2 bits shift)
  localparam integer ADDR_LSB = $clog2(AXIL_DATA_WIDTH/8);
  // The address width for the RAM (word addressable)
  localparam integer MEM_ADDR_WIDTH = AXIL_ADDRESS_WIDTH - ADDR_LSB;

  // --------------------------------------------------------------------
  // AXI-Stream Loopback Logic
  // --------------------------------------------------------------------

  assign AXIS_C2H_tdata  = AXIS_H2C_tdata;
  assign AXIS_C2H_tkeep  = AXIS_H2C_tkeep;
  assign AXIS_C2H_tlast  = AXIS_H2C_tlast;
  assign AXIS_C2H_tvalid = AXIS_H2C_tvalid;
  assign AXIS_H2C_tready = AXIS_C2H_tready;

  // -----------------------------------------------------------------------------
  // Internal signals
  // -----------------------------------------------------------------------------

  logic                          write_enable;
  logic [AXIL_DATA_WIDTH-1:0]    write_data;
  logic [MEM_ADDR_WIDTH-1:0]     write_address;

  logic                          read_enable;
  logic [MEM_ADDR_WIDTH-1:0]     read_address;
  logic                          read_pending;
  logic [AXIL_DATA_WIDTH-1:0]    read_data;

  logic                          araddr_valid;
  logic [AXIL_ADDRESS_WIDTH-1:0] araddr_latched;

  logic                          wdata_valid;
  logic [AXIL_DATA_WIDTH-1:0]    wdata_latched;
  logic                          awaddr_valid;
  logic [AXIL_ADDRESS_WIDTH-1:0] awaddr_latched;

  // -----------------------------------------------------------------------------
  // DPRAM Instantiation
  // -----------------------------------------------------------------------------
  // Port A: Disconnected (Reserved for RTL)
  // Port B: Connected to AXI-Lite
  
  DPRAM #(
    .DATA_WIDTH    (AXIL_DATA_WIDTH),
    .ADDRESS_WIDTH (MEM_ADDR_WIDTH)
  ) generic_DPRAM (
    // --- PORT A (User RTL - Currently Disconnected) ---
    .clka  (AXI_clock),
    .rsta  (~AXI_reset_n),
    .ena   (1'b0),         // Disabled
    .wea   (1'b0),         // Write Disabled
    .addra ('0),           // Address 0
    .dina  ('0),           // Data 0
    .douta (),             // Open

    // --- PORT B (AXI-Lite Access) ---
    .clkb  (AXI_clock),
    .rstb  (~AXI_reset_n),
    .enb   (write_enable | read_enable),
    .web   (write_enable), // Write Enable (1=Write, 0=Read)
    .addrb (write_enable ? write_address : read_address),
    .dinb  (write_data),
    .doutb (read_data)
  );

  // -----------------------------------------------------------------------------
  // AXI Lite Writes
  // -----------------------------------------------------------------------------

  always_ff @(posedge AXI_clock) begin
    if (~AXI_reset_n) begin
      wdata_valid   <= '0;
      wdata_latched <= '0;
      awaddr_valid  <= '0;
      awaddr_latched<= '0;
      AXIL_bvalid   <= '0;
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
      
      // Execute Write (One Cycle Pulse)
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
  assign AXIL_bresp   = '0; // OKAY

  // Trigger write when we have both address and data latched
  assign write_enable  = awaddr_valid & wdata_valid & ~AXIL_bvalid;
  assign write_data    = wdata_latched;
  // Convert byte address to word address
  assign write_address = awaddr_latched[AXIL_ADDRESS_WIDTH-1 : ADDR_LSB];

  // -----------------------------------------------------------------------------
  // AXI Lite Reads
  // -----------------------------------------------------------------------------

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
      
      // Execute Read (Pulse enable to RAM)
      if (read_enable) begin
        araddr_valid <= 1'b0;
        read_pending <= 1'b1;
      end
      
      // Wait for RAM latency (1 cycle) then assert Valid
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
  assign AXIL_rresp   = '0; // OKAY
  assign AXIL_rdata   = read_data;

  // Trigger read when we have an address and we aren't currently writing
  assign read_enable  = araddr_valid & ~AXIL_rvalid & ~write_enable;
  // Convert byte address to word address
  assign read_address = araddr_latched[AXIL_ADDRESS_WIDTH-1 : ADDR_LSB];

endmodule