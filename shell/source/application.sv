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
  // Internal Registers and Signals
  // --------------------------------------------------------------------
  
  // AXI-Lite Register File (4 x 32-bit registers)
  // Register Map:
  // 0x00: Control/Status Register 0
  // 0x04: Data Register 1
  // 0x08: Data Register 2
  // 0x0C: Data Register 3
  logic [AXIL_DATA_WIDTH-1:0] slv_reg0;
  logic [AXIL_DATA_WIDTH-1:0] slv_reg1;
  logic [AXIL_DATA_WIDTH-1:0] slv_reg2;
  logic [AXIL_DATA_WIDTH-1:0] slv_reg3;

  // Latching signals for Write Address
  logic                          axi_aw_en;
  logic [AXIL_ADDRESS_WIDTH-1:0] axi_awaddr;

  // Responses are always OKAY (2'b00) in this example
  assign AXIL_bresp = 2'b00;
  assign AXIL_rresp = 2'b00;


  // --------------------------------------------------------------------
  // AXI-Lite Write Channel Logic
  // --------------------------------------------------------------------

  // Implement axil_awready and axil_wready generation
  // We assert ready when valid is detected and we aren't already processing
  always_ff @(posedge AXI_clock) begin
    if (AXI_reset_n == 1'b0) begin
      AXIL_awready <= 1'b0;
      AXIL_wready  <= 1'b0;
      axi_aw_en    <= 1'b1;
    end else begin
      // Address Ready Logic
      if (~AXIL_awready && AXIL_awvalid && AXIL_wvalid && axi_aw_en) begin
        AXIL_awready <= 1'b1;
        axi_aw_en    <= 1'b0;
      end else if (AXIL_bready && AXIL_bvalid) begin
        AXIL_awready <= 1'b0;
        axi_aw_en    <= 1'b1;
      end else begin
        AXIL_awready <= 1'b0;
      end

      // Data Ready Logic
      if (~AXIL_wready && AXIL_wvalid && AXIL_awvalid && axi_aw_en) begin
        AXIL_wready <= 1'b1;
      end else begin
        AXIL_wready <= 1'b0;
      end
    end
  end

  // Implement Write Response Logic (B Channel)
  always_ff @(posedge AXI_clock) begin
    if (AXI_reset_n == 1'b0) begin
      AXIL_bvalid <= 1'b0;
    end else begin
      if (AXIL_awready && AXIL_awvalid && ~AXIL_bvalid && AXIL_wready && AXIL_wvalid) begin
        AXIL_bvalid <= 1'b1;
      end else if (AXIL_bready && AXIL_bvalid) begin
        AXIL_bvalid <= 1'b0;
      end
    end
  end

  // Implement Register Write Logic
  // Writes occur when both Address and Data valid/ready handshakes are complete
  // We effectively check the clock cycle where we assert the ready signals
  always_ff @(posedge AXI_clock) begin
    if (AXI_reset_n == 1'b0) begin
      slv_reg0 <= 0;
      slv_reg1 <= 0;
      slv_reg2 <= 0;
      slv_reg3 <= 0;
    end else begin
      if (AXIL_wready && AXIL_wvalid && AXIL_awready && AXIL_awvalid) begin
        // Use wstrb for byte-enables
        // Register 0 (Offset 0x00)
        if (AXIL_awaddr[3:2] == 2'h0) begin
          for (int i = 0; i < (AXIL_DATA_WIDTH/8); i++) begin
            if (AXIL_wstrb[i]) slv_reg0[(i*8)+:8] <= AXIL_wdata[(i*8)+:8];
          end
        end
        // Register 1 (Offset 0x04)
        if (AXIL_awaddr[3:2] == 2'h1) begin
          for (int i = 0; i < (AXIL_DATA_WIDTH/8); i++) begin
            if (AXIL_wstrb[i]) slv_reg1[(i*8)+:8] <= AXIL_wdata[(i*8)+:8];
          end
        end
        // Register 2 (Offset 0x08)
        if (AXIL_awaddr[3:2] == 2'h2) begin
          for (int i = 0; i < (AXIL_DATA_WIDTH/8); i++) begin
            if (AXIL_wstrb[i]) slv_reg2[(i*8)+:8] <= AXIL_wdata[(i*8)+:8];
          end
        end
        // Register 3 (Offset 0x0C)
        if (AXIL_awaddr[3:2] == 2'h3) begin
          for (int i = 0; i < (AXIL_DATA_WIDTH/8); i++) begin
            if (AXIL_wstrb[i]) slv_reg3[(i*8)+:8] <= AXIL_wdata[(i*8)+:8];
          end
        end
      end
    end
  end


  // --------------------------------------------------------------------
  // AXI-Lite Read Channel Logic
  // --------------------------------------------------------------------

  // Implement axil_arready generation
  always_ff @(posedge AXI_clock) begin
    if (AXI_reset_n == 1'b0) begin
      AXIL_arready <= 1'b0;
      axi_awaddr   <= 0;
    end else begin
      if (~AXIL_arready && AXIL_arvalid) begin
        AXIL_arready <= 1'b1;
        axi_awaddr   <= AXIL_araddr; // Latch address
      end else begin
        AXIL_arready <= 1'b0;
      end
    end
  end

  // Implement Read Data and Response Logic (R Channel)
  always_ff @(posedge AXI_clock) begin
    if (AXI_reset_n == 1'b0) begin
      AXIL_rvalid <= 1'b0;
      AXIL_rdata  <= 0;
    end else begin
      // When we accepted a read address, we output data in the next cycle
      if (AXIL_arready && AXIL_arvalid && ~AXIL_rvalid) begin
        AXIL_rvalid <= 1'b1;
        // Address Decoding
        case (AXIL_araddr[3:2])
          2'h0   : AXIL_rdata <= slv_reg0;
          2'h1   : AXIL_rdata <= slv_reg1;
          2'h2   : AXIL_rdata <= slv_reg2;
          2'h3   : AXIL_rdata <= slv_reg3;
          default: AXIL_rdata <= 0;
        endcase
      end else if (AXIL_rvalid && AXIL_rready) begin
        AXIL_rvalid <= 1'b0;
      end
    end
  end


  // --------------------------------------------------------------------
  // AXI-Stream Loopback Logic
  // --------------------------------------------------------------------
  
  // Data flows from H2C (Host to Card) -> C2H (Card to Host)
  assign AXIS_C2H_tdata  = AXIS_H2C_tdata;
  assign AXIS_C2H_tkeep  = AXIS_H2C_tkeep;
  assign AXIS_C2H_tlast  = AXIS_H2C_tlast;
  assign AXIS_C2H_tvalid = AXIS_H2C_tvalid;
  
  // We tell H2C we are ready only if the C2H destination is ready to accept
  assign AXIS_H2C_tready = AXIS_C2H_tready;

endmodule
