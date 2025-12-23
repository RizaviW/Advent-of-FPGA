module application #(
  parameter integer                     CSR_DATA_WIDTH    = 32,
  parameter integer                     CSR_ADDRESS_WIDTH = 16
)(
  // System Signals
  input  logic                          clock,
  input  logic                          reset_n,

  // Configuration Interface
  output logic                          CSR_write_enable,
  output logic [CSR_DATA_WIDTH-1:0]     CSR_write_data,
  output logic [CSR_ADDRESS_WIDTH-1:0]  CSR_write_address,
  output logic                          CSR_read_enable,
  input  logic [CSR_DATA_WIDTH-1:0]     CSR_read_data,
  output logic [CSR_ADDRESS_WIDTH-1:0]  CSR_read_address,

  // Streaming Interface
  output logic [63:0]                   AXIS_C2H_tdata,
  output logic [7:0]                    AXIS_C2H_tkeep,
  output logic                          AXIS_C2H_tlast,
  input  logic                          AXIS_C2H_tready,
  output logic                          AXIS_C2H_tvalid,
  input  logic [63:0]                   AXIS_H2C_tdata,
  input  logic [7:0]                    AXIS_H2C_tkeep,
  input  logic                          AXIS_H2C_tlast,
  output logic                          AXIS_H2C_tready,
  input  logic                          AXIS_H2C_tvalid
);

  assign AXIS_C2H_tdata    = AXIS_H2C_tdata;
  assign AXIS_C2H_tkeep    = AXIS_H2C_tkeep;
  assign AXIS_C2H_tlast    = AXIS_H2C_tlast;
  assign AXIS_C2H_tvalid   = AXIS_H2C_tvalid;
  assign AXIS_H2C_tready   = AXIS_C2H_tready;
  assign CSR_write_enable  = '0;
  assign CSR_write_data    = '0;
  assign CSR_write_address = '0;
  assign CSR_read_enable   = '0;
  assign CSR_read_address  = '0;

endmodule