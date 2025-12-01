###############################################################################
# User Configuration
# Link Width   - x1
# Link Speed   - gen1
# Family       - artix7
# Part         - xc7a200t
# Package      - fbg484
# Speed grade  - -2
# PCIe Block   - X0Y0
###############################################################################


###############################################################################
# Pinout and Related I/O Constraints
###############################################################################
#
# SYS reset (input) signal.  The sys_reset_n signal should be
# obtained from the PCI Express interface if possible.  For
# slot based form factors, a system reset signal is usually
# present on the connector.  For cable based form factors, a
# system reset signal may not be available.  In this case, the
# system reset signal must be generated locally by some form of
# supervisory circuit.  You may change the IOSTANDARD and LOC
# to suit your requirements and VCCO voltage banking rules.
# Some 7 series devices do not have 3.3 V I/Os available.
# Therefore the appropriate level shift is required to operate
# with these devices that contain only 1.8 V banks.
#
# SYS clock 0 MHz (input) signal. The system_clock_p and system_clock_n
# signals are the PCI Express reference clock. Virtex-7 GT
# Transceiver architecture requires the use of a dedicated clock
# resources (FPGA input pins) associated with each GT Transceiver.
# To use these pins an IBUFDS primitive (refclk_ibuf) is
# instantiated in user's design.
# Please refer to the Virtex-7 GT Transceiver User Guide
# (UG) for guidelines regarding clock resource selection.
#
###############################################################################


create_clock -period 10.000 -name sys_clk [get_ports system_clock_p]

set_false_path -from [get_ports system_reset_n]
set_property IOSTANDARD LVCMOS18 [get_ports system_reset_n]
set_property PULLTYPE PULLUP [get_ports system_reset_n]

set_property LOC IBUFDS_GTE2_X0Y2 [get_cells clock_buffer]
set_property PACKAGE_PIN F6 [get_ports system_clock_p]
set_property PACKAGE_PIN E6 [get_ports system_clock_n]
set_property PACKAGE_PIN J1 [get_ports system_reset_n]
set_property PACKAGE_PIN C9 [get_ports {PCIe_RX_n}]
set_property PACKAGE_PIN D9 [get_ports {PCIe_RX_p}]
set_property PACKAGE_PIN C7 [get_ports {PCIe_TX_n}]
set_property PACKAGE_PIN D7 [get_ports {PCIe_TX_p}]

set_property INTERNAL_VREF 0.75 [get_iobanks 34]
set_property CFGBVS VCCO [current_design]
set_property CONFIG_VOLTAGE 3.3 [current_design]
set_property BITSTREAM.CONFIG.SPI_BUSWIDTH 4 [current_design]
set_property BITSTREAM.CONFIG.CONFIGRATE 16 [current_design]
set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]
