#!/bin/bash
set -e

# ==============================================================================
# CONFIGURATION
# ==============================================================================
# 1. Resolve Paths (Always run relative to this script)
cd "$(dirname "${BASH_SOURCE[0]}")"; ROOT_DIR="$(pwd)"

# 2. Vivado Settings
VIVADO_PATH="/home/wijdaan/Xilinx/2025.1/Vivado/settings64.sh"
PART="xc7a200tfbg484-2"
JOBS=8

# 3. Project Structure
PROJ_NAME="proj"
BUILD_DIR="$ROOT_DIR/$PROJ_NAME"
SRC_DIR="$ROOT_DIR/../source"
CNSTR_DIR="$ROOT_DIR/constraints"

# 4. Input Scripts
BD_TCL="$ROOT_DIR/system.tcl"
GEN_TCL="$ROOT_DIR/generate.tcl"

# 5. Output & Tools
BUILD_TCL="$ROOT_DIR/build.tcl"
FLASH_TOOL="$ROOT_DIR/../../flash/flash.sh"
BIN_FILE="$PROJ_NAME/acorn.bin" # Relative to ROOT_DIR for the flash tool

# ==============================================================================
# FUNCTIONS
# ==============================================================================
usage() {
    echo "Usage: $0 [-b] [-f] [-c]"
    echo "  (no args) Setup project structure only"
    echo "  -b        Build (Setup + Synthesis + Impl + Bitstream)"
    echo "  -f        Flash .bin to FPGA (skips build unless -b is used)"
    echo "  -c        Clean artifacts"
    exit 1
}

clean() {
    echo "[*] Cleaning artifacts..."
    rm -rf "$BUILD_DIR" "$BUILD_TCL" .Xil *.jou *.log
}

flash_fpga() {
    echo "[*] Flashing FPGA..."
    if [ ! -x "$FLASH_TOOL" ]; then echo "Error: Flash tool missing at $FLASH_TOOL"; exit 1; fi
    if [ ! -f "$BIN_FILE" ];   then echo "Error: Binary missing at $BIN_FILE"; exit 1; fi
    
    "$FLASH_TOOL" "$BIN_FILE"
}

setup_vivado() {
    if [ -f "$VIVADO_PATH" ]; then
        source "$VIVADO_PATH"
    else
        echo "Error: Vivado settings not found"; exit 1
    fi
}

generate_tcl() {
    cat <<EOT > "$BUILD_TCL"
    # --------------------------------------------------------------------------
    # 1. Project Setup
    # --------------------------------------------------------------------------
    create_project $PROJ_NAME "$BUILD_DIR" -part $PART -force
    set_property target_language Verilog [current_project]

    # --------------------------------------------------------------------------
    # 2. Add Sources & Constraints
    # --------------------------------------------------------------------------
    if {[file exists "$BD_TCL"]} { source "$BD_TCL" }

    if {[file exists "$SRC_DIR"]} {
        add_files "$SRC_DIR"
        set sv_files [glob -nocomplain "$SRC_DIR/*.sv"]
        if {\$sv_files ne ""} { set_property file_type SystemVerilog [get_files *.sv] }
    }

    if {[file exists "$CNSTR_DIR"]} { add_files -fileset constrs_1 "$CNSTR_DIR" }
    
    update_compile_order -fileset sources_1
EOT

    # Append Compilation Logic if requested
    if [ "$COMPILE" -eq 1 ]; then
        cat <<EOT >> "$BUILD_TCL"
    
    # --------------------------------------------------------------------------
    # 3. Compilation & Binary Generation
    # --------------------------------------------------------------------------
    puts "--> Starting Compilation..."
    launch_runs impl_1 -to_step write_bitstream -jobs $JOBS
    wait_on_run impl_1
    
    open_run impl_1
    if {[file exists "$GEN_TCL"]} { source "$GEN_TCL" }
EOT
    fi
}

# ==============================================================================
# EXECUTION
# ==============================================================================
COMPILE=0
FLASH=0
SKIP_VIVADO=0

while getopts "bfc" opt; do
    case ${opt} in
        b) COMPILE=1 ;;
        f) FLASH=1; SKIP_VIVADO=1 ;; # Default to skip build if flashing
        c) clean; exit 0 ;;
        *) usage ;;
    esac
done

# Logic Adjustment: If -b AND -f are used, we must run Vivado
if [ "$COMPILE" -eq 1 ]; then
    SKIP_VIVADO=0
fi

# 1. Run Vivado (Build or Setup)
if [ "$SKIP_VIVADO" -eq 0 ]; then
    setup_vivado
    generate_tcl
    
    echo "[*] Launching Vivado..."
    vivado -mode batch -source "$BUILD_TCL" -notrace
    
    rm -f "$BUILD_TCL" vivado*.jou vivado*.log
fi

# 2. Run Flash
if [ "$FLASH" -eq 1 ]; then
    flash_fpga
fi

echo "[SUCCESS] Done."