#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: $(basename $0) /path/to/firmware.bin" >&2
  exit 1
fi

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
FIRMWARE_FILE=$(realpath "$1")

echo "--- FPGA Programming ---"
echo "Config:    $SCRIPT_DIR/acorn.cfg"
echo "Proxy Bit:   $SCRIPT_DIR/proxy.bit"
echo "Firmware:  $FIRMWARE_FILE"
echo "------------------------"

openocd \
  -f "$SCRIPT_DIR/acorn.cfg" \
  -c "init" \
  -c "pld load 0 $SCRIPT_DIR/proxy.bit" \
  -c "flash probe 0" \
  -c "flash write_image erase $FIRMWARE_FILE 0" \
  -c "shutdown"
