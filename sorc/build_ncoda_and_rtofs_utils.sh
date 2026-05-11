#!/bin/bash

# Build NCODA and RTOFS specific utilities

# 0. Input Arguments
if [[ $# -ne 1 ]]; then
    echo "ERROR: Missing required argument."
    echo "Usage: $0 <machine_name>"
    echo "Example: $0 wcoss2"
    exit 1
fi

MACHINE=$1
echo ">>> Target machine set to: $MACHINE"
# Exporting MACHINE in case the underlying scripts rely on it as an environment variable
export MACHINE

set -eux

# 1. Build Libraries
echo ">>> Building libraries..."
cd ../libs
./build_libs.sh

# 2. Build and Install RTOFS
echo ">>> Building and installing RTOFS model..."
cd ../sorc
./build_rtofs.sh || { echo "ERROR: build_rtofs.sh failed."; exit 1; }
./build_rtofs.sh install || { echo "ERROR: build_rtofs.sh install failed."; exit 1; }

echo ">>> SUCCESS! Built NCODA and RTOFS utilities and installed."
exit 0
