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

# Exit if machine is not exactly "wcoss2"
if [[ "$MACHINE" != "wcoss2" ]]; then
    echo "ERROR: Unsupported machine '$MACHINE', currently supported on 'wcoss2' only."
    exit 1
fi

echo ">>> Target machine set to: $MACHINE"
# Exporting MACHINE in case the underlying scripts rely on it as an environment variable
export MACHINE

set -eux

# 1. Build Libraries/Utilities
echo ">>> Building libraries..."
cd ../libs
./build_libs.sh
cd ../sorc

# 2. Build and Install NCODA
echo ">>> Building and installing RTOFS model..."

# 2.1 Clone NCODA
echo ">>> Cloning NCODA repository..."
if [[ ! -d "rtofs_ncoda.fd" ]]; then
   git clone git@github.com:NOAA-EMC/NCODA.git rtofs_ncoda.fd
else
   echo ">>> rtofs_ncoda.fd already exists, skipping clone."
fi

# 2.2 Setup fix directory symlink
# Moving up to the directory above to create the link
cd ..
echo ">>> Linking fix directory..."
# The -n flag ensures it doesn't nest the link if run multiple times
ln -sfn /lfs/h2/emc/couple/noscrub/dan.iredell/RTOFSFIX/20260219/fix/ fix

cd sorc
./build_rtofs.sh || { echo "ERROR: build_rtofs.sh failed."; exit 1; }
./build_rtofs.sh install || { echo "ERROR: build_rtofs.sh install failed."; exit 1; }

echo ">>> SUCCESS! Built NCODA and RTOFS utilities and installed."
exit 0
