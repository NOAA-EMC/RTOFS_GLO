#!/bin/bash

# Build the NCODA for RTOFS

# 0. Input Arguments
if [[ $# -ne 1 ]]; then
    echo "ERROR: Missing required argument."
    echo "Usage: $0 <machine_name>"
    echo "Example: $0 wcoss2"
    exit 1
fi

MACHINE=$1
echo ">>> Target machine set to: $MACHINE"

set -eux

# Execute logic ONLY if machine is wcoss2
if [[ "$MACHINE" == "wcoss2" ]]; then
    echo ">>> Machine is wcoss2. Proceeding with NCODA build..."

    # 1. Clone NCODA
    echo ">>> Cloning NCODA repository..."
    if [[ ! -d "rtofs_ncoda.fd" ]]; then
        git clone git@github.com:NOAA-EMC/NCODA.git rtofs_ncoda.fd
    else
        echo ">>> rtofs_ncoda.fd already exists, skipping clone."
    fi

    # 2. Setup fix directory symlink
    # Moving up to the directory above to create the link
    cd ..
    echo ">>> Linking fix directory..."
    # The -n flag ensures it doesn't nest the link if run multiple times
    ln -sfn /lfs/h2/emc/couple/noscrub/dan.iredell/RTOFSFIX/20260219/fix/ fix

    # 3. Build NCODA
    echo ">>> Building NCODA..."
    cd sorc/rtofs_ncoda.fd
    ./build_ncoda.sh

    # Sanity check
    if [[ ! -x "rtofs_ncoda" ]]; then
        echo "ERROR: NCODA build failed to produce the executable (rtofs_ncoda)."
        exit 1
    fi

    echo ">>> SUCCESS! NCODA build completed for wcoss2."

else
    echo ">>> Machine is NOT wcoss2 ($MACHINE). Skipping NCODA build."
fi

exit 0
