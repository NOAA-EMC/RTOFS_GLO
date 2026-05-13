#!/bin/bash

# Build the UFS and UFS_UTILS

# 0. Input Arguments
if [[ $# -ne 2 ]]; then
    echo "ERROR: Missing required arguments."
    echo "Usage: $0 <RUN_ENVIR> <machine_name>"
    echo "Options for RUN_ENVIR: emc, nco"
    echo "Options for machine_name: wcoss2, ursa, orion, gaeac6"
    exit 1
fi

RUN_ENVIR=$1
MACHINE=$2
echo ">>> Target RUN_ENVIR set to: $RUN_ENVIR"
echo ">>> Target machine set to: $MACHINE"

set -eux

cd ..

# 1. Clone UFS-weather-model (and update submodules) and UFS_UTILS (for mppnccombine)
git submodule update --init --recursive

# Ensure top-level exec directory exists
mkdir -p exec
TOP_EXEC="$(pwd)/exec"

# 2. Setup and build ufs_utils
echo ">>> Building ufs_utils for $RUN_ENVIR on $MACHINE..."
cd sorc/ufs_utils.fd/fix
./link_fixdirs.sh "$RUN_ENVIR" "$MACHINE"
cd ..
./build_all.sh

# Sanity check: verify mppnccombine was built and is executable
if [[ ! -x "exec/mppnccombine" ]]; then
    echo "ERROR: exec/mppnccombine was not generated or is not executable."
    exit 1
fi
cp exec/mppnccombine "$TOP_EXEC/"
cd ../..

# 3. Build UFS
echo ">>> Building UFS..."
cd scripts/pre/
./compile_ufs.sh 

# Return to top-level BEFORE checking the sorc/ relative path
cd ../..

# Sanity check: verify ufs_model.x was built and is executable
if [[ ! -x "sorc/ufs_model.fd/tests/ufs_model.x" ]]; then
    echo "ERROR: sorc/ufs_model.fd/tests/ufs_model.x was not generated or is not executable."
    exit 2
fi
cp sorc/ufs_model.fd/tests/ufs_model.x "$TOP_EXEC/"

# 4. Final verification
echo ">>> Verifying final executables..."
if [[ ! -x "exec/mppnccombine" || ! -x "exec/ufs_model.x" ]]; then
    echo "ERROR: Executables failed to copy to the top-level exec/ directory."
    exit 3
fi

echo ">>> SUCCESS! Build of forecast model is complete and executables verified."
exit 0
