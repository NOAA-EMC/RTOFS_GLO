#!/bin/bash

# Path to the UFS source code
cwd=$(pwd)
export UFSsrc=$(readlink -f "$cwd/../../sorc/ufs_model.fd")

# ==========================================
# 1. Machine Detection
# ==========================================
source "${UFSsrc}/tests/detect_machine.sh"

# ==========================================
# 2. Sandbox Paths & Job Parameters
# ==========================================
SANDBOX_DIR="/lfs/h2/emc/ptmp/santha.akella/it1"
#SANDBOX_DIR="/work2/noaa/stmp/santa/EXPDIR/it1"
JOB_NAME="run_datm_cdeps_mx025"
WALLTIME="00:30:00"

# ==========================================
# 3. Component Resolutions & Data Sources
# ==========================================
OCNRES="025"
ICERES="${OCNRES:0:1}.${OCNRES:1}"
DATM_SRC="GEFS"
#
# Coordinate changes to the following with those in
# ufs.configure, datm_in, datm.streams
# The file containing forcing values is set via: get_forcing_paths.sh
#
if [[ "$OCNRES" == "025" && "$DATM_SRC" == "GEFS" ]]; then
    MESH_ATM="mesh.datm.1536x768.nc"
    TOTAL_TASKS=256
fi

# Authoritative FIX directory versions
MOM6_FIX_VER="20250128"
DATM_FIX_VER="20220805"

export OCNRES ICERES DATM_SRC MESH_ATM MOM6_FIX_VER DATM_FIX_VER

# ==========================================
# 4. Assign Defaults based on MACHINE_ID
# ==========================================
if [[ "$MACHINE_ID" == "wcoss2" || "$MACHINE_ID" == "acorn" ]]; then
    SCHEDULER="PBS"
    ACCOUNT="RTOFS-DEV"
    QUEUE="devhigh"
    TASKS_PER_NODE=128
elif [[ "$MACHINE_ID" == "orion" ]]; then
    SCHEDULER="SLURM"
    ACCOUNT="marine-cpu"
    QUEUE="batch"
    PARTITION="orion"
    TASKS_PER_NODE=40
elif [[ "$MACHINE_ID" == "hercules" ]]; then
    echo "FATAL: Hercules is explicitly disabled for this configuration due to data/node instability."
    exit 1
else
    echo "ERROR: Unsupported MACHINE_ID detected: $MACHINE_ID"
    exit 1
fi

# Ceiling division to calculate required nodes
NODES=$(( (TOTAL_TASKS + TASKS_PER_NODE - 1) / TASKS_PER_NODE ))
