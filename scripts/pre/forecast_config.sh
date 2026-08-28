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
JOB_NAME="run_datm_cdeps_mx025"
WALLTIME="00:30:00"
NODES=2
TASKS_PER_NODE=128
TOTAL_TASKS=$(( NODES * TASKS_PER_NODE ))

# ==========================================
# 3. Component Resolutions & Data Sources
# ==========================================
OCNRES="025"
ICERES="${OCNRES:0:1}.${OCNRES:1}"
DATM_SRC="GEFS"
MESH_ATM="mesh.datm.1536x768.nc"

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
elif [[ "$MACHINE_ID" == "orion" || "$MACHINE_ID" == "hercules" ]]; then
    SCHEDULER="SLURM"
    ACCOUNT="marine-cpu"
    QUEUE="batch"
    PARTITION="$MACHINE_ID"
else
    echo "ERROR: Unsupported MACHINE_ID detected: $MACHINE_ID"
    exit 1
fi
