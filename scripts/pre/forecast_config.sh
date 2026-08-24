#!/bin/bash

# Path to the UFS source code
cwd=$(pwd)
UFSsrc=$(readlink -f "$cwd/../../sorc/ufs_model.fd")

# ==========================================
# 1. Machine Detection
# ==========================================
# Point to the authoritative UFS detection script
source "${UFSsrc}/tests/detect_machine.sh"

# ==========================================
# 2. Sandbox Paths & Common Parameters
# ==========================================
SOURCE_DIR="/lfs/h2/emc/ptmp/santha.akella/FV3_RT/rt_961228/datm_cdeps_mx025_gefs_intel"
SANDBOX_DIR="$PWD/it1"

JOB_NAME="run_datm_cdeps_mx025"
WALLTIME="00:30:00"
NODES=2
TASKS_PER_NODE=128
TOTAL_TASKS=$(( NODES * TASKS_PER_NODE ))

# ==========================================
# 3. Assign Defaults based on MACHINE_ID
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
