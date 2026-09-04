#!/bin/bash

if [[ -z "${MACHINE_ID}" ]]; then
    echo "ERROR: MACHINE_ID must be set before sourcing this script."
    return 1 2>/dev/null || exit 1
fi

machine=$(echo "${MACHINE_ID}" | cut -d. -f1)

# ====================================================================
# 1. Authoritative FIX Directories (From global-workflow)
# Reference: https://github.com/NOAA-EMC/global-workflow/blob/develop/sorc/link_workflow.sh
# ====================================================================
case "${machine}" in
    "wcoss2" | "acorn") FIX_DIR="/lfs/h2/emc/global/noscrub/emc.global/FIX/fix" ;;
    "hera" | "ursa") FIX_DIR="/scratch3/NCEPDEV/global/role.glopara/fix" ;;
    "orion" | "hercules") FIX_DIR="/work2/noaa/global/role-global/fix" ;;
    "gaeac6") FIX_DIR="/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix" ;;
    "aws-ec2") FIX_DIR="/lustre/global/data/fix" ;;
    "derecho") FIX_DIR="/lustre/desc1/p/nral0032/global/data/fix" ;;
    "noaacloud") FIX_DIR="/lustre/fix" ;;
    *)
        echo "FATAL: Unknown target machine ${machine}, couldn't set FIX_DIR"
        return 1 2>/dev/null || exit 1
        ;;
esac

export MOM6_FIX_DIR="${FIX_DIR}/mom6/${MOM6_FIX_VER}/${OCNRES}"

# Needed for grid_spec.nc at p025
export DATM_FIX_DIR="${FIX_DIR}/datm/${DATM_FIX_VER}"
