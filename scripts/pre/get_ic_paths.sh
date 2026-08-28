#!/bin/bash

if [[ -z "${MACHINE_ID}" ]]; then
    echo "ERROR: MACHINE_ID must be set before sourcing this script."
    return 1 2>/dev/null || exit 1
fi

machine=$(echo "${MACHINE_ID}" | cut -d. -f1)

case "${machine}" in
    "wcoss2") INPUTDATA_ROOT="/lfs/h2/emc/nems/noscrub/emc.nems/RT/NEMSfv3gfs/input-data-20260617" ;;
    "orion" | "hercules") INPUTDATA_ROOT="/work/noaa/nems/role-nems/RT/NEMSfv3gfs/input-data-20210825" ;;
    *)
        echo "FATAL: Unknown target machine ${machine}, couldn't set INPUTDATA_ROOT"
        return 1 2>/dev/null || exit 1
        ;;
esac

export INPUTDATA_ROOT

export MOM6_IC_DIR="${INPUTDATA_ROOT}/MOM6_IC/${OCNRES}/2011100100"
export CICE_IC_DIR="${INPUTDATA_ROOT}/CICE_IC/${OCNRES}"
