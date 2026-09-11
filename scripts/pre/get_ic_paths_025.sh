#!/bin/bash

if [[ -z "${MACHINE_ID}" ]]; then
    echo "ERROR: MACHINE_ID must be set before sourcing this script."
    return 1 2>/dev/null || exit 1
fi

# Ensure OCNRES/ICERES logic is set
OCNRES="${OCNRES:-025}"
if [[ "${OCNRES}" == "025" ]]; then ICERES="0.25"; else ICERES="${OCNRES}"; fi

machine=$(echo "${MACHINE_ID}" | cut -d. -f1)

case "${machine}" in
    "wcoss2" | "acorn")
        INPUTDATA_ROOT="/lfs/h2/emc/nems/noscrub/emc.nems/RT/NEMSfv3gfs/input-data-20260617"
        MOM6_IC_DIR="${INPUTDATA_ROOT}/MOM6_IC/${OCNRES}/2011100100"
        CICE_IC_FILE="${INPUTDATA_ROOT}/CICE_IC/${OCNRES}/cice_model_${ICERES}.cpc.res_2011100100.nc"
        ;;
    "orion" | "hercules")
        INPUTDATA_ROOT="/work/noaa/epic/UFS-WM_RT/NEMSfv3gfs/input-data-20251015"
        MOM6_IC_DIR="${INPUTDATA_ROOT}/MOM6_IC/${OCNRES}/2011100100"
        CICE_IC_FILE="${INPUTDATA_ROOT}/CICE_IC/${OCNRES}/cice_model_${ICERES}.cpc.res_2011100100.nc"
        ;;
    "ursa")
        INPUTDATA_ROOT="/scratch4/NAGAPE/epic/role-epic/UFS-WM_RT/NEMSfv3gfs/input-data-20260617"
        MOM6_IC_DIR="${INPUTDATA_ROOT}/MOM6_IC/${OCNRES}/2011100100"
        CICE_IC_FILE="${INPUTDATA_ROOT}/CICE_IC/${OCNRES}/cice_model_${ICERES}.cpc.res_2011100100.nc"
        ;;
    *)
        echo "FATAL: Unknown target machine ${machine}"
        echo "couldn't set INPUTDATA_ROOT, MOM6_IC_DIR, and CICE_IC_FILE"
        return 1 2>/dev/null || exit 1
        ;;
esac

export INPUTDATA_ROOT MOM6_IC_DIR CICE_IC_FILE

# Dynamically set the CICE restart target to match the 0.25 ice_in configuration
export CICE_IC_TARGET="./cice_model.res.nc"
