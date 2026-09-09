#!/bin/bash

if [[ -z "${MACHINE_ID}" ]]; then
    echo "ERROR: MACHINE_ID must be set before sourcing this script."
    return 1 2>/dev/null || exit 1
fi

machine=$(echo "${MACHINE_ID}" | cut -d. -f1)

case "${machine}" in
    "ursa")
        MOM6_IC_DIR="/scratch5/NCEPDEV/rstprod/Santha.Akella/data/restart/zg"
        CICE_IC_FILE="/scratch5/NCEPDEV/rstprod/Santha.Akella/data/restart/zg/iced.2025-12-15-00000.nc"
        ;;
    "wcoss2" | "acorn" | "orion")
        echo "FATAL: 0.08 IC paths not yet defined for ${machine}"
        return 1 2>/dev/null || exit 1
        ;;
    *)
        echo "FATAL: Unknown target machine ${machine}"
        return 1 2>/dev/null || exit 1
        ;;
esac

export MOM6_IC_DIR CICE_IC_FILE

# Dynamically set the CICE restart target to match the 0.08 ice_in configuration
export CICE_IC_TARGET="./RESTART/iced.2025-12-15-00000.nc"
