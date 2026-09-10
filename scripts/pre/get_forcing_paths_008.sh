#!/bin/bash

if [[ -z "${MACHINE_ID}" ]]; then
    echo "ERROR: MACHINE_ID must be set before sourcing this script."
    return 1 2>/dev/null || exit 1
fi

machine=$(echo "${MACHINE_ID}" | cut -d. -f1)

case "${machine}" in
    "ursa")
        FORCING_FILE="/scratch5/NCEPDEV/rstprod/Santha.Akella/data/forcing/zg/gfs.2025121400-2025123118_positive.nc"
        ;;
    "wcoss2" | "acorn" | "orion")
        echo "FATAL: 0.08 forcing paths not yet defined for ${machine}"
        return 1 2>/dev/null || exit 1
        ;;
    *)
        echo "FATAL: Unknown target machine ${machine}"
        return 1 2>/dev/null || exit 1
        ;;
esac

export FORCING_FILE
