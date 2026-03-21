#!/bin/bash

# 1. Environment Detection Setup
if [[ -z "${MACHINE_ID:-}" || -z "${host_env:-}" ]]; then
    # Resolve the directory where this script lives
    SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
    
    # Path to the directory containing the utilities (scripts/post/)
    UTILS_DIR=$(readlink -f "${SCRIPT_DIR}/..")
    UTILS_PATH="${UTILS_DIR}/get_machine_dev_prod.sh"

    if [[ -f "${UTILS_PATH}" ]]; then
        # Move to the utilities directory so the 
        # 'source ./detect_machine.sh' inside the utility works!
        pushd "${UTILS_DIR}" > /dev/null
            source "./get_machine_dev_prod.sh"
        popd > /dev/null
    else
        echo "ERROR: Cannot find environment utility at: ${UTILS_PATH}"
        exit 1
    fi
fi

# 2. WCOSS2 Safety Check
if [[ "${MACHINE_ID}" == "wcoss2" ]]; then
    if [[ "${host_env}" == "dev" ]]; then
        echo ">>> [WCOSS2] Confirmed: Running on Development node (${host_name})."
    else
        echo "-----------------------------------------------------------------------"
        echo "CRITICAL ERROR: SCRIPT NOT ALLOWED TO RUN"
        echo "Machine   : ${host_name}"
        echo "Env Type  : ${host_env}"
        echo "Status    : RESTRICTED (Production Node)"
        echo "-----------------------------------------------------------------------"
        exit 1
    fi
fi

# --- Rest of the NCODA logic follows ---
