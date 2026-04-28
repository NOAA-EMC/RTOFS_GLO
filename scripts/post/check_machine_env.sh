#!/bin/bash
# check_machine_env.sh
# Centralized script to detect machine and enforce WCOSS2 dev-node safety.

# Determine the directory of THIS script
UTILS_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)

# 1. Environment Detection Setup
if [[ -z "${MACHINE_ID:-}" || -z "${host_env:-}" ]]; then
    UTILS_PATH="${UTILS_DIR}/get_machine_dev_prod.sh"

    if [[ -f "${UTILS_PATH}" ]]; then
        pushd "${UTILS_DIR}" > /dev/null
            source "./get_machine_dev_prod.sh"
        popd > /dev/null
    else
        echo "ERROR: Cannot find environment utility at: ${UTILS_PATH}"
        exit 2
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
        exit 3
    fi
fi
