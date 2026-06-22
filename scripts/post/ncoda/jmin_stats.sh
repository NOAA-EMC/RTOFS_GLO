#!/bin/bash
set -euo pipefail

# 1. Machine and environment & Safety Check
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
CHECK_ENV_PATH="$(readlink -f "${SCRIPT_DIR}/../check_machine_env.sh")"

if [[ -f "${CHECK_ENV_PATH}" ]]; then
    source "${CHECK_ENV_PATH}" || exit 3
else
    echo "ERROR: Machine and environment check missing at ${CHECK_ENV_PATH}"
    exit 2
fi

# 2. Gather summary of fit-to-observations, i.e., cost functional (jmin)

# RTOFS operational version
rtofs_version="v2.5"

# Get Current Date in YYYYMMDD format
#current_date="20260622"
current_date=$(date +%Y%m%d)

# Output path (organized by run date)
oPath="/lfs/h2/emc/couple/noscrub/$USER/RTOFS_OM/${rtofs_version}/jmin_stat/${current_date}"

# Ensure output directory exists before calling the script to gather jmin data
mkdir -p "$oPath"

JMIN_SCRIPT="${SCRIPT_DIR}/get_jmin_stats.sh"

if [[ -f "${JMIN_SCRIPT}" ]]; then
    echo ">>> Gathering NCODA cost functional (jmin) for ${current_date}..."

    # Example Usage: ./get_jmin_stats.sh "v2.5" "20260622" "${oPath}"
    "${JMIN_SCRIPT}" "${rtofs_version}" "${current_date}" "${oPath}" || exit 1
else
    echo "ERROR: Script to gather NCODA jmin stats: ${JMIN_SCRIPT} not found."
    exit 2
fi

# --- 3. Generate Time Series Plots ---
PLOT_SCRIPT="${SCRIPT_DIR}/plot_jmin.py"

if [[ -f "${PLOT_SCRIPT}" ]]; then
    echo "------------------------------------------------"
    echo ">>> Loading Python environment for plotting..."
    # Ensure clean state and load required WCOSS2 modules
    module reset
    module load intel ve/rtofs || echo "WARNING: Failed to load ve/rofs modules."

    echo ">>> Generating Time Series Plots..."
    # Modify `plot_jmin_config.yaml` as needed
    "${PLOT_SCRIPT}" plot_jmin_config.yaml || echo "WARNING: Plot generation failed for ${current_date}"
else
    echo "FATAL ERROR: Plotting script not found at ${PLOT_SCRIPT}. Skipping plots."
    exit 3
fi

echo "------------------------------------------------"
echo ">>> jmin_stats.sh completed for ${current_date}."
echo "------------------------------------------------"

exit 0
