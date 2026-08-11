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

# 2. Gather Analysis Verification (Innov) Stats

# RTOFS operational version
rtofs_version="v2.5"

# Get Current Date in YYYYMMDD format
current_date=$(date +%Y%m%d)

# Output path (organized by run date)
oPath="/lfs/h2/emc/couple/noscrub/$USER/RTOFS_OM/${rtofs_version}/vrfy_stat/${current_date}"

# Ensure output directory exists before calling the script to gather innov data
mkdir -p "$oPath"

INNOV_SCRIPT="${SCRIPT_DIR}/get_innov_stats.sh"

if [[ -f "${INNOV_SCRIPT}" ]]; then
    echo ">>> Gathering NCODA verification stats (innov) for ${current_date}..."

    # Example Usage: ./get_innov_stats.sh "v2.5" "20260811" "${oPath}"
    "${INNOV_SCRIPT}" "${rtofs_version}" "${current_date}" "${oPath}" || exit 1
else
    echo "ERROR: Script to gather NCODA innov stats: ${INNOV_SCRIPT} not found."
    exit 2
fi

# --- 3. Generate Time Series & Profile Plots ---
PLOT_SCRIPT="${SCRIPT_DIR}/plot_innov.py"

if [[ -f "${PLOT_SCRIPT}" ]]; then
    echo "------------------------------------------------"
    echo ">>> Loading Python environment for plotting..."
    # Ensure clean state and load required WCOSS2 modules
    module reset
    module load intel ve/rtofs || echo "WARNING: Failed to load ve/rtofs modules."

    echo ">>> Generating Innov Time Series and Profile Plots..."
    # Modify `plot_innov_config.yaml` as needed
    "${PLOT_SCRIPT}" "${SCRIPT_DIR}/plot_innov_config.yaml" || echo "WARNING: Plot generation failed for ${current_date}"
else
    echo "FATAL ERROR: Plotting script not found at ${PLOT_SCRIPT}. Skipping plots."
    exit 3
fi

echo "------------------------------------------------"
echo ">>> innov_stats.sh completed for ${current_date}."
echo "------------------------------------------------"

exit 0
