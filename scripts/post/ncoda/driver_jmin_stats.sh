#!/bin/bash
set -euo pipefail

# ==============================================================================
# Block 1: Input Validation & Setup
# ==============================================================================
if [[ $# -ne 4 ]]; then
    echo "Usage: $0 <dir> <start_date_YYYYMMDD> <end_date_YYYYMMDD> <output_path>" >&2
    echo "Example to gather from v2.5 prod: $0 /lfs/h1/ops/prod/com/rtofs/v2.5 20260324 20260330 /lfs/h2/emc/stmp/${USER}/ops" >&2
    exit 1
fi

BASE_DIR="${1%/}"
START_DATE=$2
END_DATE=$3
OUT_PATH=$4

# Ensure output directory exists
mkdir -p "${OUT_PATH}"

# Get absolute path to the scripts
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
JMIN_SCRIPT="${SCRIPT_DIR}/jmin_stats.sh"
PY_PLOTTER="${SCRIPT_DIR}/plot_jmin_comp.py"
YAML_CONFIG="${SCRIPT_DIR}/plot_jmin_config.yaml"

if [[ ! -f "${JMIN_SCRIPT}" ]]; then
    echo "FATAL ERROR: Extraction script not found at ${JMIN_SCRIPT}" >&2
    exit 2
fi

if [[ ! -f "${PY_PLOTTER}" ]]; then
    echo "FATAL ERROR: Python plotter not found at ${PY_PLOTTER}" >&2
    exit 3
fi

if [[ ! -f "${YAML_CONFIG}" ]]; then
    echo "FATAL ERROR: YAML config not found at ${YAML_CONFIG}" >&2
    exit 4
fi

# ==============================================================================
# Block 2: Date Iteration & Data Extraction
# ==============================================================================
echo ">>> Starting data extraction from ${START_DATE} to ${END_DATE}..."

current_date="${START_DATE}"

# Switch to output directory so CSVs are written directly there
pushd "${OUT_PATH}" > /dev/null

while [[ "${current_date}" -le "${END_DATE}" ]]; do
    echo "Processing date: ${current_date}"
    
    # Call the extraction script (don't exit if one day is missing/fails, just warn)
    if "${JMIN_SCRIPT}" "${current_date}" "${BASE_DIR}"; then
        echo "  -> Success"
    else
        echo "  -> WARNING: Failed to process ${current_date}. Continuing to next day." >&2
    fi
    
    # Increment date by 1 day
    current_date=$(date -d "${current_date} + 1 day" +%Y%m%d)
done

popd > /dev/null

echo ">>> Data extraction complete. Files saved to: ${OUT_PATH}"

# ==============================================================================
# Block 3: Python Environment & Plotting
# ==============================================================================
echo ">>> Loading Python environment for plotting..."
# Ensure clean state and load required WCOSS2 modules
module reset

# FATAL error if modules fail to load
module load intel ve/hafs || { echo "FATAL ERROR: Failed to load ve/hafs modules." >&2; exit 5; }

echo ">>> Generating plots via ${PY_PLOTTER}..."
# Pass the output directory and the config file to the Python script (using the executable directly)
"${PY_PLOTTER}" "${OUT_PATH}" "${YAML_CONFIG}"

echo ">>> All tasks complete."
exit 0
