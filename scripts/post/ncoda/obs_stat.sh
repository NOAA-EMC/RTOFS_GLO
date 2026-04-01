#!/bin/bash

# 1. Machine and environment & Safety Check
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
CHECK_ENV_PATH="$(readlink -f "${SCRIPT_DIR}/../check_machine_env.sh")"

if [[ -f "${CHECK_ENV_PATH}" ]]; then
    source "${CHECK_ENV_PATH}" || exit 3
else
    echo "ERROR: Machine and environment check missing at ${CHECK_ENV_PATH}"
    exit 2
fi

# 2. Binary obs converter Executable Check and Build Logic
# Path to the source directory and the expected executable
RTOFS_DA_DIR=$(readlink -f "${SCRIPT_DIR}/../../../sorc/rtofs_da.fd")
EXEC_PATH="${RTOFS_DA_DIR}/exec/read_binary_qc_obs.x"

if [[ ! -f "${EXEC_PATH}" ]]; then
    echo ">>> Executable NOT found at: ${EXEC_PATH}"
    echo ">>> Attempting to build using build_all.sh..."

    if [[ -f "${RTOFS_DA_DIR}/build_all.sh" ]]; then
        pushd "${RTOFS_DA_DIR}" > /dev/null
            ./build_all.sh
        popd > /dev/null

        # Verify if build was successful
        if [[ -f "${EXEC_PATH}" ]]; then
            echo ">>> Build Successful! Executable created."
        else
            echo "-----------------------------------------------------------------------"
            echo "ERROR: Build failed. ${EXEC_PATH} still does not exist."
            echo "Check the build logs in ${RTOFS_DA_DIR}"
            echo "-----------------------------------------------------------------------"
            exit 4
        fi
    else
        echo "ERROR: build_all.sh not found in ${RTOFS_DA_DIR}. Cannot compile."
        exit 5
    fi
else
    echo ">>> Found Executable: ${EXEC_PATH}"
fi

# 3. Convert NCODA Quality- Controlled observations format: binary to netcdf
CONV_SCRIPT="${SCRIPT_DIR}/convert_ncoda_binary_qc_obs.sh"

# RTOFS operational version
rtofs_version="v2.5"

# Get Current Date in YYYYMMDD format
#current_date="20260321"
current_date=$(date +%Y%m%d)

# RTOFS Cycle Date (Same as current_date)
rtofs_date="${current_date}"

# Observation Date (current_date - 1)
#o_date=$(date -u -d "${current_date} - 1 days" +%Y%m%d)

# Output path (organized by run date)
oPath="/lfs/h2/emc/couple/noscrub/$USER/RTOFS_OM/${rtofs_version}/obs_stat/${rtofs_date}"

# Ensure output directory exists before calling the converter
mkdir -p "$oPath"

if [[ -f "${CONV_SCRIPT}" ]]; then
    echo ">>> Launching NCODA Binary-to-NetCDF conversion for ${current_date}..."

    # Example Usage: ./convert_ncoda_binary_qc_obs.sh "v2.5" "20260321" "${oPath}"
    "${CONV_SCRIPT}" "${rtofs_version}" "${rtofs_date}" "${oPath}" || exit 6
else
    echo "ERROR: Conversion script ${CONV_SCRIPT} not found."
    exit 7
fi

# 4. Create a csv file that logs all observation counts
AUDIT_SCRIPT="${SCRIPT_DIR}/audit_ncoda_obs.sh"

if [[ -f "${AUDIT_SCRIPT}" ]]; then
    echo ">>> Starting NCODA Observation Audit and CSV Generation..."

    # Execute: pass the RTOFS run date and the full output path
    "${AUDIT_SCRIPT}" "${rtofs_date}" "${oPath}" || exit 8
else
    echo "ERROR: Audit script not found at ${AUDIT_SCRIPT}"
    exit 9
fi

# --- Housekeeping: Remove non-CSV files from folders older than 30 days ---
# Use $(dirname "$oPath") to target the parent directory containing all date folders
archive_base=$(dirname "${oPath}")

if [[ -d "${archive_base}" ]]; then
    echo ">>> Running Housekeeping in: ${archive_base}"

    # -maxdepth 1: stay in the archive folder
    # -name "20[0-9]*": target only YYYYMMDD folders
    # -type d: only directories
    # -ctime +30: older than 30 days
    # Execute a sub-find to delete everything EXCEPT .csv files within those folders
    find "${archive_base}" -maxdepth 1 -name "20[0-9][0-9][0-9][0-9][0-9][0-9]" -type d -ctime +30 \
       -exec find {} -type f ! -name "*.csv" -delete \;

    echo ">>> Housekeeping complete."
else
    echo "FATAL ERROR: Archive base ${archive_base} not found. Skipping cleanup."
    exit 10
fi

# --- 5. Generate Time Series Plots ---
PLOT_SCRIPT="${SCRIPT_DIR}/plot_obs_stat.py"

if [[ -f "${PLOT_SCRIPT}" ]]; then
    echo "------------------------------------------------"
    echo ">>> Loading Python environment for plotting..."
    # Ensure clean state and load required WCOSS2 modules
    module reset
    module load intel ve/hafs || echo "WARNING: Failed to load ve/hafs modules."

    echo ">>> Generating Time Series Plots..."
    # Add or remove platforms here as needed (case-insensitive)
    for plat in sfc profile viirs.npp goes metop; do
        "${PLOT_SCRIPT}" "${archive_base}" "$plat" || echo "WARNING: Plot generation failed for $plat."
    done
else
    echo "FATAL ERROR: Plotting script not found at ${PLOT_SCRIPT}. Skipping plots."
    exit 11
fi

echo "------------------------------------------------"
echo ">>> obs_stat.sh completed for ${current_date}."
echo "------------------------------------------------"

exit 0
