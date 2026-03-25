#!/bin/bash

# 1. Environment Detection Setup
if [[ -z "${MACHINE_ID:-}" || -z "${host_env:-}" ]]; then
    SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
    UTILS_DIR=$(readlink -f "${SCRIPT_DIR}/..")
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

# 3. Binary obs converter Executable Check and Build Logic
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

# 4. Convert NCODA Quality- Controlled observations format: binary to netcdf
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

# 5. Create a csv file that logs all observation counts
AUDIT_SCRIPT="${SCRIPT_DIR}/audit_ncoda_obs.sh"

if [[ -f "${AUDIT_SCRIPT}" ]]; then
    echo ">>> Starting NCODA Observation Audit and CSV Generation..."

    # Execute: pass the RTOFS run date and the full output path
    "${AUDIT_SCRIPT}" "${rtofs_date}" "${oPath}" || exit 8
else
    echo "ERROR: Audit script not found at ${AUDIT_SCRIPT}"
    exit 9
fi

# --- Housekeeping: Remove folders older than 30 days ---
# Use $(dirname "$oPath") to target the parent directory containing all date folders
archive_base=$(dirname "${oPath}")

if [[ -d "${archive_base}" ]]; then
    echo ">>> Running Housekeeping in: ${archive_base}"

    # -maxdepth 1: stay in the archive folder
    # -name "20[0-9]*": target only YYYYMMDD folders
    # -type d: only directories
    # -ctime +30: older than 30 days
    find "${archive_base}" -maxdepth 1 -name "20[0-9][0-9][0-9][0-9][0-9][0-9]" -type d -ctime +30 -exec rm -rf {} +

    echo ">>> Housekeeping complete."
else
    echo "FATAL ERROR: Archive base ${archive_base} not found. Skipping cleanup."
    exit 10
fi

echo "------------------------------------------------"
echo ">>> obs_stat.sh completed for ${current_date}."
echo "------------------------------------------------"

exit 0
