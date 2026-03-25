#!/bin/bash

# --- 1. Environment Setup (Modules) ---
# Resolve the root RTOFS_GLO directory (dir_mod0)
# Script is in: RTOFS_GLO/scripts/post/ncoda
# dir_mod0 should be RTOFS_GLO
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
DIR_MOD0=$(readlink -f "${SCRIPT_DIR}/../../..")

# Define the build directory where load_modules.sh lives
BASE_DA="${DIR_MOD0}/sorc/rtofs_da.fd"

if [[ -f "${BASE_DA}/load_modules.sh" ]]; then
    echo ">>> Loading modules from: ${DIR_MOD0}/versions/build.ver"
    # source load_modules.sh <path_to_rtofs_glo_root>
    source "${BASE_DA}/load_modules.sh" "${DIR_MOD0}"
else
    echo "ERROR: Cannot find ${BASE_DA}/load_modules.sh"
    echo "Attempting manual fallback for WCOSS2..."
    module load intel/19.1.3.304 PrgEnv-intel/8.3.3 netcdf/4.7.4
fi

# Verify environment
module list

# --- 2. Configuration (Date and Path Logic) ---
# $1: rtofs_version is passed from obs_stat.sh (RTOFS production version: vx.x)
# $2: current_date is passed from obs_stat.sh (current_date in YYYYMMDD)
# $3: oPath is passed from obs_stat.sh (where output will be saved)

# If no argument is provided, default to v2.5
rtofs_version=${1:-"v2.5"}

# If no argument is provided, default to current UTC date
rtofs_date=${2:-$(date -u +%Y%m%d)}

# oPath: Where the converted NetCDF files will be saved
oPath=${3:-"/lfs/h2/emc/stmp/${USER}/qc_decode/${rtofs_date}"}

# o_date is typically 1 days prior to the RTOFS cycle date
o_date=$(date -u -d "${rtofs_date} - 1 days" +%Y%m%d)

# inBase: Where the operational RTOFS NCODA binary files live
inBase="/lfs/h1/ops/prod/com/rtofs/${rtofs_version}/rtofs.${rtofs_date}/ncoda/ocnqc"

# exec: Path to the executable verified/built by obs_stat.sh
# BASE_DA was defined in Section 1
exec="${BASE_DA}/exec/read_binary_qc_obs.x"

# Ensure output directory exists
mkdir -p "$oPath"

echo "------------------------------------------------"
echo "RTOFS Cycle Date : ${rtofs_date}"
echo "Observation Date : ${o_date}"
echo "Input Base Path  : ${inBase}"
echo "Output Path      : ${oPath}"
echo "Executable       : ${exec}"
echo "------------------------------------------------"

# --- 3. Logging & Core Processing Function ---

log_file="${oPath}/conversion_${rtofs_date}.log"
exec > >(tee -a "$log_file") 2>&1

count_pass=0
count_fail=0

log_msg() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1"
}

# --- Core Processing Function ---
# $1:subdir, $2:in_file, $3:out_file, $4:oType
run_conv() {
    local in_f="${inBase}/${1}/${2}"
    local out_f="$3"
    local o_type="$4"

    if [[ -f "$in_f" ]]; then
        log_msg "CONVERTING: [${in_f}] -> [${out_f}]"
        
        # Run the executable
        if "${exec}" "${in_f}" "${o_type}" "${oPath}" "${out_f}"; then
            
            # --- Filename Cleanup ---
            # Check if Fortran created a file with a space (e.g., "filename .nc")
            # We look for the pattern in oPath and move it to the clean out_f
            actual_gen_file=$(ls ${oPath}/${2}*.nc 2>/dev/null | grep -v "${out_f}" | head -n 1)

            if [[ -n "${actual_gen_file}" ]]; then
                mv "${actual_gen_file}" "${oPath}/${out_f}"
                log_msg "CLEANUP: Renamed [$(basename "${actual_gen_file}")] to [${out_f}]"
            fi
            
            ((count_pass++))
        else
            log_msg "ERROR: Executable failed for ${in_f}"
            ((count_fail++))
        fi
    else
        log_msg "MISSING: ${in_f} (Skipping)"
        ((count_fail++))
    fi
}

# --- Main Execution ---
log_msg "START: RTOFS QC Processing Suite"
echo "------------------------------------------------"

set +e # Tell the shell: "Don't die if a command returns non-zero"
       # This is necessary because not all the observations data exists for all hours (00, 06, 12, 18)

# 1. SST: Standard Satellites (6hr intervals)
for sat in amsr goes himawari msg; do
    for hr in 00 06 12 18; do
        run_conv "$sat" "${o_date}${hr}.${sat}" "${o_date}${hr}.${sat}.sst.nc" "sst"
    done
done

# 2. SST: METOP & VIIRS (3hr intervals)
for hr in {00..21..03}; do
    run_conv "metop" "${o_date}${hr}.metop" "${o_date}${hr}.metop.sst.nc" "sst"
    for sub in jpss npp; do
        run_conv "viirs" "${o_date}${hr}.${sub}" "${o_date}${hr}.viirs.${sub}.sst.nc" "sst"
    done
done

# 3. Ice (amsr, ssmi)
for inst in amsr ssmi; do
    run_conv "ice" "${o_date}00.${inst}" "${o_date}.${inst}.ice.nc" "ice"
done

# 4. Standard Daily Types (SSH, SSS, Velocity, Sfc, Profile)
for type in ssh sss velocity sfc profile; do
    run_conv "$type" "${o_date}00.${type}" "${o_date}.${type}.nc" "$type"
done

# 5. MDB (Special case: subdirectory is 'sss')
run_conv "sss" "${o_date}00.mdb" "${o_date}.mdb.sss.nc" "mdb"

# --- Final Summary & CSV Generation ---
echo "------------------------------------------------"
log_msg "FINISHED: Processing Complete. Starting Data Audit..."
echo "------------------------------------------------"


