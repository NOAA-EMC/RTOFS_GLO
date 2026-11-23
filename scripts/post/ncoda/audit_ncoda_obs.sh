#!/bin/bash
# audit_ncoda_obs.sh
# Purpose: Scan RTOFS observation NetCDF output and generate an observation count CSV.

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)

# 1. --- Input Validation ---
if [[ -z "$1" ]] || [[ -z "$2" ]]; then
    echo "------------------------------------------------------------------"
    echo "ERROR: Missing required arguments."
    echo "Usage: ./audit_ncoda_obs.sh <rtofs_date> <oPath>"
    echo "Example: ./audit_ncoda_obs.sh 20260321 /lfs/h2/emc/.../20260321"
    echo "------------------------------------------------------------------"
    exit 1
fi

rtofs_date=$1
oPath=$2

if [[ ! -d "$oPath" ]]; then
    echo "ERROR: Target directory does not exist: $oPath"
    exit 1
fi

# 2. --- Setup ---
csv_out="${oPath}/obs_counts_${rtofs_date}.csv"

# Load modules (silent)
module load intel ve/rtofs >/dev/null 2>&1

# GARBAGE CHECK: Ensure sanity of nc files.
for nc_file in $(ls "${oPath}"/*.nc 2>/dev/null | sort); do
    if [[ -f "$nc_file" ]]; then
        fname=$(basename "$nc_file")
        if [[ ! -s "$nc_file" ]]; then
            echo "################################################"
            echo "FATAL ERROR: Empty file detected: ${fname}"
            echo "DROP DEAD: Conversion likely failed at binary-to-nc stage."
            echo "################################################"
            exit 1
        fi

        nc_hdr=$(ncdump -h "$nc_file")
        nobs_line=$(echo "$nc_hdr" | grep -i "nobs =" | head -1)
        if [[ -z "$nobs_line" ]]; then
            echo "################################################"
            echo "FATAL ERROR: Missing 'nobs' dimension in: ${fname}"
            echo "DROP DEAD: File is corrupt or invalid observation NetCDF."
            echo "################################################"
            exit 1
        fi

        if ! echo "$nc_hdr" | grep -q "data_type_name"; then
            echo "WARNING: ${fname} is missing data_type_name lookup table."
        fi
    fi
done

echo "------------------------------------------------"
echo ">>> STARTING DATA AUDIT FOR: ${rtofs_date}"
echo ">>> SCANNING: ${oPath}"
echo "------------------------------------------------"

# --- Final Summary & CSV Generation ---
echo "Obs_Date,File_Name,Obs_Plat,Obs_Type,Obs_Count,Good_Count,Data_Types" > "${csv_out}"

# 3. --- Audit Loop ---
for nc_file in $(ls "${oPath}"/*.nc 2>/dev/null | sort); do
    if [[ -f "$nc_file" ]]; then
        fname=$(basename "$nc_file")

        IFS='.' read -r -a parts <<< "$fname"
        obs_date_file="${parts[0]}"

        case "$fname" in
            *.sst.nc)      otype="SST" ;;
            *.ice.nc)      otype="ICE" ;;
            *.profile.nc)  otype="PROFILE" ;;
            *.ssh.nc)      otype="SSH" ;;
            *.sss.nc)      otype="SSS" ;;
            *.velocity.nc) otype="VELOCITY" ;;
            *.sfc.nc)      otype="SFC" ;;
            *)             otype="OTHER" ;;
        esac

        if [[ ${#parts[@]} -ge 5 ]]; then
            obs_plat="${parts[1]}.${parts[2]}"
        else
            obs_plat="${parts[1]}"
        fi

        # Extract nobs, good_count (flag==0), and decoded data_type_name labels via utils.py
        py_summary=$(PYTHONPATH="${SCRIPT_DIR}:${PYTHONPATH}" python3 -c "
from utils import summarize_obs_nc
nobs, good, types = summarize_obs_nc('${nc_file}')
print(f'{nobs}|{good}|{types}')
" 2>/dev/null)

        if [[ -n "$py_summary" ]]; then
            IFS='|' read -r obs_val good_val data_types <<< "$py_summary"
        else
            obs_val=$(ncdump -h "$nc_file" | grep -i "nobs =" | head -1 | awk '{print $3}' | tr -d ';')
            obs_val=${obs_val:-0}
            good_val=""
            data_types=""
        fi

        echo "${obs_date_file},${fname},${obs_plat},${otype},${obs_val},${good_val},\"${data_types}\"" >> "${csv_out}"

        printf "  %s | %-12s | %-8s | Count: %-8s | Good: %-8s | Types: %s\n" \
            "$obs_date_file" "$obs_plat" "$otype" "$obs_val" "${good_val:-N/A}" "${data_types:-N/A}"
    fi
done

echo "------------------------------------------------"
echo ">>> AUDIT COMPLETE. CSV: ${csv_out}"
echo "------------------------------------------------"

exit 0
