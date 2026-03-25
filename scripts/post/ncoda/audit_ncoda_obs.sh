#!/bin/bash
# audit_ncoda_obs.sh
# Purpose: Scan RTOFS NCODA NetCDF observations output and generate an observation count CSV.

# 1. --- Input Validation ---
# Check if both arguments are provided and not empty
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

# Check if the provided directory actually exists
if [[ ! -d "$oPath" ]]; then
    echo "ERROR: Target directory does not exist: $oPath"
    exit 1
fi

# 2. --- Setup ---
csv_out="${oPath}/obs_counts_${rtofs_date}.csv"

# Load modules (silent)
module load intel netcdf >/dev/null 2>&1

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

        # DIMENSION CHECK: Ensure 'nobs' exists via ncdump
        nobs_line=$(ncdump -h "$nc_file" | grep -i "nobs =" | head -1)
        if [[ -z "$nobs_line" ]]; then
            echo "################################################"
            echo "FATAL ERROR: Missing 'nobs' dimension in: ${fname}"
            echo "DROP DEAD: File is corrupt or invalid NCODA NetCDF."
            echo "################################################"
            exit 1
        fi
    fi
done

echo "------------------------------------------------"
echo ">>> STARTING DATA AUDIT FOR: ${rtofs_date}"
echo ">>> SCANNING: ${oPath}"
echo "------------------------------------------------"

# --- Final Summary & CSV Generation ---

# CSV Header
echo "Obs_Date,File_Name,Obs_Plat,Obs_Type,Obs_Count" > "${csv_out}"

# 3. --- Audit Loop ---
for nc_file in $(ls "${oPath}"/*.nc 2>/dev/null | sort); do
    if [[ -f "$nc_file" ]]; then
        fname=$(basename "$nc_file")

        # Split filename into array 'parts' using '.' as delimiter
        IFS='.' read -r -a parts <<< "$fname"
        
        # Field 1 is always the Date
        obs_date_file="${parts[0]}"
        
        # Determine Obs_Type first to help define the Platform logic
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

        # Logic for Obs_Plat:
        # If it's a 5-part name (date.plat.sub.type.nc), join fields 2 and 3
        # If it's a 4-part name (date.plat.type.nc) or 3-part (date.plat.nc), use field 2
        if [[ ${#parts[@]} -ge 5 ]]; then
            obs_plat="${parts[1]}.${parts[2]}"
        else
            obs_plat="${parts[1]}"
        fi

        # Extract 'nobs' dimension count via ncdump
        obs_val=$(ncdump -h "$nc_file" | grep -i "nobs =" | head -1 | awk '{print $3}' | tr -d ';')
        obs_val=${obs_val:-0}

        # Write to CSV
        echo "${obs_date_file},${fname},${obs_plat},${otype},${obs_val}" >> "${csv_out}"
        
        # Print formatted output to STDOUT
        printf "  %s | %-12s | %-8s | Count: %s\n" "$obs_date_file" "$obs_plat" "$otype" "$obs_val"
    fi
done

echo "------------------------------------------------"
echo ">>> AUDIT COMPLETE. CSV: ${csv_out}"
echo "------------------------------------------------"

exit 0
