#!/bin/bash
set -euo pipefail

# 1. Parse arguments (Defaults: v2.5, today, default path)
if [[ $# -gt 3 ]]; then
    echo "Usage: $0 [rtofs_version] [run_dir_date] [oPath]"
    echo "Example: $0 v2.5 $(date +%Y%m%d) /path/to/output"
    exit 1
fi

rtofs_version="${1:-v2.5}"
run_dir_date="${2:-$(date +%Y%m%d)}"
OUT_PATH="${3:-/lfs/h2/emc/couple/noscrub/${USER}/RTOFS_OM/${rtofs_version}/vrfy_stat/${run_dir_date}}"

# 2. Construct Paths and File Names
SEARCH_PATH="/lfs/h1/ops/prod/com/rtofs/${rtofs_version}/rtofs.${run_dir_date}/rtofs_glo.t00z.ncoda_hycom_var.OUTPUT.*"

# Use array to safely expand glob
shopt -s nullglob
TARGET_FILES=(${SEARCH_PATH})
shopt -u nullglob

if [[ ${#TARGET_FILES[@]} -eq 0 ]]; then
    echo "FATAL ERROR: No OUTPUT files found matching: ${SEARCH_PATH}" >&2
    exit 2
fi

TARGET_FILE="${TARGET_FILES[0]}"

# 3. Ensure output directory exists
mkdir -p "${OUT_PATH}"

echo "File found: ${TARGET_FILE}"
echo "Extracting Verification Stats to: ${OUT_PATH}"

# 4. Extract Stats using awk
awk -v out_dir="${OUT_PATH}" -v date="${run_dir_date}" '
BEGIN {
    count["Ice Cover"] = 0
    count["Temperature"] = 0
    count["Salinity"] = 0
    count["Geopotential"] = 0
}

/^Analysis Verification:/ {
    category = $0
    sub(/^Analysis Verification:[ \t]*/, "", category)
    sub(/[ \t\r]*$/, "", category) 

    count[category]++
    in_block = 1

    if (category == "Ice Cover") {
        out_file = out_dir "/ice_cover_" date ".csv"
        header = "ObsType,Bias_Fcst,Bias_Anl,RMS_Fcst,RMS_Anl,N"
    } else if (category == "Temperature") {
        if (count[category] == 1) {
            out_file = out_dir "/temperature_" date ".csv"
            header = "ObsType,Bias_Fcst,Bias_Anl,RMS_Fcst,RMS_Anl,N"
        } else {
            out_file = out_dir "/t_z_" date ".csv"
            header = "Depth,Bias_Fcst,Bias_Anl,RMS_Fcst,RMS_Anl,N"
        }
    } else if (category == "Salinity") {
        if (count[category] == 1) {
            out_file = out_dir "/salinity_" date ".csv"
            header = "ObsType,Bias_Fcst,Bias_Anl,RMS_Fcst,RMS_Anl,N"
        } else {
            out_file = out_dir "/s_z_" date ".csv"
            header = "Depth,Bias_Fcst,Bias_Anl,RMS_Fcst,RMS_Anl,N"
        }
    } else if (category == "Geopotential") {
        if (count[category] == 1) {
            out_file = out_dir "/geopotential_" date ".csv"
            header = "ObsType,Bias_Fcst,Bias_Anl,RMS_Fcst,RMS_Anl,N"
        } else {
            out_file = out_dir "/geo_z_" date ".csv"
            header = "Depth,Bias_Fcst,Bias_Anl,RMS_Fcst,RMS_Anl,N"
        }
    } else {
        in_block = 0
        next
    }

    print header > out_file
    next
}

in_block {
    if (NF == 0) next;
    if ($0 ~ /Mean Bias/ || $0 ~ /Forecast.*Analysis/) next;

    # Valid data lines end with observation count (integer) and RMS (float)
    if ($NF ~ /^[0-9]+$/ && $(NF-1) ~ /^[0-9.-]+$/) {
        n = $NF
        rms_a = $(NF-1)
        rms_f = $(NF-2)
        bias_a = $(NF-3)
        bias_f = $(NF-4)

        name = $1
        for (i = 2; i <= NF - 5; i++) {
            name = name " " $i
        }
        
        # Clean up depth labels (e.g., "10." -> "10")
        if (header ~ /^Depth/) {
            sub(/\.$/, "", name)
        }

        printf "%s,%s,%s,%s,%s,%s\n", name, bias_f, bias_a, rms_f, rms_a, n >> out_file
    } else {
        # End of the block hit
        in_block = 0
    }
}
' "${TARGET_FILE}"

echo "Extraction successful."
exit 0
