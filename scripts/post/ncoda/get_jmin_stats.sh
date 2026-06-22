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
OUT_PATH="${3:-/lfs/h2/emc/couple/noscrub/${USER}/RTOFS_OM/${rtofs_version}/jmin_stat/${run_dir_date}}"

# Calculate previous day (log_date) from run_dir_date
log_date=$(date -u -d "${run_dir_date} - 1 days" +%Y%m%d)

# 2. Construct Paths and File Names
# NCODA hycom_var log file path uses run_dir_date for the folder and log_date for the file
TARGET_FILE="/lfs/h1/ops/prod/com/rtofs/${rtofs_version}/rtofs.${run_dir_date}/ncoda/logs/hycom_var/hycom_var.${log_date}00.out"

# Output CSV
OUT_FILE="${OUT_PATH}/jmin_${log_date}.csv"

# 3. Ensure output directory exists
mkdir -p "${OUT_PATH}"

# 4. Check existence of the target log file
if [[ ! -s "${TARGET_FILE}" ]]; then
    echo "FATAL ERROR: File missing or empty: ${TARGET_FILE}" >&2
    exit 2
fi

echo "File found: ${TARGET_FILE}"
echo "Extracting Jmin data to: ${OUT_FILE}"

# 5. Extract Stats using awk
awk '
BEGIN {
    print "Category,ObsType,Jmin,N"
}

/^Jmin Diagnostic:/ {
    category = substr($0, 18);
    sub(/[ \t\r]+$/, "", category); 
    in_block = 1;
    next;
}

in_block && NF == 2 && $1 == "Jmin" && $2 == "N" {
    next;
}

in_block && NF >= 3 {
    n = $NF;
    jmin = $(NF-1);
    
    name = $0;
    sub(/^[ \t]+/, "", name);
    sub(/[ \t]+[0-9.-]+[ \t]+[0-9]+[ \t\r]*$/, "", name);
    
    printf "%s,%s,%s,%s\n", category, name, jmin, n;
    next;
}

in_block && NF == 0 {
    in_block = 0;
    next;
}
' "${TARGET_FILE}" > "${OUT_FILE}"

echo "Extraction successful."
exit 0
