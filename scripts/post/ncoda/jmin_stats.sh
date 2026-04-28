#!/bin/bash
set -euo pipefail

# 1. Input Validation
if [[ $# -lt 2 ]]; then
    echo "Usage: $0 <date_YYYYMMDD> <base_path>" >&2
    echo "Example: $0 20260326 /lfs/h1/ops/prod/com/rtofs/v2.5" >&2
    exit 1
fi

RTOFS_DATE=$1
BASE_PATH="${2%/}"

# 2. Date Math
PREV_DATE=$(date -d "${RTOFS_DATE} - 1 day" +%Y%m%d)

# 3. Construct File Names
TARGET_FILE="${BASE_PATH}/rtofs.${RTOFS_DATE}/ncoda/logs/hycom_var/hycom_var.${PREV_DATE}00.out"
OUT_FILE="jmin_${RTOFS_DATE}.csv"

# 4. Check existence
if [[ ! -s "${TARGET_FILE}" ]]; then
    echo "FATAL ERROR: File missing or empty: ${TARGET_FILE}" >&2
    exit 2
fi

echo "File found: ${TARGET_FILE}"
echo "Extracting data to: ${OUT_FILE}"

# 5. Extract Stats using awk
awk '
/^Jmin Diagnostic:/ {
    category = substr($0, 18);
    sub(/[ \t\r]+$/, "", category); 
    
    print "Category,Metric,Jmin,N"
    
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
    sub(/[ \t]+[0-9.]+[ \t]+[0-9]+[ \t\r]*$/, "", name);
    
    printf "%s,%s,%s,%s\n", category, name, jmin, n;
    next;
}

in_block && NF == 0 {
    print ""; 
    in_block = 0;
    next;
}
' "${TARGET_FILE}" > "${OUT_FILE}"

echo "Done."
exit 0
