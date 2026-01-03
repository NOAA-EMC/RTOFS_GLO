#!/bin/bash
#==============================================================================
# Driver for RTOFS Atmospheric Forcing
# Handles time-window expansion, sequential staging, and record verification.
#==============================================================================
set -x

msg="${RUN}_atmforcing.sh has begun on $(hostname) at $(date)"
postmsg "$msg"

if [ $# -lt 3 ]; then
  echo "USAGE: ${RUN}_atmforcing.sh start_date end_date interval"
  exit 2
fi

# Inputs
sdate=$1
edate=$2
intvl=$3

# Expand window by 3 hours for temporal buffer
sdate=$($NDATE -3 $sdate)
edate=$($NDATE 3  $edate)

# Set Network and Filename based on Mode
if [[ "${RUN_MODE}" == "analysis" ]]; then
    export netwk="gdas"
    fName="${RUN_MODE}.t.dat"
else
    export netwk="gfs"
    fName="${RUN_MODE}.${RUN_STEP}.t.dat"
fi

cd "${DATA}" || exit 1

# --- PRE-LOOP CLEANUP ---
[[ -f "${fName}" ]] && rm -f "${fName}"
touch "${fName}"

# --- Staging Loop ---
idate=$sdate
n_expected=0
while [[ "${idate}" -le "${edate}" ]]; do
    
    echo "Processing date: ${idate}"

    # Call the staging script (Sequential execution)
    "${USHrtofs}/${RUN}_atmforcing_stage.sh" "${idate}"

    err=$?
    if [ $err -ne 0 ]; then
        echo "FATAL ERROR: Staging failed for ${idate}"
        exit $err
    fi

    ((n_expected++))
    idate=$("${NDATE}" "${intvl}" "${idate}")
done

# --- FINAL VERIFICATION ---
# Count actual lines in the record file
n_actual=$(wc -l < "${fName}")

echo "Verification: Expected ${n_expected} lines, found ${n_actual} lines in ${fName}."

if [[ "${n_actual}" -ne "${n_expected}" ]]; then
    echo "FATAL ERROR: Line count mismatch in ${fName}!"
    echo "This indicates one or more time steps failed to log correctly."
    exit 3
fi

# -------------------------------------------------------------------
# FUTURE STEPS (Phase 2): WGRIB -> nc -> CDEPS
# WGRIB -> nc
# nc prep for CDEPS, incl any checks
# concatenate and done.
# -------------------------------------------------------------------

msg="${RUN}_atmforcing.sh HAS ENDED NORMALLY ON $(hostname) at $(date)"
postmsg "$msg"
