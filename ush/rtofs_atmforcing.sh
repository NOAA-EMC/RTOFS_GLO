#!/bin/sh
set -x

msg="${RUN}_atmforcing.sh has begun on $(hostname) at $(date)"
postmsg "$msg"

if [ $# -lt 3 ] ; then 
  echo USAGE:  ${RUN}_atmforcing.sh start_date end_date interval
  exit 2
fi

sdate=$1
edate=$2
intvl=$3
sdate=$($NDATE -3 $sdate)
edate=$($NDATE 3 $edate)

# -- needed or not ?? --
# Incorporate sea level pressure
export sea_lev_pres=PRMSL
export atmgds=
# --

if [[ "${RUN_MODE}" == "analysis" ]]; then
    export netwk="gdas"  # Nowcast mode
else
    export netwk="gfs"   # Forecast mode
fi

cd "${DATA}" || exit 1

# Default NPROCS to 1 if not set
NPROCS=${NPROCS:-1}

# Clean up existing command files if running in parallel
if [[ ${NPROCS} -gt 1 ]]; then
    rm -f cmdfile_tmp cmdfile.*
fi

idate=$sdate
NTIME=0
while [[ "${idate}" -le "${edate}" ]]; do
    cmd="${USHrtofs}/${RUN}_atmforcing_stage.sh ${idate}"
    
    if [[ ${NPROCS} -eq 1 ]]; then
        # Run sequentially
        ${cmd}
    else
        # Append to command file for parallel processing
        echo "${cmd}" >> cmdfile_tmp
    fi
    
    ((NTIME++))
    idate=$("${NDATE}" "${intvl}" "${idate}")
done

# Parallel execution
if [[ ${NPROCS} -gt 1 && -f cmdfile_tmp ]]; then
    # Split the command file into chunks based on NPROCS
    split -l "${NPROCS}" cmdfile_tmp cmdfile.
    
    for cfile in cmdfile.*; do
        [[ -e "${cfile}" ]] || continue  # Handle case where no files exist
        
        # Pad the command file with sleeps so it matches NPROCS if necessary
        cmdlen=$(wc -l < "${cfile}")
        while [[ ${cmdlen} -lt ${NPROCS} ]]; do
            echo 'sleep 1' >> "${cfile}"
            ((cmdlen++))
        done
        
        # Execute via mpirun
        chmod +x "${cfile}"
        mpirun ./"${cfile}" >> "${pgmout}" 2>errfile
        export err=$?; if [ $err -ne 0 ]; then exit $err; fi
    done
    
    # Cleanup (Optional: uncomment if needed)
    # rm -f cmdfile_tmp cmdfile.*
fi

# -------------------------------
# Add following in this sequence:
# -------------------------------
# WGRIB -> nc
# nc prep for CDEPS, incl any checks
# concatenate and done.
# -------------------------------

msg="${RUN}_atmforcing.sh HAS ENDED NORMALLY ON $(hostname) at $(date)"
postmsg "$msg"
