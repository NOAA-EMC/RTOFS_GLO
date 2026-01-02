#!/bin/sh
set -xa

export PS4='$SECONDS + '

cd $DATA

msg="$(basename -- "$0") JOB has begun on $(hostname) at $(date)"
postmsg "$msg"
# --------------------------------------------------------------------------- #

# 1. Set up inputs for run

dtg=${PDYm1}00

echo $dtg

IDM=$(cat ${BLKDATA_FILE} | grep idm | cut -d' ' -f1 | tr -d '[:space:]')
JDM=$(cat ${BLKDATA_FILE} | grep jdm | cut -d' ' -f1 | tr -d '[:space:]')
SIZN="${IDM}x${JDM}"

# 2. Link to ncoda var ice coverage restart file

typec=icecov_sfc_1o${SIZN}

if [ -e $COMIN/ncoda/hycom_var/restart/${typec}_${dtg}_0000_analfld ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typec}_${dtg}_0000_analfld ./icecov_${dtg}_analfld
else
   msg="$COMIN/ncoda/hycom_var/restart/${typec}_${dtg}_0000_analfld is missing"
   err_exit $msg
fi

# 3. Link in topo files

ln -f -s ${FIXrtofs}/depth_GLBb0.08_09m11ob2_mom6.nc depth_GLBb0.08_09m11ob2_mom6.nc

# 4. Create sic.nc file

$EXECrtofs/rtofs_glo2d_ice.sh depth_GLBb0.08_09m11ob2_mom6.nc icecov_${dtg}_analfld
err=$?; export err ; err_chk
echo " error from rtofs_glo2d_ice.sh=",$err

if [ -f "icecov_${dtg}_analfld.nc" ]; then
    mv "icecov_${dtg}_analfld.nc" sic.nc
else
    echo "WARNING: icecov_${dtg}_analfld.nc not found. Skipping rename."
fi

msg="THE $(basename -- "$0") JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"
