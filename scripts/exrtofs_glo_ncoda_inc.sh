#!/bin/sh
set -xa

# Script name:        exrtofs_glo_ncoda_inc.sh
# Script description: Creates ocean (sea ice) increment
#                     (restart modification) files used in UFS integration.

export PS4='$SECONDS + '

cd $DATA

msg="RTOFS_GLO_NCODA_INC JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

# --------------------------------------------------------------------------- #

# 1. Set up times link to fix and archive inputs

incup_hours=6

dtg=${PDYm1}00
dtgm1=$($EXECrtofs/rtofs_dtg $dtg -d -1)
dtgm2=$($EXECrtofs/rtofs_dtg $dtg -h -$incup_hours)
jday=$($EXECrtofs/rtofs_dtg -f %Y%j $dtg)
fcst='0024'
echo dtg12 $dtg $dtgm1 $dtgm2

mode=incup
inputgrid=0p08
BLKDATA_FILE=${PARMrtofs}/${RUN}_${modID}.res_${inputgrid}.${mode}.blkdat.input
IDM=$(cat ${BLKDATA_FILE} | grep idm | cut -d' ' -f1 | tr -d '[:space:]')
JDM=$(cat ${BLKDATA_FILE} | grep jdm | cut -d' ' -f1 | tr -d '[:space:]')
KDM=$(cat ${BLKDATA_FILE} | grep kdm | cut -d' ' -f1 | tr -d '[:space:]')
KDM=$(awk '/kdm/ {print $1}' ${BLKDATA_FILE})
SIZN="${IDM}x${JDM}"

reg=GLB
DEPTH_FILE=${FIXrtofs}/depth_${reg}.${inputgrid}_09m11ob2_mom6.nc
#ln -f -s ${DEPTH_FILE} depth_GLBb0.08_09m11ob2_mom6.nc
#ln -f -s ${FIXrtofs}/regional.mom6.nc .

# 2. Link to NCODA output files
 
typet=seatmp_lyr_1o${SIZN}
types=salint_lyr_1o${SIZN}
typeu=uucurr_lyr_1o${SIZN}
typev=vvcurr_lyr_1o${SIZN}
typec=icecov_sfc_1o${SIZN}
typethbg=lyrthk_lyr_1o${SIZN}

export salininc=salint_${dtg}_analinc
export tempinc=seatmp_${dtg}_analinc
export uvelinc=uvel_${dtg}_analinc  # u vel increm on p-grid
export vvelinc=vvel_${dtg}_analinc
export icefld=icecov_${dtg}_analfld
export lyrthbg=lyrthk_${dtgm1}_fcstfld

export TShinc=MOM.res_Y${jday:0:4}_D${jday:4:3}_S00000_inc.TSzh.nc
export UVinc=MOM.res_Y${jday:0:4}_D${jday:4:3}_S00000_inc.UV.nc

# Check for the existence of NCODA output files
# Temperature
if [ -e $COMIN/ncoda/hycom_var/restart/${typet}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typet}_${dtg}_0000_analinc ./${tempinc}
else
   msg="$COMIN/ncoda/hycom_var/restart/${typet}_${dtg}_0000_analinc is missing"
   err_exit $msg
fi
# Salinity
if [ -e $COMIN/ncoda/hycom_var/restart/${types}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${types}_${dtg}_0000_analinc ./${salininc}
else
   msg="$COMIN/ncoda/hycom_var/restart/${types}_${dtg}_0000_analinc is missing"
   err_exit $msg
fi
# Current - U-component
if [ -e $COMIN/ncoda/hycom_var/restart/${typeu}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typeu}_${dtg}_0000_analinc ./${uvelinc}
else
   msg="$COMIN/ncoda/hycom_var/restart/${typeu}_${dtg}_0000_analinc is missing"
   err_exit $msg
fi
# Current - V-component
if [ -e $COMIN/ncoda/hycom_var/restart/${typev}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typev}_${dtg}_0000_analinc ./${vvelinc}
else
   msg="$COMIN/ncoda/hycom_var/restart/${typev}_${dtg}_0000_analinc is missing"
   err_exit $msg
fi
# Ice Coverage
if [ -e $COMIN/ncoda/hycom_var/restart/${typec}_${dtg}_0000_analfld ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typec}_${dtg}_0000_analfld ./${icefld}
else
   msg="$COMIN/ncoda/hycom_var/restart/${typec}_${dtg}_0000_analfld is missing"
   err_exit $msg
fi
# Background state layer thickness
if [ -e $COMIN/ncoda/hycom_var/restart/${typethbg}_${dtgm1}_0024_fcstfld ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typethbg}_${dtgm1}_0024_fcstfld ./${lyrthbg}
else
   msg="$COMIN/ncoda/hycom_var/restart/${typethbg}_${dtgm1}_0024_fcstfld is missing"
   err_exit $msg
fi

# Link the MOM6 template restart files 
# Define the list of MOM6 template restart files you need to link
mom6_restart_files="MOM.res.nc MOM.res_1.nc MOM.res_3.nc MOM.res_4.nc"

# Loop through each file
for f in ${mom6_restart_files}; do
    # Construct the full source path
    src="$COMIN/RESTART/${dtg:0:8}.000000.$f"

    if [ -e "$src" ]; then
        ln -sf "$src" "$f"
    else
        msg="$src is missing"
        err_exit "$msg"
    fi
done

# Copy and modify input file with local vars
cp ${PARMrtofs}/${RUN}_${modID}_ncoda_inc2mom6nc_lyr.input ./ncoda_inc2mom6nc_lyr.input
sed -i -e "s/&TShincname/$TShinc/g" \
       -e "s/&UVincname/$UVinc/g" \
       -e "s/&IDM/$IDM/g" \
       -e "s/&JDM/$JDM/g" \
       -e "s/&KDM/$KDM/g" \
       -e "s/&seatmpinc/${tempinc}/g" \
       -e "s/&salintinc/${salininc}/g" \
       -e "s/&uvelinc/${uvelinc}/g" \
       -e "s/&vvelinc/${vvelinc}/g" \
       -e "s/&lyrthkname/${lyrthbg}/g" ./ncoda_inc2mom6nc_lyr.input 

# 3. Create ocean increment files
$EXECrtofs/rtofs_ncodaz_inc2mom6nc_glb_lyr.x < ncoda_inc2mom6nc_lyr.input >> $pgmout
err=$?; export err ; err_chk
echo " error from rtofs_ncodaz_inc2mom6nc_glb_lyr=",$err
cp $TShinc $COMOUT/rtofs_glo.$TShinc
cp $UVinc $COMOUT/rtofs_glo.$UVinc

# 4. Create sea ice restart modification file
$USHrtofs/rtofs_glo2d_ice.sh ${DEPTH_FILE} ${icefld} icecov ${DATA}
err=$?; export err ; err_chk
echo " error from rtofs_glo2d_ice.sh=",$err

if [ -f "icecov_${dtg}_analfld.nc" ]; then
    mv "icecov_${dtg}_analfld.nc" $COMOUT/sic.nc
else
    echo "WARNING: icecov_${dtg}_analfld.nc not found. Skipping rename."
fi

msg="THE RTOFS_GLO_NCODA_INC JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"
