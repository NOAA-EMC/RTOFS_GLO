#!/bin/sh
set -xa
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_ncoda_inc.sh                               #
# Script description:                                                         #
#                                                                             #
# Author:        Dan Iredell     Org: NP23         Date: 2020-07-30           #
#                                                                             #
# Abstract: Remap an archive file to an NCODA analysis, new layer depths.     #
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             #
# Script history log:                                                         #
# 2020-07-30  Dan Iredell                                                     #
# 2023-02-08  Dmitry Dukhovskoy modified for updated ncoda_archv_lyrinc       #
# 2026-01-12  Zulema Garraffo modified for MOM
#                                                                             #
###############################################################################

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
#inputgrid=0.08, change to 0p08
reg=GLB
DEPTH_FILE=${FIXrtofs}/depth_${reg}${inputgrid}_09m11ob2_mom6.nc 
IDM=$(ncdump -h ${DEPTH_FILE} | grep 'nx =' | cut -d' ' -f3)
JDM=$(ncdump -h ${DEPTH_FILE} | grep 'ny =' | cut -d' ' -f3)
KDM=41 # or get from restart file?
SIZN="${IDM}x${JDM}"

ln -f -s ${FIXrtofs}/depth_${reg}.${inputgrid}_09m11ob2_mom6.nc .
ln -f -s ${FIXrtofs}/regional.mom6.nc .

# 2. link to ncoda hycom var restart files
 
typet=seatmp_lyr_1o${SIZN}
types=salint_lyr_1o${SIZN}
typeu=uucurr_lyr_1o${SIZN}
typev=vvcurr_lyr_1o${SIZN}
typethbg=lyrthk_lyr_1o${SIZN}

export salininc=salint_${dtg}_analinc
export tempinc=seatmp_${dtg}_analinc
export uvelinc=uvel_${dtg}_analinc  # u vel increm on p-grid
export vvelinc=vvel_${dtg}_analinc
export lyrthbg=lyrthk_${dtgm1}_fcstfld

#names can be changed. Names in INPUT will be MOM.inc.TSzh.nc, MOM.inc.UV.nc 
export TShar=MOM.res_Y${jday:0:4}_D${jday:4:3}_S00000_inc.TSzh.nc
export UVar=MOM.res_Y${jday:0:4}_D${jday:4:3}_S00000_inc.TSzh.nc

# Check for the existence of analysis increment files
# These are needed to create the HYCOM incremental update file
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
# Background state layer thickness
if [ -e $COMIN/ncoda/hycom_var/restart/${typethbg}_${dtgm1}_0024_fcstfld ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typethbg}_${dtgm1}_0024_fcstfld ./${lyrthbg}
else
   msg="$COMIN/ncoda/hycom_var/restart/${typethbg}_${dtgm1}_0024_fcstfld is missing"
   err_exit $msg
fi

# Link the MOM6 template restart files 
if [ -e $COMINm1/RESTART/${dtg:0:8}_000000.MOM.res.nc]; then
   ln -sf  $COMINm1/RESTART/${dtg:0:8}_000000.MOM.res.nc MOM.res.nc
else
   msg="$COMINm1/${dtg:0:8}_000000.MOM.res.nc is missing"
   err_exit $msg
fi
if [ -e $COMINm1/RESTART/${dtg:0:8}_000000.MOM.res_1.nc]; then
   ln -sf  $COMINm1/RESTART/${dtg:0:8}_000000.MOM_1.res.nc MOM.res.nc
else
   msg="$COMINm1/${dtg:0:8}_000000.MOM_1.res.nc is missing"
   err_exit $msg
fi
if [ -e $COMINm1/RESTART/${dtg:0:8}_000000.MOM_3.res.nc]; then
   ln -sf  $COMINm1/RESTART/${dtg:0:8}_000000.MOM_3.res.nc MOM.res.nc
else
   msg="$COMINm1/${dtg:0:8}_000000.MOM_3.res.nc is missing"
   err_exit $msg
fi
if [ -e $COMINm1/RESTART/${dtg:0:8}_000000.MOM_4.res.nc]; then
   ln -sf  $COMINm1/RESTART/${dtg:0:8}_000000.MOM_4.res.nc MOM.res.nc
else
   msg="$COMINm1/${dtg:0:8}_000000.MOM_4.res.nc is missing"
   err_exit $msg
fi


# copy modify input file with local vars
cp ${PARMrtofs}/${RUN}_${modID}.ncoda_inc2mom6nc.input ./ncoda_inc2mom6nc.input
sed -i -e "s/&TShincname/$TShar/" \
       -e "s/&UVincname/$UVar/" \
       -e "s/&IDM/$IDM/g" \
       -e "s/&JDM/$JDM/g" \
       -e "s/&KDM/$KDM/g" \
       -e "s/&seatmpinc/${tempinc}/g" \
       -e "s/&salintinc/${salininc}/g" \
       -e "s/&uvelinc/${uvelinc}/g" \
       -e "s/&vvelinc/${vvelinc}/g" \
       -e "s/&lyrthknam/${lyrthbg}" \

$EXECrtofs/rtofs_ncodaz_inc2mom6nc_glb_lyr.x < ncoda_inc2mom6nc_lyr.input >> $pgmout
err=$?; export err ; err_chk
echo " error from rtofs_ncodaz_inc2mom6nc_glb_lyr=",$err

msg="THE RTOFS_GLO_NCODA_INC JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"

#
# calculate increment file for assimilation
#

