#!/bin/sh
set -xa

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
hday=$($USHrtofs/rtofs_date_normal2hycom.sh $dtg)
hday2=$(echo $hday $incup_hours 24 | awk '{printf("%9.3f", $1-($2/$3))}')
jday=$($USHutil/date2jday.sh ${dtg:0:8})
jday2=$($USHutil/date2jday.sh ${dtgm2:0:8})
archday=${jday:0:4}_${jday:4:3}_${dtg:8:2}
archday2=${jday2:0:4}_${jday2:4:3}_${dtgm2:8:2}

echo dtg12 $dtg $dtgm1 $dtgm2
echo hday $hday $hday2 jday $jday $jday2
echo archday $archday $archday2

mode=incup
BLKDATA_FILE=${PARMrtofs}/${RUN}_${modID}.${inputgrid}.${mode}.blkdat.input
IDM=$(cat ${BLKDATA_FILE} | grep idm | cut -d' ' -f1 | tr -d '[:space:]')
JDM=$(cat ${BLKDATA_FILE} | grep jdm | cut -d' ' -f1 | tr -d '[:space:]')
JDMA=$(expr ${JDM} \- 1)
SIZN="${IDM}x${JDM}"

ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.a  regional.grid.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.b  regional.grid.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.a regional.depth.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.b regional.depth.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.iso.sigma.a      iso.sigma.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.iso.sigma.b      iso.sigma.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.tbaric.a         tbaric.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.tbaric.b         tbaric.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_ssh.a      relax.ssh.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_ssh.b      relax.ssh.b

# 2. link to ncoda hycom var restart files
 
typet=seatmp_lyr_1o${SIZN}
types=salint_lyr_1o${SIZN}
typeu=uucurr_lyr_1o${SIZN}
typev=vvcurr_lyr_1o${SIZN}
typep=lyrprs_lyr_1o${SIZN}

ln -sf  $COMINm1/rtofs_glo.t00z.n00.archv.a    archv.${archday}.a
ln -sf  $COMINm1/rtofs_glo.t00z.n00.archv.b    archv.${archday}.b

export lyrprinc=lyrprs_${dtg}_analinc
export salininc=salint_${dtg}_analinc
export stempinc=seatmp_${dtg}_analinc
export upvelinc=upvel_${dtg}_analinc  # u vel increm on p-grid
export vpvelinc=vpvel_${dtg}_analinc

# Check for the existence of analysis increment files
# These are needed to create the HYCOM incremental update file
# Temperature
if [ -e $COMIN/ncoda/hycom_var/restart/${typet}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typet}_${dtg}_0000_analinc ./${stempinc}
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
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typeu}_${dtg}_0000_analinc ./${upvelinc}
else
   msg="$COMIN/ncoda/hycom_var/restart/${typeu}_${dtg}_0000_analinc is missing"
   err_exit $msg
fi
# Current - V-component
if [ -e $COMIN/ncoda/hycom_var/restart/${typev}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typev}_${dtg}_0000_analinc ./${vpvelinc}
else
   msg="$COMIN/ncoda/hycom_var/restart/${typev}_${dtg}_0000_analinc is missing"
   err_exit $msg
fi
# Layer Pressure
if [ -e $COMIN/ncoda/hycom_var/restart/${typep}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typep}_${dtg}_0000_analinc ./${lyrprinc}
else
   msg="$COMIN/ncoda/hycom_var/restart/${typep}_${dtg}_0000_analinc is missing"
   err_exit $msg
fi

# Create sea ice concentration file: sic.nc
$USHrtofs/rtofs_glo_ice_update.sh

ar=archv_1_inc.${archday}
rm -f $ar.[a,b]

# copy modify input file with local vars
cp ${PARMrtofs}/${RUN}_${modID}.ncoda_archv_lyr.input ./ncoda_archv.input
sed -i -e "s/&archday/$archday/" \
       -e "s/&archname/$ar/" \
       -e "s/&IDM/$IDM/g" \
       -e "s/&JDM/$JDM/g" \
       -e "s/&dtg/$dtg/g" \
       -e "s/&lyrprsinc/${lyrprinc}/g" \
       -e "s/&salintinc/${salininc}/g" \
       -e "s/&seatmpinc/${stempinc}/g" \
       -e "s/&upvelinc/${upvelinc}/g" \
       -e "s/&vpvelinc/${vpvelinc}/g" ./ncoda_archv.input
#ln -s ${PARMrtofs}/${RUN}_${modID}.zlevels zi.txt

$EXECrtofs/rtofs_ncoda_archv_lyrinc < ncoda_archv.input >> $pgmout
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_archv_lyrinc=",$err

#
# calculate increment file for assimilation
#
/bin/rm -f archd_1.${dtg}.a archd_1.${dtg}.b
cat << E-o-D > hycom_diff.input
41    'kk    ' = number of layers involved
1.0    'nscale' = scale  difference by 1.0/nscale
0    'nbox  ' = smooth difference over a 2*nbox+1 square
archv_1_inc.${archday}.a
archv.${archday}.a
archd_1.${dtg}
Analysis - Background
17T Sigma2*; GDEM4.2; KPP; SeaWiFS chl; HYCOM+CICE; A=20;Smag=.05;
Z(7):1-7,Z(16):8,Z(2):10-16,Z(13):dp00/f/x=36/1.18/262;Z(3):400-600m; GPCPsnow
sigma:84-14m; depth_GLBb0.08_11; apply offlux to CICE; 2.2.99DHi-900
E-o-D
date
$EXECrtofs/rtofs_hycom_diff < hycom_diff.input
err=$?; export err ; err_chk
echo " error from rtofs_hycom_diff=",$err

#
# change time on archd*.b file
#

sed -e "s/${hday}/${hday2}/g" archd_1.${dtg}.b > archd_1.${dtg}.b2

cp archd_1.$dtg.a  $COMOUT/rtofs_glo.incupd.$archday2.a
cp archd_1.$dtg.b2 $COMOUT/rtofs_glo.incupd.$archday2.b
cp archv_1_inc.$archday.a $COMOUT/rtofs_glo.archv_1_inc.$archday.a
cp archv_1_inc.$archday.b $COMOUT/rtofs_glo.archv_1_inc.$archday.b
cp ssmi.$dtg.r     $COMOUT/rtofs_glo.ssmi.$dtg.r

msg="THE RTOFS_GLO_NCODA_INC JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"

