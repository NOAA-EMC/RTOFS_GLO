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
#                                                                             #
###############################################################################

export PS4='$SECONDS + '

cd $DATA

msg="RTOFS_GLO_NCODA_INC JOB has begun on `hostname` at `date`"
postmsg "$msg"

# --------------------------------------------------------------------------- #

# 1. Set up times link to fix and archive inputs

incup_hours=6

dtg=${PDYm1}00
dtgm1=`$EXECrtofs/rtofs_dtg $dtg -d -1`
dtgm2=`$EXECrtofs/rtofs_dtg $dtg -h -$incup_hours`
hday=`$USHrtofs/rtofs_date_normal2hycom.sh $dtg`
hday2=`echo $hday $incup_hours 24 | awk '{printf("%9.3f", $1-($2/$3))}'`
jday=`$USHutil/date2jday.sh ${dtg:0:8}`
jday2=`$USHutil/date2jday.sh ${dtgm2:0:8}`
archday=${jday:0:4}_${jday:4:3}_${dtg:8:2}
archday2=${jday2:0:4}_${jday2:4:3}_${dtgm2:8:2}

echo dtg12 $dtg $dtgm1 $dtgm2
echo hday $hday $hday2 jday $jday $jday2
echo archday $archday $archday2

mode=incup
BLKDATA_FILE=${PARMrtofs}/${RUN}_${modID}.${inputgrid}.${mode}.blkdat.input
IDM=`cat ${BLKDATA_FILE} | grep idm | cut -d' ' -f1 | tr -d '[:space:]'`
JDM=`cat ${BLKDATA_FILE} | grep jdm | cut -d' ' -f1 | tr -d '[:space:]'`
JDMA=`expr ${JDM} \- 1`
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
 
typet=seatmp_pre_1o${SIZN}
types=salint_pre_1o${SIZN}
typeu=uucurr_pre_1o${SIZN}
typev=vvcurr_pre_1o${SIZN}
typep=lyrprs_pre_1o${SIZN}
typec=icecov_sfc_1o${SIZN}

ln -sf  $COMINm1/rtofs_glo.t00z.n00.archv.a    archv.${archday}.a
ln -sf  $COMINm1/rtofs_glo.t00z.n00.archv.b    archv.${archday}.b

# Check for the existence of analysis increment files
# These are needed to create the HYCOM incremental update file
# Temperature
if [ -e $COMIN/ncoda/hycom_var/restart/${typet}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typet}_${dtg}_0000_analinc ./temp_${dtg}_analinc
else
   echo "WARNING - $COMIN/ncoda/hycom_var/restart/${typet}_${dtg}_0000_analinc missing."
   echo "No Temperature increments created"
fi
# Salinity
if [ -e $COMIN/ncoda/hycom_var/restart/${types}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${types}_${dtg}_0000_analinc ./saln_${dtg}_analinc
else
   echo "WARNING - $COMIN/ncoda/hycom_var/restart/${types}_${dtg}_0000_analinc missing.."
   echo "No Salinity increments created"
fi
# Current - U-component
if [ -e $COMIN/ncoda/hycom_var/restart/${typeu}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typeu}_${dtg}_0000_analinc ./uvel_${dtg}_analinc
else
   echo "WARNING - $COMIN/ncoda/hycom_var/restart/${typeu}_${dtg}_0000_analinc missing."
   echo "No U-Velocity increments created"
fi
# Current - V-component
if [ -e $COMIN/ncoda/hycom_var/restart/${typev}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typev}_${dtg}_0000_analinc ./vvel_${dtg}_analinc
else
   echo "WARNING - $COMIN/ncoda/hycom_var/restart/${typev}_${dtg}_0000_analinc missing."
   echo "No V-Velocity increments created"
fi
# Layer Pressure
if [ -e $COMIN/ncoda/hycom_var/restart/${typep}_${dtg}_0000_analinc ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typep}_${dtg}_0000_analinc ./lypr_${dtg}_analinc
else
   echo "WARNING - $COMIN/ncoda/hycom_var/restart/${typep}_${dtg}_0000_analinc missing."
   echo "No Layer Pressure increments created"
fi
# Ice Coverage
if [ -e $COMIN/ncoda/hycom_var/restart/${typec}_${dtg}_0000_analfld ]; then
   ln -sf  $COMIN/ncoda/hycom_var/restart/${typec}_${dtg}_0000_analfld ./icecov_${dtg}_analfld
else
   echo "WARNING - $COMIN/ncoda/hycom_var/restart/${typec}_${dtg}_0000_analfld missing."
   echo "No ICE COVERAGE increments created"
fi

#create ssmi.r file
rm -f ssmi1.a ssmi2.a ssmi.$dtg.r
$EXECrtofs/rtofs_raw2hycom icecov_${dtg}_analfld $IDM $JDM 999.00 ssmi1.a > ssmi1.b
err=$?; export err ; err_chk
echo " error from rtofs_raw2hycom=",$err
$EXECrtofs/rtofs_hycom_expr ssmi1.a "ONE" $IDM $JDM  0.01 0 ssmi2.a > ssmi2.b
err=$?; export err ; err_chk
echo " error from rtofs_hycom_expr=",$err
$EXECrtofs/rtofs_hycom2raw8 ssmi2.a $IDM $JDM 1 1 $IDM $JDMA ssmi.$dtg.r
err=$?; export err ; err_chk
echo " error from rtofs_hycom2raw8=",$err

ar=archv_1_inc.${archday}
rm -f $ar.[a,b]

# copy modify input file with local vars
cp ${PARMrtofs}/${RUN}_${modID}.ncoda_archv.input ./ncoda_archv.input
sed -i -e "s/&archday/$archday/" -e "s/&archname/$ar/" -e "s/&IDM/$IDM/g" -e "s/&JDM/$JDM/g" -e "s/&dtg/$dtg/g" ./ncoda_archv.input
ln -s ${PARMrtofs}/${RUN}_${modID}.zlevels zi.txt

$EXECrtofs/rtofs_ncoda_archv_inc < ncoda_archv.input
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_archv_inc=",$err

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

