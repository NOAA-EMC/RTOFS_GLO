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
postmsg "$jlogfile" "$msg"

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

cat << E-o-D > ncoda_archv.input
archv.${archday}.a
$ar.a
   0      'intflg' = vertical interpolation flag (0=T&S, 1=th&S)
   0      'isoflg' = preserve isopycnal layer flag (0=n,1=y,2=y&layT,3=y&isoT)
   1      'nddflg' = ncoda density displcment flag (0=no,1=flnm_p,2=flnm_p+T&S)
 000	  'iexpt ' = experiment number x10 (000=from archive file)
   3	  'yrflag' = days in year flag (0=360,  1=366,  2=366J1, 3=actual)
$IDM     'idm   ' = longitudinal array size
$JDM     'jdm   ' = latitudinal  array size
2224     'itest ' = longitudinal test point (optional, default 0)
1449     'jtest ' = latitudinal  test point (optional, default 0)
  41	  'kdm   ' = number of layers
  41      'nhybrd' = number of hybrid levels (0=all isopycnal)
  14      'nsigma' = number of sigma  levels (nhybrd-nsigma z-levels)
   1.00   'dp0k  ' = layer   1 deep    z-level spacing minimum thickness (m)
   1.80   'dp0k  ' = layer   2 deep    z-level spacing minimum thickness (m)
   3.24   'dp0k  ' = layer   3 deep    z-level spacing minimum thickness (m)
   4.68   'dp0k  ' = layer   4 deep    z-level spacing minimum thickness (m)
   4.93   'dp0k  ' = layer   5 deep    z-level spacing minimum thickness (m)
   5.81   'dp0k  ' = layer   6 deep    z-level spacing minimum thickness (m)
   6.87   'dp0k  ' = layer   7 deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer   8 deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer   9 deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer  10 deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer  11 deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer  12 deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer  13 deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer  14 deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer   A deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer   B deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer   C deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer   D deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer   E deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer   F deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer   G deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer   H deep    z-level spacing minimum thickness (m)
   8.00   'dp0k  ' = layer   I deep    z-level spacing minimum thickness (m)
  10.00   'dp0k  ' = layer  15 deep    z-level spacing minimum thickness (m)
  16.40   'dp0k  ' = layer  16 deep    z-level spacing minimum thickness (m)
  35.92   'dp0k  ' = layer  17 deep    z-level spacing minimum thickness (m)
  42.38   'dp0k  ' = layer  18 deep    z-level spacing minimum thickness (m)
  50.02   'dp0k  ' = layer  19 deep    z-level spacing minimum thickness (m)
  59.02   'dp0k  ' = layer  20 deep    z-level spacing minimum thickness (m)
  69.64   'dp0k  ' = layer  21 deep    z-level spacing minimum thickness (m)
  82.18   'dp0k  ' = layer  22 deep    z-level spacing minimum thickness (m)
  96.97   'dp0k  ' = layer  23 deep    z-level spacing minimum thickness (m)
 114.43   'dp0k  ' = layer  24 deep    z-level spacing minimum thickness (m)
 135.02   'dp0k  ' = layer  25 deep    z-level spacing minimum thickness (m)
 159.33   'dp0k  ' = layer  26 deep    z-level spacing minimum thickness (m)
 188.01   'dp0k  ' = layer  27 deep    z-level spacing minimum thickness (m)
 221.84   'dp0k  ' = layer  28 deep    z-level spacing minimum thickness (m)
 261.78   'dp0k  ' = layer  29 deep    z-level spacing minimum thickness (m)
 400.00   'dp0k  ' = layer  30 deep    z-level spacing minimum thickness (m)
 600.00   'dp0k  ' = layer  31 deep    z-level spacing minimum thickness (m)
 600.00   'dp0k  ' = layer  32 deep    z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer   1 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer   2 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer   3 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer   4 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer   5 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer   6 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer   7 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer   8 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer   9 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer  10 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer  11 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer  12 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer  13 shallow z-level spacing minimum thickness (m)
   1.00   'ds0k  ' = layer  14 shallow z-level spacing minimum thickness (m)
   1.0	  'dp00i ' = deep iso-pycnal spacing minimum thickness (m)
  83.0    'isotop' = shallowest depth for isopycnal layers (m), <0 from file
   0.03   'deniso' = isopycnal if layer is within deniso of target density
   0.5    'thnthk' = minimum ratio of thin to thick isppycnal layers (0 to 1)
  34.0	  'thbase' = reference density (sigma units)
   1	  'vsigma' = spacially varying isopycnal target densities (0=F,1=T)
  17.00   'sigma ' = layer  1 isopycnal target density (sigma units)
  18.00   'sigma ' = layer  2 isopycnal target density (sigma units)
  19.00   'sigma ' = layer  3 isopycnal target density (sigma units)
  20.00   'sigma ' = layer  4 isopycnal target density (sigma units)
  21.00   'sigma ' = layer  5 isopycnal target density (sigma units)
  22.00   'sigma ' = layer  6 isopycnal target density (sigma units)
  23.00   'sigma ' = layer  7 isopycnal target density (sigma units)
  24.00   'sigma ' = layer  8 isopycnal target density (sigma units)
  25.00   'sigma ' = layer  9 isopycnal target density (sigma units)
  26.00   'sigma ' = layer 10 isopycnal target density (sigma units)
  27.00   'sigma ' = layer 11 isopycnal target density (sigma units)
  28.00   'sigma ' = layer 12 isopycnal target density (sigma units)
  29.00   'sigma ' = layer 13 isopycnal target density (sigma units)
  29.90   'sigma ' = layer 14 isopycnal target density (sigma units)
  30.65   'sigma ' = layer  A isopycnal target density (sigma units)
  31.35   'sigma ' = layer  B isopycnal target density (sigma units)
  31.95   'sigma ' = layer  C isopycnal target density (sigma units)
  32.55   'sigma ' = layer  D isopycnal target density (sigma units)
  33.15   'sigma ' = layer  E isopycnal target density (sigma units)
  33.75   'sigma ' = layer  F isopycnal target density (sigma units)
  34.30   'sigma ' = layer  G isopycnal target density (sigma units)
  34.80   'sigma ' = layer  H isopycnal target density (sigma units)
  35.20   'sigma ' = layer  I isopycnal target density (sigma units)
  35.50   'sigma ' = layer 15 isopycnal target density (sigma units)
  35.80   'sigma ' = layer 16 isopycnal target density (sigma units)
  36.04   'sigma ' = layer 17 isopycnal target density (sigma units)
  36.20   'sigma ' = layer 18 isopycnal target density (sigma units)
  36.38   'sigma ' = layer 19 isopycnal target density (sigma units)
  36.52   'sigma ' = layer 20 isopycnal target density (sigma units)
  36.62   'sigma ' = layer 21 isopycnal target density (sigma units)
  36.70   'sigma ' = layer 22 isopycnal target density (sigma units)
  36.77   'sigma ' = layer 23 isopycnal target density (sigma units)
  36.83   'sigma ' = layer 24 isopycnal target density (sigma units)
  36.89   'sigma ' = layer 25 isopycnal target density (sigma units)
  36.97   'sigma ' = layer 26 isopycnal target density (sigma units)
  37.02   'sigma ' = layer 27 isopycnal target density (sigma units)
  37.06   'sigma ' = layer 28 isopycnal target density (sigma units)
  37.10   'sigma ' = layer 29 isopycnal target density (sigma units)
  37.17   'sigma ' = layer 30 isopycnal target density (sigma units)
  37.30   'sigma ' = layer 31 isopycnal target density (sigma units)
  37.42   'sigma ' = layer 32 isopycnal target density (sigma units)
   0.5    'hicemn' = minimum ice thickness (m)
temp_${dtg}_analinc
saln_${dtg}_analinc
uvel_${dtg}_analinc
vvel_${dtg}_analinc
lypr_${dtg}_analinc
regional.depth.a
NONE      
NONE
1  'i1stn ' = i-origin of ncoda subregion
1  'j1stn ' = j-origin of ncoda subregion
${IDM}  'idmn  ' = i-extent of ncoda subregion (<=idm; 0 implies idm)
${JDM}  'jdmn  ' = j-extent of ncoda subregion (<=jdm; 0 implies jdm)
41 'kncoda' = number   of ncoda levels
NONE
E-o-D
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
cp ssmi.$dtg.r     $COMOUT/rtofs_glo.ssmi.$dtg.r

