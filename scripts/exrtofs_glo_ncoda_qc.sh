#!/bin/sh
set -xa
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_ncoda_qc.sh                                #
# Script description:                                                         #
#                                                                             #
# Author:        Dan Iredell     Org: NP23         Date: 2020-07-30           #
#                                                                             #
# Abstract: This script runs the ncoda qc step                                #
#                                                                             #
# Sub-scripts called:                                                         #
#    rtofs_ncoda_prep_ice.sh
#    rtofs_ncoda_ice_qc.sh
#    rtofs_ncoda_prep_sfc_sfcr_prof.sh
#    rtofs_ncoda_profile_qc.sh
#    rtofs_ncoda_sfcobs_qc.sh
#    rtofs_ncoda_sss_qc.sh
#    rtofs_ncoda_ssh_qc.sh
#    rtofs_ncoda_amsr_qc.sh
#    rtofs_ncoda_goes_qc.sh
#    rtofs_ncoda_himawari_qc.sh
#    rtofs_ncoda_jpss_qc.sh
#    rtofs_ncoda_metop_qc.sh 
#    rtofs_ncoda_npp_qc.sh 
#    rtofs_ncodaqc2com.sh 
#                                                                             #
# Script history log:                                                         #
# 2020-07-30  Dan Iredell                                                     #
#                                                                             #
###############################################################################

export PS4='$SECONDS + '

cd $DATA

#defaults (hera)
masscfpcopy=0

msg="RTOFS_GLO_NCODA_QC JOB has begun on `hostname` at `date`"
postmsg "$msg"
# --------------------------------------------------------------------------- #

# 1.a Populate DATA/ocnqc with QC files from COMINm1/ncoda/ocnqc
echo timecheck RTOFS_GLO_NCODA_QC start get at `date`

mkdir -p $DATA/ocnqc
mkdir -p $DATA/ocnqc/incoming

# if there is no qc data, then skip this step
if test -e $COMINm1/ncoda/ocnqc/
then

cp -p -f $COMINm1/ncoda/ocnqc/incoming/*control $DATA/ocnqc/incoming
rm -f cmdfile.cpin
for dtyp in amsr goes himawari ice metop profile sfc ssh sss velocity viirs; do
  mkdir -p $DATA/ocnqc/$dtyp
  if compgen -G "$COMINm1/ncoda/ocnqc/$dtyp/*" > /dev/null
  then
    echo "cp -p -f $COMINm1/ncoda/ocnqc/$dtyp/* $DATA/ocnqc/$dtyp" >> cmdfile.cpin
  fi
done

chmod +x cmdfile.cpin
if [ $masscfpcopy -eq 1 ]
then
  echo mpirun cfp ./cmdfile.cpin > cpin.out
  mpirun cfp ./cmdfile.cpin >> cpin.out
else
  sh ./cmdfile.cpin
fi
err=$? ; export err ; err_chk
date

fi

# 1.b link in var restart files from COMINm1
for v in glbl_var hycom_var nhem_var shem_var;do
   if test -e $COMINm1/ncoda/$v
   then
      ln -sf $COMINm1/ncoda/$v $DATA/
   fi
done

# 1.c link in topo files
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.a  ${DATA}/regional.grid.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.b  ${DATA}/regional.grid.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.a ${DATA}/regional.depth.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.b ${DATA}/regional.depth.b

echo timecheck RTOFS_GLO_NCODA_QC finish get at `date`

# 2. Combine pre-qc and qc into one stream for ice and surface_obs/profile
echo timecheck RTOFS_GLO_NCODA_QC start qc at `date`

# combine ice proccessing into one stream
echo "#!/bin/ksh" > runiceqc.sh
echo "$USHrtofs/rtofs_ncoda_prep_ice.sh" >> runiceqc.sh
echo "$USHrtofs/rtofs_ncoda_ice_qc.sh" >> runiceqc.sh
chmod +x runiceqc.sh

# combine surface obs and profiles into one stream
echo "#!/bin/ksh" > runsurf.sh
echo "$USHrtofs/rtofs_ncoda_prep_sfc_sfcr_prof.sh" >> runsurf.sh
echo "$USHrtofs/rtofs_ncoda_profile_qc.sh" >> runsurf.sh
echo "$USHrtofs/rtofs_ncoda_sfcobs_qc.sh" >> runsurf.sh
echo "$USHrtofs/rtofs_ncoda_sss_qc.sh" >> runsurf.sh
echo "$USHrtofs/rtofs_ncoda_vel_qc.sh" >> runsurf.sh
chmod +x runsurf.sh

# 3. Put all scripts into command file for cfp

date
echo "$USHrtofs/rtofs_ncoda_ssh_qc.sh > ssh.qc.out 2>&1" > cmdfile.qc
echo "./runsurf.sh > surf.qc.out 2>&1" >> cmdfile.qc
echo "./runiceqc.sh > ice.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_npp_qc.sh > npp.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_jpss_qc.sh > jpss.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_metop_qc.sh > metop.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_himawari_qc.sh > himawari.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_goes_qc.sh > goes.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_amsr_qc.sh > amsr.qc.out 2>&1" >> cmdfile.qc
# sss and vel moved to runsurf
#echo "$USHrtofs/rtofs_ncoda_sss_qc.sh > sss.qc.out 2>&1" >> cmdfile.qc
#echo "$USHrtofs/rtofs_ncoda_vel_qc.sh > vel.qc.out 2>&1" >> cmdfile.qc

chmod +x cmdfile.qc
echo mpirun cfp ./cmdfile.qc > qc.out
mpirun cfp ./cmdfile.qc >> qc.out
err=$? ; export err ; err_chk
date
echo timecheck RTOFS_GLO_NCODA_QC finish qc at `date`

# 4. Run data alarm (counts)

export OCN_DATA_DIR=$DATA/ocnqc
echo "NCODA DATA ALARM"
#NCODA alarm
$EXECrtofs/rtofs_ncoda_alarm ${PDYm1}00
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_alarm=",$err
mkdir $DATA/logs/alarm
cp -p  fort.61 $DATA/logs/alarm/ncoda_alarm.counts.${PDYm1}00.out
cp -p  fort.62 $DATA/logs/alarm/ncoda_alarm.qc.${PDYm1}00.out
cp -p  fort.63 $DATA/logs/alarm/ncoda_alarm.qc_lvl.${PDYm1}00.out

# 5. Copy last 15 days of qc data back to COMOUT/ncoda
echo timecheck RTOFS_GLO_NCODA_QC start put at `date`

mkdir -p $COMOUT/ncoda/ocnqc
mkdir -p $COMOUT/ncoda/ocnqc/incoming

cp -p -f $DATA/ocnqc/incoming/*obs_control $COMOUT/ncoda/ocnqc/incoming

for typ in `ls $DATA/ocnqc/incoming/*obs_control`; do
  fnam=`basename $typ`
  cp -p $typ ${COMOUT}/${RUN}_${modID}.ncodaqc.t${cyc}z.$fnam
done

rm -f cmdfile.cpout
for dtyp in amsr goes himawari ice metop profile sfc ssh sss velocity viirs; do
  echo "$USHrtofs/rtofs_ncodaqc2com.sh $dtyp" >> cmdfile.cpout
done

chmod +x cmdfile.cpout
if [ $masscfpcopy -eq 1 ]
then
  echo mpirun cfp ./cmdfile.cpout > cpout.out
  mpirun cfp ./cmdfile.cpout >> cpout.out
else
  sh ./cmdfile.cpout
fi
err=$? ; export err ; err_chk
date
echo timecheck RTOFS_GLO_NCODA_QC finish put at `date`

cd $DATA/logs
for dtyp in $(ls $DATA/logs);do
  mkdir -p $COMOUT/ncoda/logs/$dtyp
  cp -p -f $dtyp/*.${PDYm1}00.* $COMOUT/ncoda/logs/$dtyp 
done

for dtyp in amsr goes himawari ice jpss metop npp ssh sss surf ; do
  cat $DATA/${dtyp}.qc.out >> $DATA/$pgmout
done

# save dumps
mkdir -p $COMOUT/dump
cp -p $DATA/dump/* $COMOUT/dump
cp -p $DATA/ice_nc/l2out.* $DATA/ice_nc/*.out $COMOUT/dump

#################################################
msg="THE RTOFS_GLO_NCODA_QC JOB HAS ENDED NORMALLY on `hostname` at `date`"
postmsg "$msg"

################## END OF SCRIPT #######################

