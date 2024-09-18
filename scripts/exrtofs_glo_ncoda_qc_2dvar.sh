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
#    rtofs_ncoda_msg_qc.sh 
#    rtofs_ncoda_npp_qc.sh 
#    rtofs_ncodaqc2com.sh 
#                                                                             #
# Script history log:                                                         #
# 2020-07-30  Dan Iredell                                                     #
#                                                                             #
###############################################################################

export PS4='$SECONDS + '

cd $DATA

msg="RTOFS_GLO_NCODA_QC_2dvar JOB has begun on $(hostname) at $(date)"
postmsg "$msg"
# --------------------------------------------------------------------------- #

# 1.a Populate DATA/ocnqc with QC files from COMprev/2dvar
echo timecheck RTOFS_GLO_NCODA_QC start get at $(date)

# 1.b link in glbl_var restart files from COMprev

# 1. link in topo files
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.a  ${DATA}/regional.grid.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.b  ${DATA}/regional.grid.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.a ${DATA}/regional.depth.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.b ${DATA}/regional.depth.b

# 2. Combine pre-qc and qc into one stream for ice and surface_obs/profile
echo timecheck RTOFS_GLO_NCODA_QC start qc at $(date)

# combine ice proccessing into one stream
#####################################echo "#!/bin/ksh" > runiceqc.sh
#####################################echo "$USHrtofs/rtofs_ncoda_prep_ice.sh" >> runiceqc.sh
#####################################echo "$USHrtofs/rtofs_ncoda_ice_qc.sh" >> runiceqc.sh
#####################################chmod +x runiceqc.sh

# combine surface obs and profiles into one stream
echo "#!/bin/ksh" > runsurf.sh
echo "$USHrtofs/rtofs_ncoda_prep_sfc_sfcr_prof_2dvar.sh $PDY$cyc" >> runsurf.sh
#####################################echo "$USHrtofs/rtofs_ncoda_profile_qc.sh" >> runsurf.sh
echo "$USHrtofs/rtofs_ncoda_sfcobs_qc_2dvar.sh" >> runsurf.sh
echo "$USHrtofs/rtofs_ncoda_sss_qc_2dvar.sh" >> runsurf.sh
#####################################echo "$USHrtofs/rtofs_ncoda_vel_qc.sh" >> runsurf.sh
chmod +x runsurf.sh

# 3. Put all scripts into command file for cfp

date
rm -f cmdfile.qc
#####################################echo "$USHrtofs/rtofs_ncoda_ssh_qc.sh > ssh.qc.out 2>&1" > cmdfile.qc
echo "./runsurf.sh > surf.qc.out 2>&1" >> cmdfile.qc
#####################################echo "./runiceqc.sh > ice.qc.out 2>&1" >> cmdfile.qc
#####################################echo "$USHrtofs/rtofs_ncoda_npp_qc.sh > npp.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_jpss_qc_2dvar.sh > jpss.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_metop_qc_2dvar.sh > metop.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_himawari_qc_2dvar.sh > himawari.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_goes_qc_2dvar.sh > goes.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_amsr_qc_2dvar.sh > amsr.qc.out 2>&1" >> cmdfile.qc
echo "$USHrtofs/rtofs_ncoda_msg_qc_2dvar.sh > msg.qc.out 2>&1" >> cmdfile.qc
# sss and vel moved to runsurf
#echo "$USHrtofs/rtofs_ncoda_sss_qc.sh > sss.qc.out 2>&1" >> cmdfile.qc
#echo "$USHrtofs/rtofs_ncoda_vel_qc.sh > vel.qc.out 2>&1" >> cmdfile.qc

mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.qc
err=$? ; export err ; err_chk
date

# 4. Run data alarm (counts)

echo timecheck RTOFS_GLO_NCODA_QC start alarm at $(date)
export OCN_DATA_DIR=$DATA/ocnqc
echo "NCODA DATA ALARM"
#NCODA alarm
$EXECrtofs/rtofs_ncoda_alarm ${PDYm1}00
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_alarm=",$err
mkdir $DATA/logs/alarm
cp -p  fort.61 $DATA/logs/alarm/ncoda_alarm.counts.${PDY}${cyc}.out
cp -p  fort.62 $DATA/logs/alarm/ncoda_alarm.qc.${PDY}${cyc}.out
cp -p  fort.63 $DATA/logs/alarm/ncoda_alarm.qc_lvl.${PDY}${cyc}.out

# 5. Copy last 15 days of qc data back to COMOUT/2dvar
echo timecheck RTOFS_GLO_NCODA_QC start put at $(date)

mkdir -p $COMOUT/2dvar/ocnqc${cyc}
mkdir -p $COMOUT/2dvar/ocnqc${cyc}/incoming

# copy incoming data to COM
cp -p -f $DATA/ocnqc/incoming/*obs_control $COMOUT/2dvar/ocnqc${cyc}/incoming
#for typ in $(ls $DATA/ocnqc/incoming/*obs_control); do
#  fnam=$(basename $typ)
#  cp -p $typ ${COMOUT}/${RUN}_${modID}.ncodaqc.t${cyc}z.$fnam
#done

# copy ocnqc data to COM
rm -f cmdfile.cpout
for dtyp in $(ls $DATA/ocnqc); do
  if [ $dtyp != incoming ]
  then
    echo "$USHrtofs/rtofs_2dvarqc2com.sh $dtyp > $dtyp.cpout.out" >> cmdfile.cpout
  fi 
done

chmod +x cmdfile.cpout
mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpout > cpout.out
err=$? ; export err ; err_chk
date
echo timecheck RTOFS_GLO_NCODA_QC finish put at $(date)

# copy all the fortran log files from the qc threads to 2dvar/logs
cd $DATA/logs
for dtyp in $(ls $DATA/logs);do
  mkdir -p $COMOUT/2dvar/logs${cyc}/$dtyp
  cp -p -f $dtyp/*.${PDY}${cyc}.* $COMOUT/2dvar/logs${cyc}/$dtyp
done

# copy all the script log files from the qc threads to pgmout
for dtyp in $(ls $DATA/*.qc.out); do
  cat ${dtyp} >> $DATA/$pgmout
done

# save dumps
mkdir -p $COMOUT/dump$cyc
cp -p $DATA/dump/* $COMOUT/dump$cyc

#################################################
msg="THE RTOFS_GLO_NCODA_QC JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"

################## END OF SCRIPT #######################

