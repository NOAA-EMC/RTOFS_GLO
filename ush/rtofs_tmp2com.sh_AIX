#!/bin/sh

###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         rtofs_tmp2com.sh                                       #
# Script description:                                                         #
#                                                                             #
# Author:        Ilya Rivin       Org: NP23         Date: 2011-05-30          #
#                                                                             #
# Abstract: This is the post-processing script for RTOFS_GLO                  #
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             # 
#                                                                             #
#                                                                             #
# Script history log:                                                         #
# XXXX-XX-XXX  Joe Dow                                                        #
#                                                                             #
###############################################################################

set -xa

echo "*** Started script $0 on hostname "`hostname`' at time '`date`

cd $DATA

if [ ${RUN_MODE} = 'analysis' ] ; then
  mode=n
else
  mode=f
fi
NPROCS=${NPROCS:-1}
if [ $NPROCS -gt 1 ]
then
  rm -f cmdfile_tmp* cmdfile.*
  export MP_PGMMODEL=mpmd
fi
#
# Write archive files copying commands in CMD
#
for afile in `ls ${DATA}/archv.????_???_??.a`
do
  cfile=`basename $afile`
  YYYY=`echo $cfile | cut -c7-10`
  DDD=`echo $cfile | cut -c12-14`
  HH=`echo $cfile | cut -c16-17`
  YYYYMMDD=`${utilscript}/date2jday.sh ${YYYY}${DDD}`
  MM=`echo $YYYYMMDD | cut -c5-6`
  DD=`echo $YYYYMMDD | cut -c7-8`
  LEAD=`${utilexec}/nhour ${YYYY}${MM}${DD}${HH} ${PDY}${mycyc}`
  tplate=${RUN}_${modID}.t${mycyc}z.${mode}${LEAD}.archv
  echo "cp -p $afile ${COMOUT}/${tplate}.a" >> cmdfile_tmp_a
  echo "cp -p ${afile%.a}.b ${COMOUT}/${tplate}.b" >> cmdfile_tmp_b
  echo "cp -p ${afile%.a}.txt ${COMOUT}/${tplate}.txt" >> cmdfile_tmp_txt
done
#
# Write restart files copying commands in CMD files if necessary
#
for rfile in `ls ${DATA}/restart_out*.b`
do
  # get HYCOM date from the restart file.
  cdate=`${USHrtofs}/rtofs_date4restart.sh $rfile`
  YYYYMMDD=`echo $cdate | cut -c1-8`
  YYYYDDD=`${utilscript}/date2jday.sh $YYYYMMDD`
  YYYY=`echo $YYYYDDD | cut -c1-4`
  DDD=`echo $YYYYDDD | cut -c5-7`
  HH=`echo $cdate | cut -c9-10`
  MM=`echo $YYYYMMDD | cut -c5-6`
  DD=`echo $YYYYMMDD | cut -c7-8`
  LEAD=`${utilexec}/nhour ${YYYY}${MM}${DD}${HH} ${PDY}${mycyc}`
  tplate=${RUN}_${modID}.t${mycyc}z.${mode}${LEAD}.restart
  if [ $LEAD -eq 0 ]
  then
    OUTDIR=$COMOUT
  else
    OUTDIR=$GESdir
  fi
  copy_restart='t'
  if [ -f ${OUTDIR}/${tplate}.b ] 
  then
    cmp $rfile ${OUTDIR}/${tplate}.b  > /dev/null
    if [ $? -eq 0 ]
    then
       copy_restart='f'
    fi
  fi
  if [ ${copy_restart} = 't' ] 
  then
    echo "cp -p $rfile ${OUTDIR}/${tplate}.b" >> cmdfile_tmp_b
    echo "cp -p ${rfile%.b}.a ${OUTDIR}/${tplate}.a" >> cmdfile_tmp_a
  fi
done
#
# Copy restart and archive files in permanent location.
#
for ftype in a b txt
do 
  if [ $NPROCS -gt 1 ] 
  then
    split -${NPROCS} cmdfile_tmp_${ftype} cmdfile.
    ls -l cmdfile.*
    for cfile in `ls cmdfile.*`
    do
      cmdlen=`cat $cfile | wc -l`
      while [ $cmdlen -lt $NPROCS ]
      do
        echo 'sleep 1' >> $cfile
        cmdlen=`expr $cmdlen + 1`
      done
      export MP_CMDFILE=$cfile
      poe -procs $NPROCS
      exit=$?
    done
    rm -f cmdfile.*
  else
    sh cmdfile_tmp_$b{ftype}
  fi
done
if [ $NPROCS -gt 1 ] ; then
  export MP_PGMMODEL=spmd
fi
## rm -f cmdfile_tmp*

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
