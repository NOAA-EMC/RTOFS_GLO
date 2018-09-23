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

typeset -Z5 SSSSS
typeset -Z2 HH

if [ ${RUN_MODE} = 'analysis' ] ; then
  mode=n
else
  mode=f
fi
NPROCS=${NPROCS:-1}
if [ $NPROCS -gt 1 ]
then
  rm -f cmdfile_tmp* cmdfile.*
fi
#
# Write archive files copying commands in CMD
#
for afile in `ls ${DATA}/archv.????_???_??.a ${DATA}/archs.????_???_??.a`
do
  cfile=`basename $afile`
  YYYY=`echo $cfile | cut -c7-10`
  DDD=`echo $cfile | cut -c12-14`
  HH=`echo $cfile | cut -c16-17`
  YYYYMMDD=`${USHutil}/date2jday.sh ${YYYY}${DDD}`
  MM=`echo $YYYYMMDD | cut -c5-6`
  DD=`echo $YYYYMMDD | cut -c7-8`
  LEAD=`${EXECutil}/nhour ${YYYY}${MM}${DD}${HH} ${PDY}${mycyc}`
  arch=`echo $cfile | cut -d. -f1`
  HYCOMarchTplate=${RUN}_${modID}.t${mycyc}z.${mode}${LEAD}.${arch}
  if [ $arch = "archv" ] ; then
    echo "cp -p -f $afile ${COMOUT}/${HYCOMarchTplate}.a" >> cmdfile_tmp_v
  fi
  if [ $arch = "archs" ] ; then
    echo "cp -p -f $afile ${COMOUT}/${HYCOMarchTplate}.a" >> cmdfile_tmp_s
  fi
  cp -p -f ${afile%.a}.b ${COMOUT}/${HYCOMarchTplate}.b
  cp -p -f ${afile%.a}.txt ${COMOUT}/${HYCOMarchTplate}.txt
done
for ifile in `ls ${DATA}/cice_inst.????-??-??-?????.nc`
do
 cfile=`basename $ifile`
 YYYY=`echo $cfile | cut -c11-14`
 MM=`echo $cfile | cut -c16-17`
 DD=`echo $cfile | cut -c19-20`
 SSSSS=`echo $cfile | cut -c22-26`
 HH=`expr $SSSSS \/ 3600`
 LEAD=`${EXECutil}/nhour ${YYYY}${MM}${DD}${HH} ${PDY}${mycyc}`
 echo "cp -p -f $cfile ${COMOUT}/${RUN}_${modID}.t${mycyc}z.${mode}${LEAD}.cice_inst" >> cmdfile_tmp_c # dont work w/ hourly
done
#
# Write restart files copying commands in CMD files if necessary
#
for rfile in `ls ${DATA}/restart_out*.b`
do
  # get HYCOM date from the restart file.
  cdate=`${USHrtofs}/rtofs_date4restart.sh $rfile`
  YYYYMMDD=`echo $cdate | cut -c1-8`
  YYYYDDD=`${USHutil}/date2jday.sh $YYYYMMDD`
  YYYY=`echo $YYYYDDD | cut -c1-4`
  DDD=`echo $YYYYDDD | cut -c5-7`
  HH=`echo $cdate | cut -c9-10`
  MM=`echo $YYYYMMDD | cut -c5-6`
  DD=`echo $YYYYMMDD | cut -c7-8`
  SSSSS=`expr $HH \* 3600`
  LEAD=`${EXECutil}/nhour ${YYYY}${MM}${DD}${HH} ${PDY}${mycyc}`
  HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.${mode}${LEAD}.restart
  CICErestTplate=${RUN}_${modID}.t${mycyc}z.${mode}${LEAD}.restart_cice
  CICEworkRestTplate=cice.restart.${YYYY}-${MM}-${DD}-${SSSSS}
  if [ $LEAD -eq 0 ]
  then
    OUTDIR=$COMOUT
  else
    OUTDIR=$GESdir
  fi
  copy_restart='t'
  if [ -f ${OUTDIR}/${HYCOMrestTplate}.b ] 
  then
    cmp $rfile ${OUTDIR}/${HYCOMrestTplate}.b  > /dev/null
    if [ $? -eq 0 ]
    then
       copy_restart='f'
    fi
  fi
  if [ ${copy_restart} = 't' ] 
  then
    cp -p -f $rfile ${OUTDIR}/${HYCOMrestTplate}.b
    echo "cp -p -f ${rfile%.b}.a ${OUTDIR}/${HYCOMrestTplate}.a" >> cmdfile_tmp_v
    echo "cp -p -f ${CICEworkRestTplate} ${OUTDIR}/${CICErestTplate}" >> cmdfile_tmp_c
  fi
done
#
# Copy restart and archive files in permanent location.
#
#dbgz 20130113
NPROCS=1
for ftype in s v c
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
     module load ics
      module load ibmpe
      export MP_LABELIO=yes
      export MP_CMDFILE=./$cfile
      mpirun.lsf >>$pgmout 2>>errfile ### < my_stdin > my_stdout
      exit=$?
    done
    #dbgz
    #rm -f cmdfile.*
  else
    sh cmdfile_tmp_${ftype}
  fi
done
## rm -f cmdfile_tmp*

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
