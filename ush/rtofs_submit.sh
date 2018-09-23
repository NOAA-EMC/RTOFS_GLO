#!/bin/sh
#########################################################################
# Usage: rtofs_submit.sh                                                #
#                                                                       #
# Description: Submitting analysis or forecast RTOFS model simulation.  #
#                                                                       #
# History:                                                              #
#    07-30-2010  Ilya Rivin                                             #
#########################################################################
set -xa

echo "*** Started script $0 on hostname "`hostname`' at time '`date`
export PS4='$SECONDS + '

cd $DATA


# --------------------------------------------------------------------------- #
# 0.  Set parameters depending on run mode
if [ ${RUN_MODE} = "analysis" ]
then
  runmode='anal'
  runname='ANALYSIS'
else
  runmode='fcst'
  runname='FORECAST'
fi 

msg="RTOFS_GLO_FORECAST_PRE JOB has begun on `hostname` at `date`"
postmsg "$jlogfile" "$msg"

#
# 1. Prepare for the model run

  touch ok
  rm -f ok
  date >> TRACK

export pgm="$EXECcode/hycom_cice"
. prep_step
startmsg
#####  mpirun.lsf /nwprod2/rtofs_code.v2.2.86F2p0i/exec/hycom_cice -procs 1800
mpirun.lsf $EXECcode/hycom_cice -procs $NPROCS >> $pgmout 2>errfile
  # mpirun.lsf $EXECcode/${RUN}_forecast -procs $NPROCS >> $pgmout 2>errfile
  # mpirun -ppn 16 $EXECrtofs/${RUN}_forecast  >> $pgmout 2>errfile
  err=$?; export err ; err_chk
  echo " error from ${RUN}_hycom_cice=",$err

  date >> TRACK

# --------------------------------------------------------------------------- #
# 2.  Check for errors

ok="unknown"
test -s ${DATA}/summary_out && ok=`tail -1 ${DATA}/summary_out`
if [ "$ok" = "normal stop" ]; then
   modelstatus=0
else
   modelstatus=1
fi

# --------------------------------------------------------------------------- #
# 3.  Ending output
  if [ $modelstatus = 0 ]
  then
       # Copy probes to /com
       for prb in `ls probe_out_*`
       do
         if [ -s $COMOUT/${RUN}_${modID}.t${mycyc}z.${runmode}.${prb} ]
         then
            cat $prb >> $COMOUT/${RUN}_${modID}.t${mycyc}z.${runmode}.${prb}
         else
            cp -p $prb $COMOUT/${RUN}_${modID}.t${mycyc}z.${runmode}.${prb}
         fi
       done

    echo "done" >$COMOUT/${RUN}_${modID}.t${mycyc}z.${runmode}.log
  else
    $USHrtofs/${RUN}_abort.sh "Abnormal model exit from ${RUN_MODE}" \
       "ABNORMAL EXIT ${runname}: problem with ${RUN_MODE} model run" -1
 fi

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
