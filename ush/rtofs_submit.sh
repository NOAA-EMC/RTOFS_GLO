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

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)
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

msg="RTOFS_GLO_$runname JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

#
# 1. Prepare for the model run

  touch ok
  rm -f ok
  date >> TRACK

  export pgm="$EXECrtofs/rtofs_hycom"
  mpiexec -n $NPROCS -ppn 120 --cpu-bind core $EXECrtofs/rtofs_hycom >> $pgmout 2>errfile
  err=$?; export err ; err_chk
  echo " error from rtofs_hycom=",$err

  date >> TRACK

# --------------------------------------------------------------------------- #
# 2.  Check for errors

ok="unknown"
test -s ${DATA}/summary_out && ok=$(tail -1 ${DATA}/summary_out)
if [ "$ok" = "normal stop" ]; then
   modelstatus=0
else
   modelstatus=1
fi

# --------------------------------------------------------------------------- #
# 3.  Ending output
  if [ $modelstatus = 0 ]
  then
    if compgen -G "probe_out_*" > /dev/null
    then
       # Copy probes to /com
       for prb in $(ls probe_out_*)
       do
         if [ -s $COMOUT/${RUN}_${modID}.t${mycyc}z.${runmode}.${prb} ]
         then
            cat $prb >> $COMOUT/${RUN}_${modID}.t${mycyc}z.${runmode}.${prb}
         else
            cp -p $prb $COMOUT/${RUN}_${modID}.t${mycyc}z.${runmode}.${prb}
         fi
       done
    fi
    echo "done" >$COMOUT/${RUN}_${modID}.t${mycyc}z.${runmode}.log
  else
    $USHrtofs/${RUN}_abort.sh "Abnormal model exit from ${RUN_MODE}" \
       "ABNORMAL EXIT ${runname}: problem with ${RUN_MODE} model run" -1
 fi

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)
