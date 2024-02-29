#!/bin/sh
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_forecast.sh                                #
# Script description:                                                         #
#                                                                             #
# Author:        Ilya Rivin      Org: NP23         Date: 2010-07-30           #
#                                                                             #
# Abstract: This script generates the forecast fields                         #
#           for the RTOFS_GLO Ocean model                                     #
#                                                                             #
# Sub-scripts called:                                                         #
#   rtofs_runstaging.sh - gets input files from COMIN FIX and PARM            #
#   rtofs_submit.sh - submits the RTOFS HYCOM simulation                      #
#   rtofs_tmp2com.sh - copies the products to COMOUT                          #
#                                                                             #
# Script history log:                                                         #
# 2010-07-30  Ilya Rivin                                                      #
#                                                                             #
###############################################################################
set -xa

export PS4='$SECONDS + '

cd $DATA
  
msg="RTOFS_GLO_FORECAST JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

# --------------------------------------------------------------------------- #
# 0. set some defaults

typeset -Z5 SSSSS
export fcstdays=${fcstdays:-4}
export inputgrid=${inputgrid:-navy_0.08}

# --------------------------------------------------------------------------- #
# 1. Get initial conditions (restart files)

  test -f restart_in.a && rm -f restart_in.a
  test -f restart_in.b && rm -f restart_in.b

# loop through forecast hours starting with most recent checking if restart files exist
  restart_found=no
  if [ $RESTART = YES ]
  then
     # LEAD is the last forecast hour minus one, f01 is fist forecast hour plus one
     LEAD=$(expr $($NHOUR $($NDATE $(expr $fcstdays \* 24 ) ${startdate}) ${PDY}${mycyc}) \- 1)
     f01=$(expr $LEAD \-  $fcstdays \* 24 \+ 2)
# NOTE - this logic does not calculate the width correctly in some cases (when moving from 3 chars to 2 chars)
# NOTE - since this is a very infrequent occurrence, it will be left in as is
     for fcst_hour in $(seq -w $LEAD -1 $f01)
     do
# find most recent forecast restart
       fh2=$fcst_hour
       if [[ $LEAD -ge 100 && $fh2 -lt 100 ]]
       then
         fh2=$(echo $fcst_hour | cut -c2-)
       fi
       if [ -s $GESIN/${RUN}_${modID}.t${mycyc}z.f${fh2}.restart.a -a \
            -s $GESIN/${RUN}_${modID}.t${mycyc}z.f${fh2}.restart.b -a \
            -s $GESIN/${RUN}_${modID}.t${mycyc}z.f${fh2}.restart_cice ]
       then
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.f${fh2}.restart.b restart_in.b
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.f${fh2}.restart.a restart_in.a
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.f${fh2}.restart_cice cice.restart_in
         echo "Forecast $RUN_STEP is started from restart: $GESIN/${RUN}_${modID}.t${mycyc}z.f${fh2}.restart.[ab]" \
              "and ${RUN}_${modID}.t${mycyc}z.f${fh2}.restart_cice"
         restart_found=yes
         break
       fi
     done

     if [ $restart_found = no ]
     then
        $USHrtofs/${RUN}_abort.sh  "FATAL ERROR: $job Missing Restart File" \
          "No restart_in.[ab] or cice.restart_in in $GESIN" 2
     fi

  else
    if [ ${CONTINUE_FORECAST} = NO ]
    then
      # Restart from the nowcast restart
      HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.n${mycyc}.restart
      CICErestTplate=${RUN}_${modID}.t${mycyc}z.n${mycyc}.restart_cice
    else
      # Restart from previous forecast step restart
      LEAD=$($NHOUR ${startdate} ${PDY}${mycyc})
      HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart
      CICErestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart_cice
    fi

    if [ -s ${COMIN}/${HYCOMrestTplate}.a ] && \
       [ -s ${COMIN}/${HYCOMrestTplate}.b ] && \
       [ -s ${COMIN}/${CICErestTplate} ]
    then
      ln -s -f ${COMIN}/${HYCOMrestTplate}.a restart_in.a
      ln -s -f ${COMIN}/${HYCOMrestTplate}.b restart_in.b
      ln -s -f ${COMIN}/${CICErestTplate} cice.restart_in
      echo "Forecast $RUN_STEP is started from restart: ${COMIN}/${HYCOMrestTplate}.[ab] and ${CICErestTplate}"
    else
      $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Restart File" \
        "No restart_in.[ab] or cice.restart_in in $COMIN" 3
    fi
  fi

  echo './cice.restart_in' > cice.restart_file

  if [ -s restart_in.a -a -s restart_in.b -a -s cice.restart_in ]
  then
    echo "Initial restart files copied"
    export startdate=$(${USHrtofs}/rtofs_date4restart.sh restart_in.b)
    export enddate=$($NDATE $(expr $fcstdays \* 24 ) ${startdate})
  else
    $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Restart File" \
        "No restart_in.[ab] or cice.restart_in in $DATA" 911
  fi

# --------------------------------------------------------------------------- #
# 2. get input files

  ksh ${USHrtofs}/${RUN}_runstaging.sh

# --------------------------------------------------------------------------- #
# 3. Run forecast

  ${USHrtofs}/${RUN}_submit.sh

  ok="unknown"
  test -s ${DATA}/summary_out && ok=$(tail -1 ${DATA}/summary_out)
  if [ "$ok" = "normal stop" ]
  then
    modelstatus=0
  else
    modelstatus=1
  fi 

# --------------------------------------------------------------------------- #
# 4. Copy results to com

#
# Note that on a failure, tmp2com will still copy archives after the latest restart
  if [ $SENDCOM = 'YES' ]
  then
    ${USHrtofs}/${RUN}_tmp2com.sh
  fi

# --------------------------------------------------------------------------- #
# 5. Check if run ran to completion and copy restart files to appropriate
#    place depending on that status.

# if model ran, then copy last restart files to comout
  if [ $modelstatus = 0 ]
  then
      if [ ${SAVE_RESTART} = YES ]
      then
        date_out=0 ; date_out1=0 
        test -s ${DATA}/restart_out.b && date_out=$(${USHrtofs}/rtofs_date4restart.sh ${DATA}/restart_out.b)
        test -s ${DATA}/restart_out1.b &&  date_out1=$(${USHrtofs}/rtofs_date4restart.sh ${DATA}/restart_out1.b)
        if [ ${date_out} -gt ${date_out1} ]
        then
          rfile=${DATA}/restart_out.b
          cdate=${date_out}
        else
          rfile=${DATA}/restart_out1.b
          cdate=${date_out1}
        fi
        YYYY=$(echo $cdate | cut -c1-4)
        MM=$(echo $cdate | cut -c5-6)
        DD=$(echo $cdate | cut -c7-8)
        HH=$(echo $cdate | cut -c9-10)
        SSSSS=$(expr $HH \* 3600)
        LEAD=$($NHOUR ${cdate} ${PDY}${mycyc})
        HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart
        CICErestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart_cice
        cp -p $rfile ${COMOUT}/${HYCOMrestTplate}.b
        cp -p ${rfile%.b}.a ${COMOUT}/${HYCOMrestTplate}.a
        cp -p cice.restart.${YYYY}-${MM}-${DD}-${SSSSS} ${COMOUT}/${CICErestTplate}
       fi
    echo "done" >$COMOUT/${RUN}_${modID}.t${mycyc}z.fcst$RUN_STEP.log
  else
    date_out=0 ; date_out1=0; rcount=0
    if [ -s ${DATA}/restart_out.b ]
    then
      date_out=$(${USHrtofs}/rtofs_date4restart.sh ${DATA}/restart_out.b)
      let rcount=rcount+1
    fi
    if [ -s ${DATA}/restart_out1.b ]
    then
      date_out1=$(${USHrtofs}/rtofs_date4restart.sh ${DATA}/restart_out1.b)
      let rcount=rcount+1
    fi
# copy latest restart file (if found) to GESOUT
    if [ $rcount -ne 0 ]
    then
      if [ ${date_out} -gt ${date_out1} ]
      then
        rfile=${DATA}/restart_out.b
        cdate=${date_out}
      else
        rfile=${DATA}/restart_out1.b
        cdate=${date_out1}
      fi
      YYYY=$(echo $cdate | cut -c1-4)
      MM=$(echo $cdate | cut -c5-6)
      DD=$(echo $cdate | cut -c7-8)
      HH=$(echo $cdate | cut -c9-10)
      SSSSS=$(expr $HH \* 3600)
      LEAD=$($NHOUR ${cdate} ${PDY}${mycyc})
      HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart
      CICErestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart_cice
      cp -p $rfile ${GESOUT}/${HYCOMrestTplate}.b
      cp -p ${rfile%.b}.a ${GESOUT}/${HYCOMrestTplate}.a
      cp -p cice.restart.${YYYY}-${MM}-${DD}-${SSSSS} ${GESOUT}/${CICErestTplate}
    fi
    $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Abnormal model exit" \
       "problem with forecast model run - return code $modelstatus" $modelstatus
  fi

#################################################
msg="THE RTOFS_GLO_FORECAST JOB HAS ENDED NORMALLY on $(hostname) at $(date)."
postmsg "$msg"

################## END OF SCRIPT #######################

