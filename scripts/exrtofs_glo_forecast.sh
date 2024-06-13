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
#     if this is a restart, then find latest restart files in DATArestart
#     if this is not a restart, then find restart files in COMin
 
  test -f restart_in.a && rm -f restart_in.a
  test -f restart_in.b && rm -f restart_in.b

  restart_found=no
  if [ $RESTART = YES ]
  then
    for rtype in out out1   # rename restarts with out string to use timestamp
    do
      if [ -s ${DATArestart}/restart_${rtype}.b ]
      then
        date_out=$(${USHrtofs}/rtofs_date4restart.sh ${DATArestart}/restart_${rtype}.b)
        mv ${DATArestart}/restart_${rtype}.a ${DATArestart}/restart_${date_out}.a
        mv ${DATArestart}/restart_${rtype}.b ${DATArestart}/restart_${date_out}.b
      fi
    done

    latestdate=0
    for rfileb in $(ls ${DATArestart}/restart_*.b)  # find and link to latest restart file
    do
      date_out=$(${USHrtofs}/rtofs_date4restart.sh ${rfileb})
      if [ $date_out -gt $latestdate ]
      then
        latestdate=$date_out
        rfile=$rfileb
      fi
    done

    # find cice restart file for this restart time and link them
    if [ $latestdate -ne 0 ]
    then
      YYYY=$(echo $latestdate | cut -c1-4)
      MM=$(echo $latestdate | cut -c5-6)
      DD=$(echo $latestdate | cut -c7-8)
      HH=$(echo $latestdate | cut -c9-10)
      SSSSS=$(expr $HH \* 3600)
      ln -sf $rfile $DATA/restart_in.b
      ln -sf ${rfile%.b}.a $DATA/restart_in.a
      ln -sf $DATArestart/cice.restart.${YYYY}-${MM}-${DD}-${SSSSS} $DATA/cice.restart_in
      echo INFO - restarting simulation from $latestdate using $rfile
      restart_found=yes
    fi

    if [ $restart_found = no ]
    then
       $USHrtofs/${RUN}_abort.sh  "FATAL ERROR: $job Missing Restart File" \
         "No restart_in.[ab] or cice.restart_in in $DATArestart" 2
    fi

# remove archives created after most recent restart; hycom will fail if these files exist
    rdate=$(${USHrtofs}/rtofs_date4restart.sh $DATA/restart_in.b)
    for afile in $(ls $DATAarchive/*.b)
    do
      archivedate=$(basename $afile | cut -d. -f2 | cut -c1-4,6-8,10-11)
      ayjul=$(echo $archivedate | cut -c1-7)
      ahour=$(echo $archivedate | cut -c8-9)
      adate=$($UTILROOT/ush/date2jday.sh $ayjul)$ahour
      if [ $adate -gt $rdate ]
      then
        echo removing ${afile%.b} files
        rm -f ${afile} ${afile%.b}.a ${afile%.b}.txt
      fi
    done

  else # RESTART=NO
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
    # remove any possible archives created
    rm -f $DATAarchive/*
  fi

  echo './cice.restart_in' > cice.restart_file

  if [ -s restart_in.a -a -s restart_in.b -a -s cice.restart_in ]
  then
    echo "Initial restart files copied"
    export enddate=$($NDATE $(expr $fcstdays \* 24 ) ${startdate})
    export startdate=$(${USHrtofs}/rtofs_date4restart.sh restart_in.b)
  else
    $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Restart File" \
        "No restart_in.[ab] or cice.restart_in in $DATA" 911
  fi

# --------------------------------------------------------------------------- #
# 2. get input files

  ksh ${USHrtofs}/${RUN}_runstaging.sh
  if [ $RESTART = YES ]
  then # create new cice files to this start time
    rm -f cice.??????.?
    export iday=$($USHrtofs/rtofs_date_normal2hycom.sh $startdate)
    $USHrtofs/${RUN}_iceforcing.sh
  fi

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

#
# --------------------------------------------------------------------------- #
# 4. If run ran to completion and copy restart and archive files to COMOUT
# if model ran, then copy last restart files to comout

  if [ $modelstatus = 0 ]
  then
      if [ $SENDCOM = 'YES' ]
      then
        ${USHrtofs}/${RUN}_tmp2com.sh
      fi
      if [ ${SAVE_RESTART} = YES ]
      then
        date_out=0 ; date_out1=0 
        test -s ${DATArestart}/restart_out.b && date_out=$(${USHrtofs}/rtofs_date4restart.sh ${DATArestart}/restart_out.b)
        test -s ${DATArestart}/restart_out1.b &&  date_out1=$(${USHrtofs}/rtofs_date4restart.sh ${DATArestart}/restart_out1.b)
        if [ ${date_out} -gt ${date_out1} ]
        then
          rfile=${DATArestart}/restart_out.b
          cdate=${date_out}
        else
          rfile=${DATArestart}/restart_out1.b
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
        cp -p ${DATArestart}/cice.restart.${YYYY}-${MM}-${DD}-${SSSSS} ${COMOUT}/${CICErestTplate}
       fi
    echo "done" >$COMOUT/${RUN}_${modID}.t${mycyc}z.fcst$RUN_STEP.log
  else
    $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Abnormal model exit" \
       "problem with forecast model run - return code $modelstatus" $modelstatus
  fi

#################################################
msg="THE RTOFS_GLO_FORECAST JOB HAS ENDED NORMALLY on $(hostname) at $(date)."
postmsg "$msg"

################## END OF SCRIPT #######################

