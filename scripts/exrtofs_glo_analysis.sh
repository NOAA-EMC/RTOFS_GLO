#!/bin/sh
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_analysis.sh                                #
# Script description:                                                         #
#                                                                             #
# Author:        Ilya Rivin      Org: NP23         Date: 2010-07-30           #
#                                                                             #
# Abstract: This script generates the analysis fields                         #
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

msg="RTOFS_GLO_ANALYSIS JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

# --------------------------------------------------------------------------- #
# 0. set some defaults

typeset -Z5 SSSSS
export fcstdays=${fcstdays:-2}
export inputgrid=${inputgrid:-navy_0.08}

# --------------------------------------------------------------------------- #
# 1. Get initial conditions (restart files)

  test -f restart_in.a && rm -f restart_in.a
  test -f restart_in.b && rm -f restart_in.b

# loop through nowcast hours starting with most recent checking if restart files exist
  restart_found=no
  if [ $RESTART = YES ]
  then
     # LEAD is the last forecast hour minus one
     LEAD=$(expr $fcstdays \* -24 + 1)
     for anal_hour in $(seq -w -1 -1 $LEAD)
     do
# find most recent analysis restart
       if [ -s $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart.a -a \
            -s $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart.b -a \
            -s $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart_cice ]
       then
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart.b restart_in.b
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart.a restart_in.a
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart_cice cice.restart_in
         echo "Analysis is started from restart: $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart.[ab]" \
              "and ${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart_cice"
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
    LEAD=$(expr $fcstdays \* -24)
    HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.n${LEAD}.restart
    CICErestTplate=${RUN}_${modID}.t${mycyc}z.n${LEAD}.restart_cice

    if [ -s ${COMIN}/${HYCOMrestTplate}.a ] && \
       [ -s ${COMIN}/${HYCOMrestTplate}.b ] && \
       [ -s ${COMIN}/${CICErestTplate} ] 
    then
      ln -s -f ${COMIN}/${HYCOMrestTplate}.a restart_in.a
      ln -s -f ${COMIN}/${HYCOMrestTplate}.b restart_in.b
      ln -s -f ${COMIN}/${CICErestTplate} cice.restart_in
      echo "Analysis is started from restart: ${COMIN}/${HYCOMrestTplate}.[ab] and ${CICErestTplate}"
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
    export enddate=${analysis_end:-${PDY}${mycyc}}
  else
    $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Restart File" \
        "No restart_in.[ab] or cice.restart_in in $DATA" 911
  fi

# --------------------------------------------------------------------------- #
# 2. get input files

  ksh ${USHrtofs}/${RUN}_runstaging.sh

# --------------------------------------------------------------------------- #
# 3. Run analysis

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

# if model ran, then copy both restart files (n00, n-06) to comout
#   n00 - for forecast step1
#   n-06 - for PDYp1 incup step
  if [ $modelstatus = 0 ]
  then
      for rfile in ${DATA}/restart_out.b ${DATA}/restart_out1.b 
      do
        cdate=$(${USHrtofs}/rtofs_date4restart.sh $rfile)
        YYYY=$(echo $cdate | cut -c1-4)
        MM=$(echo $cdate | cut -c5-6)
        DD=$(echo $cdate | cut -c7-8)
        HH=$(echo $cdate | cut -c9-10)
        SSSSS=$(expr $HH \* 3600)
        LEAD=$($NHOUR ${cdate} ${PDY}${mycyc})
        HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.n${LEAD}.restart
        CICErestTplate=${RUN}_${modID}.t${mycyc}z.n${LEAD}.restart_cice
        cp -p $rfile ${COMOUT}/${HYCOMrestTplate}.b
        cp -p ${rfile%.b}.a ${COMOUT}/${HYCOMrestTplate}.a
        cp -p cice.restart.${YYYY}-${MM}-${DD}-${SSSSS} ${COMOUT}/${CICErestTplate}
      done
    echo "done" >$COMOUT/${RUN}_${modID}.t${mycyc}z.anal.log
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
      HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.n${LEAD}.restart
      CICErestTplate=${RUN}_${modID}.t${mycyc}z.n${LEAD}.restart_cice
      cp -p $rfile ${GESOUT}/${HYCOMrestTplate}.b
      cp -p ${rfile%.b}.a ${GESOUT}/${HYCOMrestTplate}.a
      cp -p cice.restart.${YYYY}-${MM}-${DD}-${SSSSS} ${GESOUT}/${CICErestTplate}
# check to see if LEAD is -06 (current incremental update length)
# if it is, then  need to also copy this restart file to COMOUT 
# note -06 is same as inc_hours in exrtofs_glo_incup.sh
      if [ $LEAD -eq -06 ]
      then
        cp -p $rfile ${COMOUT}/${HYCOMrestTplate}.b
        cp -p ${rfile%.b}.a ${COMOUT}/${HYCOMrestTplate}.a
        cp -p cice.restart.${YYYY}-${MM}-${DD}-${SSSSS} ${COMOUT}/${CICErestTplate}
      fi
    fi
    $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Abnormal model exit" \
       "problem with analysis model run - return code $modelstatus" $modelstatus
  fi

#################################################
msg="THE RTOFS_GLO_ANALYSIS JOB HAS ENDED NORMALLY on $(hostname) at $(date)."
postmsg "$msg"

################## END OF SCRIPT #######################

