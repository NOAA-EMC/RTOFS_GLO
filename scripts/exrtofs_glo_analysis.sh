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
# 0. date and time stuff

  export fcstdays=${fcstdays:-2}
  export enddate=${analysis_end:-$PDY}
  export startdate=$($NDATE -$(expr $fcstdays \* 24) ${enddate}${mycyc} | cut -c1-8)
  export inputgrid=${inputgrid:-navy_0.08}

# --------------------------------------------------------------------------- #
# 1. Get input files
  ksh ${USHrtofs}/${RUN}_runstaging.sh

# --------------------------------------------------------------------------- #
# 2. Get initial conditions (restart files)

  typeset -Z5 sc
  typeset -Z5 SSSSS

  LEAD=$(expr $fcstdays \* -24)

  HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.n${LEAD}.restart
  CICErestTplate=${RUN}_${modID}.t${mycyc}z.n${LEAD}.restart_cice

  test -f restart_in.a && rm -f restart_in.a
  test -f restart_in.b && rm -f restart_in.b

  if [ $RESTART = YES ]
  then
     anal_hour=$(${NHOUR} ${enddate}${mycyc} ${PDY}${mycyc})
     while [ $anal_hour -ge $LEAD ]
     do       
       if [ -s $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart}.b ]
       then
         # find most recent analysis restart
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart.b restart_in.b
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart.a restart_in.a
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart_cice cice.restart_in
         echo "Analysis is started from restart: $GESIN/${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart.[ab]" \
              " and ${RUN}_${modID}.t${mycyc}z.n${anal_hour}.restart_cice"

       # replace limits file
         sday=$($USHrtofs/rtofs_date_normal2hycom.sh $(${NDATE} +$anal_hour ${PDY}${mycyc}))
         echo "  $sday $eday false false  " > limits
         break

       fi

       anal_hour=$(expr $anal_hour - 1)
     done
     if [ $anal_hour -lt $LEAD ]
     then
        $USHrtofs/${RUN}_abort.sh  "FATAL ERROR: $job Missing Restart File" \
          "No restart_in.[ab] or cice.restart_in in $GESIN" 2
    fi 

  else 
    if [ -s ${COMIN}/${HYCOMrestTplate}.a ] && \
       [ -s ${COMIN}/${HYCOMrestTplate}.b ] && \
       [ -s ${COMIN}/${CICErestTplate} ] 
    then
      ln -s -f ${COMIN}/${HYCOMrestTplate}.a restart_in.a
      ln -s -f ${COMIN}/${HYCOMrestTplate}.b restart_in.b
      ln -s -f ${COMIN}/${CICErestTplate} cice.restart_in
      echo "Analysis is started from restart: ${COMIN}/${HYCOMrestTplate}.[ab] and ${COMIN}/${CICErestTplate}"
    else
      $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Restart File" \
        "No restart_in.[ab] or cice.restart_in in $COMIN" 3
    fi
  fi
  echo './cice.restart_in' > cice.restart_file

  if [ -s restart_in.a -a -s restart_in.b -a -s cice.restart_in ]
  then
    echo "Initial restart files copied"
    basetime=1900123100
    hdate=$(awk '{  if (NR==2) { print $5 } }'  < restart_in.b | cut -d. -f1)
    dater=$($NDATE $(expr $hdate \* 24)  ${basetime})
    if [ ${dater} -ne ${startdate}${mycyc} ] 
    then 
      $USHrtofs/${RUN}_abort.sh  "FATAL ERROR: $job Mismatch Restart dates" \
        "expected=${startdate}${mycyc} actual=$dater" 2
    fi
  else
      $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Restart File" \
        "No restart_in.[ab] or cice.restart_in in $DATA" 911
  fi

# --------------------------------------------------------------------------- #
# 2. Run analysis

  ${USHrtofs}/${RUN}_submit.sh

  ok="unknown"
  test -s ${DATA}/summary_out && ok=$(tail -1 ${DATA}/summary_out)
  if [ "$ok" = "normal stop" ]; then
    modelstatus=0
  else
    modelstatus=1
  fi 
  if [ $modelstatus = 0 ]
  then
      # Copy to /com initial restart file for forecast step
      for rfile in ${DATA}/restart_out.b ${DATA}/restart_out1.b 
      do
        cdate=$(${USHrtofs}/rtofs_date4restart.sh $rfile)
        if [ $cdate -eq $PDY$mycyc ]
        then          
          YYYYMMDD=$(echo $cdate | cut -c1-8)
          YYYYDDD=$(sh $USHutil/date2jday.sh $YYYYMMDD)
          YYYY=$(echo $YYYYDDD | cut -c1-4)
          DDD=$(echo $YYYYDDD | cut -c5-7)
          MM=$(echo $YYYYMMDD | cut -c5-6)
          DD=$(echo $YYYYMMDD | cut -c7-8)
          HH=$(echo $cdate | cut -c9-10)
          SSSSS=$(expr $HH \* 3600)
          HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.n${HH}.restart
          CICErestTplate=${RUN}_${modID}.t${mycyc}z.n${HH}.restart_cice
          cp -p $rfile ${COMOUT}/${HYCOMrestTplate}.b
          cp -p ${rfile%.b}.a ${COMOUT}/${HYCOMrestTplate}.a
          cp -p cice.restart.${YYYY}-${MM}-${DD}-${SSSSS} ${COMOUT}/${CICErestTplate}
        fi
      done
    echo "done" >$COMOUT/${RUN}_${modID}.t${mycyc}z.anal.log
  else
    $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Abnormal model exit" \
       "problem with analysis model run - return code $modelstatus" $modelstatus
  fi

# --------------------------------------------------------------------------- #
# 3. Copy results in the archive

  if [ $SENDCOM = 'YES' ]
  then
    ksh ${USHrtofs}/${RUN}_tmp2com.sh
  fi

#################################################
msg="THE RTOFS_GLO_ANALYSIS JOB HAS ENDED NORMALLY on $(hostname) at $(date)."
postmsg "$msg"

################## END OF SCRIPT #######################

