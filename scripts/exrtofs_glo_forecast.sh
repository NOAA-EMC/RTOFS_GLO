#!/bin/sh
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_analysis.sh                                #
# Script description:                                                         #
#                                                                             #
# Author:        Ilya Rivin      Org: NP23         Date: 2010-07-30           #
#                                                                             #
# Abstract: This script generates the forecast fields                         #
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
  
msg="RTOFS_GLO_FORECAST JOB has begun on `hostname` at `date`"
postmsg "$jlogfile" "$msg"

# --------------------------------------------------------------------------- #
# 0. date and time stuff

  export fcstdays=${fcstdays:-6}
  export startdate=${startdate:-${PDY}}
  export enddate=`$NDATE \` expr $fcstdays \* 24 \`  ${startdate}${mycyc} | cut -c1-8`
  export inputgrid=${inputgrid:-navy_0.08}

# --------------------------------------------------------------------------- #
# 2. Get input files
  $USHrtofs/${RUN}_runstaging.sh

# --------------------------------------------------------------------------- #
# 2. Get initial conditions (restart files)

  typeset -Z5 SSSSS

  test -f restart_in.a && rm -f restart_in.a
  test -f restart_in.b && rm -f restart_in.b

  if [ $RESTART = YES ]
  then
     fcst_hour=`$NHOUR ${enddate}${mycyc} ${PDY}${mycyc}`
     while [ $fcst_hour -ge 0 ]
     do       
       if [ -s $GESIN/${RUN}_${modID}.t${mycyc}z.restart_f${fcst_hour}.b ]
       then
         # find most recent forecast restart
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.restart_f${fcst_hour}.b restart_in.b
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.restart_f${fcst_hour}.a restart_in.a
         ln -s -f $GESIN/${RUN}_${modID}.t${mycyc}z.restart_cice_f${fcst_hour} cice.restart_in
         echo "Forecast is started from restart: $GESIN/${RUN}_${modID}.t${mycyc}z.restart_f${fcst_hour}.[ab]" \
              " and $GESIN/${RUN}_${modID}.t${mycyc}z.restart_cice_f${fcst_hour} ."

       # replace limits file
         sday=`$USHrtofs/rtofs_date_normal2hycom.sh \` $NDATE +$fcst_hour ${PDY}${mycyc} \` `
         echo "  $sday $eday false false  " > limits
         break

       fi

       fcst_hour=`expr $fcst_hour - 1`
     done
     if [ $fcst_hour -lt 0 ]
     then
        $USHrtofs/${RUN}_abort.sh  "Missing Restart File" \
          "ABNORMAL EXIT FORECAST: NO FILE for restart_in.[ab] and ${CICEworkRestTplate}" 2
    fi 

  else 
    if [ ${CONTINUE_FORECAST} = NO ] 
    then
      #
      # Restart from the nowcast restart
      HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.n${mycyc}.restart
      CICErestTplate=${RUN}_${modID}.t${mycyc}z.n${mycyc}.restart_cice
    else
      #
      # Restart from previous forecast step restart
      LEAD=`$NHOUR ${startdate}${mycyc} ${PDY}${mycyc}`
      HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart
      CICErestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart_cice
    fi
    if [ -s ${COMIN}/${HYCOMrestTplate}.a ] && \
       [ -s ${COMIN}/${HYCOMrestTplate}.b ] && \
       [ -s ${COMIN}/${CICErestTplate} ]
    then
      ln -s -f ${COMIN}/${HYCOMrestTplate}.a restart_in.a
      ln -s -f ${COMIN}/${HYCOMrestTplate}.b restart_in.b
      ln -s -f ${COMIN}/${CICErestTplate}  cice.restart_in
      echo "Forecast is started from restart: ${COMIN}/${HYCOMrestTplate}.[ab]" \
           "and ${COMIN}/${CICErestTplate}"
    else
      $USHrtofs/${RUN}_abort.sh "Missing Restart File" \
        "ABNORMAL EXIT FORECAST: NO FILE for restart_in.[ab] or${CICEworkRestTplate} (need "  \
        " ${COMIN}/${HYCOMrestTplate}.[ab]) and ${COMIN}/${CICErestTplate}" 2
      fi
  fi
  echo './cice.restart_in' > cice.restart_file

  if [ -s restart_in.a -a -s restart_in.b -a -s cice.restart_in ]
  then
    echo "Initial restart files copied"
    basetime=1900123100
    hdate=`awk '{  if (NR==2) { print $5 } }'  < restart_in.b | cut -d. -f1 `
    dater=`$NDATE \` expr $hdate \* 24 \`  ${basetime}`
    if [ ${dater} -ne ${startdate}${mycyc} ] 
    then 
      $USHrtofs/${RUN}_abort.sh  "RESTAERT DATES: expected=${startdate}${mycyc} actual=$dater" \
        "ABNORMAL EXIT FORECAST: WRONG DATE in the restart file" 2
    fi
  else
      $USHrtofs/${RUN}_abort.sh "Missing Restart File" \
        "ABNORMAL EXIT FORECAST: FATAL ERROR--NO RESTART FILE" 911
  fi

# --------------------------------------------------------------------------- #
# 2.# 1. Run forecast

  ${USHrtofs}/${RUN}_submit.sh

  ok="unknown"
  test -s ${DATA}/summary_out && ok=`tail -1 ${DATA}/summary_out`
  if [ "$ok" = "normal stop" ]; then
    modelstatus=0
  else
    modelstatus=1
  fi 
  if [ $modelstatus = 0 ] 
  then
      if [ ${SAVE_RESTART} = YES ]
      then
        # Copy to /com initial restart file for the next forecast step
        date_out=0 ; date_out1=0 
        test -s ${DATA}/restart_out.b && date_out=`${USHrtofs}/rtofs_date4restart.sh ${DATA}/restart_out.b`
        test -s ${DATA}/restart_out1.b &&  date_out1=`${USHrtofs}/rtofs_date4restart.sh ${DATA}/restart_out1.b`
        if [ ${date_out} -gt ${date_out1} ]
        then
          rfile=${DATA}/restart_out.b
          cdate=${date_out}
        else
          rfile=${DATA}/restart_out1.b
          cdate=${date_out1}
        fi
        LEAD=`$NHOUR ${cdate} ${PDY}${mycyc}`
        YYYY=`echo $cdate | cut -c1-4`
        MM=`echo $cdate | cut -c5-6`
        DD=`echo $cdate | cut -c7-8`
        HH=`echo $cdate | cut -c9-10`
        SSSSS=`expr $HH \* 3600`
        HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart
        CICErestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart_cice
        cp -p $rfile ${COMOUT}/${HYCOMrestTplate}.b
        cp -p ${rfile%.b}.a ${COMOUT}/${HYCOMrestTplate}.a
        cp -p cice.restart.${YYYY}-${MM}-${DD}-${SSSSS} ${COMOUT}/${CICErestTplate}
       fi
    echo "done" >$COMOUT/${RUN}_${modID}.t${mycyc}z.anal.log
  else
    $USHrtofs/${RUN}_abort.sh "Abnormal model exit from analyis" \
       "ABNORMAL EXIT ANALYSIS: problem with analysis model run" -1
  fi

# --------------------------------------------------------------------------- #
# 3. Copy results in the archive

  if [ $SENDCOM = 'YES' ]
  then
    ksh ${USHrtofs}/${RUN}_tmp2com.sh
  fi

#################################################
msg="THE RTOFS_GLO_FORECAST JOB HAS ENDED NORMALLY on `hostname` at `date`."
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################

