#!/bin/ksh
#########################################################################
# Usage: rtofs_prestaging.sh                                            #
#                                                                       #
# Description: Staging input files for analysis or forecast             #
#              pre-processing                                           #
#                                                                       #
# History:                                                              #
#    07-30-2010  Ilya Rivin                                             #
#########################################################################
set -xa

echo "*** Started script $0 on hostname "`hostname`' at time '`date`
export PS4='$SECONDS + '

cd $DATA

# --------------------------------------------------------------------------- #
# 1  Set parameters depending on run mode
if [ ${RUN_MODE} = "analysis" ]
then
  runmode='anal'
  runstep=${runmode}
  runname='ANALYSIS_PRE'
  intvl=1 #atmospheric forcing frequency
else
  runmode='fcst'
  runstep=${runmode}${stepnum}
  runname='FORECAST_PRE'
  intvl=3 #atmospheric forcing frequency
fi 

# --------------------------------------------------------------------------- #
# 1  Set up the start time and end time for the analysis or forecast
  sday=`$utilscript/date_normal2hycom.sh $startdate$mycyc`
  eday=`$utilscript/date_normal2hycom.sh $enddate$mycyc`
  echo "  $sday $eday false false  " > limits

# 2. Copy the necessary input files for the model forecast

# 2.a Get topography (depth, grid and mask)
  for fil in depth grid
  do
    for type in a b
    do
      if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.regional.${fil}.${type} ]
      then
        ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.regional.${fil}.${type} regional.${fil}.${type}
        echo "regional.${fil}.${type} copied" 
      else
        $USHrtofs/${RUN}_abort.sh "Missing Topography files" \
          "ABNORMAL EXIT FORECAST_PRE: NO FILE for regional.${fil}.${type}" 2
      fi
    done
  done

# 2.h copy interpolator data
  cp $PARMrtofs/${RUN}_${modID}.${inputgrid}.${runmode}.intp_pars.dat intp_pars.dat

  if [[ -s intp_pars.dat ]]
  then
      echo "interpolator data copied"
  else
      $USHrtofs/${RUN}_abort.sh "Missing Interpolator Data" \
        "ABNORMAL EXIT $runmode : NO FILE for intp_pars.dat" 2
  fi

# 3. Create Forcing files

# 3.a call atmforcing to get the forcing files 

#dbgz 20121213
#echo "*** NO FORCING FOR NOW"
#exit



  $USHrtofs/${RUN}_atmforcing.sh $startdate$mycyc $enddate$mycyc $intvl 
#dbgz link monthly forcing
#-  for cfile in `ls /marine/noscrub/wx20am/global_0.08/data/forcing.*.[ab]`
#-  do
#-    ln -s -f $cfile .
#-  done

# 3.b correct air temperature over ice.
   
  # Ilya dbgz: the correction is done now in intp.f. 
  # Anyhow, atmforcing_correct.f90 should be debugged!!!
  $USHrtofs/${RUN}_atmforcing_correct.sh

# 4. Copy output in archive

  cp -p $DATA/listflx.dat ${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.listflx.dat
  for fil in airtmp precip presur radflx shwflx surtmp tauewd taunwd vapmix wndspd
  do
    for type in a b
    do
      if [ -s forcing.${fil}.${type} ]
      then
        cp -p forcing.${fil}.${type} ${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.forcing.${fil}.${type} 
        echo "forcing.${fil}.${type} copied" 
        if [[ ${type} = a ]]; then
        $DBNROOT/bin/dbn_alert MODEL RTOFS_GLO_FORCINGA $job ${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.forcing.${fil}.${type}
        fi
       else
        $USHrtofs/${RUN}_abort.sh "Missing Fix Forcing File" \
          "ABNORMAL EXIT FORECAST: NO FILE for forcing.${fil}.${type}" 2
      fi
    done
  done

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
