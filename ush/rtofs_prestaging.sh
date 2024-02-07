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
  # Set intvl=3 for retrospectives when only 3 hour forcing is available.
  # Remember to change 3 lines in parm/rtofs_glo.navy_0.08.anal.ice_in.
  # Change atm_data_type, sss_data_type, sst_data_type from 'cfsr' to 'nogaps'.
  intvl=3 #atmospheric forcing frequency
else
  runmode='fcst'
  runstep=${runmode}${stepnum}
  runname='FORECAST_PRE'
  intvl=3 #atmospheric forcing frequency
fi 

# --------------------------------------------------------------------------- #
# 1  Set up the start time and end time for the analysis or forecast
  sday=`$USHrtofs/rtofs_date_normal2hycom.sh $startdate$mycyc`
  eday=`$USHrtofs/rtofs_date_normal2hycom.sh $enddate$mycyc`
  echo "  $sday $eday false false  " > limits

# 2. Copy the necessary input files for the model forecast

# 2.a Get topography (depth, grid, mask, cice) and offluxxg
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
#
# Relax files for SSH are needed for restart2archive only
  for type in a b
  do
      if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.relax_ssh.${type} ]
      then
        ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.relax_ssh.${type} relax.ssh.${type}
        echo "relax.ssh.${type} copied" 
      else
        $USHrtofs/${RUN}_abort.sh "Missing SSH Relaxation files" \
          "ABNORMAL EXIT FORECAST_PRE: NO FILE for relax.ssh.${type}" 2
      fi
   done

if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.regional.cice.r ]
then
  ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.regional.cice.r regional.cice.r
  echo "regional.cice.r copied" 
else
  $USHrtofs/${RUN}_abort.sh "Missing Topography files" \
    "ABNORMAL EXIT FORECAST_PRE: NO FILE for regional.cice.r" 2
fi
if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.forcing.offlux.a ] \
  && [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.forcing.offlux.b ] 
then
  ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.forcing.offlux.a  forcing.offlux.a 
  ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.forcing.offlux.b  forcing.offlux.b 
  echo "forcing.offlux.[ab] copied" 
else
  $USHrtofs/${RUN}_abort.sh "Missing Topography files" \
    "ABNORMAL EXIT FORECAST_PRE: NO FILE for forcing.offlux.[ab]" 2
fi

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

  # Prepare sea ice forcing
  $USHrtofs/${RUN}_iceforcing.sh

# 4. Copy output in archive

  cp -p $DATA/listflx.dat ${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.listflx.dat
  for fil in airtmp glbrad lwdflx precip presur radflx shwflx surtmp vapmix wndewd wndnwd wndspd 
  do
    for type in a b
    do
      if [ -s forcing.${fil}.${type} ]
      then
        cp -p forcing.${fil}.${type} ${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.forcing.${fil}.${type} 
        echo "forcing.${fil}.${type} copied" 
          if [ $SENDDBN = YES ]
          then
            if [[ ${type} = a ]]; then
              $DBNROOT/bin/dbn_alert MODEL RTOFS_GLO_FORCINGA $job \
              ${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.forcing.${fil}.${type}
            else
              msg="File${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.forcing.${fil}.${type} not posted to db_net."
              postmsg "$msg"
            fi
          fi
       else
        $USHrtofs/${RUN}_abort.sh "Missing Fix Forcing File" \
          "ABNORMAL EXIT FORECAST: NO FILE for forcing.${fil}.${type}" 2
      fi
    done
  done
  for fil in airtmp glbrad lwdflx vapmix wndewd wndnwd
  do
    for type in r B
    do
      if [ -s cice.${fil}.${type} ]
      then
        cp -p cice.${fil}.${type} ${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.cice.${fil}.${type} 
        echo "forcing.${fil}.${type} copied" 
          if [ $SENDDBN = YES ]
          then
            if [[ ${type} = r ]]; then
              $DBNROOT/bin/dbn_alert MODEL RTOFS_GLO_CICER $job \
              ${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.cice.${fil}.${type}
            else
              msg="File ${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.cice.${fil}.${type} not posted to db_net."
              postmsg "$msg"
            fi
          fi
      else
        $USHrtofs/${RUN}_abort.sh "Missing cice Forcing File" \
          "ABNORMAL EXIT FORECAST: NO FILE for cice.${fil}.${type}" 2
      fi
    done
  done

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
