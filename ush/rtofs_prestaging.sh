#!/bin/sh
set -xa

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)
export PS4='$SECONDS + '

cd $DATA

# --------------------------------------------------------------------------- #
# 1.  Set parameters depending on run mode

if [[ "${RUN_MODE}" == "analysis" ]]; then
    runmode="anal"
    runstep="${runmode}"
    runname="ANALYSIS_PRE"
  # Set intvl=3 for retrospectives when only 3 hr forcing is available.
    intvl=1  # Atmospheric forcing frequency: 1hr
else
    runmode="fcst"
    runstep="${runmode}${stepnum}"
    runname="FORECAST_PRE"
    intvl=3  # Atmospheric forcing frequency: 3hr
fi

# --------------------------------------------------------------------------- #
# 2.  Set up the start and end times for the analysis or forecast

  sday=$($USHrtofs/days_past_refDate.sh $startdate)
  eday=$($USHrtofs/days_past_refDate.sh $enddate)
  echo "  $sday $eday false false  " > limits

# --------------------------------------------------------------------------- #
# 3. Create Forcing files

  $USHrtofs/${RUN}_atmforcing.sh $startdate $enddate $intvl

# --------------------------------------------------------------------------- #
# 4. Copy output in archive -- ?? Dan: Do we need the following ??

  # temporarily retaining to remind myself of forcing file template
  #cp -p forcing.${fil}.${type} ${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.forcing.${fil}.${type} 

  # Check
  #if [[ ${type} = a ]]; then
  #  $DBNROOT/bin/dbn_alert MODEL RTOFS_GLO_FORCINGA $job \
  #  ${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.forcing.${fil}.${type}
  #else
  #  msg="File${COMOUT}/${RUN}_${modID}.${runstep}.t${mycyc}z.forcing.${fil}.${type} not posted to db_net."
  #  postmsg "$msg"
  #fi

  #$USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Fix Forcing File" \
  #"NO FILE for forcing.${fil}.${type}" 2

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)
