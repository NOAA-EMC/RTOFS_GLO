#!/bin/sh
set -xa

export PS4='$SECONDS + '

cd $DATA

msg="RTOFS_GLO_ANALYSIS_PRE JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

# --------------------------------------------------------------------------- #
# 0. date and time stuff

# Get 48 hours for NCODA (first record of forcings is not to be used)
  export forcingHRS=48

  export fcstdays=${fcstdays:-2}
  export enddate=${analysis_end:-${PDY}${mycyc}}
  export startdate=$($NDATE -$(expr $fcstdays \* ${forcingHRS}) ${enddate})

# --------------------------------------------------------------------------- #
# 1  Set up the start time and end time for the analysis

  sday=$($USHrtofs/days_past_refDate.sh $startdate)
  eday=$($USHrtofs/days_past_refDate.sh $enddate)
  echo "  $sday $eday false false  " > limits

# --------------------------------------------------------------------------- #
# 2. Do staging

  $USHrtofs/${RUN}_prestaging.sh 

#################################################
msg="THE RTOFS_GLO_ANALYSIS_PRE JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"

################## END OF SCRIPT #######################
