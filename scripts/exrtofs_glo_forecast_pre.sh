#!/bin/sh
set -xa

export PS4='$SECONDS + '

cd $DATA

msg="RTOFS_GLO_FORECAST_PRE JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

# --------------------------------------------------------------------------- #
# 1. Set up the start and end time
# Get atmospheric forcings for 4 days
  export forcingDAYS=4

  export fcstdays=${fcstdays:-${forcingDAYS}}
  export enddate=$($NDATE $(expr $fcstdays \* 24) ${startdate})
  export startdate=${startdate:-${PDY}${mycyc}}

# --------------------------------------------------------------------------- #
# 2. Do staging

  $USHrtofs/${RUN}_prestaging.sh 

#################################################
msg="THE RTOFS_GLO_FORECAST_PRE JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"

################## END OF SCRIPT #######################
