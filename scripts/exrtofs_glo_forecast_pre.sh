#!/bin/sh
set -xa
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_forecast_pre.sh                            #
# Script description:                                                         #
#                                                                             #
# Author:        Ilya Rivin      Org: NP23         Date: 2010-07-30           #
#                                                                             #
# Abstract: This script generates the input  fields                           #
#           for the RTOFS_GLO Ocean model forecast step                       #
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             #
# Script history log:                                                         #
# 2010-07-30  Ilya Rivin                                                      #
#                                                                             #
###############################################################################

export PS4='$SECONDS + '

cd $DATA

msg="RTOFS_GLO_FORECAST_PRE JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

# --------------------------------------------------------------------------- #
# 0. date and time stuff

  export fcstdays=${fcstdays:-4}
  export startdate=${startdate:-${PDY}${mycyc}}
  startice=$startdate
  export iday=$($USHrtofs/rtofs_date_normal2hycom.sh $startice)
  export enddate=$($NDATE $(expr $fcstdays \* 24) ${startdate})
  export inputgrid=${inputgrid:-navy_0.08}

# --------------------------------------------------------------------------- #
# 1 Do staging
  $USHrtofs/${RUN}_prestaging.sh 

#################################################
msg="THE RTOFS_GLO_FORECAST_PRE JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"

################## END OF SCRIPT #######################

