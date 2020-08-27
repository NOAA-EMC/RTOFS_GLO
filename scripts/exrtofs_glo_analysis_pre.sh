#!/bin/sh
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_analysis_pre.sh                            #
# Script description:                                                         #
#                                                                             #
# Author:        Ilya Rivin      Org: NP23         Date: 2010-07-30           #
#                                                                             #
# Abstract: This script generates input fields for                            #
#           for the RTOFS_GLO Ocean model analysis.                           #
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

msg="RTOFS_GLO_ANALYSIS_PRE JOB has begun on `hostname` at `date`"
postmsg "$jlogfile" "$msg"

# --------------------------------------------------------------------------- #
# 0. date and time stuff

  export fcstdays=${fcstdays:-2}
  export enddate=${analysis_end:-$PDY}
# startdate for V2 is 24 hours, but get 48 hours for NCODA purposes (and bug on first record of forcings)
  export startice=`$NDATE -\` expr $fcstdays \* 24 \`  ${enddate}'00' | cut -c1-8`
  export iday=`$USHrtofs/rtofs_date_normal2hycom.sh $startice$mycyc`
  export startdate=`$NDATE -\` expr $fcstdays \* 48 \`  ${enddate}'00' | cut -c1-8`
  export inputgrid=${inputgrid:-navy_0.08}

# --------------------------------------------------------------------------- #
# 1  Set up the start time and end time for the analysis
  sday=`$USHrtofs/rtofs_date_normal2hycom.sh $startdate$mycyc`
  eday=`$USHrtofs/rtofs_date_normal2hycom.sh $enddate$mycyc`
  echo "  $sday $eday false false  " > limits

# --------------------------------------------------------------------------- #
# 2. Do staging
  $USHrtofs/${RUN}_prestaging.sh 

#################################################
msg="THE RTOFS_GLO_ANALYSIS_PRE JOB HAS ENDED NORMALLY on `hostname` at `date`"
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
