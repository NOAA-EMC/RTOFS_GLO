#!/bin/bash

###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         rtofs_ncodaqc2com.sh                                   #
# Script description:                                                         #
#                                                                             #
# Author:        Dan Iredell      Org: NP23         Date: 2011-05-30          #
#                                                                             #
# Abstract: This copies last 15 days of NCODA QC files to COMOUT  
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             # 
# Script history log:                                                         #
# XXXX-XX-XXX  Joe Dow                                                        #
#                                                                             #
###############################################################################

set -xa

echo "*** Started script $0 for $1 on hostname "$(hostname)' at time '$(date)

if [ $# -ne 1 ]
then
  echo "USAGE: $0 <data type>"
  exit 2
fi

type=$1
cd $DATA/ocnqc/$type
mkdir -p $COMOUT/ncoda/ocnqc/$type
for d in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
  backymdh=$( $EXECrtofs/rtofs_dtg -d -$d ${PDY}00 )
  backymd=${backymdh:0:8}
  if compgen -G "${backymd}*" > /dev/null
  then
     cp -p -f ${backymd}*.* $COMOUT/ncoda/ocnqc/$type
  fi
done

echo "*** Finished script $0 for $1 on hostname "$(hostname)' at time '$(date)
