#!/bin/sh
#
# Usage: date_from_restart.sh <restart_file.b>
# Example: date_from_restart.sh /emc1/seaspara/simulations/ncep1.3/nwprod_coarse/atl1/initial/ofs_atl.t00z.restart_out.b
#
if [ $# -lt 1 ] ; then echo "WRONG NUMBER OF PARAMETERS" ; exit 1 ; fi
nwutil=/nwprod/util/exec
basetime=1900123100   ## 1901010100
hdate=`awk '{  if (NR==2) { print $5 } }'  < $1 | cut -d. -f1 `
echo `${nwutil}/ndate \` expr $hdate \* 24 \`  ${basetime}`