#!/bin/ksh
############################################################
# This script converts the date to the day since 1901010100
# it is used by the HYCOM part of the Ocean Forecast Model
#
# Usage: date2hycomday.sh 2001010200
#
#############################################################
# Set the basetime corresponding to the date 1901010100 
btime=1900123100 

# Set the current date
cdate=`echo $1 | cut -c1-10`          

chour=`$NHOUR $cdate $btime`
cday=`expr $chour / 24`
typeset -Z2 residual=`expr $chour % 24 \* 100 / 24` 
dayhycom=${cday}.${residual}
echo $dayhycom
