#!/bin/sh
# 
# Extracting necessary fields from GFS/GDAS forcing files.
# Output files:sfluxgrbout pgrbout
# 
set -x

echo "*** Started script $0 on hostname "`hostname`' at time '`date`

if [ $# -ne 4 ] ; then
  echo USAGE:  "$0 <sfluxgrb in> <pgrb in> <sfluxgrb out> <pgrb out>"
  exit 2
fi
sfluxgrb=$1
pgrb=$2
sfluxgrbout=$3
pgrbout=$4
export utilexec=/nwprod/util/exec
cd `dirname $sfluxgrbout`
#
# Extract forcing from sfluxgrb
test -f $sfluxgrbout && mv $sfluxgrbout $sfluxgrbout.$$
for stuff in  \
  "FLUX=UFLX LEVEL=sfc" \
  "FLUX=VFLX LEVEL=sfc" \
  "FLUX=TMP LEVEL='2 m above gnd'" \
  "FLUX=SPFH LEVEL='2 m above gnd'" \
  "FLUX=PRATE LEVEL=sfc" \
  "FLUX=UGRD LEVEL='10 m above gnd'" \
  "FLUX=VGRD LEVEL='10 m above gnd'" \
  "FLUX=SHTFL LEVEL=sfc" \
  "FLUX=LHTFL LEVEL=sfc" \
  "FLUX=DLWRF LEVEL=sfc" \
  "FLUX=ULWRF LEVEL=sfc" \
  "FLUX=DSWRF LEVEL=sfc" \
  "FLUX=USWRF LEVEL=sfc" \
  "FLUX=TMP LEVEL=sfc" \
  "FLUX=PRES LEVEL=sfc" \
  "FLUX=LAND LEVEL=sfc"
do 
  eval $stuff 
  $utilexec/wgrib $sfluxgrb \
     | grep ${FLUX} | grep "${LEVEL}" \
     | $utilexec/wgrib  $sfluxgrb -i -grib -append \
    -o $sfluxgrbout
done
#
# Extract forcing from pgrb
test -f $pgrbout && mv $pgrbout $pgrbout.$$
$utilexec/wgrib $pgrb \
   | grep PRMSL \
   | $utilexec/wgrib $pgrb -i -grib -append \
   -o $pgrbout


#   kpds567=reshape (source=  &
#  &         (/124,  1 , 0    &  ! UFLX
#  &         ,125,   1 , 0    &  ! VFLX
#  &         , 11, 105 , 2    &  ! TMP 2m
#  &         , 51, 105 , 2    &  ! SPFH 2m
#  &         , 59,   1 , 0    &  ! PRATE
#  &         , 33, 105 ,10    &  ! UGRD 10m
#  &         , 34, 105 ,10    &  ! VGRD 10m
#  &         ,122,   1 , 0    &  ! SHTFL
#  &         ,121,   1 , 0    &  ! LHTFL
#  &         ,205,   1 , 0    &  ! DLWRF
#  &         ,212,   1 , 0    &  ! ULWRF
#  &         ,204,   1 , 0    &  ! DSWRF
#  &         ,211,   1 , 0    &  ! USWRF
#  &         , 11,   1 , 0    &  ! TMP
#  &         ,  1,   1 , 0    &  ! PRES
#  &         , 81,   1 , 0 /) &  ! LAND
#  &         ,shape = (/ 3,nmrf /) )  

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
