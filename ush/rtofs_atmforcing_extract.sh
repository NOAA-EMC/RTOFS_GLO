#!/bin/sh
#########################################################################
# Usage: rtofs_atmforcing_extract.sh  <sfluxgrb in> <sfluxgrb out> \    #
#                                         [ <pgrb in> <pgrb out> ]      #
#                                                                       #
# Description: Extract surface fields from atmospheric model output     #
#                                                                       #
# History:                                                              #
#    07-01-2010  Ilya Rivin                                             #
#########################################################################
# 
# Extracting necessary fields from GFS/GDAS forcing files.
# Output files:sfluxgrbout pgrbout
# 
set -x

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)

if [ $# -ne 4 ] && [ $# -ne 2 ] ; then
  echo USAGE:  "$0 <sfluxgrb in> <sfluxgrb out> [ <pgrb in> <pgrb out> ]"
  exit 2
fi
sfluxgrb=$1
sfluxgrbout=$2
if [ $# -eq 4 ] ; then
  pgrb=$2
  pgrbout=$4
fi 

## export EXECutil=/nwprod/util/exec

TYPEa='ave'
TYPEi='fcst'
if [ ${RUN_MODE} = "analysis" ]
then
  TYPEx='fcst'
else
  TYPEx='ave'
fi
cd $(dirname $sfluxgrbout)
#
# Extract forcing from sfluxgrb
test -f $sfluxgrbout && mv $sfluxgrbout $sfluxgrbout.$$
for stuff in  \
  "FLUX=UFLX LEVEL=sfc TYPE=$TYPEa" \
  "FLUX=VFLX LEVEL=sfc TYPE=$TYPEa" \
  "FLUX=TMP LEVEL='2 m above gnd' TYPE=$TYPEi" \
  "FLUX=SPFH LEVEL='2 m above gnd' TYPE=$TYPEi" \
  "FLUX=PRATE LEVEL=sfc TYPE=$TYPEa" \
  "FLUX=UGRD LEVEL='10 m above gnd' TYPE=$TYPEi" \
  "FLUX=VGRD LEVEL='10 m above gnd' TYPE=$TYPEi" \
  "FLUX=SHTFL LEVEL=sfc TYPE=$TYPEx" \
  "FLUX=LHTFL LEVEL=sfc TYPE=$TYPEx" \
  "FLUX=DLWRF LEVEL=sfc TYPE=$TYPEx" \
  "FLUX=ULWRF LEVEL=sfc TYPE=$TYPEx" \
  "FLUX=DSWRF LEVEL=sfc TYPE=$TYPEx" \
  "FLUX=USWRF LEVEL=sfc TYPE=$TYPEx" \
  "FLUX=TMP LEVEL=sfc TYPE=$TYPEi" \
  "FLUX=PRES LEVEL=sfc TYPE=$TYPEi" \
  "FLUX=LAND LEVEL=sfc TYPE=$TYPEi"
do 
  eval $stuff 
  $WGRIB $sfluxgrb \
     | grep ${FLUX} | grep "${LEVEL}" | grep "${TYPE}" \
     | $WGRIB  $sfluxgrb -i -grib -append \
    -o $sfluxgrbout
  err=$?; export err ; err_chk
done
#
# Extract forcing from pgrb
if [ $# -eq 4 ] ; then
  test -f $pgrbout && mv $pgrbout $pgrbout.$$
  $WGRIB $pgrb \
    | grep PRMSL \
    | $WGRIB $pgrb -i -grib -append \
    -o $pgrbout
  err=$?; export err ; err_chk
fi

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

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)
