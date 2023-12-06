#!/bin/sh
#NOTE:############################################################################
##  This script gets the sub surface data from the bufr tank and then
##  writes out dump text files
##  1. use DUMPJB to dump BUFR data to local directory
##  this script should be followed by running a decoder.
##  Y. Hao  IMSG AT EMC/NCEP/NOAA  10/08/2018. Dump only: ZG May 2019
###################################################################################
echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)
set -x

DIROUT=$DATA/dump
DIRTMP=$DATA/tmp
mkdir -p $DIROUT
mkdir -p $DIRTMP
export TMPDIR=$DIRTMP
cd $DIRTMP

today=${PDY}00
run_dtg=${PDYm1}00

export HHback=132  # monthly tanks look back (in hours)
export HHbacks=48  # surface lookup back (in hours)
export HHbackp=132  # profile lookup back (in hours)
export HHfwd=12    # surface lookup forward (in hours)

export lkbck_obsdtg_s=$($NDATE -${HHbacks} ${run_dtg})
export lkbck_obsdtg_p=$($NDATE -${HHbackp} ${run_dtg})
export lkahd_obsdtg=$($NDATE +${HHfwd} ${run_dtg})

#start date for surface and profiles, end date
#with centertime and interval, for surface dump will be from 12+hhbacks 
#back to 12+11.99, from hour 12. 
startdate_s=${lkbck_obsdtg_s:0:8}
startdate_p=${lkbck_obsdtg_p:0:8}
enddate=${lkahd_obsdtg:0:8}

export CenterTime=12
export interval=11.999
export DUPC="off" #turn off duplicate check

#SURFACE and SURFACE RESTRICTED, go back to $startdate_s
for dtyp in shipsu ships shipub shipsb
do
  ADATE=${startdate_s}
  rm -f $dtyp.${enddate}
  rm -f $dtyp.out.${enddate}
  touch $dtyp.${enddate}
  touch $dtyp.out.${enddate}
  while [[ $ADATE -le $enddate ]]
  do
    #------------------------------------------------------------------------
    #  dump data to current running directory
    $DUMPJB ${ADATE}$CenterTime $interval $dtyp
    mv $DATA/$dtyp.ibm $DIRTMP/$dtyp.ibm.$ADATE
    mv $DATA/$dtyp.out $DIRTMP/$dtyp.out1.$ADATE
    cat $DIRTMP/$dtyp.ibm.$ADATE >> $DIRTMP/$dtyp.${enddate}
    cat $DIRTMP/$dtyp.out1.$ADATE >> $DIRTMP/$dtyp.out.${enddate}
    ADATE=$ADATE'00'
    ADATE=$($NDATE +24 $ADATE | cut -c1-8)
  done # while [[ $ADATE -le  $enddate ]]
  mv $DIRTMP/$dtyp.${enddate} $DIROUT/.
  mv $DIRTMP/$dtyp.out.${enddate} $DIROUT/.
done # dtyp

#PROFILES, DAILY, go back daily to $startdate_p
for dtyp in mbuoyb dbuoyb
do
  ADATE=${startdate_p}
  rm -f $dtyp.${enddate}
  rm -f $dtyp.out.${enddate}
  touch $dtyp.${enddate}
  touch $dtyp.out.${enddate}
  while [[ $ADATE -le $enddate ]]
  do
    #------------------------------------------------------------------------
    #  dump data to current running directory
    $DUMPJB $ADATE$CenterTime $interval $dtyp
    mv $DATA/$dtyp.ibm $DIRTMP/$dtyp.ibm.$ADATE
    mv $DATA/$dtyp.out $DIRTMP/$dtyp.out1.$ADATE
    cat $DIRTMP/$dtyp.ibm.$ADATE >> $DIRTMP/$dtyp.${enddate}
    cat $DIRTMP/$dtyp.out1.$ADATE >> $DIRTMP/$dtyp.out.${enddate}
    ADATE=$ADATE'00'
    ADATE=$($NDATE +24 $ADATE | cut -c1-8)
  done # while [[ $ADATE -le  $enddate ]]
  mv $DIRTMP/$dtyp.${enddate} $DIROUT/.
  mv $DIRTMP/$dtyp.out.${enddate} $DIROUT/.
done # dtyp

#PROFILES, monthly, go back to $startdate_p
ADATE=${startdate_p}
export CenterTime=12
export interval=$HHback
for dtyp in subpfl bathy tesac xbtctd
do
  ADATE=$enddate
  #------------------------------------------------------------------------
  #  dump data to current running directory
  echo $dtyp
  $DUMPJB $ADATE$CenterTime $interval $dtyp
  mv $DATA/$dtyp.ibm $DIRTMP/$dtyp.$ADATE
  mv $DATA/$dtyp.out $DIRTMP/$dtyp.out.$ADATE
  mv $DIRTMP/$dtyp.$ADATE $DIROUT/.
  mv $DIRTMP/$dtyp.out.$ADATE $DIROUT/.
done #dtyp

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)

exit 0
