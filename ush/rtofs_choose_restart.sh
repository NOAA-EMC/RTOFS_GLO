#!/bin/sh
#
# Find best available RTOFS-Global analysis restart for given date
# exit=22 means no restart is found.
#
#
set -x

echo "*** Started script $0 on hostname "`hostname`' at time '`date`

if [ $# -ne 1 ] ; then
  echo "USAGE: $0 <YYYYMMDDHH>"
  exit -1
fi
YMDH=$1
##-
##- Specify if running independenly
##-
##- envir=para
##- NET=rtofs
##- RUN=rtofs
##- modID=glo
##- utilexec=/nwprod/util/exec
##- HOMErtofs=${HOMErtofs:-/glocean/save/$LOGNAME/hycom_glo/projects/$projID}
##- EXECrtofs=${HOMErtofs}/exec
##- FIXrtofs=${HOMErtofs}/fix
##- PDY=`echo $YMDH | cut -c1-8`
##- HOMEout=
##- COMIN=${HOMEout}/com/${NET}/${envir}/${RUN}.${PDY}
##- GESdir=${HOMEout}/nwges/${envir}/${RUN}.${PDY}
##- DATA=/stmp/${USER}/`basename ${0%.sh}`
##- rm -rf $DATA ; mkdir -p $DATA 
##-
##- 
rlist=${DATA}/rlist
errors=${DATA}/errors
rlistYMDH=${DATA}/rlistYMDH
rlistYMDHsorted=${DATA}/rlistYMDHsorted
basetime=${basetime:-1900123100}
gridb=${FIXrtofs}/${RUN}_${modID}.navy_0.08.regional.grid.b
#
#
# Create a list of available restart files.
#
test -f $DATA/restart_b_choosen && rm -f $DATA/restart_b_choosen
test -f $rlist && rm -f $rlist
test -f $rlistYMDH && rm -f $rlistYMDH
test -f $rlistYMDHsorted && rm -f $rlistYMDHsorted
find $DATA/ -name "${RUN}_${modID}.t??z.[nf]*.restart.b" > $rlist
find `dirname $GESdir`/${RUN}.????????/ -name "${RUN}_${modID}.t??z.[nf]*.restart.b" >> $rlist
find `dirname $COMIN`/${RUN}.????????/ -name "${RUN}_${modID}.t??z.[nf]*.restart.b" >> $rlist
test -s $rlist || exit 22
for restb in `cat $rlist`
do
  if [ -s ${restb%.b}.a ] 
  then
    rdate=`awk '{  if (NR==2) { print $5 } }'  < $restb | cut -d. -f1 `
    dater=`$utilexec/ndate \` expr $rdate \* 24 \`  ${basetime}`
    test $dater -eq $YMDH && \
      echo "$restb" `basename ${restb} | cut -d. -f3 | cut -c2- ` 1>> $rlistYMDH
  fi
done
test -s $rlistYMDH || exit 22
sort -nb +1 $rlistYMDH | cut -d' ' -f1 > ${rlistYMDH}sorted
##rm -f $rlist ; rm -f $rlistYMDH
#
# Find the best restart file with correct range.
# NOTE: The  corresponding check in restart.f subroutine
#       in the HYCOM code is of teh type
#       (abs(hmina(k)-hminb).gt.abs(hminb)*1.e-4 
#       So we're comparing .a vs .b files upto 5th digit.
#
IDM=`head -1 $gridb | awk '{print $1}'`
JDM=`head -2 $gridb | tail -1 | awk '{print $1}'`
RestartFound='NO'
for restb in `cat ${rlistYMDH}sorted`
do
  diffs=$DATA/diffs_`basename ${restb%.b}`
  test -f $DATA/hycom_range_stdout && rm -f $DATA/hycom_range_stdout
  test -f $DATA/hycom_range_a && rm -f $DATA/hycom_range_a
  test -f $DATA/hycom_range_b && rm -f $DATA/hycom_range_b
  test -f $diffs && rm -f $diffs
  time $EXECrtofs/${RUN}_hycom_range ${restb%.b}.a $IDM $JDM \
    | awk '{printf ( "%16.6E%16.6E\n", $4, $5)}' \
    > $DATA/hycom_range_stdout
  export err=$?; err_chk
  test $err -eq 0 || exit 22
  nl=`wc $DATA/hycom_range_stdout | awk '{print $1-3}'`
  head -${nl} $DATA/hycom_range_stdout > $DATA/hycom_range_a
  cut -c40-71 $restb | tail +3 | awk '{printf ( "%16.6E%16.6E\n", $1, $2)}' \
    > $DATA/hycom_range_b
  diff $DATA/hycom_range_a $DATA/hycom_range_b > $diffs
  if [ `cat $diffs | wc -l` -eq 0 ]
  then
    echo $restb > $DATA/restart_b_choosen
    RestartFound='YES'
    break
  fi
done
if [ ${RestartFound} = 'YES' ]
then
  exit 0
else
  exit 22
fi

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
