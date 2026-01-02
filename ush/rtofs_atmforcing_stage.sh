#!/bin/sh
set -x

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)

if [ $# -ne 1 ] ; then
  echo USAGE:  "$0 <idate> "
  echo "INPUT: "
  echo "OUTPUT: "
  exit 2
fi
idate=$1

envirges=${envirges:-prod}

test -d $DATA/$idate && rm -rf $DATA/$idate ; mkdir -p $DATA/$idate 

ffile=none
for sflux in sfcflxfv3 ## sfcflx2 sfcflx
do
  ffile=$(ksh ${USHrtofs}/${RUN}_atmforcing_getges.sh -e $envirges -n ${netwk} -t ${sflux} -v $idate) 
  err=$?
  if [ $err -eq 0 ]
  then
    forcefile=$ffile
    fn1=$sflux
    break
  else
    ffile=none
  fi
done

if [ ! -s $ffile ] || [ $ffile == 'none' ]
then
  $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Atmospheric Forcing File" \
    "No Valid flux file for time $idate" 4
fi
# check validity of file
$WGRIB2 -checksum -1 $forcefile > /dev/null
err=$?
if [ $err -ne 0 ]
then
  $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Corrupted Atmospheric Forcing File " \
    "FLUX FILE $forcefile failed checksum" 4
fi
echo "forcefile $forcefile"

fflxfile=${DATA}/${idate}/${RUN}'.'$(basename $forcefile)

# Copy and convert to grib2
if [ $fn1 == 'sfcflx' ]; then
 cp -p $forcefile $fflxfile
 $CNVGRIB -g12 ${fflxfile} ${fflxfile}.grib2
 flxfile=${fflxfile}.grib2
else
 cp -p $forcefile $fflxfile
 flxfile=${fflxfile}
fi # fn1 loop

# Index the grib file
$GRB2INDEX $flxfile $flxfile.idx

test -f $flxfile.idx && rm -f $flxfile.idx 
test -f ${DATA}/${idate}/dump.grb && rm -f ${DATA}/${idate}/dump.grb 

$GRB2INDEX $flxfile $flxfile.idx

echo ${RUN_MODE}
if [[ ${RUN_MODE} == "analysis" ]]; then
  echo ${RUN_MODE}
  fName=${RUN_MODE}".t.dat"
else
  echo ${RUN_STEP}
  fName=${RUN_MODE}.${RUN_STEP}".t.dat"
fi

# Varies based on which flux file was used above
if [ $fn1 == 'sfcflx' ]; then
  echo $idate $flxfile '<' $flxfile >>${fName}
else
  echo $idate $flxfile '<' $forcefile >>${fName}
fi

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)
