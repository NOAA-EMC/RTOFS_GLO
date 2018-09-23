#!/bin/sh
#########################################################################
# Usage: rtofs_atmforcing_stage.sh  idate                               #
#                                                                       #
# Description: Stage input data for the atmosphere-ocean interpolator.  #
#                                                                       #
# History:                                                              #
#    04-08-2009   Ilya Rivin                                            #
#########################################################################
set -x

echo "*** Started script $0 on hostname "`hostname`' at time '`date`

if [ $# -ne 1 ] ; then
  echo USAGE:  "$0 <idate> "
  echo "INPUT: "
  echo "OUTPUT: "
  exit 2
fi
idate=$1

test -d $DATA/$idate && rm -rf $DATA/$idate ; mkdir -p $DATA/$idate 

# NOTE: here is the possibility that pressure and forcing will be from different cycles. Check later.
ffile=none
for sflux in sfcflxfv3 ## sfcflx2 sfcflx
do
#dbgz
#  ffile=`ksh ${USHrtofs}/${RUN}_atmforcing_getges.sh -q -e ${envir} -n ${netwk} -t ${sflux} -v $idate` 
#  ffile=`ksh ${USHrtofs}/${RUN}_atmforcing_getges.sh -q -e prod -n ${netwk} -t ${sflux} -v $idate` 
  ffile=`ksh ${USHrtofs}/${RUN}_atmforcing_getges.sh -q -e prod -n ${netwk} -t ${sflux} -v $idate` 
  err=$?
  if [ $err -eq 0 ]
  then
    forcefile=$ffile
    fn1=$sflux
    break
  fi
done

if [ -z $ffile ] || [ $ffile == 'none' ]
then
  $USHrtofs/${RUN}_abort.sh "Missing Atmosperic Forcing File" \
    "ABNORMAL EXIT FORECAST: NO FILE sfcflx at $idate" 4
 
fi
echo "forcefile $forcefile"

fflxfile=${DATA}/${idate}/${RUN}'.'`basename $forcefile`
if [ $useslp = YES ] 
then
#  pgrbfile=`ksh ${USHrtofs}/${RUN}_atmforcing_getges.sh -q -e ${envir} -n ${netwk} -t pgbges -v $idate`
  pgrbfile=`ksh ${USHrtofs}/${RUN}_atmforcing_getges.sh -q -e prod -n ${netwk} -t pgbges -v $idate`
  echo "pgrbfile $pgrbfile"
  prsfile=${DATA}/${idate}/${RUN}'.'`basename $pgrbfile`
  $USHrtofs/${RUN}_atmforcing_extract.sh $forcefile $flxfile $pgrbfile $prsfile
else
if [ $fn1 == 'sfcflx' ]; then
cp -p $forcefile $fflxfile
/nwprod/util/exec/cnvgrib -g12 ${fflxfile} ${fflxfile}.grib2
flxfile=${fflxfile}.grib2
else
cp -p $forcefile $fflxfile
flxfile=${fflxfile}
fi # fn1 loop
fi # useslp loop
##cp -p $forcefile $fflxfile
##.. prsfile=$pgrbfile 
${EXECutil}/grbindex $flxfile $flxfile.idx
if [ $useslp = YES ] 
then
  ${EXECutil}/grbindex $prsfile $prsfile.idx
fi
# Shift grid 

#dbgz
#####> flxfile1=${DATA}/${idate}/${RUN}'.'`basename $forcefile`
#####> cp -p $flxfile $flxfile1
#####> time ${EXECutil}/copygb -g"255 0 4320 2180 89990 42 128 -89990 359958 83 83 00" -x -i -o $flxfile $flxfile1
#####> time ${EXECutil}/copygb -g"255 0 2647 1324 89980 00 128 -89980 -136 136 136 64" -x -i -o $flxfile $flxfile1
#####> ${EXECutil}/grbindex $flxfile1 $flxfile1.idx


#add atmospheric pressure
# this test assumes that atm grid resolution is constant during the run
if [ $useslp = YES ] 
then
  if [ -z "$atmgds" ]
  then
     export GRBFILE=$flxfile
     export IDXFILE=$flxfile.idx
     export pgm="${RUN}_getkpds"
     . prep_step
     startmsg
     $EXECrtofs/${RUN}_getkpds >>$pgmout 2>errfile
     err=$?; export err ; err_chk
     echo " error from ${RUN}_getkpds=",$err

     atmgds='255 '`cat kpds.dat`
    export err=$?; err_chk
  fi
  # NOTE: this extraction is important if $pgrbfile file is used instead of $prsfile.
  #       prs file is supposed to have only pressure field. Still, retained for 
  #       safety reasons. 
  rec_number=`${EXECutil}/wgrib -v ${prsfile} | grep ${sea_lev_pres} | cut -c1-3`
  ${EXECutil}/wgrib -d ${rec_number} -grib ${prsfile} -o ${DATA}/${idate}/dump.grb 
  ${EXECutil}/copygb -g"$atmgds" -x -a -i0 ${DATA}/${idate}/dump.grb $flxfile 
fi

test -f $flxfile.idx && rm -f $flxfile.idx 
test -f $prsfile.idx && rm -f $prsfile.idx 
test -f ${DATA}/${idate}/dump.grb && rm -f ${DATA}/${idate}/dump.grb 

${EXECutil}/grbindex $flxfile $flxfile.idx

if [ $useslp = YES ] 
then
  echo $idate $flxfile '<' $forcefile $pgrbfile >>t.dat 
else
if [ $fn1 == 'sfcflx' ]; then
  echo $idate $flxfile '<' $flxfile >>t.dat
else
  echo $idate $flxfile '<' $forcefile >>t.dat
fi # fn1 loop
fi #useslp loop

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
