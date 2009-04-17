#!/bin/sh
#########################################################################
# Usage: rtofs_atmforcing_stage.sh                                      #
#                                                                       #
# Description:                                                          #
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
forcefile=`${USHrtofs}/rtofs_atmforcing_getges.sh -q -e ${envir} -n ${netwk} -t sfcflx -v $idate`
pgrbfile=`${USHrtofs}/rtofs_atmforcing_getges.sh -q -e ${envir} -n ${netwk} -t pgbges -v $idate`
echo "forcefile $forcefile"
echo "pgrbfile $pgrbfile"
flxfile=${DATA}/${idate}/${RUN}'.'`basename $forcefile`
prsfile=${DATA}/${idate}/${RUN}'.'`basename $pgrbfile`
$USHrtofs/rtofs_atmforcing_extract.sh $forcefile $pgrbfile $flxfile $prsfile
##.. cp -p $forcefile $flxfile
##.. prsfile=$pgrbfile 
${utilexec}/grbindex $flxfile $flxfile.idx
${utilexec}/grbindex $prsfile $prsfile.idx

#add atmospheric pressure
# this test assumes that atm grid resolution is constant during the run
if [ -z "$atmgds" ]
then
   export GRBFILE=$flxfile
   export IDXFILE=$flxfile.idx
   $EXECrtofs/rtofs_getkpds >>$pgmout 2>errfile
   export err=$?; err_chk
   atmgds='255 '`cat kpds.dat`
  export err=$?; err_chk
fi
# NOTE: this extraction is important if $pgrbfile file is used instead of $prsfile.
#       prs file is supposed to have only pressure field. Still, retained for 
#       safety reasons. 
rec_number=`${utilexec}/wgrib -v ${prsfile} | grep ${sea_lev_pres} | cut -c1-3`
${utilexec}/wgrib -d ${rec_number} -grib ${prsfile} -o ${DATA}/${idate}/dump.grb 
${utilexec}/copygb -g"$atmgds" -x -a -i0 ${DATA}/${idate}/dump.grb $flxfile 

rm -f $flxfile.idx 
rm -f $prsfile.idx 
rm -f ${DATA}/${idate}/dump.grb 

${utilexec}/grbindex $flxfile $flxfile.idx

echo $idate $flxfile >>t.dat 

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
