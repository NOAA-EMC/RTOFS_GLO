#!/bin/sh

#NOTE:############################################################################
##  1. Get ice concentration L1b data from bufr tank.
##  2. Run ssmis_tol2 to create Level 2 NetCDF file.   
##  2. Run readssmisu to create input files for ncoda_qc 
##  Hao,Y  IMSG AT EMC/NCEP/NOAA  03/26/2019
###################################################################################
echo "*** Started script $0 on hostname "`hostname`' at time '`date`
set -x

mkdir -p $DATA/ice_nc
cd $DATA/ice_nc
# set DATA to the directory where the data is to dumped to (for dumpjb)
export DATA=$DATA/ice_nc
export TMPDIR=$DATA/ice_nc

dtyp=ssmisu
HH=12
interval=11.999
export DUPC="off" #turn off duplicate check

# run for two days
for rundate in ${PDYm1} ${PDYm2}
do

rm -f fort.52 l2out.f285.51.nc $dtyp.ibm $dtyp.out l2out.f285.51.${rundate}00.nc

echo Running bufr decoder for $rundate
$DUMPJB $rundate$HH $interval $dtyp

#input data must be in fortran unit 11
  export FORT11="$dtyp.ibm" 
  ${EXECrtofs}/rtofs_ssmis_tol2 >>$pgmout 2>errfile
  err=$? ; export err ; err_chk
  mv l2out.f285.51.nc l2out.f285.51.${rundate}00.nc

done

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`

exit 0

