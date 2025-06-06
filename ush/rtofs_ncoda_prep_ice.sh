#!/bin/sh
#
# Program Name: rtofs_ncoda_prep_ice.sh
#
# Abstract: 
#   Get ice concentration L1b data from bufr tank.
#   Run ssmis_tol2 to create Level 2 NetCDF file.   
#
# Usage: rtofs_ncoda_prep_ice.sh
#
# Executables called:
# rtofs_ssmis_tol2
#
# Input Files: bufr data type ssmisu
#
# Output Files:
# $DATA/ice_nc/l2out.f285.51.yyyymmddhh.nc
#

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)
set -x

mkdir -p $DATA/ice_nc
cd $DATA/ice_nc

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
  mv $DATA/$dtyp.ibm $DATA/ice_nc/$dtyp.$rundate.ibm
  mv $DATA/$dtyp.out $DATA/ice_nc/$dtyp.$rundate.out

# create l2out files
  pgm=rtofs_ssmis_tol2
  . prep_step
  export FORT11="$dtyp.$rundate.ibm"
  ${EXECrtofs}/rtofs_ssmis_tol2 >>$pgmout 2>errfile
  err=$? ; export err ; err_chk
  mv l2out.f285.51.nc l2out.f285.51.${rundate}00.nc
  [[ -f fort.52 ]] && mv fort.52 $rundate.fort.52

done

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)

exit 0

