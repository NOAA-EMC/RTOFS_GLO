#!/bin/sh

if [ $# -lt 1 ]
then
  whattodo=compile
else
  whattodo=$1
fi

case $whattodo in
  compile)
module purge
module use ../../modulefiles 
module load build_rtofs_hycom.module
module list
cd src_2.2.99DHMTi-dist2B_relo_cice_v4.0e
csh comp_ice.csh > ../rtofsglo.compile.hycom.log 2>&1
;;

  clean)
cd src_2.2.99DHMTi-dist2B_relo_cice_v4.0e
csh clean_ice.csh 
;;

  install)
mkdir -p ../../exec
cp src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/hycom_cice ../../exec/rtofs_hycom
;;
esac


