#!/bin/sh

if [ $# -lt 1 ]
then
  whattodo=compile
else
  whattodo=$1
fi


case $whattodo in
  compile)
BASE=`pwd`
dir_mods="$(dirname ${BASE})"
dir_mod0="$(dirname ${dir_mods})"
echo ${dir_mod0}
source ${dir_mod0}/versions/build.ver
module purge
module load envvar/${envvar_ver}
module load intel/${intel_ver}
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}
module load cray-mpich/${cray_mpich_ver}
module load zlib/${zlib_ver}
module load netcdf/${netcdf3_ver}
module load hdf5/${hdf5_ver}
module list
cd src_2.2.99DHMTi-dist2B_relo_cice_v4.0e
csh comp_ice.csh > ../rtofsglo.compile.hycom.log 2>&1
;;

  esmf)
BASE=`pwd`
dir_mods="$(dirname ${BASE})"
dir_mod0="$(dirname ${dir_mods})"
echo ${dir_mod0}
source ${dir_mod0}/versions/build.ver
module purge
module load envvar/${envvar_ver}
module load intel/${intel_ver}
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}
module load cray-mpich/${cray_mpich_ver}
module load netcdf/${netcdf3_ver}
module list
cd esmf_4_0_0rp2;sh makeit > make.out 2>&1;cd ..
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


