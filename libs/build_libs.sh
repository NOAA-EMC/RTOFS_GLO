#!/bin/sh 

# available options build|clean|install|all
# build is equivalent to install (both do build/install)
# all does clean/build
# default is all

if [ $# -lt 1 ]
then
  whattodo=all
else
  whattodo=$1
fi

# clean
if [[ $whattodo == clean || $whattodo == all || $whattodo == build || $whattodo == install ]]
then
 echo $whattodo
 cd ./sorc/rtofs_mpi_mods
 make clean
 cd ../..
 cd ./sorc/rtofs_hycomiot
 make clean
 cd ../..
fi

# build/all/install
if [[ $whattodo == build || $whattodo == all || $whattodo == install ]]
then
BASE=`pwd`
dir_mod0="$(dirname ${BASE})"
echo ${dir_mod0}

source ${dir_mod0}/versions/build.ver

module purge
module load envvar/${envvar_ver}
module load intel/${intel_ver}
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}
module load cray-mpich/${cray_mpich_ver}
module load bacio/${bacio_ver}
module load w3nco/${w3nco_ver}
module load g2/${g2_ver}
module list

 echo $whattodo
 cd ./sorc/rtofs_mpi_mods
 make ser
 make mpi
 cd ../..
 cd ./sorc/rtofs_hycomiot
 make
fi

exit
