#!/bin/sh 

#module purge
#odule use ../modulefiles
#odule load build_rtofs_libs_wcoss2.module
#odule list
#xit

set -x

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

 mkdir -p incmod
 rm -f incmod/rtofs*/*.mod
 rm -f librtofs*.a
 cd ./sorc/rtofs_mpi_mods
 rm -f makefile dimensions.h *.mod *.o
 cp makefile_serial makefile
 make ser
 rm -f makefile dimensions.h *.mod *.o
 cp makefile_mpi makefile
 make mpi
 rm -f makefile dimensions.h *.mod *.o
 cd ../rtofs_hycomiot
 make install
