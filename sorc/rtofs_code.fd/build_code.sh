#!/bin/sh

mkdir -p ../../exec

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

module load bacio/${bacio_ver}
module load bufr/${bufr_ver}
module load g2/${g2_ver}
module load sigio/${sigio_ver}
module load sp/${sp_ver}
module load w3nco/${w3nco_ver}

module load jasper/${jasper_ver}
module load libpng/${libpng_ver}
module load zlib/${zlib_ver}
module load netcdf/${netcdf4_ver}
module load hdf5/${hdf5_ver}

module list

make > rtofsglo.compile.code.log 2>&1

