#!/bin/bash

# What does this do?
# 1. Get versions of modules listed at a path.
# 2. Load them into the environment.
#

if [[ $# -lt 1 ]]; then
  echo " "
  echo "Usage: "
  echo "$0" "Path to RTOFS_GLO directory"
  echo " "
  echo "Example Inputs: "
  echo " /u/santha.akella/my_ptmp/test1/RTOFS_GLO/"
  echo " "
  echo " "
  exit 1
fi
echo " "

set -eu

RTOFS_GLO_path=${1}

source ${RTOFS_GLO_path}/versions/build.ver

set +x
module purge
module load envvar/${envvar_ver}
module load intel/${intel_ver}
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}
module load cray-mpich/${cray_mpich_ver}
module load cray-libsci/${cray_libsci_ver}

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
set -x

