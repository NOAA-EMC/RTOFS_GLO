#!/bin/sh
set -x

rm -f ../exec/*

module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163
module load bacio/2.0.2
module load w3nco/2.0.6
module load g2/3.1.0
module load grib_util/1.1.1
module load NetCDF/4.5.0
module load HDF5-serial/1.8.20
module load jasper/1.900.1
module load libpng/1.2.59
module load zlib/1.2.11

export RTOFS_LIB=/gpfs/dell2/emc/modeling/noscrub/Dan.Iredell/gt/EMC_rtofs_shared/rtofs_lib.v1.1.0
export RTOFS_LIB_INC=/gpfs/dell2/emc/modeling/noscrub/Dan.Iredell/gt/EMC_rtofs_shared/rtofs_lib.v1.1.0/incmod

make > rtofsglo.compile.log 2>&1

