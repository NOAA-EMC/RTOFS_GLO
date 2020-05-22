#!/bin/sh
set -x

# replace RTOFS_LIB with the location of the rtofs libraries (eg librtofs_hycomiot_4.a)
export RTOFS_LIB=/location/of/rtofs_lib/libraries
export RTOFS_LIB_INC=$RTOFS_LIB/incmod

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

make > rtofsglo.compile.log 2>&1

