#!/bin/sh

set -xa

# Script name: rtofs_combine_nc.sh
# Script description: Combines NetCDF output files output
#                     from different processors using an IO_LAYOUT > 1.

export PS4='$SECONDS + '

cd $DATA

msg="RTOFS_GLO_COMBINE_NC JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

# --------------------------------------------------------------------------- #

# -- load modules
module purge
module load PrgEnv-intel/8.1.0
module load intel/19.1.3.304
module load craype/2.7.17
module load python/3.12.0
module load hdf5/1.10.6
module load netcdf/4.7.4
module load nco/5.2.4
module load cmake/3.20.2

module list
# --
# Once [FRE-NCtools](https://github.com/NOAA-EMC/FRE-NCtools) is available as a module
# following will not be needed. Just add "module load ..."
path_to_fre="/lfs/h2/emc/couple/noscrub/santha.akella/fre-nctools_15Jan2026/bin/"


# -- Inputs
input_path="/lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR/prod/com/rtofs/v2.5/rtofs.20250503.3200.8x8/"
input_file_type="ocnp_2025_122_00.nc"
output_file="ocnp.nc"
# --


args="-v -n4" # arguments to mppnccombine

$path_to_fre/mppnccombine ${args} ${output_file} ${input_path}/${input_file_type}*
err=$?; export err ; err_chk
echo " error from mppnccombine=",$err

if [ -f "${output_file}" ]; then
  mv "${output_file}" $COMOUT/
else
  echo "WARNING: ${output_file} not found."
fi

msg="RTOFS_GLO_COMBINE_NC JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"
