#!/bin/bash

if [[ $# -lt 1 ]]
then
  echo " "
  echo "Usage: "
  echo "$0" "machine_name, path_to_input_file, date"
  echo " "
  echo "Allowed machine names: dwood (dogwood), cac (cactus)"
  exit 1
fi

machName=$1
inputPath=$2
dataDate=$3

# defaults
topog_file="/lfs/h2/emc/couple/noscrub/santha.akella/data_files/v2.5/topog/depth_GLBb0.08_09m11.nc"

# load module(s) that provide python packages (xarray)
source set_py_modules.sh ${machName}

# work or scratch directory
cwd=$(pwd)
if [ -e ${cwd}/scratch ]
then
  /usr/bin/rm -rf ${cwd}/scratch
fi
/usr/bin/mkdir -p ${cwd}/scratch
echo "Output will be in a new scratch directory: "
echo ${cwd}"/scratch"

# Check if there is topography file (needed) that is readable
if [ -e ${topog_file} ]
then
# echo "Found topography file at: "${topog_file}
  echo " "
else
  echo "A topography file is needed for this to work."
  echo "See /lfs/h2/emc/couple/noscrub/santha.akella/data_files/v2.5/topog/README_get_GLBb_topo.md for details."
fi

# convert format of file: bin to netcdf 
#python convert_bin_nc_format.py ${dataDate} ${inputPath}

