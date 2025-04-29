#!/bin/bash

if [[ $# -lt 3 ]]; then
  echo " "
  echo "Usage: "
  echo "$0" "machine_name, path_to_input_files, date"
  echo " "
  echo "Allowed machine names: dwood (dogwood), cac (cactus)"
  echo " "
  echo "Example of path to input files: "
  echo "/lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR1/prod/com/rtofs/v2.5/"
  echo " "
  echo "Date example: 20250331"
  echo " "
  echo "Variable name, options: icecov or icethk or icetmp or mixlyr"
  echo " "
  echo "Type of field: inc (increment) or fld (full field)?"
  echo " "
  exit 1
fi
echo " "

machName=$1
inputPath=$2
dataDate=$3
vName=$4
fType=$5

# defaults
topog_file="/lfs/h2/emc/couple/noscrub/santha.akella/data_files/v2.5/topog/depth_GLBb0.08_09m11.nc"

# work or scratch directory
cwd=$(pwd)
if [[ -e ${cwd}/scratch_${dataDate} ]]; then
  /usr/bin/rm -rf ${cwd}/scratch_${dataDate}
fi
/usr/bin/mkdir -p ${cwd}/scratch_${dataDate}
echo "Output will be saved in a new scratch directory: "
echo ${cwd}"/scratch_${dataDate}"

# Check if there is topography file (needed) that is readable
if [[ -e ${topog_file} ]]; then
  echo " "
  echo "Using topography file at: "
  echo ${topog_file}
  echo " "
else
  echo "A topography file is needed for this to work."
  echo "See /lfs/h2/emc/couple/noscrub/santha.akella/data_files/v2.5/topog/README_get_GLBb_topo.md for details."
  exit 2
fi

# load module(s) that provide python packages (xarray)
source ${cwd}/set_py_modules.sh ${machName}

# convert format of file: bin to netcdf 
${cwd}/./convert_bin_inc_to_nc.py --proc_date ${dataDate} \
                                  --data_path_pref ${inputPath} \
                                  --output_path ${cwd}/scratch_${dataDate} \
                                  --var_name ${vName} \
                                  --file_type ${fType}
err=$?
if [[ ${err} -ne 0 ]]; then
    exit "${err}"
fi

ncFileCount=$(find ${cwd}/scratch_${dataDate}/*.nc | wc -l)
if [[ ${ncFileCount} -gt 0 ]]; then
  echo " "
  echo "All done!"
  exit 0
else
  echo "Something went wrong, check output or logs and try again."
  exit 3
fi
