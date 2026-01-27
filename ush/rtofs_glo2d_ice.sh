#!/bin/sh

if [[ $# -ne 4 ]]; then
  echo " "
  echo "Usage: "
  echo "$0" "topog_file ncoda_file variable_name output_path"
  echo " "
  echo "Example inputs: "
  echo "topog_file:    depth_GLBb0.08_09m11ob2_mom6.nc"
  echo "ncoda_file:    icecov_20251215_analfld"
  echo "variable_name: options: icecov, icethk, icetmp, mixlyr"
  echo "output_path:   /lfs/h2/emc/stmp/santha.akella/test/"
  echo " "
  exit 1
fi
echo " "

set -ux

msg="$(basename -- "$0") JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

# Allowed variable names
allowed_variables=("icecov" "icethk" "icetmp" "mixlyr")

is_not_allowed=true
# --------------------------------------------------------------------------- #

# Check for validity of inputs
topog_file=$1
ncoda_file=$2
var_name=$3
output_path=$4

# Is there is a topography file in the "fix/", it is needed.
if [[ ! -f "${topog_file}" ]]; then
  echo "A topography file is needed for this script to work."
  echo "Check in ${FIXrtofs} for depth_*.nc"
  exit 1
fi

# Ice coverage file
if [[ ! -f "${ncoda_file}" ]]; then
  echo "A NCODA binary file is needed for this script to work."
  echo "For example: icecov_yyyymmdd00_analfld"
  exit 1
fi

# Variable name
if [[ -z "${var_name}" ]]; then
  echo "Variable name (string) is empty. Valid options are: icecov, icethk, icetmp, mixlyr."
  exit 1
fi

# Loop through the  allowed variables to check for a match
for val in "${allowed_variables[@]}"; do
  if [ "${var_name}" = "$val" ]; then
    is_not_allowed=false
    break # Exit loop early if a match is found
  fi
done

# Check the flag set within the loop
if [ "$is_not_allowed" = true ]; then
    echo "Error: variable name '${var_name}' is not an allowed value."
    echo "Valid options are: icecov, icethk, icetmp, mixlyr."
    exit 2
fi

# Output path check
if [[ ! -d "${output_path}" ]]; then
  echo "Error: Output path '${output_path}' does not exist or is not a directory."
  exit 1
fi

if [[ ! -w "${output_path}" ]]; then
  echo "Error: Output path '${output_path}' is not writable. Check permissions."
  exit 1
fi
# --------------------------------------------------------------------------- #

# Load module(s) that provide python packages (such as xarray)
#source ${HOMErtofs}/scripts/load_py_modules.sh

echo "Converting format for ${var_name}..."

set +x  # Turn off tracing to keep the log clean
$USHrtofs/rtofs_convert_bin_inc_to_nc.py --topog_file "${topog_file}" \
                                         --input_file "${ncoda_file}" \
                                         --var_name   "${var_name}" \
                                         --output_path "${output_path}" 2>&1 | tee output.log

# Get the exit code of the Python script, NOT the tee command
err=${PIPESTATUS[0]}
export err
set -x  # Turn tracing back on

# Run error check
err_chk
echo " error from rtofs_convert_bin_inc_to_nc.py = $err"

msg="THE $(basename -- "$0") JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"
