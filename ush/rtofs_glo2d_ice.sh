#!/bin/sh

if [[ $# -lt 3 ]]; then
  echo " "
  echo "Usage: "
  echo "$0" "topog_file ncoda_2d_file variable_name"
  echo " "
  echo "Example input: "
  echo "topog_file:  depth_GLBb0.08_09m11ob2_mom6.nc"
  echo "ncoda_2d_file: icecov_20251215_analfld"
  echo "variable_name, options: icecov, icethk, icetmp, mixlyr"
  echo " "
  exit 1
fi
echo " "

set -ux

msg="$(basename -- "$0") JOB has begun on $(hostname) at $(date)"
#postmsg "$msg"

# Allowed variable names
allowed_variables=("icecov" "iceth" "icetmp" "mixlyr")

is_not_allowed=true
# --------------------------------------------------------------------------- #

# Check for validity of inputs
topog_file=$1
ncoda_2d_file=$2
var_name=$3

# Is there is a topography file in the "fix/", it is needed.
if [[ ! -f "${topog_file}" ]]; then
  echo "A topography file is needed for this script to work."
  echo "Check in ${FIXrtofs} for depth_*.nc"
  exit 1
fi

# Ice coverage file
if [[ ! -f "${ncoda_2d_file}" ]]; then
  echo "A 2-D NCODA binary file is needed for this script to work."
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
# --------------------------------------------------------------------------- #

# Load module(s) that provide python packages (such as xarray)
source ${HOMErtofs}/scripts/load_py_modules.sh

#xx.py ${topog_file} ${ncoda_2d_file} ${var_name} > output.log 2>&1
err=$?; export err ; err_chk
echo " error from xx=",$err

msg="THE $(basename -- "$0") JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"
