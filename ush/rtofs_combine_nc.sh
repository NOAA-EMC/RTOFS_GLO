#!/bin/sh

#=======================================================================
# Script name: rtofs_combine_nc.sh
# Purpose: Combines NetCDF output files written out by
#          different processors using an IO_LAYOUT > 1.
#=======================================================================

# Input argument check (3 or 4 arguments only)
if [[ $# -lt 3 || $# -gt 4 ]]; then
  echo " "
  echo "Usage: "
  echo "$0 [is_output] path_to_input_files input_file_prefix output_file_name"
  echo " "
  echo "  is_output (optional): True (default) or False"
  echo " "
  echo "Example inputs: "
  echo "  is_output:           False"
  echo "  path_to_input_files: /lfs/h2/emc/.../comin"
  echo "  input_file_prefix:   20250502.000000.MOM.res.nc"
  echo "  output_file_name:    20250502.000000.MOM.res.nc"
  echo " "
  exit 1
fi
echo " "

# Parse arguments based on count
if [[ $# -eq 3 ]]; then
  is_output="True"  # Default value
  path_to_input_files=$1
  input_file_prefix=$2
  output_file_name=$3
elif [[ $# -eq 4 ]]; then
  is_output=$1
  path_to_input_files=$2
  input_file_prefix=$3
  output_file_name=$4
fi

set -x
#export PS4='$SECONDS + '

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)

# --------------------------------------------------------------------------- #

echo "---------------------------------------------------"
echo "  IS_OUTPUT:  $is_output"
echo "  INPUT_PATH: $path_to_input_files"
echo "  INPUT:      $input_file_prefix"
echo "  OUTPUT:     $output_file_name"
echo "---------------------------------------------------"

# Determine combine_args based on is_output
# Using ,, to convert string to lowercase for safe comparison
if [[ "${is_output,,}" == "true" || "${is_output}" == ".true." ]]; then
  combine_args="-n4"
else
  combine_args="-h 16384 -m"
fi

echo "  FLAGS:     ${combine_args}"
echo "---------------------------------------------------"

# Safety checks
if [ ! -d "${path_to_input_files}" ]; then
  echo "FATAL ERROR: Input directory ${path_to_input_files} does not exist!"
  exit 2
fi

if [ ! -x "${EXECrtofs}/mppnccombine" ]; then
  echo "FATAL ERROR: mppnccombine executable not found in ${EXECrtofs}!"
  exit 3
fi

# Check if input files exist
shopt -s nullglob
input_files=("${path_to_input_files}/${input_file_prefix}".*)
shopt -u nullglob # Disable it immediately so it doesn't affect other commands

if [ ${#input_files[@]} -eq 0 ]; then
  echo "FATAL ERROR: No input files found matching ${path_to_input_files}/${input_file_prefix}.*"
  exit 4
fi

# Combine files
echo "Combining ${#input_files[@]} files..."
${EXECrtofs}/mppnccombine ${combine_args} ${output_file_name} ${path_to_input_files}/${input_file_prefix}.*

# Check Status
EXIT_STATUS=$?
if [ $EXIT_STATUS -ne 0 ]; then
  echo "FATAL ERROR: mppnccombine failed with exit code $EXIT_STATUS"
  exit 5
elif [ ! -s "${output_file_name}" ]; then
  echo "FATAL ERROR: mppnccombine succeeded, but ${output_file_name} is missing or has 0 bytes!"
  exit 6
else
  echo "SUCCESS: Created ${output_file_name}"
  ls -lh "${output_file_name}"
fi

# --------------------------------------------------------------------------- #

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)
exit 0
