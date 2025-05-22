#!/bin/bash

if [[ $# -lt 3 ]]; then
  echo " "
  echo "Usage: "
  echo "$0" "machine_name system_name path_to_input_files variable_name start_date num_days save_plots"
  echo " "
  echo " "
  echo "Example inputs: "
  echo "cac v2p4 /lfs/h2/emc/ptmp/santha.akella/data/arch2nc/v2p4 SSH 2025-03-31 10 yes/no"
  echo " "
  echo " "
  echo "Note: "
  echo "- Data is NOT fetched for start_date, begin a day before, as needed."
  echo "- Saving plots will (of course) slow down."
  echo " "
  echo " "
  exit 1
fi
echo " "

set -ux

fn () {
  find . -wholename "$1" -print
}

machName=$1
system_name=$2
input_data_path=$3
varName=$4
start_date=$5
num_days=$6
make_plots=$7

if [[ ${make_plots} == "yes" ]]; then
  extra_args="--gen_plot"
else
  extra_args=""
fi

# Check if the input directory exists
if [[ ! -d "${input_data_path}" ]]; then
  echo "Error: Directory '${input_data_path}' does not exist. Fix and try again."
  exit 2
fi

cwd=$(pwd)
# load module(s) that provide python packages (xarray)
source ${cwd}/set_py_modules.sh ${machName}

# work or scratch directory
output_data_path=${cwd}/scratch_${system_name}/${varName}
if [[ -e ${output_data_path} ]]; then
  /usr/bin/rm -rf ${output_data_path}
fi
/usr/bin/mkdir -p ${output_data_path}
echo "Output will be saved in a new scratch directory: " ${output_data_path}
echo " "

echo "Processing data for..."
for i in $(seq 1 ${num_days}); do

  year=$(date -d "$start_date + $i days" "+%Y")
  mon=$(date -d "$start_date + $i days" "+%m")
  day=$(date -d "$start_date + $i days" "+%d")

  fDate="$year-$mon-$day"
  fStr="${input_data_path}/*${varName}*${fDate}*.nc"
  fName=$(find "${input_data_path}" -wholename ${fStr})
# echo "${fName}"

  # Global
  ./diagnostics_global.py --data_file ${fName} \
                          --varName ${varName} \
                          --output_path ${output_data_path} \
                          ${extra_args} 

  # Arctic
  ./diagnostics_arctic.py --data_file ${fName} \
                          --varName ${varName} \
                          --output_path ${output_data_path} \
                          ${extra_args} 

  # Antarctic
  ./diagnostics_antarctic.py --data_file ${fName} \
                             --varName ${varName} \
                             --output_path ${output_data_path} \
                             ${extra_args} 

  # Eq Pacific
  ./diagnostics_eq_pac.py --data_file ${fName} \
                          --varName ${varName} \
                          --output_path ${output_data_path} \
                          ${extra_args} 

  # Atlantic
  ./diagnostics_atlantic.py --data_file ${fName} \
                            --varName ${varName} \
                            --output_path ${output_data_path} \
                            ${extra_args} 

  # Tropics and extratropics
  ./diagnostics_tropics.py --data_file ${fName} \
                           --varName ${varName} \
                           --output_path ${output_data_path} \
                           ${extra_args} 

  # East Pacific (cut out region)
  ./diagnostics_e_pac.py --data_file ${fName} \
                         --varName ${varName} \
                         --output_path ${output_data_path} \
                           ${extra_args} 
done
echo " "
