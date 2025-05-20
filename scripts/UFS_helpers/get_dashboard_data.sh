#!/bin/bash

fn () {
  find . -wholename "$1" -print
}

echo " "
#set -ux

data_path="/lfs/h2/emc/ptmp/santha.akella/data/arch2nc/v2p4"
varName="SSH"
start_date="2025-03-31"
num_days=10
output_path="xx" <-- create if needed
extra_args="--gen_plot"

# Check if the input directory exists
if [[ ! -d "${data_path}" ]]; then
  echo "Error: Directory '${data_path}' does not exist. Fix and try again."
  exit 1
fi

echo " "
echo "Processing data for..."
for i in $(seq 1 ${num_days}); do

  year=$(date -d "$start_date + $i days" "+%Y")
  mon=$(date -d "$start_date + $i days" "+%m")
  day=$(date -d "$start_date + $i days" "+%d")

  fDate="$year-$mon-$day"
  fStr="${data_path}/*${varName}*${fDate}*.nc"
  fName=$(find "${data_path}" -wholename ${fStr})
  echo "${fName}"

  ./diagnostics_global.py --data_file ${fName} --varName ${varName}
# ./diagnostics_global.py --data_file ${fName} --varName ${varName} ${extra_args}
# ./diagnostics_global.py --data_file ${fName} --varName ${varName} ${extra_args} --output_path ${output_path}
done
echo " "

#./diagnostics_global.py --data_file ~/my_ptmp/data/arch2nc/v2p5_SSH_2025-04-01T00:00.nc --varName SSH --gen_plot

