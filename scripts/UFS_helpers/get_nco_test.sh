#!/bin/bash

if [[ $# -lt 4 ]]; then
  echo " "
  echo " Get data from parallel run by NCO."
  echo " "
  echo "Usage: "
  echo "$0" "start_date num_days output_path_base file_type"
  echo " "
  echo " "
  echo "Example inputs: "
  echo "2025-05-29 8 v2p5 /lfs/h2/emc/ptmp/santha.akella/data/rtofs rtofs_glo.t00z.n00.archs."
  echo " "
  echo "Note: Data is NOT fetched for start_date, begin a day before, if needed."
  echo " "
  exit 1
fi
echo " "

set -ux
    
start_date="$1" #"2025-05-29"
num_days="$2" #8

system_name="v2p5_nco"

output_path_base="$4" #/lfs/h2/emc/ptmp/santha.akella/data/rtofs
file_type="$5" #rtofs_glo.t00z.n00.archs.
# --

nco_para_path=/lfs/h1/ops/para/com/rtofs/v2.5/

CMD1=cp

for i in $(seq 1 ${num_days}); do
  data_date=$(date -d "$start_date + $i days" "+%Y%m%d")

  year=$(date -d "$start_date + $i days" "+%Y")
  mon=$(date -d "$start_date + $i days" "+%m")
  day=$(date -d "$start_date + $i days" "+%d")

  nco_file1=${nco_para_path}/rtofs.${data_date}/${file_type}a
  nco_file2=${nco_para_path}/rtofs.${data_date}/${file_type}b

  echo " "
  echo "Fetching data from NCO para for..." ${data_date}
  echo " "

  output_path=${output_path_base}/${system_name}/${data_date}
  mkdir -p ${output_path}
  cd ${output_path}

  $CMD1 ${nco_file1} .
  $CMD1 ${nco_file2} .
  cd -

done

echo " "
exit 0
