#!/bin/bash

if [[ $# -lt 5 ]]; then
  echo " "
  echo "Usage: "
  echo "$0" "start_date num_days system_name output_path_base file_type"
  echo " "
  echo " "
  echo "Example inputs: "
  echo "2025-03-31 10 v2p4 /lfs/h2/emc/ptmp/santha.akella/data/rtofs rtofs_glo.t00z.n00.archs."
  echo " "
  echo " "
  exit 1
fi
echo " "

set -u

start_date="$1" #"2025-03-31"
num_days="$2" #10
system_name="$3" # v2p4 or v2p5
output_path_base="$4" #/lfs/h2/emc/ptmp/santha.akella/data/rtofs
file_type="$5" #rtofs_glo.t00z.n00.archs.

# RTOFS HPSS paths
# ----------------
# v2.4
v2p4_path_pref=/NCEPPROD/5year/hpssprod/runhistory/

# v2.5
v2p5_path_pref=/NCEPDEV/emc-ocean/5year/Dan.Iredell
# ----------------

CMD1=/usr/local/bin/htar
CMD2=tar

for i in $(seq 1 ${num_days}); do
  data_date=$(date -d "$start_date + $i days" "+%Y%m%d")

  year=$(date -d "$start_date + $i days" "+%Y")
  mon=$(date -d "$start_date + $i days" "+%m")
  day=$(date -d "$start_date + $i days" "+%d")

  echo " "
  echo "Fetching data from HPSS for..." ${data_date}
  echo " "

  if [[ "${system_name}" = "v2p4" ]];then
    hpss_file=${v2p4_path_pref}/rh${year}/${year}${mon}/${data_date}/com_rtofs_v2.4_rtofs.${data_date}.ab.tar
    file_to_get=./${file_type}*
  elif [[ "${system_name}" = "v2p5" ]];then
    hpss_file=${v2p5_path_pref}/EMC.rtofs.v2.5.a/rtofs.${data_date}/rtofs.ab.tar
    file_to_get=${file_type}*
  else
    echo "Exiting! Did not code for input RTOFS version: "${system_name} 
    exit 1
  fi
  #echo ${hpss_file}
  #echo ${file_to_get}

  output_path=${output_path_base}/${system_name}/${data_date}
  mkdir -p ${output_path}
  cd ${output_path}
  mkdir -p ${output_path}/TMP
  $CMD1 -xvf ${hpss_file} ${file_to_get}
  $CMD2 -xvzf ${file_type}*.tgz -C TMP
  mv ${output_path}/TMP/* ${output_path}
  rm -f *.tgz
  rmdir TMP
  cd -
done

echo " "
exit 0
