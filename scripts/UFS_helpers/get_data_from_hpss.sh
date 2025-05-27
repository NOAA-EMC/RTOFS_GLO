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
  echo "Note: Data is NOT fetched for start_date, begin a day before, if needed."
  echo " "
  exit 1
fi
echo " "

set -ux

start_date="$1" #"2025-03-31"
num_days="$2" #10
system_name="$3" # v2p4 or v2p5 or v2p5_bad
output_path_base="$4" #/lfs/h2/emc/ptmp/santha.akella/data/rtofs
file_type="$5" #rtofs_glo.t00z.n00.archs.

# RTOFS HPSS paths
# ----------------
# source the file containing definition of HPSS path
source rtofs_hpss_path.sh

hpss_path=$(rtofs_hpss_path "${system_name}")
echo ${hpss_path}
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

  case ${system_name} in
    v2p4)
    # version 2.4
    hpss_file=${hpss_path}/rh${year}/${year}${mon}/${data_date}/com_rtofs_v2.4_rtofs.${data_date}.ab.tar
    file_to_get=./${file_type}*
    ;;

    v2p5)
    # version 2.5
    hpss_file=${hpss_path}/EMC.rtofs.v2.5.a/rtofs.${data_date}/rtofs.ab.tar
    file_to_get=${file_type}*
    ;;
  
    v2p5_bad)
    # version 2.5 parallel discontinued on 04/24/2025
    hpss_file=${hpss_path}/rtofs.v2.5.test01/rtofs.${data_date}/rtofs.ab.tar
    file_to_get=${file_type}*
    ;;
  
    *)
    # There is no default case
    echo -n "Exiting! Did not code for input RTOFS version: "${system_name}
    exit 1
    ;;
  esac
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
