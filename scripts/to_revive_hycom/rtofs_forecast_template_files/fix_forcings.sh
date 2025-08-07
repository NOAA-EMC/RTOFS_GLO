#!/bin/bash

if [[ $# -lt 1 ]]; then
  echo " "
  echo "Usage: "
  echo "$0" "start date of experiment"
  echo " "
  exit 1
fi
echo " "

module reset

export py_mod="ve/evs/2.0"
module use /apps/dev/modulefiles/
module load ${py_mod}
module list
# --

run_date=$1
#run_date=20250326

ptmp_path=/lfs/h2/emc/ptmp/santha.akella/
# --

root_path=/lfs/h2/emc/eib/noscrub/dan.iredell/forcing/
forcing_path=${root_path}/${run_date}/
exp_dir=${ptmp_path}/rtofs.${run_date}/

./rename_forcings.py --forcing_path ${forcing_path} --exp_dir ${exp_dir}
