#!/bin/bash

if [ $# -lt 1 ]
then
  echo " "
  echo "Usage: "
  echo $0 "machine name"
  echo " "
  echo "Allowed machine names: dwood, cac"
  exit 1
fi
mach=$1

echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "Purging all modules and loading EVS python"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"


if [ ${mach} == "dwood" ] ||\
   [ ${mach} == "cac" ]
then
  export py_mod="ve/evs/2.0"
  module use /apps/dev/modulefiles/
fi

module purge
module load ${py_mod}
module list
