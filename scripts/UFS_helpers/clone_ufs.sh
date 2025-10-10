#!/bin/bash

# A script to clone the UFS weather model- for data atmosphere only.

if [[ $# -lt 1 ]]; then
  echo " "
  echo "Usage: "
  echo "$0" "Full path to the location where you want source code to reside"
  echo " "
  echo "Example Inputs: "
  echo " /lfs/h2/emc/ptmp/santha.akella/RTOFS_GLO/sorc dev_10oct2025_ssmi"
  echo " "
  echo " "
  exit 1
fi
echo " "

set -eux

UFSpath=${1}
UFS_branch=${2}

# hard coded for now. 
UFS_repo=git@github.com:sanAkel/ufs-weather-model.git
# Instead git@github.com:ufs-community/ufs-weather-model.git can be used
# In that case, checkout ALL (a lot of unnecessary- for data atm) submodules.

echo " "
echo " Cloning the UFS Weathe Model "
echo " "

if [[ -d "${UFSpath}" ]]; then
  echo "Error: Directory '${UFSpath}' already exists. Expected a new/clean path. Fix and try again."
  exit 2
else
  echo ""
  echo "Creating: " ${UFSpath} 
  echo ""
  mkdir -p ${UFSpath}
  cd ${UFSpath}
fi

git clone -b ${UFS_branch} ${UFS_repo}
cd ${UFSpath}/ufs-weather-model

git submodule update --init --recursive stochastic_physics CMakeModules CMEPS-interface/CMEPS MOM6-interface/MOM6 CICE-interface/CICE CDEPS-interface/CDEPS

echo " "
echo " "
echo "Find source code at ${UFSpath}"
echo "All done!"
exit 0
