#!/bin/bash

# A script to clone submodules of the UFS weather model needed for data atmosphere only.

echo " "

set -eux

UFSpath=../../sorc/ufs_code.fd

if [[ ! -d "${UFSpath}" ]]; then
  echo "Error: Directory '${UFSpath}' does not exist. Fix and try again."
  exit 1
fi
cd ${UFSpath}

echo " "
echo " Cloning submodules of the UFS Weathe Model "
echo " "

git submodule update --init --recursive stochastic_physics CMakeModules CMEPS-interface/CMEPS MOM6-interface/MOM6 CICE-interface/CICE CDEPS-interface/CDEPS

echo " "
echo " "
echo "Find source code at ${UFSpath}"
echo "All done!"
exit 0
