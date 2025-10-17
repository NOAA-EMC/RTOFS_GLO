#!/bin/bash

# A script to clone the UFS weather model

set -eux

MACHINE=$(hostname | cut -c 1-6)
cwd=$(pwd)

UFSpath=$cwd/../../sorc/ufs_model.fd/

if [[ ! -d "${UFSpath}" ]]; then
  echo "Error: Path: '${UFSpath}' does not exist."
  echo "Fix your clone and try again."
  exit 2
fi

#echo ""
#echo "On machine:" ${MACHINE}
#echo ""

if [[ (${MACHINE} == "clogin") || (${MACHINE} == "dlogin") ]]; then
  nJobs=1
else
  nJobs=8
fi
#echo ${nJobs}

cd ${UFSpath}
git submodule update --init --recursive --jobs ${nJobs}

echo " "
echo " "
echo "Find source code at ${UFSpath}"
echo "All done!"
exit 0
