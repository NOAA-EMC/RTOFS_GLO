#!/bin/bash

# A script to clone the UFS weather model

set -eux

cwd=$(pwd)
UFSpath=$cwd/../../sorc/ufs_model.fd/

if [[ ! -d "${UFSpath}" ]]; then
  echo "Error: Path: '${UFSpath}' does not exist."
  echo "Fix your clone and try again."
  exit 2
fi

cd ${UFSpath}

git submodule update --init --recursive --jobs 8

echo " "
echo " "
echo "Find source code at ${UFSpath}"
echo "All done!"
exit 0
