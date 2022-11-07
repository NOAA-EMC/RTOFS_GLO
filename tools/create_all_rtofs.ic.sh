#!/bin/bash
# This script populates the directory rtofs.$PDYm1 given $PDY
#
# Scripts called
# ./pull_lotsa_stuff.sh
# ./pull_and_create_hycom_var.sh
# ./pull_rtofs_archive_and_restart.sh
# 

if [ $# -eq 1 ]
then
  export PDY=$1
else
  echo USAGE: $0 YYYYMMDD
  exit -2
fi

. ./user.config

# OUTDIR is rtofs under inputroot
export OUTDIR=$inputroot/rtofs

#
# call script to pull NCODA ocnqc, nhem, shem, glbl files
./pull_lotsa_stuff.sh $PDY

#
# call script to pull hycom_var data and recreate that directory
./pull_and_create_hycom_var.sh $PDY

#
# call script to pull rtofs archives and restart files and untar the the .tgz files
./pull_rtofs_archive_and_restart.sh $PDY


