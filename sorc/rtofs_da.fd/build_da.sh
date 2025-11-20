#!/bin/sh

#########################################
# THIS SCRIPT CAN COMPILE 
# 1. Contents of xx
#########################################

# INPUTS:
# ------
BUILD_TYPE=""
BASE=`pwd`

# can override makefile BUILD_TYPE (only debug is valid)
if [[ $# -eq 1 && $1 == debug ]]; then
  BUILD_TYPE="debug"
  compstr=${BUILD_TYPE}
else
  compstr="release"
fi

dir_mods="$(dirname ${BASE})"
dir_mod0="$(dirname ${dir_mods})"

echo " "
echo "Load modules listed at: "
echo " "
echo ${dir_mod0}"/versions/build.ver"
source ${BASE}/load_modules.sh ${dir_mod0}

make ${BUILD_TYPE} -f makefile > rtofs_da.compile.${compstr}.log 2>&1
