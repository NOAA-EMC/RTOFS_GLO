#!/bin/bash

# A compile script for the UFS weather model - for data atmosphere only.

set -eux

cwd=$(pwd)
UFSsrc=$cwd/../../sorc/ufs_model.fd/
APP="NG-GODAS"

if [[ ! -d "${UFSsrc}" ]]; then
  echo "Error: Source code path: '${UFSsrc}' does not exist."
  echo "Fix your clone and try again."
  exit 2
fi

echo " "
echo " Building the UFS Weather Model for (application): " ${APP}
echo " Path to the source code: " ${UFSsrc}
echo " "

#
# Valid only for WCOSS2; enable parallel restart I/O
# TODO: Remove following option when parallel restart option _works_.
# ----
PARALLEL_RESTART="NO"

EXEC_NAME="ufs_model.x"

FASTER=ON
#
# D O  N O T  E D I T  B E L O W

cd ${UFSsrc} || false

source "./tests/detect_machine.sh"
source "./tests/module-setup.sh"

MAKE_OPT="-DAPP=${APP}"

if [[ "${FASTER}" == ON ]] ; then
    MAKE_OPT+=" -DFASTER=ON -DCMAKE_BUILD_TYPE=Release"
else
    MAKE_OPT+=" -DDEBUG=ON -DCMAKE_BUILD_TYPE=Debug"
fi

case "${EXEC_NAME}" in
  "ufs_model.x") COMPILE_ID=0 ;;
  *) echo "Unsupported executable name: ${EXEC_NAME}"; exit 1 ;;
esac
CLEAN_BEFORE=YES
CLEAN_AFTER=NO

# The test/compile.sh script adds " -DENABLE_PARALLELRESTART=ON" when compiling on WCOSS2, which is causing issues
# TODO: when ufs-weather-model#2716 is fixed, return to using tests/compile.sh
if [[ "${MACHINE_ID}" == "wcoss2" && "${PARALLEL_RESTART:-}" == "NO" ]]; then
   set +x
   module use modulefiles
   module load "ufs_wcoss2.intel"
   module list
   set -x

   BUILD_NAME="fv3_${COMPILE_ID}"
   BUILD_DIR="$(pwd)/build_${BUILD_NAME}"
   if [[ "${CLEAN_BEFORE}" == "YES" ]]; then
      rm -rf "${BUILD_DIR}"
   fi

   BUILD_DIR=${BUILD_DIR} BUILD_VERBOSE=1 BUILD_JOBS=${BUILD_JOBS:-8} CMAKE_FLAGS="${MAKE_OPT}" ./build.sh

   mv "${BUILD_DIR}/ufs_model" "tests/${BUILD_NAME}.exe"
   cp modulefiles/ufs_wcoss2.intel.lua "tests/modules.${BUILD_NAME}.lua"
   if [[ "${CLEAN_AFTER}" == "YES" ]]; then
      rm -rf "${BUILD_DIR}"
   fi
else
   BUILD_JOBS=${BUILD_JOBS:-8} ./tests/compile.sh "${MACHINE_ID}" "${MAKE_OPT}" "${COMPILE_ID}" "intel" "${CLEAN_BEFORE}" "${CLEAN_AFTER}"
fi
mv "./tests/fv3_${COMPILE_ID}.exe" "./tests/${EXEC_NAME}"
if [[ ! -f "./tests/modules.ufs_model.lua" ]]; then mv "./tests/modules.fv3_${COMPILE_ID}.lua" "./tests/modules.ufs_model.lua"; fi
if [[ ! -f "./tests/ufs_common.lua" ]]; then cp "./modulefiles/ufs_common.lua" ./tests/ufs_common.lua; fi

echo " "
echo " "
echo "Find ${EXEC_NAME} at ${UFSsrc}/tests/"
echo "All done!"
exit 0
