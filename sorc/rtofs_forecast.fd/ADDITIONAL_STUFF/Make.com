#!/bin/csh
#
set echo
cd $cwd
#
# --- Usage:  ./Make.com >& Make.log
#
# --- make hycom with TYPE from this directory's name (src_*_$TYPE).
# --- assumes dimensions.h is correct for $TYPE.
#
# --- set ARCH to the correct value for this machine.
# --- ARCH that start with A are for ARCTIC patch regions
#
#setenv ARCH alphaL
#setenv ARCH alpha
#setenv ARCH amd64
#setenv ARCH intel
#setenv ARCH o2k
#setenv ARCH sp3
#setenv ARCH sp4
#setenv ARCH sp5
#setenv ARCH sun64
#setenv ARCH sun
#setenv ARCH t3e
#setenv ARCH xt3
#setenv ARCH Asp5
#
setenv ARCH sp5
#
setenv TYPE `echo $cwd | awk -F"_" '{print $NF}'`
#
if (! -e ../config/${ARCH}_${TYPE}) then
  echo "ARCH = " $ARCH "  TYPE = " $TYPE "  is not supported"
  exit 1
endif
#
# --- esmf needs additional environment variables.
#
if ($TYPE == "esmf") then
  switch ($ARCH)
  case 'sp4':
    setenv BEI_HOME /site/BEI
    setenv ESMF_DIR ${BEI_HOME}/esmf/2.2.2r
    breaksw
  case 'o2k':
    setenv BEI_HOME /usr/local/usp/BEI
    setenv ESMF_DIR ${BEI_HOME}/esmf/2.2.2r
    breaksw
  case 'xt3':
    setenv BEI_HOME /usr/local/usp/BEI
    setenv ESMF_DIR ${BEI_HOME}/esmf/2.2.2r
    breaksw
  default:
    echo "TYPE = esmf  needs BEI_HOME and ESMF_DIR"
    exit (1)
  endsw
endif
#
# --- some machines require gmake
#
#gmake ARCH=$ARCH TYPE=$TYPE hycom
make hycom ARCH=$ARCH TYPE=$TYPE
