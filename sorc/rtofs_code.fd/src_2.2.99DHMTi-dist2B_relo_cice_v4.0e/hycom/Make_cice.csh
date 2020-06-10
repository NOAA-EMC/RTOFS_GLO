# #
# DISTRIBUTION STATEMENT B: Distribution authorized to U.S. Government
# agencies based upon the reasons of possible Premature Distribution
# and the possibility of containing Software Documentation as listed
# on Table 1 of DoD Instruction 5230.24, Distribution Statements on
# Technical Documents, of 23 August 2012. Other requests for this
# document shall be made to Dr. Ruth H. Preller, Superintendent,
# Oceanography Division, U.S. Naval Research Laboratory, DEPARTMENT
# OF THE NAVY, John C. Stennis Space Center, MS 39529-5004; (228)
# 688-4670 (voice); ruth.preller@nrlssc.navy.mil (e-mail).
# #
#!/bin/csh
#
set echo
setenv HYCOM_DIR $cwd
cd ${HYCOM_DIR}
#
# --- Usage:  ./Make_cice.com >& Make_cice.log
#
# --- make cice (ESMF HYCOM component) with TYPE=cice.
# --- this directory's name must be src_*_cice.
# --- assumes dimensions.h is correct for TYPE=cice (i.e. for mpi).
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
#setenv ARCH sun64
#setenv ARCH sun
#setenv ARCH t3e
#setenv ARCH xt3
#
setenv ARCH sp6_nofl
#
setenv TYPE `echo $cwd | awk -F"_" '{print $NF}'`
#
if     ($TYPE != "cice") then
  echo "TYPE must be cice to invoke cice make target"
  exit 1
endif
#
if (! -e ../config/${ARCH}_${TYPE}) then
  echo "ARCH = " $ARCH "  TYPE = " $TYPE "  is not supported"
  exit 1
endif
#
# --- cice needs additional environment variables.
#
if ($TYPE == "cice") then
  switch ($ARCH)
  case 'sp5':
    setenv BEI_HOME /site/BEI
    setenv ESMF_DIR ${BEI_HOME}/esmf/4.0.0rp2
    breaksw
  case 'o2k':
    setenv BEI_HOME /usr/local/usp/BEI
    setenv ESMF_DIR ${BEI_HOME}/esmf/4.0.0rp2
    breaksw
  case 'xt3':
    setenv BEI_HOME /usr/local/usp/BEI
    setenv ESMF_DIR ${BEI_HOME}/esmf/4.0.0rp2
    breaksw
  default:
    echo "TYPE = cice  needs BEI_HOME and ESMF_DIR"
    exit (1)
  endsw
endif
#
# --- make CICE component
#
setenv CICE_DIR ./CICE
cd ${CICE_DIR}
/bin/rm -f comp_ice.log
./comp_ice | tee comp_ice.log
#
# --- make HYCOM component, and update hycom_cice
#
cd ${HYCOM_DIR}
# --- force a relink, because CICE is not in the dependencies
/bin/rm hycom_cice
make ARCH=$ARCH TYPE=$TYPE hycom_cice
# --- some machines require gmake
#gmake ARCH=$ARCH TYPE=$TYPE hycom_cice
