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
#module swap compiler compiler/intel/12.1.3
#module swap mpi      mpi/intel/ibmpe
#module list
#setenv ARCH intelsse-pe-sm-relo
#
#module swap compiler compiler/intel/12.1.3
#module swap mpi      mpi/intel/impi/4.1.3
#module list
#setenv ARCH intelsse-impi-sm-SD-relo
#setenv ARCH intelsse-impi-sm-relo
#
#module switch PrgEnv-cray PrgEnv-intel
unset echo
module switch  intel intel/15.0.0.090
module load    craype-hugepages2M
module list
set echo
setenv ARCH xc40-intel-relo
#
#setenv ARCH intel-pgi-SD-relo
#
setenv TYPE `echo $cwd | awk -F"_" '{print $NF}'`
echo "ARCH = " $ARCH "  TYPE = " $TYPE
#
if (! -e ../config/${ARCH}_${TYPE}) then
  echo "ARCH = " $ARCH "  TYPE = " $TYPE "  is not supported"
  exit 1
endif
#
# --- some machines require gmake
#
#gmake ARCH=$ARCH TYPE=$TYPE hycom
make hycom ARCH=$ARCH TYPE=$TYPE
#
if ( $ARCH == "Asp5" || $ARCH == "sp5") then
  ldedit -bdatapsize=64K -bstackpsize=64K hycom
endif
if ( $ARCH == "Asp6" || $ARCH == "sp6") then
  ldedit -bdatapsize=64K -bstackpsize=64K hycom
endif
if ( $ARCH == "Asp6-nofl" || $ARCH == "sp6-nofl") then
  ldedit -bdatapsize=64K -bstackpsize=64K hycom
endif
