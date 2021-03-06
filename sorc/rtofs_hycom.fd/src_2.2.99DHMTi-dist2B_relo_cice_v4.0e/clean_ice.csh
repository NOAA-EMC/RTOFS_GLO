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
#! /bin/csh -f
#
set echo
#
# --- clean out executables and object file
#
/bin/rm -rf hycom_cice esmf/compile hycom/hycom_cice  
/bin/rm -rf hycom/*.o  hycom/*.mod
