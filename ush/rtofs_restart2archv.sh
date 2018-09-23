#!/bin/ksh

###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         rtofs_restart2archv.sh                                 #
# Script description:                                                         #
#                                                                             #
# Authors: Bhavani Rajan & Zulema Garrafalo   Org: NP23  Date: 2015-01-20     #
#                                                                             #
# Abstract: This script creates the n-48 archv and archs file from the restart#
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             #
# Imported variables:                                                         #
#                    RUN                                                      #
#                    PARMrtofs                                                #
#                    EXECrtofs
#                                                                             #
#                                                                             #
# Script history log:                                                         #
# XXXX-XX-XXX  Joe Dow                                                        #
#                                                                             #
###############################################################################

set -xa
echo "*** Started script $0 on hostname "`hostname`' at time '`date`

export pgm="${RUN}_restart2archv"
. prep_step
startmsg
$EXECrtofs/${RUN}_restart2archv < $PARMrtofs/${RUN}_restart2archv.in 
export err=$?; err_chk
echo "Error from restart2archv ",$err 
## For 2d archive files
head -27 archv_n00.b > archs_n00.b

export pgm="${RUN}_hycom_extract"
. prep_step
startmsg
$EXECrtofs/${RUN}_hycom_extract archv_n00.a 4500 3298 1 1 1 17 archs_n00.a > $pgmout
export err=$?; err_chk
echo "Error from restart2archv:hycom_extract ",$err

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`

