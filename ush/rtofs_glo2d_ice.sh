#!/bin/sh
#
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         rtofs_glo2d_1hrly.sh                                   #
# Script description:                                                         #
#                                                                             #
# Authors: Bhavani Rajan & Ilya Rivin  Org: NP23         Date: 2020-07-02     #
#                                                                             #
# Abstract: This script creates the surface ice fields for RTOFS-Global       #
#           every 1 or 3 hours in netCDF format                               #
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             # 
# Executables called:                                                         #
#                    rtofs_archv2ncdf2d                                       #
#                                                                             # 
#                                                                             #
# Imported variables:                                                         #
#                    RUN                                                      #
#                    modID                                                    #
#                    fhr                                                      #
#                    EXECrtofs                                                #
#                    PARMglobal                                               #
#                    mode                                                     #
#                                                                             #
#                                                                             #
# Script history log:                                                         #
# XXXX-XX-XXX  Joe Dow                                                        #
#                                                                             #
###############################################################################

set -x
typeset -Z3 fhr

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)

export CDF021=${RUN}_${modID}_2ds_${mode}${fhr}_ice.nc

export pgm="rtofs_archv2ncdf2d"
. prep_step
startmsg
${EXECrtofs}/rtofs_field2ncdf2d < ${PARMrtofs}/${RUN}_${modID}.${inputgrid}.field2ncdf2d.in >> $pgmout 2>>errfile 
export err=$? ; err_chk

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)
