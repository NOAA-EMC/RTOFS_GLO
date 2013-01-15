#!/bin/sh
#
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         rtofs_glo2d_3hrly.sh                                   #
# Script description:                                                         #
#                                                                             #
# Authors: Bhavani Rajan & Ilya Rivin  Org: NP23         Date: 2011-07-20     #
#                                                                             #
# Abstract: This script creates the surface fields for RTOFS-Global           #
#           every 3 hours in netCDF format                                    #
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

echo "*** Started script $0 on hostname "`hostname`' at time '`date`

export CDF030=${RUN}_${modID}_2ds_${mode}${fhr}_3hrly_diag.nc
export CDF031=${RUN}_${modID}_2ds_${mode}${fhr}_3hrly_prog.nc

${EXECrtofs}/${RUN}_archv2ncdf2d < ${PARMrtofs}/${RUN}_${modID}.${inputgrid}.archv2ncdf2d.in >> $pgmout 2>>errfile 

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
