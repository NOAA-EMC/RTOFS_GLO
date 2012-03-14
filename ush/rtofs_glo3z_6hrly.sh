#!/bin/sh
#
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         rtofs_glo3z_6hrly.sh                                   #
# Script description:  rtofs_glo3z_6hrly.sh <region>                          #
#                                                                             #
# Authors: Bhavani Rajan & Ilya Rivin  Org: NP23         Date: 2011-07-20     #
#                                                                             #
# Abstract: This script creates the volume fields for RTOFS-Global            #
#           sub-region every  6 hrs in netCDF format.                         #
#           The 3 regions are : US East Coast,                                #
#                               US West Coast                                 #
#                               and the Alaskan region                        #
#                                                                             #
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             # 
# Executables called:                                                         #
#                    rtofs_archv2ncdf3z                                       #
#                                                                             # 
#                                                                             #
# Imported variables:                                                         #
#                    RUN                                                      #
#                    modID                                                    #
#                    fhr                                                      #
#                    EXECrtofs                                                #
#                    PARMglobal                                               #
#                    mode                                                     #
#                    DATA                                                     #
#                                                                             #
#                                                                             #
# Script history log:                                                         #
# XXXX-XX-XXX  Joe Dow                                                        #
#                                                                             #
###############################################################################

set -x

echo "*** Started script $0 on hostname "`hostname`' at time '`date`

export reg=$1
export CDF033=${RUN}_${modID}_3dz_${mode}${fhr}_6hrly_${reg}.nc

cp ${PARMrtofs}/${RUN}_${modID}.${inputgrid}.archv_$reg.in $DATA/archv_$reg.in

timex ${EXECrtofs}/${RUN}_archv2ncdf3z < $DATA/archv_$reg.in >> $pgmout 2>>errfile

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
