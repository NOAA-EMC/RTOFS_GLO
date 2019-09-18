#!/bin/sh

#
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         rtofs_create_regions_mpmd_weights.sh                   #
# Script description:                                                         #
#                                                                             #
# Authors: Bhavani Rajan & Ilya Rivin  Org: NP23         Date: 2013-08-20     #
#                                                                             #
# Abstract: This script submits 11 jobs for the 11 sub regions on individual  #
#           nodes to create GRIB2 files directly                              #
#                                                                             #
#                                                                             #
# Sub-scripts called:    cmdfile.$i (i from 1 to 11)                          # 
#                                                                             #
# Executables called:                                                         #
#                                                                             #
#                                                                             #
# Imported variables:                                                         #
#                    RUN                                                      #
#                    utilexec                                                 #
#                    USHrtofs                                                #
#                                                                             #
#                                                                             #
# Script history log:                                                         #
# XXXX-XX-XXX  Joe Dow                                                        #
#                                                                             #
###############################################################################
set -xeu

echo "*** Started script $0 on hostname "`hostname`' at time '`date`

cd $DATA 
# Submit seperate scripts for the 11 regions

test -f cmdfile && rm -f cmdfile cmdfile.*
rm -f cmdfile.for.cfp

ifile=1
for region in alaska arctic bering guam gulf_alaska honolulu hudson_baffin samoa trop_paci_lowres west_atl west_conus

do
   
   echo "ksh $USHrtofs/${RUN}_nc2grib2.sh $region > out_${region} 2>&1"               >> cmdfile.$ifile
   chmod +x cmdfile.$ifile
   echo "./cmdfile.$ifile" >> cmdfile
   ifile=`expr $ifile + 1`

   echo "ksh $USHrtofs/${RUN}_nc2grib2.sh $region > out_${region} 2>&1" >> cmdfile.for.cfp
done

chmod +x cmdfile
#echo "mpirun.lsf ${utilexec}/mpiserial"
#mpirun.lsf ${utilexec}/mpiserial
#echo mpirun cfp ./cmdfile > create.regions.out
#mpirun cfp ./cmdfile > create.regions.out

chmod +x cmdfile.for.cfp
echo mpirun cfp ./cmdfile.for.cfp > create.regions.out
mpirun cfp ./cmdfile.for.cfp > create.regions.out
err=$? ; export err ; err_chk

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
