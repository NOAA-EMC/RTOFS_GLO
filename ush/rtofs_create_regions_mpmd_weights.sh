#!/bin/sh
set -xeu

echo "*** Started script $0 on hostname "`hostname`' at time '`date`

cd $DATA 
# Submit seperate scripts for the 11 regions

test -f cmdfile && rm -f cmdfile cmdfile.*

ifile=1
for region in alaska arctic bering guam gulf_alaska honolulu hudson_baffin samoa trop_paci_lowres west_atl west_conus

do
   
   echo "$USHrtofs/${RUN}_nc2grib2.sh_prod $region > out_${region} 2>&1"               >> cmdfile.$ifile
   chmod +x cmdfile.$ifile
   echo "./cmdfile.$ifile" >> cmdfile
   ifile=`expr $ifile + 1`

done

chmod +x cmdfile
echo "mpirun.lsf ${utilexec}/mpiserial"
mpirun.lsf ${utilexec}/mpiserial
exit=$?

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`