#!/bin/sh
#
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         rtofs_glo3z_daily.sh                                   #
# Script description:                                                         #
#                                                                             #
# Authors: Bhavani Rajan & Ilya Rivin  Org: NP23         Date: 2011-07-20     #
#                                                                             #
# Abstract: This script creates the volume fields for RTOFS-Global            #
#           every day in netCDF format                                        #
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             # 
# Executables called:                                                         #
#                    rtofs__archv2ncdf3z                                      #
#                                                                             # 
#                                                                             #
# Imported variables:                                                         #
#                    RUN                                                      #
#                    modID                                                    #
#                    DATA                                                     #
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

cp -f -p ${PARMrtofs}/${RUN}_${modID}.${inputgrid}.archv_full.in archv.in

touch mltio.nc 3diio.nc 3ztio.nc 3zsio.nc 3zrio.nc 3zvio.nc 3zuio.nc 3zcio.nc 3zwio.nc
rm -f mltio.nc 3diio.nc 3ztio.nc 3zsio.nc 3zrio.nc 3zvio.nc 3zuio.nc 3zcio.nc 3zwio.nc

export CDF033=${RUN}_${modID}_3dz_${mode}${fhr}_daily_3zuio.nc
export CDF034=${RUN}_${modID}_3dz_${mode}${fhr}_daily_3zvio.nc
export CDF035=${RUN}_${modID}_3dz_${mode}${fhr}_daily_3ztio.nc
export CDF036=${RUN}_${modID}_3dz_${mode}${fhr}_daily_3zsio.nc

touch run_script.sh; rm run_script.sh
echo \#!/bin/sh > run_script.sh
echo ${EXECrtofs}/${RUN}_archv2ncdf3z \< $DATA/archv2ncdf3z_\$1.in \>\> $pgmout 2\>\>errfile >> run_script.sh

for fnam in uvl vvl tem sal 
do
  uvl1=0
  vvl1=0
  tem1=0
  sal1=0
  case $fnam in
    uvl) uvl1=33 ;;
    vvl) vvl1=34 ;;
    tem) tem1=35 ;;
    sal) sal1=36 ;;
  esac

  pgmout=outfile

  rm change_archv; touch change_archv
  echo "s/1uvl/$uvl1/g" >> change_archv
  echo "s/1vvl/$vvl1/g" >> change_archv
  echo "s/1tem/$tem1/g" >> change_archv
  echo "s/1sal/$sal1/g" >> change_archv

  sed -f change_archv archv.in > archv2ncdf3z_$fnam.in

done
#dbgz 20120112
# NPROCS=1
if [ $NPROCS -gt 1 ] 
then
    touch scp.sh; rm -f scp.sh
    echo sh ./run_script.sh tem > scp.sh
    echo sh ./run_script.sh sal >> scp.sh
    echo sh ./run_script.sh uvl >> scp.sh
    echo sh ./run_script.sh vvl >> scp.sh
    module load ics
    module load ibmpe
    export MP_LABELIO=yes
    export MP_CMDFILE=./scp.sh
    mpirun.lsf >>$pgmout 2>errfile 
else 
  for fnam in uvl vvl tem sal 
  do
    sh ./run_script.sh $fnam
  done
fi

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
