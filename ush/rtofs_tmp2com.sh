#!/bin/sh

###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         rtofs_tmp2com.sh                                       #
# Script description:                                                         #
#                                                                             #
# Author:        Ilya Rivin       Org: NP23         Date: 2011-05-30          #
#                                                                             #
# Abstract: This is the post-processing script for RTOFS_GLO                  #
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             # 
#                                                                             #
#                                                                             #
# Script history log:                                                         #
# XXXX-XX-XXX  Joe Dow                                                        #
#                                                                             #
###############################################################################

set -xa

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)

cd $DATA

#typeset -Z5 SSSSS
#typeset -Z2 HH

if [ ${RUN_MODE} = 'analysis' ] ; then
  mode=n
else
  mode=f
fi
NPROCS=${NPROCS:-1}
if [ $NPROCS -gt 1 ]
then
  rm -f cmdfile_tmp* cmdfile.*
fi
#
# Write archive files copying commands in CMD
#
for afile in $(ls ${DATA}/arch?.????_???_??.a)
do
  cfile=$(basename $afile)
  YYYY=$(echo $cfile | cut -c7-10)
  DDD=$(echo $cfile | cut -c12-14)
  HH=$(echo $cfile | cut -c16-17)
  YYYYMMDD=$(${USHutil}/date2jday.sh ${YYYY}${DDD})
  MM=$(echo $YYYYMMDD | cut -c5-6)
  DD=$(echo $YYYYMMDD | cut -c7-8)
  LEAD=$($NHOUR ${YYYY}${MM}${DD}${HH} ${PDY}${mycyc})
  arch=$(echo $cfile | cut -d. -f1)
  HYCOMarchTplate=${RUN}_${modID}.t${mycyc}z.${mode}${LEAD}.${arch}
  if [ $arch = "archv" ] ; then
    echo "cp -p -f $afile ${COMOUT}/${HYCOMarchTplate}.a" >> cmdfile_tmp_v
    cp -p -f ${afile%.a}.b ${COMOUT}/${HYCOMarchTplate}.b
    cp -p -f ${afile%.a}.txt ${COMOUT}/${HYCOMarchTplate}.txt
  fi
  if [ $arch = "archs" ] ; then
    echo "cp -p -f $afile ${COMOUT}/${HYCOMarchTplate}.a" >> cmdfile_tmp_s
    cp -p -f ${afile%.a}.b ${COMOUT}/${HYCOMarchTplate}.b
    cp -p -f ${afile%.a}.txt ${COMOUT}/${HYCOMarchTplate}.txt
  fi
  if [ $arch = "arche" ] ; then
    if [ ! -s ${COMOUT}/${HYCOMarchTplate}.a ] ; then
      echo "cp -p -f $afile ${COMOUT}/${HYCOMarchTplate}.a" >> cmdfile_tmp_e
      cp -p -f ${afile%.a}.b ${COMOUT}/${HYCOMarchTplate}.b
    fi
  fi
done
if compgen -G "${DATA}/cice_inst.????-??-??-?????.nc" > /dev/null
then
  for ifile in $(ls ${DATA}/cice_inst.????-??-??-?????.nc)
  do
    cfile=$(basename $ifile)
    YYYY=$(echo $cfile | cut -c11-14)
    MM=$(echo $cfile | cut -c16-17)
    DD=$(echo $cfile | cut -c19-20)
    SSSSS=$(echo $cfile | cut -c22-26)
#    HH=$(expr $SSSSS \/ 3600)
    HH=$(printf "%02d\n" $(expr $SSSSS \/ 3600))
    LEAD=$($NHOUR ${YYYY}${MM}${DD}${HH} ${PDY}${mycyc})
    echo "cp -p -f $cfile ${COMOUT}/${RUN}_${modID}.t${mycyc}z.${mode}${LEAD}.cice_inst" >> cmdfile_tmp_c # dont work w/ hourly
  done
fi
#
# This script no longer writes restart files to COMOUT.
# The calling program determines whether the program
#   - succeeded (and copies the restart files to COMOUT)
#   - fails (and copies the restart files to GESOUT)
#

touch cmdfile_tmp_v cmdfile_tmp_c cmdfile_tmp_e cmdfile_tmp_s
cat cmdfile_tmp_v cmdfile_tmp_c cmdfile_tmp_e cmdfile_tmp_s > cmdfile_tmp_all
chmod +x cmdfile_tmp_all

runpara=1
if [[ $NPROCS -gt 1 && $runpara -eq 1 ]]
then
#  mpirun cfp ./cmdfile_tmp_all >> cptmp2out.out
  mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile_tmp_all > cptmp2out.out
  err=$? ; export err ; err_chk
else
  sh ./cmdfile_tmp_all >> cptmp2out.out
  err=$? ; export err ; err_chk
fi

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)
