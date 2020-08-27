#!/bin/sh
#########################################################################
# Usage: rtofs_iceforcing.sh start-date end-date interval               #
#                                                                       #
# Description: This script creates CICE forcing files from              #
#              HYCOM forcing files                                      #
#              for the Ocean Forecast Model                             #
#                                                                       #
# History:                                                              #
#    12-12-2014  Ilya Rivin:                                            #
#########################################################################
# expected macros exported to this script:
#   DATA - work dir
#   mycyc - cycle (not used now)
#   PDY - present day, also PDYmN thru PDYpNN to cover input dates
#   mode - anal (nowcast) or fcst (forecast)
#   XXXXrtofs - root of rtofs directory for fix, exec, etc.
#   FLUXDIR - location of flux files. default is /com/gfs/prod/....
#   USHutil
#   EXECutil
#####
# future: use $cyc to grab latest flux files (not always 00z cycle)
set -x

echo "*** Started script $0 on hostname "`hostname`' at time '`date`

if [ $# -ne 0 ] ; then 
  echo USAGE:  ${RUN}_iceforcing.sh 
  exit 2
fi

cd $DATA

#sh ${USHutil}/setup.sh

mode=$RUN_MODE
if [ ${RUN_MODE} = "analysis" ]
then
 export mode=anal
else
 export mode=fcst
fi

BLKDATA_FILE=${PARMrtofs}/${RUN}_${modID}.${inputgrid}.${mode}.blkdat.input
IDM=`cat ${BLKDATA_FILE} | grep idm | cut -d' ' -f1`
JDM=`cat ${BLKDATA_FILE} | grep jdm | cut -d' ' -f1`
JDMA=`expr ${JDM} \- 1`

# Determine NREC
ftime=0.0
NREC=0
while read line
do
  str=`echo $line | awk '{printf ("%-7s\n", $1)}'`
  if [ .$str == .wndspd: ]
  then
     tim=`echo $line | awk '{printf ("%11.5f\n", $4)}'`
     let NREC=NREC+1
     if [[ $tim > $iday ]]
     then
       ftime=$tim
       break
     fi
  fi
done < forcing.wndspd.b
echo starting cice forcing at record $NREC at timemark $ftime

totrec=`grep -c wndspd forcing.wndspd.b`
let tailrec=totrec-NREC

#Create the ice forcing starting at the specified time step
mkdir tempforcing
cd tempforcing
rm -f netQlw.[ab]

export pgm="${RUN}_hycom_extract"
for field in airtmp glbrad lwdflx radflx shwflx surtmp vapmix wndewd wndnwd; do
  rm -f forcing.$field.a forcing.$field.b
  ${EXECrtofs}/rtofs_hycom_extract  ../forcing.$field.a $IDM $JDM 1 $NREC 1 0 forcing.$field.a > forcing.$field.list
  err=$?; export err ; err_chk
  echo " error from ${RUN}_hycom_extract=",$err
  head -5 ../forcing.$field.b > forcing.$field.b
  tail -$tailrec ../forcing.$field.b >> forcing.$field.b
done

cp -p ../forcing.offlux.? .

export pgm="${RUN}_hycom_expr"
. prep_step
startmsg
${EXECrtofs}/${RUN}_hycom_expr forcing.radflx.a forcing.shwflx.a ${IDM} ${JDM} 1.0 -1.0 netQlw.a > netQlw.b
err=$?; export err ; err_chk
echo " error from first ${RUN}_hycom_expr=",$err

rm -f surtmp4.[ab]

export pgm="${RUN}_hycom_expr"
. prep_step
startmsg
${EXECrtofs}/${RUN}_hycom_expr forcing.surtmp.a SQSQ ${IDM} ${JDM} 1.0 273.15 surtmp4.a > surtmp4.b
err=$?; export err ; err_chk
echo " error from second ${RUN}_hycom_expr=",$err

rm -f lwdflx.[AB]

export pgm="${RUN}_hycom_expr"
. prep_step
startmsg
${EXECrtofs}/${RUN}_hycom_expr netQlw.a surtmp4.a ${IDM} ${JDM} 1.0 538.65e-10 lwdflx.A > lwdflx.B
err=$?; export err ; err_chk
echo " error from third ${RUN}_hycom_expr=",$err

mv forcing.lwdflx.a forcing.lwdflx1.a
rm -f forcing.lwdflx.b

export pgm="${RUN}_hycom_expr"
. prep_step
startmsg
${EXECrtofs}/${RUN}_hycom_expr lwdflx.A forcing.offlux.a ${IDM} ${JDM} 1.0 1.0 forcing.lwdflx.a repeat > forcing.lwdflx.b
err=$?; export err ; err_chk
echo " error from fourth ${RUN}_hycom_expr=",$err

export pgm="${RUN}_hycom_expr"
. prep_step
startmsg
${EXECrtofs}/${RUN}_hycom_expr forcing.airtmp.a ONE ${IDM} ${JDM} 1.0 273.15 forcing.airtmpCice.a > forcing.airtmpCice.b
err=$?; export err ; err_chk
echo " error from fifth ${RUN}_hycom_expr=",$err

for cfield in airtmp glbrad lwdflx vapmix wndewd wndnwd
do
  if [ "${cfield}" = 'airtmp' ]
  then
    extra='Cice'
  else
    extra=''
  fi
  export pgm="${RUN}_hycom_expr"
  . prep_step
  startmsg
  ${EXECrtofs}/${RUN}_hycom2raw8 forcing.${cfield}${extra}.a ${IDM} ${JDM} 1 1 ${IDM} ${JDMA} ../cice.${cfield}.r > ../cice.${cfield}.B
  err=$?; export err ; err_chk
  echo " error from ${RUN}_hycom2raw8=",$err

done
# End of generating the ice forcing files

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
