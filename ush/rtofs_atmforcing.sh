#!/bin/sh
#########################################################################
# Usage: rtofs_atmforcing.sh start-date end-date interval               #
#                                                                       #
# Description: This script copies the GFS/GDAS flux files and           #
#              interpolate them to generates the seashore forcing files #
#              for the Ocean Forecast Model                             #
#                                                                       #
# History:                                                              #
#    07-21-2010  Ilya Rivin:                                            #
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

if [ $# -lt 3 ] ; then 
  echo USAGE:  ${RUN}_atmforcing.sh start-date end-date interval
  exit 2
fi

#sh ${USHutil}/setup.sh

sdate=$1
edate=$2
intvl=$3
sdate=`$NDATE -3 $sdate`
edate=`$NDATE 3 $edate`

# to incorporate sea level pressure
export sea_lev_pres=PRMSL
export atmgds=

idate=$sdate
NTIME=0

# Copy the surface flux files from GFS/GDAS
# jpdt_table is the table to pick PDS data from

cd $DATA
rm -rf t.dat
if [ ${RUN_MODE} = "analysis" ]
then
 export netwk=gdas # Nowcast mode
 echo "0 0 8 0 0 0 0 0 0 0 0 0 0 0" > jpdt_table.dat 
# echo "8 8 0 0 8 0 0 0 0 0 0 0 0 0 0 0" > jpdt_table.dat
else
 export netwk=gfs  # Forecast mode
 echo "0 0 8 0 0 0 0 8 8 8 8 0 0 0" > jpdt_table.dat
# echo "8 8 0 0 8 0 0 0 0 0 0 0 0 0 0 0" > jpdt_table.dat
fi

NPROCS=${NPROCS:-1}
if [ $NPROCS -gt 1 ]
then
  rm -f cmdfile_tmp cmdfile.*
fi
while [ $idate -le $edate ]
do
   if [ $NPROCS -eq 1 ]
   then
     $USHrtofs/${RUN}_atmforcing_stage.sh $idate
   else
     echo $USHrtofs/${RUN}_atmforcing_stage.sh $idate >> cmdfile_tmp
   fi
   NTIME=`expr $NTIME + 1`
   idate=`$NDATE $intvl $idate` 
done

if [ $NPROCS -gt 1 ] 
then
  split -${NPROCS} cmdfile_tmp cmdfile.
  ls cmdfile.*
  for cfile in `ls cmdfile.*`
  do
    cmdlen=`cat $cfile | wc -l`
    while [ $cmdlen -lt $NPROCS ]
    do
      echo 'sleep 1' >> $cfile
      cmdlen=`expr $cmdlen + 1`
    done
    #module load ics
    #module load ibmpe
    #export MP_LABELIO=yes
    #export MP_CMDFILE=./$cfile 
    #mpirun.lsf >>$pgmout 2>errfile 
    mpirun ./$cfile >>$pgmout 2>errfile 
    exit=$?
    ## rm -f cmdfile.*
  done
  ## rm -f cmdfile_tmp
fi

#################################################################
# Interpolate to RTOFS grid
#################################################################
#- UNIT 11  - FILE intp_pars.dat, COTROL RUN PARAMETRS
#- UNIT 18  - FILE regional.grid.b, DESCRIPTOR FOR HYCOM GRID 
#-            (READ IN mod_geom.f90)
#- UNIT 19  - FILE regional.grid.a, HYCOM GRID 
#-            (READ IN mod_geom.f90)
#- UNIT 12  - FILE jpdt_table.dat is the PDT Table 4.0 or 4.8
#-            depending on if the data is instantaneos (4.0)
#-            or averaged (table 4.8)
#- UNIT 33  - FILE listflx.dat, LIST OF DATES AND MRF FLUS FILES TO 
#-            BE USED IN INTERPOLATION.
#- UNIT 42  - FILE regional.depth.a, HYCOM BATHIMETRY 
#-            (READ IN mod_geom.f90)
#- UNIT 44  - FILE regional.mask.a, HYCOM mask
#-            (READ IN mod_geom.f90)
#################################################################
pgm=${RUN}_atmforcing
. prep_step

msg=" `date`  -- $pgm for $adate started "
postmsg "$jlogfile" "$msg"

echo ${NTIME} > listflx.dat 
sort -n -o ts.dat t.dat
cat ts.dat >> listflx.dat 
rm -f t.dat ts.dat

export FORT11=intp_pars.dat
export FORT12=jpdt_table.dat
export FORT33=listflx.dat

$EXECrtofs/${RUN}_atmforcing >>$pgmout 2>errfile
err=$?; export err ; err_chk
echo " error from ${RUN}_atmforcing=",$err

# End of genrating the forcing files

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
