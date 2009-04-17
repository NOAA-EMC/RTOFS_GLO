#!/bin/sh
#########################################################################
# Usage: rtofs_atmforcing.sh                                            #
#                                                                       #
# Description: This script copies the GFS/GDAS flux files and           #
#              interpolate them to generates the seashore forcing files #
#              for the Ocean Forecast Model                             #
#                                                                       #
# History:                                                              #
#    06-16-2005  Dan Iredell                                            #
#    06-01-2006  Ilya Rivin: add sea level pressure forcing             #
#########################################################################
# expected macros exported to this script:
#   DATA - work dir
#   cyc - cycle (not used now)
#   PDY - present day, also PDYmN thru PDYpNN to cover input dates
#   mode - anal (nowcast) or fcst (forecast)
#   XXXXrtofs - root of rtofs directory for fix, exec, etc.
#   FLUXDIR - location of flux files. default is /com/gfs/prod/....
#   utilscript
#   utilexec
#####
# future: use $cyc to grab latest flux files (not always 00z cycle)
set -x

echo "*** Started script $0 on hostname "`hostname`' at time '`date`

if [ $# -lt 3 ] ; then 
  echo USAGE:  rtofs_atmforcing.sh start-date end-date interval
  exit 2
fi

sh ${utilscript}/setup.sh

sdate=$1
edate=$2
intvl=$3
sdate=`${utilexec}/ndate -3 $sdate`
edate=`${utilexec}/ndate 3 $edate`

# to incorporate sea level pressure
export sea_lev_pres=PRMSL
export atmgds=

idate=$sdate
NTIME=0

# Copy the surface flux files from GFS/GDAS

cd $DATA
rm -rf t.dat
if [ $mode = "anal" ]
then
 export netwk=gdas # Nowcast mode
else
 export netwk=gfs  # Forecast mode     
fi

NPROCS=${NPROCS:-1}
if [ NPROCS -gt 1 ]
then
  rm -f cmdfile_tmp cmdfile.*
  export MP_PGMMODEL=mpmd
fi
while [ $idate -le $edate ]
do
   if [ $NPROCS -eq 1 ]
   then
     $USHrtofs/rtofs_atmforcing_stage.sh $idate
   else
     echo $USHrtofs/rtofs_atmforcing_stage.sh $idate >> cmdfile_tmp
   fi
   NTIME=`expr $NTIME + 1`
   idate=`${utilexec}/ndate $intvl $idate` 
done

if [ $NPROCS -gt 1 ] 
then
  split -${NPROCS} cmdfile_tmp cmdfile.
  ls cmdfile.*
  for cfile in `ls cmdfile.*`
  do
    cmdlen=`cat $cfile | wc -l`
    while [ $cmdlen -lt $NPROCS]
    do
      echo 'sleep 1' >> $cfile
      cmdlen=`expr $cmdlen + 1`
    done
    export MP_CMDFILE=$cfile
    poe -procs $NPROCS
    exit=$?
    ## rm -f cmdfile.*
  done
  export MP_PGMMODEL=spmd
  ## rm -f cmdfile_tmp
fi

#################################################################
# Interpolate to RTOFS grid
#################################################################
#- UNIT  7  - FILE intp_pars.dat, COTROL RUN PARAMETRS
#- UNIT  8  - FILE regional.grid.b, DESCRIPTOR FOR HYCOM GRID 
#-            (READ IN mod_geom.f90)
#- UNIT  9  - FILE regional.grid.a, HYCOM GRID 
#-            (READ IN mod_geom.f90)
#- UNIT 33  - FILE listflx.dat, LIST OF DATES AND MRF FLUS FILES TO 
#-            BE USED IN INTERPOLATION.
#- UNIT 59  - FILE regional.depth.a, HYCOM BATHIMETRY 
#-            (READ IN mod_geom.f90)
#- UNIT 61  - FILE regional.mask.a, HYCOM mask
#-            (READ IN mod_geom.f90)
#- UNIT 81  - MRF GRIBBED FLUXES FILES WITH THE NAMES FROM THE LIST 
#-            SPECIFIED IN listflx.dat.
#- UNIT 82  - THE SAME      
#################################################################
pgm=rtofs_atmforcing
. prep_step

msg=" `date`  -- $pgm for $adate started "
postmsg "$jlogfile" "$msg"

echo ${NTIME} > listflx.dat 
sort -n -o ts.dat t.dat
cat ts.dat >> listflx.dat 
rm -f t.dat ts.dat

export XLFRTEOPTS="unit_vars=yes:buffering=disable_all"
export XLFUNIT_7=intp_pars.dat
export XLFUNIT_8=regional.grid.b
export XLFUNIT_9=regional.grid.a
export XLFUNIT_33=listflx.dat
export XLFUNIT_59=regional.depth.a
export XLFUNIT_61=regional.mask.a

# dbx $EXECrtofs/rtofs_atmforcing <<EOF
# run
# where
# quit
# EOF

$EXECrtofs/rtofs_atmforcing >>$pgmout 2>errfile
export err=$?; err_chk

# End of genrating the forcing files

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
