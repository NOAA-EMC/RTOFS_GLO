#!/bin/bash
# this script pulls the restart file from the NAVY
# if before 20201205, then from RTOFS V1 archives (as n-48.restart)
#    /NCEPPROD/hpssprod/runhistory/rhYYYY/YYYYMM/YYYYMMDD/com_rtofs_prod_rtofs.YYYYMMDD.restart.tar
#    ./rtofs_glo.t00z.n-48.restart.a
#    ./rtofs_glo.t00z.n-48.restart.b
#    ./rtofs_glo.t00z.n-48.restart_cice
# if after 20201205, then from my archive (on MWF)
#    /NCEPDEV/emc-ocean/5year/emc.ncodapa/navy_restarts/restarts.YYYYMMDD.tar
#    restart_rYYYYMMDDHH_930.a.gz
#    restart_rYYYYMMDDHH_930.b.gz
#    cice.restart.YYYYMMDDHH_930.gz

# then these are renamed as 
#    rtofs_glo.t00z.n-24.restart.a
#    rtofs_glo.t00z.n-24.restart.b
#    rtofs_glo.t00z.n-24.restart_cice
# and placed in the xxxxxxxx directory
#

if [ $# -eq 1 ]
then
export PDY=$1
else
echo USAGE: $0 PDY
exit -2
fi

# is this after 20201205, then check if day of week is m,w,f
# date --date='1959-05-03' +%u
if [ $PDY -gt 20201205 ]
then
    yyyy=`echo $PDY | cut -c1-4`
    mm=`echo $PDY | cut -c5-6`
    dd=`echo $PDY | cut -c7-8`

    dow=`date -d ${yyyy}-${mm}-${dd} +%u`
    wdy=`date -d ${yyyy}-${mm}-${dd} +%a`

    if [[ $dow -ne 1 && $dow -ne 3 && $dow -ne 5 ]]
    then
       echo $PDY is a $wdy. Please choose a day that is Mon Wed or Fri
       exit
    fi
fi

echo Pulling restart files for $PDY

. ./user.config

# OUTDIR is rtofs under inputroot
export OUTDIR=$inputroot/rtofs

account=${account:-hurricane}
OUTDIR=${OUTDIR:-/scratch2/NCEPDEV/$GROUP/$USER/COMDIR/com/rtofs/prod}

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load prod_util/1.1.0

PDYm2=`$NDATE \` expr -2 \* 24 \` ${PDY}00 | cut -c1-8`
PDYm1=`$NDATE \` expr -1 \* 24 \` ${PDY}00 | cut -c1-8`
PDYp1=`$NDATE \` expr 1 \* 24 \` ${PDY}00 | cut -c1-8`

if [ $PDY -gt 20201205 ]
then
   COMOUT=$OUTDIR/rtofs.$PDYp1
else
   COMOUT=$OUTDIR/rtofs.$PDYm1
fi 

echo 
echo Important: restart files will be in $COMOUT
echo

mkdir -p $TMPDIR/stage.$PDY

#
# Submit service job to pull navy restart
cat << eofA > $TMPDIR/stage.$PDY/pull.navy.restart.sh
#!/bin/ksh -l
#SBATCH --ntasks=1
#SBATCH --partition=service
#SBATCH --time=05:00:00
#SBATCH --account=$account
#SBATCH --job-name=pull_navyrestart.$PDY
#SBATCH -o $TMPDIR/stage.$PDY/pull_navy.restart.$PDY.%j
#SBATCH -q batch

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load prod_util/1.1.0
module load hpss

set -x

pdy=$PDY
yyyy=\`echo \$pdy | cut -c1-4\`
mm=\`echo \$pdy | cut -c5-6\`

# get rtofs data
mkdir -p $COMOUT
cd $COMOUT

#rtofs v1
if [ \$pdy -le 20201205 ]
then
   hdir=/NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/com_rtofs_prod_rtofs.\$pdy.restart.tar
   rlist="./rtofs_glo.t00z.n-48.restart.a ./rtofs_glo.t00z.n-48.restart.b ./rtofs_glo.t00z.n-48.restart_cice"
   htar -xvf \$hdir \$rlist
   mv rtofs_glo.t00z.n-48.restart.a rtofs_glo.t00z.n-24.restart.a
   mv rtofs_glo.t00z.n-48.restart.b rtofs_glo.t00z.n-24.restart.b
   mv rtofs_glo.t00z.n-48.restart_cice rtofs_glo.t00z.n-24.restart_cice
else
   hdir=/NCEPDEV/emc-ocean/5year/emc.ncodapa/navy_restarts/restarts.\$pdy.tar
   rlist="restart_r\${pdy}00_930.a.gz restart_r\${pdy}00_930.b.gz cice.restart.\${pdy}00_930.gz"
   htar -xvf \$hdir \$rlist
   gunzip restart_r\${pdy}00_930.a.gz
   gunzip restart_r\${pdy}00_930.b.gz
   gunzip cice.restart.\${pdy}00_930.gz
   mv restart_r\${pdy}00_930.a rtofs_glo.t00z.n-24.restart.a
   mv restart_r\${pdy}00_930.b rtofs_glo.t00z.n-24.restart.b
   mv cice.restart.\${pdy}00_930 rtofs_glo.t00z.n-24.restart_cice
fi

eofA

sbatch $TMPDIR/stage.$PDY/pull.navy.restart.sh

exit

