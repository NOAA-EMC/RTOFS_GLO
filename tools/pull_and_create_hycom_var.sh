#!/bin/bash
# this script pulls the required hycom_var data and recreates the hycom_var directory
# rtofs restart and forcing files 
#

if [ $# -eq 1 ]
then
export PDY=$1
else
echo USAGE: $0 PDY
exit -2
fi

# account=${account:-hurricane}
# OUTDIR=${OUTDIR:-/scratch2/NCEPDEV/$GROUP/$USER/COMDIR/com/rtofs/prod}

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load prod_util/1.1.0

PDYm1=`$NDATE \` expr -1 \* 24 \` ${PDY}00 | cut -c1-8`
COMOUT=$OUTDIR/rtofs.$PDYm1

echo PDY is $PDY
echo account is $account
echo COMOUT is $OUTDIR/rtofs.$PDYm1

mkdir -p $COMOUT/ncoda/hycom_var
mkdir -p $TMPDIR/stage.$PDY

#
# Submit service job to pull hycom listing
cat << eofB > $TMPDIR/stage.$PDY/pull.hycom.listing.sh
#!/bin/ksh -l
#SBATCH --ntasks=1
#SBATCH --partition=service
#SBATCH --time=01:00:00
#SBATCH --account=$account
#SBATCH --job-name=pull_hycomlisting.$PDYm1
#SBATCH -o $TMPDIR/stage.$PDY/pull_hycom.listing.$PDYm1.%j
#SBATCH -q batch

module load hpss

set -x

pdy=$PDYm1
#cd $TMPDIR/stage.$PDY
mkdir -p $COMOUT/ncoda/hycom_var/restart
cd $COMOUT/ncoda/hycom_var/restart

htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/hycom_var_listing.tar

eofB

alljobids=$(sbatch $TMPDIR/stage.$PDY/pull.hycom.listing.sh | cut -d " " -f4)

#
# Submit 15 service jobs to pull hycom var
for i in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14
do
ipdy=`$NDATE \` expr -$i \* 24 \` ${PDYm1}00 | cut -c1-8`

cat << eofn > $TMPDIR/stage.$PDY/pullhycomvar.sub.$ipdy.sh
#!/bin/ksh -l
#SBATCH --ntasks=1
#SBATCH --partition=service
#SBATCH --time=02:00:00
#SBATCH --account=$account
#SBATCH --job-name=pull_hycomvar.$i
#SBATCH -o $TMPDIR/stage.$PDY/pull_hycomvar.$ipdy.%j
#SBATCH -q batch

module load hpss

set -x

pdy=$ipdy
#mkdir -p $TMPDIR/stage.$PDY/3dvar/rtofs.PDY/ncoda.\$pdy
#cd $TMPDIR/stage.$PDY/3dvar/rtofs.PDY/ncoda.\$pdy
mkdir -p $COMOUT/ncoda/3dvar/ncoda.\$pdy
cd $COMOUT/ncoda/3dvar/ncoda.\$pdy

htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/hycom.tar

eofn

jobid=$(sbatch $TMPDIR/stage.$PDY/pullhycomvar.sub.$ipdy.sh | cut -d " " -f4)
alljobids=$alljobids:$jobid

done

#
# Submit batch job to remake hycom_var
pdy=$PDYm1
cat << eofC > $TMPDIR/stage.$PDY/remake.$pdy.sh
#!/bin/ksh -l
#SBATCH --ntasks=1
#SBATCH --partition=service
#SBATCH --time=02:00:00
#SBATCH --account=$account
#SBATCH --job-name=remake_hycomvar.$pdy
#SBATCH -d afterok:$alljobids
#SBATCH -o $TMPDIR/stage.$PDY/remake_hycomvar.$pdy.%j
#SBATCH -q batch

set -x

mkdir -p $COMOUT/ncoda/hycom_var/restart
#cd $TMPDIR/stage.$PDY
cd $COMOUT/ncoda/hycom_var/restart

while read line
do
  str=\`echo \$line | awk '{printf ("%s\n", \$9)}'\`
  fn=\`echo \$str | cut -d/ -f4\`
  sz=\`echo \$line | awk '{printf ("%i\n", \$5)}'\`

  froms=\`find ../../3dvar -name \$fn\`
  for v in \$froms;do from=\$v;done

  echo linking \$from 
  ln -sf \$from .
  rc=\$?
  if [ \$rc -ne 0 ];then echo return code \$rc \$from ;fi

done < listing.hycom_var.restart.$pdy

#echo clean up \$TMPDIR

date

eofC

sbatch $TMPDIR/stage.$PDY/remake.$pdy.sh

