#!/bin/bash
# this script pulls the gdas data for PDY

if [ $# -eq 1 ]
then
  export PDY=$1
else
  echo USAGE: $0 YYYYMMDD
  exit -2
fi

. ./user.config

# gfs data is in gfs/prod under inputroot
OUTDIR=$inputroot/gfs/prod

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load prod_util/1.1.0

echo PDY is $PDY
echo account is $account
echo OUTDIR is $OUTDIR

mkdir -p $OUTDIR
mkdir -p $TMPDIR/stage.$PDY

#Submit service job to get gdas sflux files
cat << eofA > $TMPDIR/stage.$PDY/pull.gdas.sh
#!/bin/ksh -l
#SBATCH --ntasks=1
#SBATCH --partition=service
#SBATCH --time=03:00:00
#SBATCH --account=$account
#SBATCH --job-name=pull_gdas.$PDY
#SBATCH -o $TMPDIR/stage.$PDY/pull_gdas.$PDY.%j
#SBATCH -q batch

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load prod_util/1.1.0
module load hpss
module list

set -x

pdy=$PDY
yyyy=`echo $PDY | cut -c1-4`
yyyymm=`echo $PDY | cut -c1-6`

cd $OUTDIR

# find version
vers=\$(hsi -P ls /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/ | grep gdas.\${pdy}_00.gdas_flux.tar.idx | cut -d_ -f3)

for c in 00 06 12 18
do
slist=
for h in 3 6 9
do
slist="\$slist ./gdas.\$pdy/\$c/atmos/gdas.t\${c}z.sfluxgrbf00\$h.grib2"
done
echo
echo \$slist
echo
htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/com_gfs_\${vers}_gdas.\${pdy}_\${c}.gdas_flux.tar \$slist
done

eofA

sbatch $TMPDIR/stage.$PDY/pull.gdas.sh

