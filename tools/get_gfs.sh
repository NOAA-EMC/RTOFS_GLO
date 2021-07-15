#!/bin/bash
# this script pulls the gfs data for PDY

if [ $# -eq 1 ]
then
  export PDY=$1
else
  echo USAGE: $0 YYYYMMDD
  exit -2
fi

. ./user.config

# OUTDIR is gfs/prod under inputroot
OUTDIR=$inputroot/gfs/prod

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load prod_util/1.1.0

echo PDY is $PDY
echo account is $account
echo OUTDIR is $OUTDIR

mkdir -p $OUTDIR
mkdir -p $TMPDIR/stage.$PDY

#Submit service job to get gfs sflux files
cat << eofA > $TMPDIR/stage.$PDY/pull.gfs.sh
#!/bin/ksh -l
#SBATCH --ntasks=1
#SBATCH --partition=service
#SBATCH --time=03:00:00
#SBATCH --account=$account
#SBATCH --job-name=pull_gfs.$PDY
#SBATCH -o $TMPDIR/stage.$PDY/pull_gfs.$PDY.%j
#SBATCH -q batch

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load prod_util/1.1.0
module load hpss

set -x

pdy=$PDY
yyyy=`echo $PDY | cut -c1-4`
yyyymm=`echo $PDY | cut -c1-6`

cd $OUTDIR

slist=
for h in 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99
do
slist="\$slist ./gfs.\$pdy/00/atmos/gfs.t00z.sfluxgrbf0\$h.grib2"
done
htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/com_gfs_prod_gfs.\${pdy}_00.gfs_flux.tar \$slist

slist=
for h in 087 090 093 096 099 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189
do
slist="\$slist ./gfs.\$pdy/06/atmos/gfs.t06z.sfluxgrbf\$h.grib2"
done
htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/com_gfs_prod_gfs.\${pdy}_06.gfs_flux.tar \$slist

eofA

sbatch $TMPDIR/stage.$PDY/pull.gfs.sh

  
