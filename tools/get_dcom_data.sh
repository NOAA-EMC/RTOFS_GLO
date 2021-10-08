#!/bin/bash -l
# this script pulls the required dcom data for rtofs 
# it is recommended that for rtofs=pdy that the data is
# pulled N days earlier 
#

if [ $# -eq 1 ]
then
  export PDY=$1
else
  echo USAGE: $0 YYYYMMDD
  exit -2
fi

. ./user.config

# OUTDIR is the directory dcom/prod under inputroot
OUTDIR=$inputroot/dcom/prod
ln -rnsf $inputroot/dcom/prod $inputroot/dcom/us007003

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load prod_util/1.1.0

echo PDY is $PDY
echo account is $account
echo OUTDIR is $OUTDIR/$PDY

mkdir -p $OUTDIR/$PDY
mkdir -p $TMPDIR/stage.$PDY

# Submit service job to pull ocnqc, nhem, shem, glbl data
cat << eofA > $TMPDIR/stage.$PDY/pull.dcom.sh
#!/bin/ksh -l
#SBATCH --ntasks=1
#SBATCH --partition=service
#SBATCH --time=03:00:00
#SBATCH --account=$account
#SBATCH --job-name=pull_dcom.$PDY
#SBATCH -o $TMPDIR/stage.$PDY/pull_dcom.$PDY.%j
#SBATCH -q batch

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load prod_util/1.1.0
module load hpss

set -x

pdy=$PDY
yyyy=`echo $PDY | cut -c1-4`
yyyymm=`echo $PDY | cut -c1-6`

cd $OUTDIR/\$pdy

dfile=/NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/dcom_prod_\$pdy.tar
d1=./sst/*
d2=./wtxtbul/satSSS/SMOS/*
d3=./wtxtbul/satSSS/SMAP/*
d4=./seaice/pda/*
d5=./wgrdbul/adt/*ncoda*
d6=./b001/xx001
d7=./b001/xx013
d8=./b001/xx101
d9=./b001/xx102
da=./b001/xx103
db=./b001/xx113
dc=./b021/xx201

echo dfile \$dfile
echo datasets "\$d1 \$d2 \$d3 \$d4 \$d5 \$d6 \$d7 \$d8 \$d9 \$da \$db \$dc "

htar -xvf \$dfile \$d1 \$d2 \$d3 \$d4 \$d5 \$d6 \$d7 \$d8 \$d9 \$da \$db \$dc

eofA

sbatch $TMPDIR/stage.$PDY/pull.dcom.sh


# monthly data info
yyyymm=`echo $PDY | cut -c1-6`
yyyy=`echo $PDY | cut -c1-4`
mm=`echo $PDY | cut -c5-6`
if [ $yyyymm -le 202103 ]
then
  monthdata=/gpfs/dell2/emc/obsproc/noscrub/Shelley.Melchior/JW/dcom_d10/$yyyy/$mm
else
  monthdata=/gpfs/dell2/emc/obsproc/noscrub/Shelley.Melchior/dcom_d10/$yyyy/$mm
fi
mkdir -p $OUTDIR/$yyyymm/b031
echo
echo \#################################################
echo
echo Monthly data for this day can be found on WCOSS 
echo $monthdata
echo and copied to
echo $OUTDIR/$yyyymm/b031
echo 
echo and unzipped and mapped the following way
echo
echo bathy.$yyyymm.dcom.gz   to  xx001
echo tesac.$yyyymm.dcom.gz   to  xx002
echo subpfl.$yyyymm.dcom.gz  to  xx005
echo xbtctd.$yyyymm.dcom.gz  to  xx006
echo
echo \#################################################
echo
exit

