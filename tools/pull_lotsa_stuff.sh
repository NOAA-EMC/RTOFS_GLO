#!/bin/bash -l
# this script pulls the required RTOFS data for PDYm1
# rtofs restart and forcing files 
# - ncoda
# -- ocnqc
# -- shem_var
# -- nhem_var
# -- glbl_var
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

mkdir -p $COMOUT/ncoda
mkdir -p $TMPDIR/stage.$PDY

#
# Submit service job to pull ocnqc, nhem, shem, glbl, and gdas forcing
cat << eofA > $TMPDIR/stage.$PDY/pull.lotsa.stuff.sh
#!/bin/ksh -l
#SBATCH --ntasks=1
#SBATCH --partition=service
#SBATCH --time=03:00:00
#SBATCH --account=$account
#SBATCH --job-name=pull_lotsastuff.$PDYm1
#SBATCH -o $TMPDIR/stage.$PDY/pull_lotsa.stuff.$PDYm1.%j
#SBATCH -q batch

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load prod_util/1.1.0
module load hpss

set -x

pdy=$PDYm1
yyyy=`echo $PDY | cut -c1-4`
mm=`echo $PDY | cut -c5-6`

# get ocnqc (plus some other stuff) in nco tanks
cd $COMOUT
htar -xvf /NCEPPROD/5year/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/com_rtofs_prod_rtofs.\$pdy.ncoda.tar

# get 2dvar from emc tanks
cd $COMOUT/ncoda
htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/glbl.tar
htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/nhem.tar
htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/shem.tar

# get forcing (not always available) from emc tanks
cd $COMOUT
htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/rtofs.forcing.tar

eofA

sbatch $TMPDIR/stage.$PDY/pull.lotsa.stuff.sh

exit

