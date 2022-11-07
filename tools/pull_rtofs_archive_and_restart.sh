#!/bin/bash
# this script pulls the RTOFS restart and ab files
# and then tar-gunzips the .tgz files
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

mkdir -p $TMPDIR/stage.$PDY

#
# Submit service job to pull rtofs restarts and archives
cat << eofA > $TMPDIR/stage.$PDY/pull.rtofs.data.sh
#!/bin/ksh -l
#SBATCH --ntasks=1
#SBATCH --partition=service
#SBATCH --time=05:00:00
#SBATCH --account=$account
#SBATCH --job-name=pull_rtofsdata.$PDYm1
#SBATCH -o $TMPDIR/stage.$PDY/pull_rtofs.data.$PDYm1.%j
#SBATCH -q batch

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load prod_util/1.1.0
module load hpss
module list

set -x

pdy=$PDYm1
yyyy=\`echo \$pdy | cut -c1-4\`
mm=\`echo \$pdy | cut -c5-6\`

# get rtofs data
cd $COMOUT

# find version
vers=\$(hsi -P ls /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/ | grep rtofs.\${pdy}.restart.tar.idx | cut -d_ -f3)

# restart file (if available)
rlist=
rlist="./rtofs_glo.t00z.n00.restart.b ./rtofs_glo.t00z.n00.restart.a.tgz ./rtofs_glo.t00z.n00.restart_cice.tgz "
rlist="\$rlist ./rtofs_glo.t00z.n-24.restart.b ./rtofs_glo.t00z.n-24.restart.a.tgz ./rtofs_glo.t00z.n-24.restart_cice.tgz "
if [ \$pdy -gt 20210427 ]
then
   rlist="\$rlist ./rtofs_glo.t00z.n-06.restart.b ./rtofs_glo.t00z.n-06.restart.a.tgz ./rtofs_glo.t00z.n-06.restart_cice.tgz "
else
   echo
   echo rtofs_glo.t00z.n-06.restart not in this archive.
   echo
fi

htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/com_rtofs_\${vers}_rtofs.\$pdy.restart.tar \$rlist

# ab in 5 year
alist=
for f in 00 01 02 03 04 05 06 07 8 9 10 11 12
do
  fd=\$f
  if [[ \$f -eq 8 || \$f -eq 9 ]]
  then
    fd=0\$f
  fi
  alist="\$alist ./rtofs_glo.t00z.f\$fd.arche.a.tgz ./rtofs_glo.t00z.f\$fd.arche.b"
  alist="\$alist ./rtofs_glo.t00z.f\$fd.archs.a.tgz ./rtofs_glo.t00z.f\$fd.archs.b"
  if [[ \$f -eq 06 || \$f -eq 12 ]]
  then
    alist="\$alist ./rtofs_glo.t00z.f\$fd.archv.a.tgz ./rtofs_glo.t00z.f\$fd.archv.b"
  fi
done
alist="\$alist ./rtofs_glo.t00z.n00.arche.a.tgz ./rtofs_glo.t00z.n00.arche.b"
alist="\$alist ./rtofs_glo.t00z.n00.archs.a.tgz ./rtofs_glo.t00z.n00.archs.b"
alist="\$alist ./rtofs_glo.t00z.n00.archv.a.tgz ./rtofs_glo.t00z.n00.archv.b"
for n in 01 02 03 04 05 06 07 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
do
  nd=\$n
  if [[ \$n -eq 8 || \$n -eq 9 ]]
  then
    nd=0\$n
  fi
  alist="\$alist ./rtofs_glo.t00z.n-\${nd}.arche.a.tgz ./rtofs_glo.t00z.n-\${nd}.arche.b"
  alist="\$alist ./rtofs_glo.t00z.n-\${nd}.archs.a.tgz ./rtofs_glo.t00z.n-\${nd}.archs.b"
  if [[ \$f -eq 06 || \$f -eq 12 || \$f -eq 18 || \$f -eq 24 ]]
  then
    alist="\$alist ./rtofs_glo.t00z.n-\${nd}.archv.a.tgz ./rtofs_glo.t00z.n-\${nd}.archv.b"
  fi
done

htar -xvf /NCEPPROD/5year/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/com_rtofs_\${vers}_rtofs.\$pdy.ab.tar \$alist

# untar the .tgz files
for a in \`ls *arch*tgz\`;do echo \$a;tar xpvzf \$a;done &
for r in \`ls *restart*tgz\`;do echo \$r;tar xpvzf \$r;done &

wait

# rm the .tgz files
rm -f *.tgz

eofA

sbatch $TMPDIR/stage.$PDY/pull.rtofs.data.sh

exit

