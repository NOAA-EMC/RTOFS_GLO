#!/bin/sh
set -x

RD=$1
#RD=20250504
Di=/lfs/h2/emc/couple/noscrub/zulema.garraffo/COMDIR/prod/com/rtofs/v2.5/rtofs.$RD
Do=/lfs/h2/emc/couple/noscrub/$USER/COMDIR/prod/com/rtofs/v2.5/rtofs.$RD
#cactus directory fix_dir
fix_dir=/lfs/h2/emc/couple/noscrub/zulema.garraffo/fix_archv2mom6
mkdir -p $Do


bindir=$home1/bin
#exec_dir=/lfs/h2/emc/couple/save/zulema.garraffo/rtofs.v3.0.0/exec
exec_dir=/lfs/h2/emc/couple/noscrub/zulema.garraffo/HYCOM-tools/mom6/src

Dout=/lfs/h2/emc/ptmp/$USER/rtofs.$RD
mkdir -p $Dout
cd $Dout
ln -sf $fix_dir/rtofs_glo.navy_0.08.regional.grid.a regional.grid.a
ln -sf $fix_dir/rtofs_glo.navy_0.08.regional.grid.b regional.grid.b
ln -sf $fix_dir/depth_GLBb0.08_09m11ob2.a regional.depth.a
ln -sf $fix_dir/depth_GLBb0.08_09m11ob2.b regional.depth.b
ln -sf $Di/rtofs_glo.t00z.n00.archv.nc .

text='MOM6'
E=950


ocnpf=rtofs_glo.t00z.n00.archv
rm -f $ocnpf.a

itest=1000
jtest=2000

nrec=1 #archv only 1 time record
#'SAME' can be replaced by a variable by variable file name 
$exec_dir/mom6nc2archv<< E-o-D  
./$ocnpf.nc
potT
SAME
salt
SIG2
34.0
SAME
h
SAME
SSH
pbt
ZERO
ZERO
SAME
u
SAME
v
NONE
KE
0            'ltracr' = number of layer     tracers
0            'ltracu' = number of u-layer   tracers (optional, default 0)
0            'ltracv' = number of v-layer   tracers (optional, default 0)
0            'itracr' = number of interface tracers (ntracr=ltracr+ltracu+ltracv+itracr, =0 skip file& name
NONE
sic
sih
sit
./$ocnpf.a
$nrec   'in_rec' = time record to read in (for potT)
$E      'iexpt ' = experiment number x10
$itest     'itest ' = i-index for debugging printout (0 no debug)
$jtest      'jtest ' = j-index for debugging printout (0 no debug)
0.3       'tmljmp' = equivalent temperature jump across mixed-layer (degC)
E-o-D
#rm -f regional.depth.? regional.grid.?
err=$?
if [ $err -ne  0 ]; then  
   exit 1
fi
date
cp -p $ocnpf.[a,b]a $Do/.

