#!/bin/bash
set -x

RD=$1
Di=/lfs/h2/emc/couple/noscrub/zulema.garraffo/COMDIR/prod/com/rtofs/v2.5/rtofs.$RD
Do=/lfs/h2/emc/couple/noscrub/$USER/COMDIR/prod/com/rtofs/v2.5/rtofs/v2.5/rtofs.$RD
fix_dir=/lfs/h2/emc/couple/noscrub/zulema.garraffo/fix_archv2mom6

E=950

Drestart=$expt_dir/restart_template
exec_dir=/lfs/h2/emc/couple/noscrub/zulema.garraffo/HYCOM-tools/archive/src
#a v2.5 restart to be put into the fix directory: 
rest_templt=/lfs/h2/emc/couple/noscrub/zulema.garraffo/restart_template

Dout=/lfs/h2/emc/ptmp/$USER/archv2restart
mkdir $Dout
cd $Dout 
ln -sf $fix_dir/rtofs_glo.navy_0.08.regional.grid.a regional.grid.a
ln -sf $fix_dir/rtofs_glo.navy_0.08.regional.grid.b regional.grid.b
ln -sf $fix_dir/depth_GLBb0.08_09m11ob2.a regional.depth.a
ln -sf $fix_dir/depth_GLBb0.08_09m11ob2.b regional.depth.b

#
# ---  input archive file
#1
#---  input restart template file
# --- output restart file
#
prefx=rtofs_glo.t00z.n00
rm -f $prefx.restart.a
ln -sf /lfs/h2/emc/couple/noscrub/zulema.garraffo/COMDIR/prod/com/rtofs/v2.5/rtofs.$RD/$prefx.archv.a .
ln -sf /lfs/h2/emc/couple/noscrub/zulema.garraffo/COMDIR/prod/com/rtofs/v2.5/rtofs.$RD/$prefx.archv.b .


$exec_dir/archv2restart << E-o-D
$prefx.archv.a
$rest_templt/$prefx.restart.a
$prefx.restart.a
$E     'iexpt '   = experiment number x10  (000=from archive file)
3       'yrflag'   = days in year flag (0=360J16,1=366J16,2=366J01,3=actual)
4500    'idm   '   = longitudinal array size
3298    'jdm   '   = latitudinal  array size
-1      'kapref'   = thermobaric reference state (-1 to 3, optional, default 0)
41      'kdm   '   = number of layers
34.0    'thbase' = reference density (sigma units)
300    'baclin'   = baroclinic time step (seconds), int. divisor of 86400
0      'rmontg'
E-o-D
#rm -f regional.grid.? regional.depth.? 
cp -p $prefx.restart.[a,b]  $Do/.
