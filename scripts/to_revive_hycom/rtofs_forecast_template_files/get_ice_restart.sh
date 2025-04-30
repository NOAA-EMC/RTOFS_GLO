#!/bin/sh -x

run_date=20250320

ptmp_path=/lfs/h2/emc/ptmp/santha.akella/
exp_dir=${ptmp_path}/rtofs.${run_date}
# --

HD=emc-ocean/5year/Dan.Iredell/rtofs.v2.5.test01/rtofs.${run_date}

cd ${exp_dir}
/usr/local/bin/htar -xvf $HD/rtofs.restart.tar '*'n-06.restart_cice*

tar -xvzf *.n-06.restart_cice.tgz
rm -f *.n-06.restart_cice.tgz
