#!/bin/bash

if [[ $# -lt 1 ]]; then
  echo " "
  echo "Usage: "
  echo "$0" "start date of experiment"
  echo " "
  exit 1
fi
echo " "

run_date=$1
#run_date=20250326

# make sure these paths exist:
ptmp_path=/lfs/h2/emc/ptmp/santha.akella/ 

fix_path=/lfs/h2/emc/eib/save/dan.iredell/rtofs.v2.5.REL1/
templ_path=/lfs/h2/emc/couple/noscrub/santha.akella/rtofs_forecast_template_files/
# --

exp_dir=${ptmp_path}/rtofs.${run_date}
mkdir -p ${exp_dir}
cd ${exp_dir}
# --

# Parm files
ln -s ${fix_path}/parm/rtofs_glo.navy_0.08.archs.input archs.input
ln -s ${fix_path}/parm/rtofs_glo.navy_0.08.patch.input patch.input
# --

# Fix files
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.cb_11_10mm.a cb.a
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.cb_11_10mm.b cb.b

ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.forcing.chl.a forcing.chl.a
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.forcing.chl.b forcing.chl.b

ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.forcing.offlux.a forcing.offlux.a
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.forcing.offlux.b forcing.offlux.b

ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.forcing.rivers.a forcing.rivers.a
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.forcing.rivers.b forcing.rivers.b

ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.iso.sigma.a iso.sigma.a
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.iso.sigma.b iso.sigma.b

ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.relax_ssh.a relax.ssh.a
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.relax_ssh.b relax.ssh.b

ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.relax_sss.a relax.sssrmx.a
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.relax_sss.b relax.sssrmx.b

ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.cice.prec_lanl_12.r cice.prec_lanl_12.r
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.cice.rhoa_ncar85-88_12.r cice.rhoa_ncar85-88_12.r

for fn in regional.cice.r regional.depth.a regional.depth.b regional.grid.a regional.grid.b ; do
  ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.${fn} ${fn}
done

for fn in surtmp4.a surtmp4.b tbaric.a tbaric.b thkdf4.a thkdf4.b veldf2.a veldf2.b veldf4.a veldf4.b ; do
  ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.${fn} ${fn}
done

ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.relax_int.a relax.intf.a
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.relax_int.b relax.intf.b

ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.relax_sal.a relax.saln.a
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.relax_sal.b relax.saln.b

ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.relax_tem.a relax.temp.a
ln -s ${fix_path}/fix/rtofs_glo.navy_0.08.relax_tem.b relax.temp.b
# --

# Run time configuration, restarts, etc
cp ${templ_path}/blkdat.input .
cp ${templ_path}/ice_in .
cp ${templ_path}/cice.restart_file .
cp ${templ_path}/limits .

cp ${templ_path}/job_card .
exit 0
