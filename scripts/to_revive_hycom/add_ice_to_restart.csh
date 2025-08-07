#!/bin/csh -x
#
module load envvar/1.0 intel/19.1.3.304 module load PrgEnv-intel/8.1.0 craype/2.7.10 netcdf/4.7.4
setenv NCDFC  /apps/prod/hpc-stack/intel-19.1.3.304/netcdf/4.7.4/
setenv NCDF   /apps/prod/hpc-stack/intel-19.1.3.304/netcdf/4.7.4/
setenv EXTRANCDF `nf-config --flibs`
#
module list
set echo
set time = 1
hostname
date
#
#
# -- Edit following --

setenv TOOLS  ~/tmp/RTOFS_GLO/sorc/HYCOM_tools.fd/

setenv OS LinuxAIF 
 
setenv data_date 20250401

setenv hpss_path /NCEPDEV/emc-ocean/5year/Dan.Iredell/EMC.rtofs.v2.5.a/rtofs.${data_date}

setenv base_dir /lfs/h2/emc/ptmp/santha.akella/data/convert_ESPC-D_to_RTOFS_v2.5
setenv work_dir ${base_dir}/add_ice
setenv restart_noIce ${base_dir}/gen_restart/new_restart

setenv irec 427
# --------------------
#
#
mkdir -p ${work_dir}
cd ${work_dir}
#
# Get ice fields from RTOFS v2.5 restart
/usr/local/bin/htar -xvf ${hpss_path}/rtofs.restart.tar '*'n-06.restart.'*'
/usr/bin/tar -xvzf *n-06.restart.a.tgz
/usr/bin/rm -f *n-06.restart.a.tgz
#
# Use hycom_extract to extract the 3 records from the RTOFS restart.a
${TOOLS}/bin/hycom_extract_${OS} rtofs_glo.t00z.n-06.restart.a 4500 3298 1 ${irec} 1 3 ice_3recs.a > ice_3recs.b

cat ${restart_noIce}.a ice_3recs.a > newrestart_withIce.a
cp ${restart_noIce}.b newrestart_withIce.b

# add the sea ice lines to the new restart.b
# i.e., the last 3 records of *n-06.restart.b to newrestart.b
tail -n 3 rtofs_glo.t00z.n-06.restart.b > last_3Lines
cat newrestart_withIce.b last_3Lines > tmp
mv tmp newrestart_withIce.b

exit 0
