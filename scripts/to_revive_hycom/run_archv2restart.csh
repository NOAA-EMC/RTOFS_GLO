#!/bin/csh -x
#PBS -N run_archv2restart
#PBS -o run_archv2restart.o
#PBS -e run_archv2restart.e
#PBS -W umask=027
#PBS -l walltime=0:30:00
#PBS -A RTOFS-DEV
#PBS -q dev
#PBS -l select=1:ncpus=8:mpiprocs=1:mem=120G
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
# -- Edit following --

setenv TOOLS  ~/tmp/RTOFS_GLO/sorc/HYCOM_tools.fd/

setenv grid_topo_path /lfs/h2/emc/couple/noscrub/santha.akella/data_files/v2.5/topog/

setenv workDir /lfs/h2/emc/ptmp/santha.akella/data/convert_ESPC-D_to_RTOFS_v2.5

# Any file suffices for a template
setenv template_restart /lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR1/prod/com/rtofs/v2.5/rtofs.20250430/rtofs_glo.t00z.n-24.restart.a

setenv archv ${workDir}/031_archm.2025_091_12_arctic.a

setenv out_restart new_restart
#----------------------------------------------
#
# Input arguments:
# ---  input archive file
# ---  input restart template file
# --- output restart file
#
mkdir -p ${workDir}/gen_restart
cd ${workDir}/gen_restart
#
if (-e ${out_restart}.a) /usr/bin/rm -f ${out_restart}.a
if (-e ${out_restart}.b) /usr/bin/rm -f ${out_restart}.b
#
ln -s ${grid_topo_path}/regional.grid.a .
ln -s ${grid_topo_path}/regional.grid.b .

ln -s ${grid_topo_path}/depth_GLBb0.08_09m11.a regional.depth.a
ln -s ${grid_topo_path}/depth_GLBb0.08_09m11.b regional.depth.b
#
$TOOLS/archive/src/archv2restart << E-o-D
$archv
$template_restart
${out_restart}.a
000     'iexpt '   = experiment number x10  (000=from archive file)
3       'yrflag'   = days in year flag (0=360J16,1=366J16,2=366J01,3=actual)
4500    'idm   '   = longitudinal array size
3298    'jdm   '   = latitudinal  array size
-1      'kapref'   = thermobaric reference state (-1 to 3, optional, default 0)
41      'kdm   '   = number of layers
34.0    'thbase' = reference density (sigma units)
60    'baclin'   = baroclinic time step (seconds), int. divisor of 86400
0     'rmontg'
E-o-D
#
exit 0
