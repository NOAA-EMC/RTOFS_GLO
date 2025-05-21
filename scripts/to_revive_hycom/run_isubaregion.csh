#!/bin/csh -x
#PBS -N rtofs_convert_GLB_y_b_0.08
#PBS -o convert_GLB_y_b_0.08.o
#PBS -e convert_GLB_y_b_0.08.e
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
# --- form interpolated subregion archive files, GLBy0.08 to GLBb0.08.
# --- GLBy0.0 has 41 layers, which are retained.
#
# -- IGNORE FOLLOWING --
#
# --- RT is the original region for the depth file
# --- R  is the original region
# --- U  is the target   region
# --- E  is the experiment number
# --- S  is the location to run from
# --- D  is the location of the original  archive files
# --- N  is the location of the subregion archive files <-- This is what we get out of all this!
# --- TR is the location of the original  topo    files
# -- --
#
setenv RT GLBy0.08
setenv R  ESPC-D-V02
setenv U  GLBb0.08
setenv E  031
#
setenv X  `echo ${E} | awk '{printf("%04.1f", $1*0.1)}'`
#
# -- Edit following --
setenv OS LinuxAIF
setenv TOOLS  ~/tmp/RTOFS_GLO/sorc/HYCOM_tools.fd/

# Path to work is done
setenv N  /lfs/h2/emc/ptmp/santha.akella/data/convert_ESPC-D_to_RTOFS_v2.5
setenv S  ${N}/${E}

# Path to ESPC-D bathymetry, grid, mapping weights
setenv TR /lfs/h2/emc/couple/noscrub/santha.akella/data_files/ESPC-D

# Path to downloaded ESPC-D daily mean archive
setenv D  /lfs/h2/emc/ptmp/santha.akella/ESPC-D/daily_mean_archm

# Date of the downloaded archive: in yyyymmddHH format; HH is UTC hour
setenv date_yyyymmddHH 2025040112
# -- --
#

setenv BINRUN ""
#
mkdir -p $N
mkdir -p $S
cd       $S
#
/bin/rm   regional.depth.a regional.depth.b
touch     regional.depth.a regional.depth.b
if (-z    regional.depth.a) then
  /bin/rm regional.depth.a
  /bin/ln -s ${TR}/depth_GLBy0.08_27.a regional.depth.a
endif
if (-z    regional.depth.b) then
  /bin/rm regional.depth.b
  /bin/ln -s ${TR}/depth_GLBy0.08_27.b regional.depth.b
endif
#
touch     regional.grid.a regional.grid.b
if (-z    regional.grid.a) then
  /bin/rm regional.grid.a
  /bin/ln -s ${TR}/regional.grid.a .
endif
if (-z    regional.grid.b) then
  /bin/rm regional.grid.b
  /bin/ln -s ${TR}/regional.grid.b .
endif
#
# --- convert archive
#
foreach ymdh ( ${date_yyyymmddHH} )
  setenv M US058GCOM-OPSnce.espc-d-031-hycom_fcst_glby008_${ymdh}_M0000_archm
  if (-e ${D}/${M}.a) then
    setenv y   `echo $ymdh | cut -c 1-4`
    setenv m   `echo $ymdh | cut -c 5-6`
    setenv d   `echo $ymdh | cut -c 7-8`
    setenv h   `echo $ymdh | cut -c 9-10`
    echo $y $m $d $h
    setenv YDH `echo $y $m $d $h | ${TOOLS}/bin/hycom_ymdh_wind_${OS} | ${TOOLS}/bin/hycom_wind_date_${OS}`
    echo  $YDH
    touch   ${N}/${E}_archm.${YDH}.b
    /bin/rm ${N}/${E}_archm.${YDH}.[ab]
    /usr/bin/time ${BINRUN} ${TOOLS}/subregion/src/isubaregion <<E-o-D
/lfs/h2/emc/couple/noscrub/santha.akella/data_files/v2.5/topog/regional.grid.a
/lfs/h2/emc/couple/noscrub/santha.akella/data_files/ESPC-D/regional.gmapi_GLBy0.08.a
/lfs/h2/emc/couple/noscrub/santha.akella/data_files/v2.5/topog/depth_GLBb0.08_09m11.a
regional.depth.a
${D}/${M}.a
${N}/${E}_archm.${YDH}.a 
${R} interpolated to ${U} 
4500  'idm   ' = target longitudinal array size
3298  'jdm   ' = target latitudinal  array size
   0  'iceflg' = ice in output archive flag (0=none,1=energy loan model)
   0  'smooth' = smooth interface depths    (0=F,1=T)
E-o-D
#
    touch  ${N}/${E}_archm.${YDH}.b
    if (-z ${N}/${E}_archm.${YDH}.b) then
      echo "missing archive file: " ${N}/${E}_archm.${YDH}.b
    # exit (2)
    endif
#
  endif
  date
end
