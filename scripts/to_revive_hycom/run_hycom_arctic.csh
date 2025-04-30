#!/bin/csh -x
#PBS -N hycom_arctic
#PBS -o hycom_arctic.o
#PBS -e hycom_arctic.e
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

setenv workDir /lfs/h2/emc/ptmp/santha.akella/data/convert_ESPC-D_to_RTOFS_v2.5

setenv input_arch 031_archm.2025_091_12

setenv IDM 4500
setenv JDM 3298
#----------------------------------------------
#
cp arctic_type.txt ${workDir}
#
cd ${workDir}
#
${TOOLS}/bin/hycom_arctic_g ${input_arch}.a ${IDM} ${JDM} arctic_type.txt ${input_arch}_arctic.a |& tee ${input_arch}_arctic.B

/usr/bin/cp ${input_arch}.b ${input_arch}_arctic.b
#----------------------------------------------

exit 0
