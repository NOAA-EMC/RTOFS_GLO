module purge
module load ips/18.0.1.163
module load impi/18.0.1                  
module load NetCDF/3.6.3
module load ESMF/4_0_0rp2
module load HDF5-serial/1.10.1
module load zlib/1.2.11

# #
# DISTRIBUTION STATEMENT B: Distribution authorized to U.S. Government
# agencies based upon the reasons of possible Premature Distribution
# and the possibility of containing Software Documentation as listed
# on Table 1 of DoD Instruction 5230.24, Distribution Statements on
# Technical Documents, of 23 August 2012. Other requests for this
# document shall be made to Dr. Ruth H. Preller, Superintendent,
# Oceanography Division, U.S. Naval Research Laboratory, DEPARTMENT
# OF THE NAVY, John C. Stennis Space Center, MS 39529-5004; (228)
# 688-4670 (voice); ruth.preller@nrlssc.navy.mil (e-mail).
# #
#! /bin/csh -f

### Change these to your own site and user directory! 
### You will need to create a Makefile Macro in bld/ and a run_ice script 
### in input_templates/.
setenv SITE NCEP.wcossphase3SMSSE
#unset echo
#module switch  intel intel/15.0.0.090
#module switch  cray-mpich cray-mpich/7.2.4
#module switch  cray-libsci cray-libsci/12.2.0
#module switch ics ics/12.1
#module load NetCDF/4.2/serial
module list
#set echo

### SYSTEM_USERDIR is the user's primary scratch disk directory
### SYSTEM_USERDIR is predefined on ORNL machines
#setenv SYSTEM_USERDIR /scr/$user
#setenv SYSTEM_USERDIR /work/$user
setenv SYSTEM_USERDIR /gpfs/dell2/emc/modeling/noscrub/$LOGNAME/tmp

### Grid resolution
# GLBb0.08 
setenv RES GLBb0.08 ; setenv GRID 4500x3297

### Decomposition (cartesian distribution, BPX=BPY=1).
### If using more than one block per processor, or
### if using a load-balanced distribution (ice_in), these 
### parameters set the initial decomposition prior to optimization;
### the actual decomposition may be very different!
### nprocs in ice_in must equal NPX*NPY ###
# for GLBb0.08 
setenv NPX  450   # number of processors in x direction
setenv NPY    4   # number of processors in y direction

setenv BPX    1   # number of blocks per processor in x direction
setenv BPY    1   # number of blocks per processor in y direction

### Specialty code
setenv USE_ESMF yes       # set to yes for ESMF runs
setenv CAM_ICE  no        # set to yes for CAM runs (single column) 
setenv SHRDIR   csm_share # location of CCSM shared code
setenv NETCDF   yes       # set to no if netcdf library is unavailable

echo $USE_ESMF, $CAM_ICE, $SHRDIR, $NETCDF

if ( $SITE == 'LANL.coyote' ) setenv NETCDF no

### Set SRCDIR and EXEDIR to your own paths!
setenv SRCDIR $cwd
setenv EXEDIR $SRCDIR/esmf              ; if !(-d $EXEDIR) mkdir -p $EXEDIR

setenv CBLD   $SRCDIR/bld
setenv OBJDIR $EXEDIR/compile           ; if !(-d $OBJDIR) mkdir -p $OBJDIR
setenv RSTDIR $EXEDIR/restart           ; if !(-d $RSTDIR) mkdir -p $RSTDIR
setenv HSTDIR $EXEDIR/history           ; if !(-d $HSTDIR) mkdir -p $HSTDIR

setenv ARCH `uname -s`
if ( $ARCH == 'UNICOS/mp') setenv ARCH UNICOS
if ( $ARCH == 'UNICOS') then
#   cp -f $CBLD/Makefile.$ARCH $CBLD/Makefile
else
#   cp -f $CBLD/Makefile.std $CBLD/Makefile
endif
setenv ARCH $ARCH.$SITE

cd $SRCDIR/source

cd $EXEDIR
echo "EXEDIR = "$EXEDIR


cd $RSTDIR


### Calculate processor tiling
@ ntask = $NPX * $NPY ; setenv NTASK $ntask
set NXGLOB = `echo $GRID | sed s/x.\*//`
set NYGLOB = `echo $GRID | sed s/.\*x//`
### x grid decomposition
@ a = $NXGLOB / $NPX ; @ rem1 = $NXGLOB % $NPX ; @ b = $a + 1
if ($rem1 == 0) setenv BLCKX $a ; if ($rem1 != 0) setenv BLCKX $b
@ a = $BLCKX / $BPX  ; @ rem2 = $BLCKX % $BPX  ; @ b = $a + 1
if ($rem2 == 0) setenv BLCKX $a ; if ($rem2 != 0) setenv BLCKX $b
### y grid decomposition
@ a = $NYGLOB / $NPY ; @ rem1 = $NYGLOB % $NPY ; @ b = $a + 1
if ($rem1 == 0) setenv BLCKY $a ; if ($rem1 != 0) setenv BLCKY $b
@ a = $BLCKY / $BPY  ; @ rem1 = $BLCKY % $BPY  ; @ b = $a + 1
if ($rem1 == 0) setenv BLCKY $a ; if ($rem1 != 0) setenv BLCKY $b
### max blocks
@ m = $BPX * $BPY ; setenv MXBLCKS $m

echo "NTASK = ", $NTASK
if ($NTASK == 1) then
   setenv COMMDIR serial
else
   setenv COMMDIR mpi 
endif
echo "NTASK = ", $NTASK, $COMMDIR, $SHRDIR
echo $NXGLOB, $NYGLOB, $BLCKX, $BLCKY, $NTASK 
cd $OBJDIR
echo "OBJDIR "$OBJDIR

### List of source code directories (in order of importance).
cat >! Filepath << EOF
$SRCDIR/drivers/esmf
$SRCDIR/source
$SRCDIR/$COMMDIR
$SRCDIR/$SHRDIR
EOF

if ( $ARCH == 'UNICOS.ORNL.phoenix' ) then
   ### use -h command for phoenix
   cc -o makdep -h command $CBLD/makdep.c           || exit 2
else if ( $ARCH == 'Linux.ORNL.jaguar' ) then
   gcc -g -o makdep $CBLD/makdep.c                  || exit 2
else
   cc -o makdep $CBLD/makdep.c                      || exit 2
endif


echo $NXGLOB, $NYGLOB $BLCKX $BLCKY $MXBLCKS
echo "ARCH = ",$ARCH

gmake VPFILE=Filepath   \
           NXGLOB=$NXGLOB NYGLOB=$NYGLOB \
           BLCKX=$BLCKX BLCKY=$BLCKY MXBLCKS=$MXBLCKS \
      -f  $CBLD/Makefile MACFILE=$CBLD/Macros.$ARCH esmf  || exit 2

cd ..
pwd                                         
echo NTASK = $NTASK
echo "global N, N procs, N blocks/proc, block_size (requested)" 
echo "x    $NXGLOB,    $NPX,           $BPX,          $BLCKX"
echo "y    $NYGLOB,    $NPY,           $BPY,          $BLCKY"
echo max_blocks = $MXBLCKS


echo max_blocks = $MXBLCKS

# --- make HYCOM component, and update hycom_cice
#
cd ${SRCDIR}/hycom

# --- force a relink, because CICE is not in the dependencies
/bin/rm -f hycom_cice   ../hycom_cice
/bin/rm -f hycom_cice.o

#   setenv BEI_HOME /site/BEI
#   setenv BEI_HOME /usr/local/BEI
#   setenv ESMF_DIR ${BEI_HOME}/esmf/pgi_11.2.0/4.0.0rp2
#   setenv BEI_HOME /usr/local/usp/COAMPS
#   setenv ESMF_DIR ${BEI_HOME}/esmf/intel/mpi/4.0.0rp2
#   setenv BEI_HOME /u/home/tjc
#   setenv ESMF_DIR ${BEI_HOME}/esmf/intel/intelmpi/4.0.0rp2
#   setenv BEI_HOME /usr/local/usp
#   setenv ESMF_DIR ${BEI_HOME}/esmf/4.0.0rp2
#   setenv BEI_HOME /usr/local/u/wallcraf/pkgs
#   setenv ESMF_DIR ${BEI_HOME}/esmf/intel/intelmpi/4.0.0rp2
#   setenv BEI_HOME /u/home/wallcraf/pkgs
#   setenv ESMF_DIR ${BEI_HOME}/esmf/intelICE/intelmpi/4.0.0rp2
#    setenv BEI_HOME /p/home/wallcraf/pkgs
#    setenv ESMF_DIR /usrx/local/esmf-4.0.0rp1
    setenv ESMF_DIR /usrx/local/prod/packages/ips/18.0.1/impi/18.0.1/esmf/4_0_0rp2

    setenv CICE_DIR ${SRCDIR}
echo "ESMF_DIR = "  $ESMF_DIR
echo "CICE_DIR = "  $CICE_DIR

#setenv ARCH Asp6-nofl 
#setenv ARCH Axt5i-mpi2io
#setenv ARCH AintelICE-mpi2io
#setenv ARCH Axe6-mpi2io
#setenv ARCH Aintel-impi
#setenv ARCH AintelICE-impi
#setenv ARCH Aintelsse-impi
#setenv ARCH Axc30-intel-relo
#setenv ARCH Axc40-intel-relo
setenv ARCH Aintelsse-impi-relo
setenv TYPE cice 
echo $ARCH $TYPE
make ARCH=$ARCH TYPE=$TYPE hycom_cice
#
ln -f hycom_cice ../hycom_cice
