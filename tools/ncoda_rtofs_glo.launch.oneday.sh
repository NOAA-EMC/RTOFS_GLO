#!/bin/bash
#
##set -x

if [ $# -ne 2 ] 
then
  echo "USAGE: $0 <configname> <YYYYMMDD>"
  exit -2
fi

export configname=$1
export today=$2
if [ ! -s ./$configname ]
then
  echo cannot find $configname
  exit -2
fi
. ./user.config
. ./$configname

echo
echo heraproj $heraproj
echo projectroot $projectroot
echo inputroot $inputroot
echo comroot $comroot
echo tmproot $tmproot
echo account $account
echo

batchloc=./batchscripts
mkdir -p $batchloc

#######################################################

module purge
module use $projectroot/modulefiles
module load runtime_hera_rtofs.module
module list

#override COMs
export COMtmp=$comroot
export COMgfs=$inputroot

# Set some run environment variables.
export SENDCOM=YES
export SENDDBN=NO
export model_ver=2.1.1
export PROJECTdir=$projectroot
export projID=NC-${model_ver} #      `basename $PROJECTdir`
export KEEPDATA=YES

export cyc=00
export cycle=t${cyc}z
export RUN_ENVIR=dev
export envir=prod # prod or para

# HERA mods - no gribbing or cdo outputs
export for_opc=NO
export grib_1hrly=NO
#--
export rtofs_glo_ver=v${model_ver}
export HOMErtofs=${PROJECTdir}

export NWROOT=$UTILROOT
export COMROOT=${COMtmp}/com
export GESROOT=${COMtmp}/nwges
export GESOUT=$GESROOT/

# observational TANKS
export DCOMROOT=$inputroot/dcom
export DCOMINAMSR=$DCOMROOT/prod
export DCOMINSSH=$DCOMROOT/prod
export DCOMINSSS=$DCOMROOT/prod
export DCOMINSST=$DCOMROOT/prod

#export NPROCS=900
#export NPROCS=1800
hindcast=NO # YES or NO
testcast=NO # YES or NO

############# this is the location of the bufr_dumplist 
export HOMEobsproc_shared_bufr_dumplist=/scratch2/NCEPDEV/marine/Dan.Iredell/obsproc

# Get the length of the forecast from a config file
RUN=rtofs
modID=glo
inputgrid=navy_0.08
. $PROJECTdir/parm/${RUN}_${modID}.${inputgrid}.config
export fcstdays=`expr ${fcstdays_step1} + ${fcstdays_step2} + ${fcstdays_step3}`
#
# Redefine default top level directories for rtofs_forcing_getges.sh script
hindcast=YES
if [ $hindcast = YES ] 
then
  export GETGES_COM=$COMgfs
fi
hindcast=NO
if [ $hindcast = NO ] &&  [ $testcast = NO ]
then
  # export GETGES_COM=/gpfs/dell2/emc/modeling/noscrub/$LOGNAME/com2   ## /com2
  # export GETGES_COM=/gpfs/hps/nco/ops/com   ## /com2
 #  export COMINgdas=${COMINgdas:-$(compath.py gfs/prod/gdas)}
  export COMINgdas=${COMgfs}/gfs
  export GETGES_COM=${GETGES_COM:-${COMINgdas/"/gfs/prod/gdas"/}}
  export envirges=prod
fi 

# hindcast!!!!!!  
#export GETGES_COM=$COMtmp/com
#echo GETGES_COM is $GETGES_COM

# Create temporary directory.
# Set paths to directories.
# for date scripts
export EXECutil=${NWROOT}/exec
export HOMEout=$COMtmp

#=== DON'T EDIT BELOW THIS LINE ==================

# very important: redefinition of the default date (PDY !!!
# if PDY is defined here, it will not be reset by setpdy utility.
export PDY=$today
export DATAROOT=$tmproot/${projID}/$PDY
mkdir -p ${DATAROOT}

# This variable is used in hidncast mode to set up forcing directories.
let analhrs=analdays*24
nowcast_start=`$NDATE -${analhrs} ${today}${cyc} | cut -c1-8`
forecast_start=$today

# Calculate forecast length.
export nowcast_end=$today
let fcsthrs=${fcstdays}*24
export forecast_end=`$NDATE $fcsthrs ${forecast_start}${cyc} | cut -c1-8`

# Test restart file in hindcast

if [ $hindcast = YES ] 
then
  RESTdir=${HOMEout}/rtofs/nwges/rtofs.`$NDATE -24 ${PDY}'00' | cut -c1-8`
  RESTfile=${RESTdir}/rtofs_glo.t${cyc}z.restart_f24
  if ! [ -s ${RESTfile}.a ] || ! [ -s  ${RESTfile}.b ]
  then
      echo "LAUNCHER ERROR: No restart found."
      echo "                FILE:  ${RESTfile}.[ab] "    
#      exit -3
   fi
fi

# Write out some info.
echo "LAUNCHER INFO: run: ${projID}, cycle: t${cyc}z, PDY=${PDY}."
echo "LAUNCHER INFO: forecast starts at ${forecast_start}, ends at ${forecast_end}."

# Set up temporary fluxes directory for hindcast
if [ $hindcast = YES ] 
then
  #
  # Nowcast forcing
  mkdir -p ${GETGES_COM}/gfs/$envir
  cdate=`$NDATE -24 ${nowcast_start}'00' | cut -c1-8`
  while [ $cdate -le $nowcast_end ] 
  do
    if [ -d $FLUXDIR/gdas.${cdate} ]
    then
      ln -s $FLUXDIR/gdas.${cdate} ${GETGES_COM}/gfs/$envir/gdas.${cdate}
    else
      echo "LAUNCHER ERROR: No forcing directory found."
      echo "                DIR:  $FLUXDIR/gdas.${cdate}"    
##      exit -3
    fi
    cdate=`$NDATE 24 ${cdate}'00' | cut -c1-8`
  done
  #
  # Forecast forcing
  cdate=`$NDATE -24 ${forecast_start}'00' | cut -c1-8`
  while [ $cdate -le $forecast_end ] 
  do
    if [ -d $FLUXDIR/gfs.${cdate} ]
    then
      ln -s $FLUXDIR/gfs.${cdate} ${GETGES_COM}/gfs/$envir/gfs.${cdate}
    else
      echo "LAUNCHER ERROR: No forcing directory found."
      echo "                DIR:  $FLUXDIR/gfs.${cdate}"    
##      exit -3
    fi
    cdate=`$NDATE 24 ${cdate}'00' | cut -c1-8`
  done

fi

# Make logs directory if necessary.
test -d $HOMEout/logs || mkdir -p $HOMEout/logs

pid=$$


# Submit the job.
##module load lsf


# area for testing only one job
testjustthisjob=0
if [ $testjustthisjob -eq 1 ]
then

echo justthisonejob no jobs

exit
fi #testjustthisjob


# Submit pre-analysis
skipthis=0
if [ $skipthis -eq 0 ];then
sleep 1
export jobid=jrtofs_analysis_pre
mkdir -p ${DATAROOT}/$jobid
cat << EOF_analysis_pre > $batchloc/rtofs.analysis_pre.$pid
#!/bin/ksh -l
#SBATCH --nodes=1 
#SBATCH --ntasks=1
#SBATCH --time=00:30:00
#SBATCH --account=$account
#SBATCH --job-name=RTOFS_PRE_ANAL
#SBATCH -o $DATAROOT/rtofs_analysis_pre.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_ANALYSIS_PRE )
EOF_analysis_pre

jobid_anal_pre=$(sbatch $batchloc/rtofs.analysis_pre.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO analysis pre is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO analysis pre not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

# Submit analysis
export jobid=jrtofs_analysis
mkdir -p ${DATAROOT}/$jobid
cat << EOF_analysis > $batchloc/rtofs.analysis.$pid
#!/bin/ksh -l
#SBATCH --ntasks=900
#SBATCH --time=01:00:00
#SBATCH --account=$account
#SBATCH --job-name=RTOFS_ANAL
#SBATCH -d afterok:$jobid_anal_pre
#SBATCH -o $DATAROOT/rtofs_analysis.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export NMPI=900
export NPROCS=900
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_ANALYSIS )
EOF_analysis

jobid_anal=$(sbatch $batchloc/rtofs.analysis.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO analysis is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO analysis not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi


sleep 1
fi # skipthis

#
# Submit forecast step1 pre
export jobid=jrtofs_forecast_step1_pre
mkdir -p ${DATAROOT}/$jobid
cat << EOF_forecast_step1_pre > $batchloc/rtofs.forecast_step1_pre.$pid
#!/bin/ksh -l
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=00:30:00
#SBATCH --account=$account
#SBATCH --job-name=RTOFS_PRE_FCST1
#SBATCH -d afterok:$jobid_anal_pre
#SBATCH -o $DATAROOT/rtofs_forecast_step1_pre.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_FORECAST_STEP1_PRE )
EOF_forecast_step1_pre

jobid_fcst1_pre=$(sbatch $batchloc/rtofs.forecast_step1_pre.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO forecast_step1 pre is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO forecast_step1 pre not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

sleep 1
export jobid=jrtofs_forecast_step1
mkdir -p ${DATAROOT}/$jobid
cat << EOF_forecast_step1 > $batchloc/rtofs.forecast_step1.$pid
#!/bin/ksh -l
#SBATCH --ntasks=900
#SBATCH --time=04:00:00
#SBATCH --account=$account
#SBATCH --job-name=RTOFS_FCST1
#SBATCH -d afterok:$jobid_anal:$jobid_fcst1_pre
#SBATCH -o $DATAROOT/rtofs_fcst1.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export NMPI=900
export NPROCS=900
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_FORECAST_STEP1 )
EOF_forecast_step1

jobid_fcst1=$(sbatch $batchloc/rtofs.forecast_step1.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO forecast step1 is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO forecast step1 not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

# stop here after fcst step1 only (and no post for forecasts)
# remember to change ../parm/*.config for number of days
stophere=1
if [ $stophere -eq 1 ]
then
  echo
  echo
  echo That was the last job
  echo
  echo
  exit -99
fi

#
# Submit forecast step2 pre
export jobid=jrtofs_forecast_step2_pre
mkdir -p ${DATAROOT}/$jobid
cat << EOF_forecast_step2_pre > $batchloc/rtofs.forecast_step2_pre.$pid
#!/bin/ksh -l
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=00:30:00
#SBATCH --account=$account
#SBATCH --job-name=RTOFS_PRE_FCST2
#SBATCH -d afterok:$jobid_fcst1_pre
#SBATCH -o $DATAROOT/rtofs_forecast_step2_pre.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_FORECAST_STEP2_PRE )
EOF_forecast_step2_pre

jobid_fcst2_pre=$(sbatch $batchloc/rtofs.forecast_step2_pre.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO forecast_step2 pre is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO forecast_step2 pre not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

sleep 1
export jobid=jrtofs_forecast_step2
mkdir -p ${DATAROOT}/$jobid
cat << EOF_forecast_step2 > $batchloc/rtofs.forecast_step2.$pid
#!/bin/ksh -l
#SBATCH --ntasks=900
#SBATCH --time=03:00:00
#SBATCH --account=$account
#SBATCH --job-name=RTOFS_FCST2
#SBATCH -d afterok:$jobid_fcst1:$jobid_fcst2_pre
#SBATCH -o $DATAROOT/rtofs_fcst2.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export NMPI=900
export NPROCS=900
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_FORECAST_STEP2 )
EOF_forecast_step2

jobid_fcst2=$(sbatch $batchloc/rtofs.forecast_step2.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO forecast step2 is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO forecast step2 not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi


# stop here after fcst step2 only (and no post for forecasts)
# remember to change ../parm/*.config for number of days
stophere=0
if [ $stophere -eq 1 ]
then
  echo
  echo
  echo That was the last job
  echo
  echo
  exit -99
fi

#
# Submit forecast post-processing
sleep 1
export jobid=jrtofs_forecast_post_2
mkdir -p ${DATAROOT}/$jobid
cat << EOF_forecast_2_post > $batchloc/rtofs.forecast_2_post.$pid
#!/bin/ksh -l
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=01:00:00
#SBATCH --account=$account
#SBATCH --job-name=RTOFS_FCST_POST
#SBATCH -d afterok:$jobid_fcst2
#SBATCH -o $DATAROOT/rtofs_forecast_2_post.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export NPROCS=1
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_FORECAST_POST_2 )
EOF_forecast_2_post

job_fcst2_post=$(sbatch $batchloc/rtofs.forecast_2_post.$pid | cut -d " " -f4)
if [ $# -gt 0 ] 
then
  echo 'LAUNCHER: RTOFS-GLO forecast post-processing_2 job is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO forecast post-processing_2 job is not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

# Submit forecast grib posts
for NN in 01 02 03 04
do
  export job=${RUN}_${modID}_forecast_grib_post_${projID}.${NN}
  export jobid=j${job}
  export NN
##
cat << EOF_forecast_grib_post > $batchloc/rtofs.forecast_grib_post.$pid
#!/bin/ksh -l
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=01:00:00
#SBATCH --account=$account
#SBATCH --job-name=RTOFS_FCST_GRIB_POST
#SBATCH -d afterok:$jobid_fcst2
#SBATCH -o $DATAROOT/rtofs_forecast_grib_post.out.$NN.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export NPROCS=1
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_FORECAST_GRIB2_POST )
EOF_forecast_grib_post

job_fcst_grib_post=$(sbatch $batchloc/rtofs.forecast_grib_post.$pid | cut -d " " -f4)

  if [ $# -gt 0 ] 
  then
    echo 'LAUNCHER: RTOFS-GLO forecast GRIB post-processing job ' $NN ' is submitted at host '`hostname`' at '`date`
  else
    echo 'LAUNCHER ERROR: RTOFS-GLO forecast GRIB post-processing job ' $NN ' is not submitted at host '`hostname`' at '`date` "error is $#"
    exit
  fi
  unset job
done

for NN in 01 02 03 04 05 06 07 08
do
  export job=${RUN}_${modID}_forecast_post_${projID}.${NN}
  export jobid=j${job}
  mkdir -p ${DATAROOT}/$jobid
  export NN
##
cat << EOF_forecast_post > $batchloc/rtofs.forecast_post.$pid
#!/bin/ksh -l
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=01:00:00
#SBATCH --account=$account
#SBATCH --job-name=RTOFS_FCST_POST
#SBATCH -d afterok:$jobid_fcst2
#SBATCH -o $DATAROOT/rtofs_forecast_post.out.$NN.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export NPROCS=1
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_FORECAST_POST )
EOF_forecast_post

job_fcst_post=$(sbatch $batchloc/rtofs.forecast_post.$pid | cut -d " " -f4)

  if [ $# -gt 0 ] 
  then
    echo 'LAUNCHER: RTOFS-GLO forecast post-processing job ' $NN ' is submitted at host '`hostname`' at '`date`
  else
    echo 'LAUNCHER ERROR: RTOFS-GLO forecast post-processing job ' $NN ' is not submitted at host '`hostname`' at '`date` "error is $#"
    exit
  fi
  unset job
done

exit
