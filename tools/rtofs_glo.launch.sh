#!/bin/bash
#
#
##set -x

if [ $# -ne 1 ] 
then
  echo "USAGE: $0 <YYYYMMDD>"
  exit -2
fi
echo 'NOTE: The run parameters are set in parm/rtofs_glo.navy_0.08.config file'

export today=$1
hindcast=NO # YES or NO
testcast=NO # YES or NO

# Set some run environment variables.
export HOMErtofs=
export PROJECTdir=/marine/save/$LOGNAME/hycom_glo/projects/RB-1.0.3
export CODEdir=/marine/save/$LOGNAME/hycom_glo/codes/rtofs_code.v2.2.18
export COMtmp=/ptmp/$LOGNAME/tmpdir/com.$$
export projID=`basename $PROJECTdir`
export model_ver=1.0.3
export code_ver=2.2.18
export cdo_ver=1.5.0
export cyc=00
export RUN_ENVIR=dev
export envir=prod # prod or para

# Get the length of the forecast from a config file
RUN=rtofs
modID=glo
inputgrid=navy_0.08
. $PROJECTdir/parm/${RUN}_${modID}.${inputgrid}.config
export fcstdays=`expr ${fcstdays_step1} + ${fcstdays_step2} + ${fcstdays_step3}`
#
# Redefine default top level directories for rtofs_forcing_getges.sh script
if [ $hindcast = YES ] 
then
  export GETGES_COM=$COMtmp
fi
if [ $hindcast = NO ] &&  [ $testcast = YES ]
then
  export GETGES_COM=/com_p6
fi 
if [ $hindcast = NO ] &&  [ $testcast = NO ]
then
  export GETGES_COM=/com
fi 
echo GETGES_COM is $GETGES_COM

# Create temporary directory.
#dbgz 20130118
#> test -d /ptmp/$LOGNAME/tmpdir/$projID && rm -rf /ptmp/$LOGNAME/tmpdir/$projID
#> mkdir -p /ptmp/$LOGNAME/tmpdir/$projID

# Set paths to directories.
export utilexec=/nwprod/util/exec
export HOMEout=/marine/noscrub/$LOGNAME/simulations/hycom_glo/$projID
export tmpdir=/ptmp/$LOGNAME/tmpdir/${projID}
FLUXDIR=/marine/noscrub/seaspara/flux/gfs/prod  # needed only if hidcast=YES
#FLUXDIR=/marine/noscrub/$LOGNAME/flux  # needed only if hidcast=YES

#=== DON'T EDIT BELOW THIS LINE ==================

# very important: redefinition of the default date (PDY !!!
# if PDY is defined here, it will not be reset by setpdy utility.
export PDY=$today
# This variable is used in hidncast mode to set up forcing directories.
let analhrs=analdays*24
nowcast_start=`$utilexec/ndate -${analhrs} ${today}${cyc} | cut -c1-8`
forecast_start=$today

# Calculate forecast length.
export nowcast_end=$today
let fcsthrs=${fcstdays}*24
export forecast_end=`$utilexec/ndate $fcsthrs ${forecast_start}${cyc} | cut -c1-8`

# Test restart file in hindcast

if [ $hindcast = YES ] 
then
  RESTdir=${HOMEout}/rtofs/nwges/rtofs.`$utilexec/ndate -24 ${PDY}'00' | cut -c1-8`
  RESTfile=${RESTdir}/rtofs_glo.t${cyc}z.restart_f24
  if ! [ -s ${RESTfile}.a ] || ! [ -s  ${RESTfile}.b ]
  then
      echo "LAUNCHER ERROR: No restart found."
      echo "                FILE:  ${RESTfile}.[ab] "    
      exit -3
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
  cdate=`$utilexec/ndate -24 ${nowcast_start}'00' | cut -c1-8`
  while [ $cdate -le $nowcast_end ] 
  do
    if [ -d $FLUXDIR/gdas.${cdate} ]
    then
      ln -s $FLUXDIR/gdas.${cdate} ${GETGES_COM}/gfs/$envir/gdas.${cdate}
    else
      echo "LAUNCHER ERROR: No forcing directory found."
      echo "                DIR:  $FLUXDIR/gdas.${cdate}"    
      exit -3
    fi
    cdate=`$utilexec/ndate 24 ${cdate}'00' | cut -c1-8`
  done
  #
  # Forecast forcing
  cdate=`$utilexec/ndate -24 ${forecast_start}'00' | cut -c1-8`
  while [ $cdate -le $forecast_end ] 
  do
    if [ -d $FLUXDIR/gfs.${cdate} ]
    then
      ln -s $FLUXDIR/gfs.${cdate} ${GETGES_COM}/gfs/$envir/gfs.${cdate}
    else
      echo "LAUNCHER ERROR: No forcing directory found."
      echo "                DIR:  $FLUXDIR/gfs.${cdate}"    
      exit -3
    fi
    cdate=`$utilexec/ndate 24 ${cdate}'00' | cut -c1-8`
  done

fi

# Make logs directory if necessary.
test -d $HOMEout/logs || mkdir -p $HOMEout/logs

# Submit the job.
module load lsf
\
#----------------------------------
#bsub < rtofs_job_command_anal.lsf

#bsub < rtofs_job_command_anal_grib_post.lsf
#bsub < rtofs_job_command_fcst_d1-3_grib_post.lsf
#bsub < rtofs_job_command_fcst_d4-6_grib_post.lsf

#bsub < rtofs_job_command_anal_post.lsf

#startdate=$forecast_start
#while [ $startdate -le $forecast_end ]
#do 
#  export startdate=`$utilexec/ndate +24 ${startdate}'00' | cut -c1-8`
#  bsub < rtofs_job_command_fcst_post.lsf
#done

for NN in 01 02 03 04 05 06 07 08
do
  export job=${RUN}_${modID}_forecast_post_${projID}.${NN}
  bsub < rtofs_job_command_fcst_post.lsf
done

exit
#-------------------------------------

#dbgz 20130110
########jobID_anal_pre=`bsub < rtofs_job_command_anal_pre.lsf | cut -d' ' -f1`
#
# Submit analysis
sleep 1
bsub < rtofs_job_command_anal_pre.lsf
sleep 5
bsub < rtofs_job_command_anal.lsf
sleep 5
bsub < rtofs_job_command_anal_post.lsf
bsub < rtofs_job_command_anal_grib2_post.lsf
echo 'LAUNCHER: RTOFS-GLO analysis is submitted at host '`hostname`' at '`date`
#dbgz
exit
#
# Submit forecast step1
sleep 1
bsub < rtofs_job_command_fcst_step1_pre.lsf
sleep 5
bsub < rtofs_job_command_fcst_step1.lsf
echo 'LAUNCHER: RTOFS-GLO forecast step1 is submitted at host '`hostname`' at '`date`
#
# Submit forecast step2
sleep 1
bsub < rtofs_job_command_fcst_step2_pre.lsf
sleep 5
bsub < rtofs_job_command_fcst_step2.lsf
echo 'LAUNCHER: RTOFS-GLO forecast step2 is submitted at host '`hostname`' at '`date`
#
# Submit forecast step3
#> sleep 1
#> bsub < rtofs_job_command_fcst_step3_pre.lsf
#> sleep 5
#> bsub < rtofs_job_command_fcst_step3.lsf
#> echo 'LAUNCHER: RTOFS-GLO forecast step3 is submitted at host '`hostname`' at '`date`

#-- startdate=$forecast_start
#-- while [ $startdate -le $forecast_end ]
#-- do 
#--    export startdate=`$utilexec/ndate +24 ${startdate}'00' | cut -c1-8`
#--    bsub < rtofs_job_command_fcst_post.lsf
#-- done

for NN in 01 02 03 04 05 06 07 08
do
  export job=${RUN}_${modID}_forecast_post_${projID}.${NN}
  bsub < rtofs_job_command_fcst_post.lsf
done

bsub < rtofs_job_command_fcst_d1-3_grib_post.lsf
bsub < rtofs_job_command_fcst_d4-6_grib_post.lsf