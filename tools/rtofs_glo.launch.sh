#!/bin/sh
#
#
##set -x

if [ $# -ne 2 ] 
then
  echo "USAGE: $0 <YYYYMMDD> <NDAYS>"
  exit -2
fi

export forecast_start=$1
forecast_days=$2
hindcast=YES # YES or NO

# Set some run environment variables.
export HOMErtofs=/marine/save/$LOGNAME/hycom_glo/projects/RB-1.0.2
export projID=`basename $HOMErtofs`
export cyc=00
export RUN_ENVIR=dev
# very important: redefinition of the default date (PDY !!!
# if PDY is defined here, it will not be reset by setpdy utility.
export PDY=$forecast_start
export envir=prod # prod or para
if [ $hindcast = YES ] 
then
  # Redefine default top level directories for rtofs_forcing_getges.sh script
  export GETGES_COM=/ptmp/$LOGNAME/tmpdir/com.$$
else 
   export GETGES_COM=/com
fi

# Create temporary directory.
# test -d /ptmp/$LOGNAME/tmpdir/$projID || rm -rf /ptmp/$LOGNAME/tmpdir/$projID
# mkdir -p /ptmp/$LOGNAME/tmpdir/$projID

# Set paths to directories.
export utilexec=/nwprod/util/exec
export HOMEout=/marine/noscrub/$LOGNAME/simulations/$projID
FLUXDIR=/marine/noscrub/$LOGNAME/flux

#=== DON'T EDIT BELOW THIS LINE ==================

# Calculate forecast length.
let forecast_hours=forecast_days*24
export forecast_end=`$utilexec/ndate $forecast_hours ${forecast_start}${cyc} | cut -c1-8`

# Write out some info.
echo "LAUNCHER INFO: run: ${projID}, cycle: t${cyc}z, PDY=${PDY}."
echo "LAUNCHER INFO: forecast starts at ${forecast_start}, ends at ${forecast_end}."

# Test the restart file.
resttplate=$HOMEout/rtofs/rtofs.${forecast_start}/rtofs_glo.t${cyc}z.next_restart
if [ ! -s ${resttplate}.a ] || [ ! -s ${resttplate}.b ]
then
  echo "LAUNCHER ERROR: Wrong restart file ${resttplate}.[ab]"
  exit -3
fi 
basetime=1900123100
hdate=`awk '{  if (NR==2) { print $5 } }'  < ${resttplate}.b | cut -d. -f1 `
dater=`$utilexec/ndate \` expr $hdate \* 24 \`  ${basetime}`
if [ ${dater} -ne ${forecast_start}${cyc} ] 
then 
  echo "LAUNCHER ERROR: Date in the restart file."
  echo "                FILE:  ${resttplate}.b" 
  echo "                DATES: forecast_start=${forecast_start} dater=$dater"
  exit -3
fi 


# Set up temporary fluxes directory for hindcast
# NOTE: done for forecast case (gfs) only. Add nowcast (gdas) case if necessary.
if [ $hindcast = YES ] 
then
  mkdir -p ${GETGES_COM}/gfs/$envir
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
test $HOMEout/logs || mkdir -p $HOMEout/logs

# Submit the forecast job.
llsubmit rtofs_job_command.sms
echo 'LAUNCHER: job rtofs_job_command.sms is submitted at host '`hostname`' at '`date`
