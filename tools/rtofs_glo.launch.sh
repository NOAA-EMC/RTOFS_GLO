#!/bin/sh
#
#
set -x

#
# Define environment variables to be passed to submitted job.
#
# HOMEDIR
# runID
# sdate
# job_name
# step_name
# dependency
# TANK
# forecast_days
# lastday
#

export SECONDS=0    
export cyc='00'     
export prev_cyc='12'
export envir='prod'
export HOMEDIR=/marine/save/$USER/hycom_glo/projects/port
export SMSBIN=/nwprod/sms/bin
export PATH="$PATH":/nwprod/sms/bin:/nwprod/util/ush
#
# Set run parameters
#
export runID=port
job='rtofs'
forecast_days=3
runtype=fcst # fcst or anal


if [ ${real_time} = 'T' ]
then
   sdate=`cut -c7-14 /com/date/t${cyc}z`
   #    sdate=`date '+%Y%m%d'`
else
   sdate=$1
fi

# modstank  - for hindcast
#export TANK=/gpfs/m/marine/noscrub/seaspara/modstank

let forecast_hours=forecast_days*24
export forecast_end=`/nwprod/util/exec/ndate $forecast_hours ${sdate}'00' | cut -c1-8`

export ENDHOUR=$forecast_hours

# relaunch???? default is sdate (run once)
export lastday=20061110
export lastday=$sdate

#
# Directories and executables
#
#dbgz
# export FLUXDIR=/marine/noscrub/seaspara/flux/gfs. #gdas
NDATE=/nwprod/util/exec/ndate
export sudotmp=/ptmp
export sudocom=/marine/noscrub/$USER/simulations
toolsdir=$HOMEDIR/tools
jobdir=$toolsdir/../jobs
logdir=$sudocom/$runID/logs
runtag=${sdate}.$$
subfile=${logdir}/rtofs_glo.sub.${runtag}.sh
########
# just in case
##export MODX=${HOMEDIR}/exec/bufr_prepmods.HOLD

####################################
# Specify NET and RUN Name and model
####################################
export NET=rtofs
export RUN=rtofs
export modID=glo # HurricaneTest
#
# NOTE: temporary: clean working directories.
#
rm -Rf ${sudotmp}/$USER/${runID}
mkdir -p ${sudotmp}/$USER/${runID}
if [ ${cold_start} = 'T' ] 
then
  rm -Rf ${sudocom}/${runID}
  mkdir -p ${sudocom}/${runID}
fi
#
test -d $logdir || mkdir -p $logdir
###dbgz   \$FLUXDIR ; \\
# NOTE: IMPORTANT: for ncep 1/3, add   \$inputgrid to the environment

YMD=$sdate
# very important: redefinition of the default date (PDY !!!
# if PDY is defined here, it will not be reset by setpdy utility.
export PDY=$YMD
PDYm1=`$NDATE -24 ${PDY}'00' | cut -c1-8`
PDYm2=`$NDATE -48 ${PDY}'00' | cut -c1-8`
PDYm3=`$NDATE -72 ${PDY}'00' | cut -c1-8`
PDYm4=`$NDATE -96 ${PDY}'00' | cut -c1-8`
PDYm5=`$NDATE -120 ${PDY}'00' | cut -c1-8`
PDYm6=`$NDATE -144 ${PDY}'00' | cut -c1-8`
PDYm7=`$NDATE -168 ${PDY}'00' | cut -c1-8`
PDYp1=`$NDATE 24 ${PDY}'00' | cut -c1-8`

#
# Create temporary directories 
#
for TME in $PDY $PDYm1 $PDYm2 $PDYp1
do
  test -d ${sudocom}/${runID}/rtofs/rtofs.${TME} || mkdir -p ${sudocom}/${runID}/rtofs/rtofs.${TME}
done
test -d ${sudotmp}/$USER/${runID}/${job}_${envir} ||  mkdir -p ${sudotmp}/$USER/${runID}/${job}_${envir}
#
# Put the restart etc in place 
#
# NOTE: we was running a day late, in real time now (see PDY calcs in JRTOFS* scripts)
#
archive_dir=${sudocom}/${runID}/rtofs
if [ $runtype = 'anal' ]
then
  restart_dir=${archive_dir}/rtofs.$PDYm1
  dates=${PDYm1}'00'
else
  restart_dir=${archive_dir}/rtofs.$PDY
  dates=${PDY}'00'
fi
if [ ${cold_start} = 'T' ] ### &&  [ ${real_time} = 'T' ] 
then
  for cfd in $PDYm1 ### $PDYm2 $PDYm3 $PDYm4 $PDYm5 $PDYm6 $PDYm7
  do
    comdir=/com/rtofs/${envir}/rtofs.$cfd
    rtofsdir=${archive_dir}/rtofs.$cfd
    test $rtofsdir && mv $rtofsdir ${rtofsdir}.bak.$$
    ln -sf $comdir $rtofsdir
  done
fi
cd ${restart_dir}
dater=`${toolsdir}/date_from_restart.sh ${restart_dir}/rtofs_glo.t00z.next_restart.b `
echo ; pwd ; 
if [ ${dater} -ne ${dates} ] 
then 
  echo 'LAUNCHER ERROR: check date in the restart file, dates=' $dates ' dater=' $dater
  echo "file:  ${restart_dir}/rtofs_${modID}.t00z.next_restart.b"
  exit 6
fi 
#>-  if [ ${cold_start} = 'T' ] 
#>-  then
#>-    cdir=`pwd`
#>-    cd ${sudocom}/${runID}/rtofs/rtofs.$PDYm1
#>-    ln -sf /com/${job}/${envir}/${job}.$PDYm1/rtofs_glo.t${cyc}z.next_restart.a .
#>-    ln -sf /com/${job}/${envir}/${job}.$PDYm1/rtofs_glo.t${cyc}z.next_restart.b .
#>-    # ln -sf /com/${job}/${envir}/${job}.$PDYm1/rtofs_${modID}.t${cyc}z.lowfreq.input .
#>-    # dbgz IMPORTANT !!!!!! ONE-TIME DEAL
#>-    # ln -sf $toolsdir/../input/rtofs_glo.t${cyc}z.lowfreq.input .
#>-    cd $cdir 
#>-  fi

#
# Prepare LL script
#
cat << EOF > ${subfile}
#@ shell=/bin/sh 
# @ job_name = jrtofs_port_00
# @ output = ${logdir}/rtofs_glo_00.${runtag}
# @ error = ${logdir}/rtofs_glo_00.${runtag}
# @ shell = /bin/sh
# @ group = devonprod
# @ wall_clock_limit = 03:00:00
# @ account_no = RTO-T2O 
# @ notification = never
## @ blocking = unlimited
#@ environment = MP_EUILIB=us ; \\
   \$SECONDS ; \\
   \$cyc ; \\
   \$prev_cyc ; \\
   \$envir ; \\
   \$runID ; \\
   \$PDY ; \\
   \$TANK ; \\
   \$ENDHOUR ; \\
   \$forecast_end ; \\
   \$lastday ; \\
   \$PATH ; \\
   \$HOMEDIR ; \\
   \$SMSBIN ; \\
   \$sudotmp ; \\
   \$sudocom ; \\
   \$MODX ; 
# Forecast
#
# @ step_name = jrtofs_port_forecast_00
# @ output = ${logdir}/rtofs_glo_00.${runtag}.\$(stepid)
# @ error = ${logdir}/rtofs_glo_00.${runtag}.\$(stepid)
# @ job_type = parallel
# @ class = devhigh
# @ total_tasks = 32
# @ node = 2
# @ resources = ConsumableMemory(1024 mb) ConsumableCpus(1)
# @ node_usage=not_shared
# @ network.MPI=csss,shared,us
# @ wall_clock_limit = 03:00:00
# @ executable = ../jobs/JRTOFS_GLO_FORECAST.sms.${envir}
# @ queue
#
# Postprocess 
#
# @ dependency = ( jrtofs_port_forecast_00 == 0 )
# @ step_name = jrtofs_port_post_00
# @ output = ${logdir}/ofs_atl_00.${runtag}.\$(stepid)
# @ error = ${logdir}/ofs_atl_00.${runtag}.\$(stepid)
# @ job_type = serial
# @ class = 1
# @ resources = ConsumableMemory(4000 mb) ConsumableCpus(1)
# @ wall_clock_limit = 03:00:00
# @ executable = ../jobs/JRTOFS_GLO_POST.sms.${envir}
# @ queue
EOF
#
# Submit the forecast cycle
#
cd ${toolsdir}
llsubmit ${subfile}






