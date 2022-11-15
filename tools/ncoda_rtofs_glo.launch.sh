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
#. ./hera.env

echo
echo heraproj $heraproj
echo account $account
echo inputroot $inputroot
echo tmproot $tmproot
echo projectroot $projectroot
echo comroot $comroot
#echo masscfpcopy $masscfpcopy
#echo launcher $launcher
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
export jlogfile=/dev/null
export COMROOTrtofs=/dev/null

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
export DCOMINHFR=$DCOMROOT/prod
export TANK=$DCOMROOT/prod

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
  export GETGES_COM=$COMgfs/gfs/prod
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
export PDYm1=`$NDATE -24 ${PDY}'00' | cut -c1-8`
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

# area for testing only one job
testjustthisjob=1
if [ $testjustthisjob -eq 1 ]
then

#########just



#########just

echo testjustthisjob
exit
fi #testjustthisjob


skipncoda=0
if [ $skipncoda -eq 0 ]
then

# Submit QC
export jobid=jrtofs_ncoda_qc
mkdir -p ${DATAROOT}/$jobid

cat << EOF_ncoda_qc > $batchloc/rtofs.qc.sbatch.$pid
#!/bin/ksh -l
#SBATCH --nodes=1 --ntasks-per-node=10
#SBATCH --ntasks=10
#SBATCH --time=00:30:00
#SBATCH --account=$account
#SBATCH --job-name=NCODA_QC
#SBATCH -o $DATAROOT/rtofs_ncoda_qc.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
export I_MPI_PIN_RESPECT_CPUSET=disable
export NPROCS=10
(time $HOMErtofs/jobs/JRTOFS_GLO_NCODA_QC )
EOF_ncoda_qc

jobid_qc=$(sbatch $batchloc/rtofs.qc.sbatch.$pid | cut -d " " -f4)
if [ $# -gt 0 ] 
then
  echo 'LAUNCHER: RTOFS-GLO ncoda qc is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO ncoda qc not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

#echo one job
#exit

# Submit Three VAR jobs
export jobid=jrtofs_hycom_var
mkdir -p ${DATAROOT}/$jobid
cat << EOF_ncoda_hycom_var > $batchloc/rtofs.hycomvar.sbatch.$pid
#!/bin/ksh -l
#SBATCH --nodes=18 --ntasks-per-node=20
#SBATCH --time=01:30:00
#SBATCH --account=$account
#SBATCH --job-name=NCODA_HYCOM
#SBATCH -d afterok:$jobid_qc
#SBATCH -o $DATAROOT/rtofs_hycom_var.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_NCODA_HYCOM_VAR )
EOF_ncoda_hycom_var

jid_hycom=$(sbatch $batchloc/rtofs.hycomvar.sbatch.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO hycom var is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO hycom var not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi


export jobid=jrtofs_glbl_var
mkdir -p ${DATAROOT}/$jobid
cat << EOF_ncoda_glbl_var > $batchloc/rtofs.glblvar.sbatch.$pid
#!/bin/ksh -l
#SBATCH --nodes=2 --ntasks-per-node=40
#SBATCH --time=00:20:00
#SBATCH --account=$account
#SBATCH --job-name=NCODA_GLBL
#SBATCH -d afterok:$jobid_qc
#SBATCH -o $DATAROOT/rtofs_glbl_var.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export NPROCS=80
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_NCODA_GLBL_VAR )
EOF_ncoda_glbl_var

jid_glbl=$(sbatch $batchloc/rtofs.glblvar.sbatch.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO glbl var is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO glbl var not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

export jobid=jrtofs_polar_var
mkdir -p ${DATAROOT}/$jobid
cat << EOF_ncoda_polar_var > $batchloc/rtofs.polarvar.sbatch.$pid
#!/bin/ksh -l
#SBATCH --nodes=1 --ntasks-per-node=40
#SBATCH --time=00:20:00
#SBATCH --account=$account
#SBATCH --job-name=NCODA_POLAR
#SBATCH -d afterok:$jobid_qc
#SBATCH -o $DATAROOT/rtofs_polar_var.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export NPROCS=40
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_NCODA_POLAR_VAR )
EOF_ncoda_polar_var

jid_polar=$(sbatch $batchloc/rtofs.polarvar.sbatch.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO polar var is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO polar var not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

fi #skipncoda da


# Submit NCODA increment
export jobid=jrtofs_ncoda_inc
mkdir -p ${DATAROOT}/$jobid
cat << EOF_ncoda_inc > $batchloc/rtofs.ncoda.inc.sbatch.$pid
#!/bin/ksh -l
#SBATCH --nodes=1 
#SBATCH --ntasks=1
#SBATCH --time=00:20:00
#SBATCH --account=$account
#SBATCH --job-name=NCODA_INC
#SBATCH -d afterok:$jid_hycom
#SBATCH -o $DATAROOT/rtofs_ncoda_inc.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_NCODA_INC )
EOF_ncoda_inc

jid_ncodainc=$(sbatch $batchloc/rtofs.ncoda.inc.sbatch.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO ncoda inc is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO ncoda inc not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

# Submit NCODA increment update
export jobid=jrtofs_incup
mkdir -p ${DATAROOT}/$jobid
cat << EOF_incup > $batchloc/rtofs.incup.sbatch.$pid
#!/bin/ksh -l
#SBATCH --ntasks=900
#SBATCH --time=00:30:00
#SBATCH --account=$account
#SBATCH --job-name=INCUP
#SBATCH -d afterok:$jid_ncodainc
#SBATCH -o $DATAROOT/rtofs_incup.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export I_MPI_PIN_RESPECT_CPUSET=disable
export NMPI=900
export NPROCS=900
(time $HOMErtofs/jobs/JRTOFS_GLO_INCUP )
EOF_incup

jid_incup=$(sbatch $batchloc/rtofs.incup.sbatch.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO incup is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO incup not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

#echo only qc and incup jobs
#exit

sleep 1
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
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
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
#SBATCH -d afterok:$jobid_anal_pre:$jid_incup
#SBATCH -o $DATAROOT/rtofs_analysis.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
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


#after analysis (no post)
#exit 


sleep 1
fi # skipthis

# Submit analysis post
export jobid=jrtofs_analysis_post
mkdir -p ${DATAROOT}/$jobid
cat << EOF_analysis_post > $batchloc/rtofs.analysis_post.$pid
#!/bin/ksh -l
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=00:30:00
#SBATCH --account=$account
#SBATCH --job-name=RTOFS_ANAL_POST
#SBATCH -d afterok:$jobid_anal
#SBATCH -o $DATAROOT/rtofs_analysis_post.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export NPROCS=1
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_ANALYSIS_POST )
EOF_analysis_post

job_anal_post=$(sbatch $batchloc/rtofs.analysis_post.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO analysis_post is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO analysis_post not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

mkdir -p ${DATAROOT}/$jobid
for NN in 01
do
  export job=${RUN}_${modID}_analysis_grib_post_${projID}.${NN}
  export jobid=j${job}
  export NN
##
cat << EOF_analysis_grib_post > $batchloc/rtofs.analysis_grib_post.$pid
#!/bin/ksh -l
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=00:30:00
#SBATCH --account=$account
#SBATCH --job-name=RTOFS_ANAL_GRIB_POST
#SBATCH -d afterok:$jobid_anal
#SBATCH -o $DATAROOT/rtofs_analysis_grib_post.out.%j
#SBATCH -q batch
module purge
module use $HOMErtofs/modulefiles
module load runtime_hera_rtofs.module
export NPROCS=1
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
export I_MPI_PIN_RESPECT_CPUSET=disable
(time $HOMErtofs/jobs/JRTOFS_GLO_ANALYSIS_GRIB2_POST )
EOF_analysis_grib_post

job_anal_grib_post=$(sbatch $batchloc/rtofs.analysis_grib_post.$pid | cut -d " " -f4)
if [ $# -gt 0 ]
then
  echo 'LAUNCHER: RTOFS-GLO analysis_grib_post is submitted at host '`hostname`' at '`date`
else
  echo 'LAUNCHER ERROR: RTOFS-GLO analysis_grib_post not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
done

echo exit after analysis post
exit

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
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
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
export NMPI=900
export NPROCS=900
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
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
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
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
export NMPI=900
export NPROCS=900
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
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
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
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
export NPROCS=1
export COMROOT=${COMtmp}/com
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
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
export COMIN=$COMROOT/rtofs/prod/rtofs.$PDY
export COMINm1=$COMROOT/rtofs/prod/rtofs.$PDYm1
export COMOUT=$COMROOT/rtofs/prod/rtofs.$PDY
export GETGES_NWG=/dev/null
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
