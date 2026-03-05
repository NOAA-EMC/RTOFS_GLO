#!/bin/sh
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_forecast.sh                                #
# Script description:                                                         #
#                                                                             #
# Abstract: This script generates the analysis fields                         #
#           for the RTOFS_GLO Ocean model                                     #
#                                                                             #
# Sub-scripts called:                                                         #
#   rtofs_abort.sh - Error handling for model crashes                         #
#   rtofs_combine_nc.sh - (via cfp) combine restarts and product files        #
#                                                                             #
###############################################################################
set -xa

export PS4='$SECONDS + '

cd $DATA

msg="RTOFS_GLO_FORECAST JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

# --------------------------------------------------------------------------- #
# 0. Get initial conditions (restart files) 
#     if this is a restart, then find latest restart files in DATArestart
#     if this is not a restart, then find restart files in COMin

# THIS IS RESTART LOGIC FOR RTOFS V2.5 (HYCOM MODEL)
# IT NEEDS TO BE UPDATED FOR RTOFS V3.0 (UFS MODEL)

wearerunningv2=0
if [ $wearerunningv2 -eq 1 ]
then

  test -f restart_in.a && rm -f restart_in.a
  test -f restart_in.b && rm -f restart_in.b

  restart_found=no
  if [ $RESTART = YES ]
  then
    for rtype in out out1   # rename restarts with out string to use timestamp
    do
      if [ -s ${DATArestart}/restart_${rtype}.b ]
      then
        date_out=$(${USHrtofs}/rtofs_date4restart.sh ${DATArestart}/restart_${rtype}.b)
        mv ${DATArestart}/restart_${rtype}.a ${DATArestart}/restart_${date_out}.a
        mv ${DATArestart}/restart_${rtype}.b ${DATArestart}/restart_${date_out}.b
      fi
    done

    latestdate=0
    for rfileb in $(ls ${DATArestart}/restart_*.b)  # find and link to latest restart file
    do
      date_out=$(${USHrtofs}/rtofs_date4restart.sh ${rfileb})
      if [ $date_out -gt $latestdate ]
      then
        latestdate=$date_out
        rfile=$rfileb
      fi
    done

    # find cice restart file for this restart time and link them
    if [ $latestdate -ne 0 ]
    then
      YYYY=$(echo $latestdate | cut -c1-4)
      MM=$(echo $latestdate | cut -c5-6)
      DD=$(echo $latestdate | cut -c7-8)
      HH=$(echo $latestdate | cut -c9-10)
      SSSSS=$(expr $HH \* 3600)
      ln -sf $rfile $DATA/restart_in.b
      ln -sf ${rfile%.b}.a $DATA/restart_in.a
      ln -sf $DATArestart/cice.restart.${YYYY}-${MM}-${DD}-${SSSSS} $DATA/cice.restart_in
      echo INFO - restarting simulation from $latestdate using $rfile
      restart_found=yes
    fi

    if [ $restart_found = no ]
    then
       $USHrtofs/${RUN}_abort.sh  "FATAL ERROR: $job Missing Restart File" \
         "No restart_in.[ab] or cice.restart_in in $DATArestart" 2
    fi

# remove archives created after most recent restart; hycom will fail if these files exist
    rdate=$(${USHrtofs}/rtofs_date4restart.sh $DATA/restart_in.b)
    for afile in $(ls $DATAarchive/*.b)
    do
      archivedate=$(basename $afile | cut -d. -f2 | cut -c1-4,6-8,10-11)
      ayjul=$(echo $archivedate | cut -c1-7)
      ahour=$(echo $archivedate | cut -c8-9)
      adate=$($UTILROOT/ush/date2jday.sh $ayjul)$ahour
      if [ $adate -gt $rdate ]
      then
        echo removing ${afile%.b} files
        rm -f ${afile} ${afile%.b}.a ${afile%.b}.txt
      fi
    done

  else # RESTART=NO
    if [ ${CONTINUE_FORECAST} = NO ]
    then
      # Restart from the nowcast restart
      HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.n${mycyc}.restart
      CICErestTplate=${RUN}_${modID}.t${mycyc}z.n${mycyc}.restart_cice
    else
      # Restart from previous forecast step restart
      LEAD=$($NHOUR ${startdate} ${PDY}${mycyc})
      HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart
      CICErestTplate=${RUN}_${modID}.t${mycyc}z.f${LEAD}.restart_cice
    fi

    if [ -s ${COMIN}/${HYCOMrestTplate}.a ] && \
       [ -s ${COMIN}/${HYCOMrestTplate}.b ] && \
       [ -s ${COMIN}/${CICErestTplate} ]
    then
      ln -s -f ${COMIN}/${HYCOMrestTplate}.a restart_in.a
      ln -s -f ${COMIN}/${HYCOMrestTplate}.b restart_in.b
      ln -s -f ${COMIN}/${CICErestTplate} cice.restart_in
      echo "Forecast $RUN_STEP is started from restart: ${COMIN}/${HYCOMrestTplate}.[ab] and ${CICErestTplate}"
    else
      $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Restart File" \
        "No restart_in.[ab] or cice.restart_in in $COMIN" 3
    fi
    # remove any possible archives created
    rm -f $DATAarchive/*
  fi

  echo './cice.restart_in' > cice.restart_file

  if [ -s restart_in.a -a -s restart_in.b -a -s cice.restart_in ]
  then
    echo "Initial restart files copied"
    export enddate=$($NDATE $(expr $fcstdays \* 24 ) ${startdate})
    export startdate=$(${USHrtofs}/rtofs_date4restart.sh restart_in.b)
  else
    $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Restart File" \
        "No restart_in.[ab] or cice.restart_in in $DATA" 911
  fi
fi # wearerunningv2=1

# --------------------------------------------------------------------------- #
# 1. copy in files from parm to top level

#get number of days to run, 
#source $PARMrtofs/rtofs_glo.navy_0.08.config
#fcst=fcst1
#  fcstdays=1
#  PD1=$PDY
  NH=96

adate=${PDY:0:4}-${PDY:4:2}-${PDY:6:2}-00000
for pfile in ice_in.forecast; do
cp $PARMrtofs/$pfile ./ice_in
  sed -i -e "s/&YMDS.nc/$adate.nc/" -e "s/&adjust_aice/none/" ./ice_in
done

for pfile in data_table datm_in datm.streams diag_table fd_ufs.yaml input.nml noahmptable.tbl ufs.configure.forecast
do
  cp $PARMrtofs/$pfile .
done
sed -i -e "s/&startup_continue/continue/" ./ufs.configure.forecast
mv ./ufs.configure.forecast ./ufs.configure

# model_configure created on the fly
for pfile in model_configure
do
  cp $PARMrtofs/$pfile .
  sed -i -e  "s/&YYYY/${PDY:0:4}/g" -e "s/&MM/${PDY:4:2}/g" -e "s/&DD/${PDY:6:2}/g" -e "s/&HH/00/g" -e "s/&NH/$NH/g" ./model_configure
done

# --------------------------------------------------------------------------- #
# 2a. link in fix files to top-level

#for ffile in cice_model.res.nc grid_cice_NEMS_mx008.nc kmtu_cice_NEMS_mx008.nc mesh.mx008.nc
for ffile in grid_cice_NEMS_mx008.nc kmtu_cice_NEMS_mx008.nc mesh.mx008.nc
do
  ln -sf $FIXrtofs/$ffile .
done

# --------------------------------------------------------------------------- #
# 2b. link or copy fix and parm files to INPUT
# fix depth 0p08 

mkdir INPUT
#for ffile in chl_mom6.nc depth_GLBb0.08_09m11ob2_mom6.nc grid_spec.nc mesh.datm.3072x1536.nc mom6_vgrid.nc ocean_hgrid.nc ocean_mosaic.nc runoff.daitren.clim.0.08deg.nc sss_mom6.nc tidal_amplitude.nc
for ffile in chl_mom6.nc grid_spec.nc mesh.datm.3072x1536.nc mom6_vgrid.nc regional.mom6.nc ocean_mosaic.nc runoff.daitren.clim.0.08deg.nc sss_mom6.nc tidal_amplitude.nc
do
  ln -sf $FIXrtofs/$ffile INPUT/.
done
  ln -s $FIXrtofs/depth_GLB.0p08_09m11ob2_mom6.nc INPUT/depth_GLBb0.08_09m11ob2_mom6.nc

for pfile in MOM_input
do
  cp $PARMrtofs/${pfile}.forecast $pfile
  sed -i -e "s/ODA_INCUPD = &Value /ODA_INCUPD = False/" ./MOM_input
  mv MOM_input INPUT/
done
for pfile in MOM_layout MOM_override
do
  cp $PARMrtofs/$pfile INPUT
done

# --------------------------------------------------------------------------- #
# 3. Populate INPUT directory with pdym1 restart and forcing

# check that MOM.res files exist
for momres in $(ls $COMIN/RESTART/${PDY}.000000.MOM.res*nc*)
do
   fn=$(basename $momres | cut -d. -f3-)
   ln -s $momres INPUT/$fn
done

# forcing (for this time period)  (change datm.streams when changing)
#ln -s $COMIN/../forcing/$PDY/gfs.2025121400-2025123118_positive.nc INPUT/gfs.forcing.files.nc
# faking that it is the 12/22 file
ln -s $COMIN/../forcing/$PDY/gfs.2025121400-2025122218_positive.nc INPUT/gfs.forcing.files.nc

# iced with right date
icedate=$(echo $PDY | cut -c1-4)-$(echo $PDY | cut -c5-6)-$(echo $PDY | cut -c7-8)-00000
ln -s $COMIN/RESTART/iced.${icedate}.nc INPUT/iced.${icedate}.nc
echo INPUT/iced.${icedate}.nc > ice.restart_file

#datm files 
ln -s $COMIN/datm.gfs.datm.r.${icedate}.nc .
echo datm.gfs.datm.r.${icedate}.nc > rpointer.atm

ln -s $COMIN/RESTART/datm.gfs.cpl.r.${icedate}.nc INPUT/.
echo  INPUT/datm.gfs.cpl.r.${icedate}.nc > rpointer.cpl

# --------------------------------------------------------------------------- #
# 4. Create other directories

mkdir history RESTART MOM6_OUTPUT

# --------------------------------------------------------------------------- #
# 5. get executable

cp $EXECrtofs/ufs_model.x fv3.exe

mpiexec -np $NMPI --cpu-bind core  ./fv3.exe >> $pgmout 2>errfile
rc=$?

echo fv3.exe completed with exit code $rc

if [ $rc -ne 0 ]
then
   echo we are saying goodbye
   $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Abnormal model exit" \
      "problem with analysis model run - return code $modelstatus" $rc
   exit
fi

# combine files (one restart and two archives) and copy to COMOUT
# too many combine commmands on one node will kill the node (probably memory)
# so need to spread the commands across nodes. 
#
# combine commands - total number of combine commands
#    restarts (1*17) + surface.diags (96) + volume.diags (16) = 129
# available procs - total procs minus the cp commands
#    nprocs - icehistory (96) - otherfiles (13) = 109
# sleep commands = max((nprocs - total cmds) / combine commands -1,16)

insertedsleepcommands=15
rm -f cmdfile.cpout
mkdir -p $COMOUT/RESTART $COMOUT/history $COMOUT/MOM6_OUTPUT

#Restarts
echo "${USHrtofs}/rtofs_combine_nc.sh False $DATA/RESTART ${PDYp4}.000000.MOM.res.nc ${COMOUT}/RESTART/${PDYp4}.000000.MOM.res.nc > cmb.restart.${PDYp4}.000000.res.out"  >> cmdfile.cpout
for i in $(seq $insertedsleepcommands};do echo "sleep 10" >> cmdfile.cpout;done
for res in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
do
   echo "${USHrtofs}/rtofs_combine_nc.sh False $DATA/RESTART ${PDYp4}.000000.MOM.res_${res}.nc ${COMOUT}/RESTART/${PDYp4}.000000.MOM.res_${res}.nc > cmb.restart.${PDYp4}.000000.res_${res}.out" >> cmdfile.cpout
   for i in $(seq $insertedsleepcommands};do echo "sleep 10" >> cmdfile.cpout;done
done

#Diagnostics (can do better than this)
for dfile in $(ls ocn*0000)
do
   dfil=$(echo $dfile | cut -d. -f1-2)
   echo "${USHrtofs}/rtofs_combine_nc.sh $DATA $dfil ${COMOUT}/$dfil > cmb.$dfil.out" >> cmdfile.cpout
   for i in $(seq $insertedsleepcommands};do echo "sleep 10" >> cmdfile.cpout;done
done

# copy singular files
for ifile in $(ls history/iceh*.nc*)
do
   echo "cp -p -f $ifile $COMOUT/history" >> cmdfile.cpout
done
fdate=$(echo $PDYp4 | cut -c1-4)-$(echo $PDYp4 | cut -c5-6)-$(echo $PDYp4 | cut -c7-8)-00000
echo "cp -p -f RESTART/iced.${fdate}.nc $COMOUT/RESTART" >> cmdfile.cpout
echo "cp -p -f RESTART/datm.gfs.cpl.r.${fdate}.nc $COMOUT/RESTART" >> cmdfile.cpout

# diagnostics and log files
for logfile in ice_diag.d mediator.log atm.log
do
  echo "cp -p -f $logfile $COMOUT/forecast.$RUN_STEP.$logfile" >> cmdfile.cpout
done
for momoutputfile in $(ls MOM6_OUTPUT)
do
   echo "cp -p -f MOM6_OUTPUT/$momoutputfile $COMOUT/MOM6_OUTPUT/forecast.$RUN_STEP.$momoutputfile" >> cmdfile.cpout
done

chmod +x cmdfile.cpout
mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpout > cpout.out
err=$? ; export err ; err_chk
date

#################################################
msg="THE RTOFS_GLO_FORECAST JOB HAS ENDED NORMALLY on $(hostname) at $(date)."
postmsg "$msg"

################## END OF SCRIPT #######################


exit

# copy archive and history files to COMOUT
rm -f cmdfile.cpout

# ocean archives
for ofile in $(ls ocn*.nc*)
do
   echo "cp -p -f $ofile $COMOUT" >> cmdfile.cpout
done
# ice history
mkdir -p $COMOUT/history
for ifile in $(ls history/iceh*.nc*)
do
   echo "cp -p -f $ifile $COMOUT/history" >> cmdfile.cpout
done
# restart
mkdir -p $COMOUT/RESTART
for rfile in $(ls RESTART/${PDYp4}.000000.MOM.res*.nc*)
do
  echo "cp -p -f $rfile $COMOUT/RESTART" >> cmdfile.cpout
done
fdate=$(echo $PDYp4 | cut -c1-4)-$(echo $PDYp4 | cut -c5-6)-$(echo $PDYp4 | cut -c7-8)-00000
echo "cp -p -f RESTART/iced.${fdate}.nc $COMOUT/RESTART" >> cmdfile.cpout
echo "cp -p -f RESTART/datm.gfs.cpl.r.${fdate}.nc $COMOUT/RESTART" >> cmdfile.cpout 

echo "cp -p -f datm.gfs.datm.r.${fdate}.nc $COMOUT" >> cmdfile.cpout

# diagnostics and log files
mkdir -p $COMOUT/MOM6_OUTPUT
for logfile in ice_diag.d mediator.log atm.log
do
  echo "cp -p -f $logfile $COMOUT/analysis.$logfile" >> cmdfile.cpout
done
for momoutputfile in $(ls MOM6_OUTPUT)
do
   echo "cp -p -f MOM6_OUTPUT/$momoutputfile $COMOUT/MOM6_OUTPUT/analysis.$momoutputfile" >> cmdfile.cpout
done

chmod +x cmdfile.cpout
mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpout
err=$? ; export err ; err_chk
date

#################################################
msg="THE RTOFS_GLO_FORECAST JOB HAS ENDED NORMALLY on $(hostname) at $(date)."
postmsg "$msg"

################## END OF SCRIPT #######################
