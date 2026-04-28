#!/bin/sh
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_analysis.sh                                #
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

msg="RTOFS_GLO_ANALYSIS JOB has begun on $(hostname) at $(date)"
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
      restart_found=yes
      echo INFO - restarting simulation from $latestdate using $rfile
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

  else  # RESTART=NO
    LEAD=$(expr $fcstdays \* -24)
    HYCOMrestTplate=${RUN}_${modID}.t${mycyc}z.n${LEAD}.restart
    CICErestTplate=${RUN}_${modID}.t${mycyc}z.n${LEAD}.restart_cice

    if [ -s ${COMIN}/${HYCOMrestTplate}.a ] && \
       [ -s ${COMIN}/${HYCOMrestTplate}.b ] && \
       [ -s ${COMIN}/${CICErestTplate} ]
    then
      ln -s -f ${COMIN}/${HYCOMrestTplate}.a restart_in.a
      ln -s -f ${COMIN}/${HYCOMrestTplate}.b restart_in.b
      ln -s -f ${COMIN}/${CICErestTplate} cice.restart_in
      echo "Analysis is started from restart: ${COMIN}/${HYCOMrestTplate}.[ab] and ${CICErestTplate}"
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
    export startdate=$(${USHrtofs}/rtofs_date4restart.sh restart_in.b)
    export enddate=${analysis_end:-${PDY}${mycyc}}
  else
    $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Restart File" \
        "No restart_in.[ab] or cice.restart_in in $DATA" 911
  fi
fi # wearerunningv2=1

# --------------------------------------------------------------------------- #
# 1. copy in files from parm to top level

let analhours=analdays*24
startdate=$($NDATE -$analhours ${PDY}00)
adate=${startdate:0:4}-${startdate:4:2}-${startdate:6:2}-00000

cp $PARMrtofs/ice_in .
sed -i -e "s/&YMDS.nc/$adate.nc/" -e "s/&adjust_aice/none/" ./ice_in

for pfile in data_table datm_in datm.streams diag_table fd_ufs.yaml input.nml noahmptable.tbl ufs.configure
do
  cp $PARMrtofs/$pfile .
done
  sed -i -e "s/&startup_continue/continue/" ./ufs.configure

# model_configure created on the fly
cp $PARMrtofs/model_configure .
sed -i -e  "s/&YYYY/${startdate:0:4}/g" -e "s/&MM/${startdate:4:2}/g" -e "s/&DD/${startdate:6:2}/g" -e "s/&HH/00/g" -e "s/&NH/$analhours/g" ./model_configure

# --------------------------------------------------------------------------- #
# 2a. link in fix files to top-level

for ffile in grid_cice_NEMS_mx008.nc kmtu_cice_NEMS_mx008.nc mesh.mx008.nc
do
  ln -sf $FIXrtofs/$ffile .
done

# --------------------------------------------------------------------------- #
# 2b. link or copy fix and parm files to INPUT

mkdir INPUT
for ffile in chl_mom6.nc grid_spec.nc mesh.datm.3072x1536.nc mom6_vgrid.nc regional.mom6.nc ocean_mosaic.nc runoff.daitren.clim.0.08deg.nc sss_mom6.nc tidal_amplitude.nc
do
  ln -sf $FIXrtofs/$ffile INPUT/.
done
ln -s $FIXrtofs/depth_GLB.0p08_09m11ob2_mom6.nc INPUT/depth_GLBb0.08_09m11ob2_mom6.nc

# more parm files to INPUT
cp $PARMrtofs/MOM_input ./INPUT
sed -i -e "s/ODA_INCUPD = &Value /ODA_INCUPD = False/" ./INPUT/MOM_input

for pfile in MOM_layout MOM_override
do
  cp $PARMrtofs/$pfile ./INPUT
done

# --------------------------------------------------------------------------- #
# 3. Populate INPUT directory with pdym1 restart and forcing

# check that MOM.res files exist
for momres in $(ls $COMIN/RESTART/${PDYm1}.000000.MOM.res*nc*)
do
   fn=$(basename $momres | cut -d. -f3-)
   ln -s $momres INPUT/$fn
done

# forcing (for this time period)
ln -s $COMIN/gfs.forcing.files.nc INPUT/gfs.forcing.files.nc

# iced with right date
icedate=$(echo $PDYm1 | cut -c1-4)-$(echo $PDYm1 | cut -c5-6)-$(echo $PDYm1 | cut -c7-8)-00000
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

if [ $rc -ne 0 ]
then
   echo we are saying goodbye
   $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Abnormal model exit" \
      "problem with analysis model run - return code $modelstatus" $rc
   exit
fi

# combine files (one restart and two archives) and copy to COMOUT
rm -f cmdfile.cpout
mkdir -p $COMOUT/RESTART $COMOUT/MOM6_OUTPUT
insertedsleepcommands=5
cpcmd="cp -p -f"
if type cpfs > /dev/null;then cpcmd=cpfs;fi

#Restarts
echo "${USHrtofs}/rtofs_combine_nc.sh False $DATA/RESTART ${PDYm1}.180000.MOM.res.nc ${COMOUT}/RESTART/${PDYm1}.180000.MOM.res.nc > cmb.restart.${PDYm1}.180000.res.out" >> cmdfile.cpout
for i in $(seq $insertedsleepcommands);do echo "sleep 5" >> cmdfile.cpout;done
echo "${USHrtofs}/rtofs_combine_nc.sh False $DATA/RESTART ${PDY}.000000.MOM.res.nc ${COMOUT}/RESTART/${PDY}.000000.MOM.res.nc > cmb.restart.${PDY}.000000.res.out" >> cmdfile.cpout
for i in $(seq $insertedsleepcommands);do echo "sleep 5" >> cmdfile.cpout;done
for res in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
do
   echo "${USHrtofs}/rtofs_combine_nc.sh False $DATA/RESTART ${PDYm1}.180000.MOM.res_${res}.nc ${COMOUT}/RESTART/${PDYm1}.180000.MOM.res_${res}.nc > cmb.restart.${PDYm1}.180000.res_${res}.out" >> cmdfile.cpout
   for i in $(seq $insertedsleepcommands);do echo "sleep 5" >> cmdfile.cpout;done
   echo "${USHrtofs}/rtofs_combine_nc.sh False $DATA/RESTART ${PDY}.000000.MOM.res_${res}.nc ${COMOUT}/RESTART/${PDY}.000000.MOM.res_${res}.nc > cmb.restart.${PDY}.000000.res_${res}.out" >> cmdfile.cpout
   for i in $(seq $insertedsleepcommands);do echo "sleep 5" >> cmdfile.cpout;done
done

#Diagnostic files
for dfile in $(ls -r ocn*nc.0000)
do
   # dfil is all but the last numeral field (e.g.0000)
   dfil=$(echo $dfile | cut -d. -f1-2)
   yyyy=$(echo $dfil | cut -d_ -f2)
   ddd=$(echo $dfil | cut -d_ -f3)
   hh=$(echo $dfil | cut -d_ -f4 | cut -d. -f1)
   filedate=$($UTILROOT/ush/date2jday.sh $yyyy$ddd)
   if [ $filedate -gt $PDY ]
   then
      alpha=f
      hhh=$($NHOUR ${filedate}${hh} ${PDY}00)
      fhour=$(printf "%03d\n" $hhh)
   else
      alpha=tm
      hhh=$($NHOUR ${PDY}00 ${filedate}${hh})
      fhour=$(printf "%03d\n" $hhh)
   fi
   field1=$(echo $dfile | cut -d_ -f1 | cut -c1-4)
# ocns
   if [ $field1 == ocns ]
   then
      rname=${RUN}_${modID}_2ds.${alpha}${fhour}.nc
   fi
# ocn3
   if [ $field1 == ocn3 ]
   then
      # save only daily's
      if [ $hh -ne 00 ]; then continue;fi
      field2=$(echo $dfile | cut -c4-8)
      rname=${RUN}_${modID}_3dz.${alpha}${fhour}.daily.$field2.nc
   fi
# ocnp
   if [ $field1 == ocnp ]
   then
      rname=ocnp.${alpha}${fhour}.nc
   fi
   echo "${USHrtofs}/rtofs_combine_nc.sh $DATA $dfil ${COMOUT}/$rname > cmb.$dfil.out" >> cmdfile.cpout
   for i in $(seq $insertedsleepcommands);do echo "sleep 5" >> cmdfile.cpout;done
done

# copy singular files
# for the ice.nc files --
#    remove the erroneous external variables and dimensions
#    rename variables to v2.5 names
for ifile in $(ls history)
do
   icedat=$(echo history/$ifile | cut -d. -f2 | cut -d- -f1-3 | tr -d "-")
   icesec=$(echo history/$ifile | cut -d. -f2 | cut -d- -f4 | tr -d "-")
   let icehr=$icesec*24/86400
   icehr=$(printf "%02d\n" $icehr)
   if [ $icedat -gt $PDY ]
   then
      alpha=f
      ihour=$($NHOUR $icedat$icehr ${PDY}00)
   else
      alpha=tm
      ihour=$($NHOUR ${PDY}00 $icedat$icehr)
   fi
   ihour=$(printf "%03d\n" $ihour)
   echo "ncatted -O -a external_variables,global,d,, history/$ifile" > cleanup.$ifile.sh
   echo "ncks -O -v time,time_bounds,TLON,TLAT,ULON,ULAT,hi_h,Tsfc_h,aice_h,uvel_h,vvel_h history/$ifile history/$ifile" >> cleanup.$ifile.sh
   echo "${USHrtofs}/rtofs_ice_change_varNames.sh history/$ifile" >> cleanup.$ifile.sh
   echo "$cpcmd history/$ifile $COMOUT/rtofs_glo_2ds.${alpha}${ihour}.ice.nc" >> cleanup.$ifile.sh
   echo "./cleanup.$ifile.sh > rename.ice.fields.$ifile.out" >> cmdfile.cpout
   chmod +x ./cleanup.$ifile.sh
done
idate=$(echo $PDYm1 | cut -c1-4)-$(echo $PDYm1 | cut -c5-6)-$(echo $PDYm1 | cut -c7-8)-64800
fdate=$(echo $PDY | cut -c1-4)-$(echo $PDY | cut -c5-6)-$(echo $PDY | cut -c7-8)-00000
echo "$cpcmd RESTART/iced.${idate}.nc $COMOUT/RESTART" >> cmdfile.cpout
echo "$cpcmd RESTART/iced.${fdate}.nc $COMOUT/RESTART" >> cmdfile.cpout
echo "$cpcmd RESTART/datm.gfs.cpl.r.${idate}.nc $COMOUT/RESTART" >> cmdfile.cpout
echo "$cpcmd RESTART/datm.gfs.cpl.r.${fdate}.nc $COMOUT/RESTART" >> cmdfile.cpout
echo "$cpcmd datm.gfs.datm.r.${idate}.nc $COMOUT">> cmdfile.cpout
echo "$cpcmd datm.gfs.datm.r.${fdate}.nc $COMOUT">> cmdfile.cpout

# diagnostics and log files
for logfile in ice_diag.d mediator.log atm.log
do
  echo "$cpcmd $logfile $COMOUT/analysis.$logfile" >> cmdfile.cpout
done
for momoutputfile in $(ls MOM6_OUTPUT)
do
   echo "$cpcmd MOM6_OUTPUT/$momoutputfile $COMOUT/MOM6_OUTPUT/analysis.$momoutputfile" >> cmdfile.cpout
done

chmod +x cmdfile.cpout
mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpout > cpout.out
err=$? ; export err ; err_chk
date

#################################################
msg="THE RTOFS_GLO_ANALYSIS JOB HAS ENDED NORMALLY on $(hostname) at $(date)."
postmsg "$msg"

exit
