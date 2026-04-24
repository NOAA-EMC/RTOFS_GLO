#!/bin/sh
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_incup.sh                                #
# Script description:                                                         #
#                                                                             #
# Abstract: This script generates the incremental update fields               #
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

msg="RTOFS_GLO_INCUP JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

# --------------------------------------------------------------------------- #
# 1. copy in files from parm

inc_hours=06
# future - calculate startdate=PDYm1-inc_hours
startdate=${PDYm2:0:4}-${PDYm2:4:2}-${PDYm2:6:2}-64800
sdate=${PDYm2}18
for pfile in ice_in
do
  cp $PARMrtofs/${pfile} ./ice_in 
  sed -i -e "s/&YMDS.nc/$startdate.nc/" -e "s/&adjust_aice/adjust_aice/g" ./ice_in
done

for pfile in data_table datm_in datm.streams diag_table fd_ufs.yaml input.nml noahmptable.tbl ufs.configure
do
  cp $PARMrtofs/$pfile .
done
sed -i -e "s/&startup_continue/startup/" ./ufs.configure

# model_configure modified on the fly
for pfile in model_configure
do 
#  cp $PARMrtofs/mom/incup/$pfile .
  cp $PARMrtofs/$pfile model_configure
  sed -i -e "s/&YYYY/${PDYm2:0:4}/g" -e "s/&MM/${PDYm2:4:2}/g" -e "s/&DD/${PDYm2:6:2}/g" -e "s/&HH/18/g" -e "s/&NH/6/g" ./model_configure
done
# --------------------------------------------------------------------------- #
# 2a. link in fix files to top-level

#for ffile in cice_model.res.nc grid_cice_NEMS_mx008.nc kmtu_cice_NEMS_mx008.nc mesh.mx008.nc
for ffile in grid_cice_NEMS_mx008.nc kmtu_cice_NEMS_mx008.nc mesh.mx008.nc
do
  ln -sf $FIXrtofs/$ffile .
done

# --------------------------------------------------------------------------- #
# 2b. link or copy fix files and parm files to INPUT

mkdir INPUT
for ffile in chl_mom6.nc grid_spec.nc mesh.datm.3072x1536.nc mom6_vgrid.nc regional.mom6.nc ocean_mosaic.nc runoff.daitren.clim.0.08deg.nc sss_mom6.nc tidal_amplitude.nc
do
  ln -sf $FIXrtofs/$ffile INPUT/.
done
ln -s $FIXrtofs/depth_GLB.0p08_09m11ob2_mom6.nc INPUT/depth_GLBb0.08_09m11ob2_mom6.nc

for pfile in MOM_input
do
  cp $PARMrtofs/${pfile} .
  sed -i -e "s/ODA_INCUPD = &Value /ODA_INCUPD = True/" ./MOM_input
  mv MOM_input INPUT/
done
for pfile in MOM_layout MOM_override
do
  cp $PARMrtofs/$pfile ./INPUT
done

# --------------------------------------------------------------------------- #
# 3. Populate INPUT directory with pdym1 restart forcing incup files

# check that MOM.res files exist
for momres in $(ls $COMINm1/RESTART/${PDYm2}.180000.MOM.res*nc*)
do
   fn=$(basename $momres | cut -d. -f3-)
   ln -s $momres INPUT/$fn
done

# forcing (for this time period)
ln -s $COMINm1/gfs.forcing.files.nc INPUT/gfs.forcing.files.nc

# incremental update files

dayinc=`$EXECrtofs/rtofs_dtg -f Y%Y_D%j_S00000 ${PDYm1}00`
ln -s $COMIN/rtofs_glo.MOM.res_${dayinc}_inc.TSzh.nc INPUT/MOM.inc.TSzh.nc
ln -s $COMIN/rtofs_glo.MOM.res_${dayinc}_inc.UV.nc INPUT/MOM.inc.UV.nc

# iced with right date
icedate=$(echo $PDYm2 | cut -c1-4)-$(echo $PDYm2 | cut -c5-6)-$(echo $PDYm2 | cut -c7-8)-64800
ln -s $COMINm1/RESTART/iced.${icedate}.nc INPUT/iced.${icedate}.nc
echo INPUT/iced.${icedate}.nc > ice.restart_file

# --------------------------------------------------------------------------- #
# 4. Create other directories, get the ice concentration analysis nc file.

mkdir history RESTART MOM6_OUTPUT

#sic.nc file
ln -s  $COMIN/sic.nc RESTART/

# --------------------------------------------------------------------------- #
# 5. get executable

cp $EXECrtofs/ufs_model.x fv3.exe

mpiexec -np $NMPI --cpu-bind core  ./fv3.exe >> $pgmout 2>errfile
rc=$?

if [ $rc -ne 0 ]
then
   echo we are saying goodbye
   $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Abnormal model exit" \
      "problem with incup model run - return code " $rc
   exit
fi

# combine files (one restart and two archives) and copy to COMOUT
rm -f cmdfile.cpout
mkdir -p $COMOUT/RESTART $COMOUT/MOM6_OUTPUT
insertedsleepcommands=5
cpcmd="cp -p -f"
if type cpfs > /dev/null;then cpcmd=cpfs;fi

#Restarts
echo "${USHrtofs}/rtofs_combine_nc.sh False $DATA/RESTART ${PDYm1}.000000.MOM.res.nc ${COMOUT}/RESTART/${PDYm1}.000000.MOM.res.nc > cmb.restart.${PDYm1}.000000.res.out" >> cmdfile.cpout
for i in $(seq $insertedsleepcommands);do echo "sleep 5" >> cmdfile.cpout;done
for res in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
do
   echo "${USHrtofs}/rtofs_combine_nc.sh False $DATA/RESTART ${PDYm1}.000000.MOM.res_${res}.nc ${COMOUT}/RESTART/${PDYm1}.000000.MOM.res_${res}.nc > cmb.restart.${PDYm1}.000000.res_${res}.out" >> cmdfile.cpout
   for i in $(seq $insertedsleepcommands);do echo "sleep 5" >> cmdfile.cpout;done
done

#Diagnostic files
# for incup we only save the last diagnostic file (ddd)
ddd=$($USHutil/date2jday.sh $PDYm1 | cut -c5-7)
#for dfile in $(ls ocn*_${ddd}_00.nc.0000)
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
adate=$(echo $PDYm1 | cut -c1-4)-$(echo $PDYm1 | cut -c5-6)-$(echo $PDYm1 | cut -c7-8)-00000
echo "$cpcmd RESTART/iced.${adate}.nc $COMOUT/RESTART" >> cmdfile.cpout
echo "$cpcmd RESTART/datm.gfs.cpl.r.${adate}.nc $COMOUT/RESTART" >> cmdfile.cpout
echo "$cpcmd datm.gfs.datm.r.${adate}.nc $COMOUT" >> cmdfile.cpout

# diagnostics and log files
mkdir $COMOUT/MOM6_OUTPUT
for logfile in ice_diag.d mediator.log atm.log
do
  echo "$cpcmd $logfile $COMOUT/incup.$logfile" >> cmdfile.cpout
done
for momoutputfile in $(ls MOM6_OUTPUT)
do
   echo "$cpcmd MOM6_OUTPUT/$momoutputfile $COMOUT/MOM6_OUTPUT/incup.$momoutputfile" >> cmdfile.cpout
done

chmod +x cmdfile.cpout
mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpout > cpout.out
err=$? ; export err ; err_chk
date

msg="THE RTOFS_GLO_INCUP JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"

exit
