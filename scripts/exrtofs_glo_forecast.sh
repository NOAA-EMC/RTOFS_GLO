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
#   rtofs_runstaging.sh - gets input files from COMIN FIX and PARM            #
#   rtofs_submit.sh - submits the RTOFS HYCOM simulation                      #
#   rtofs_tmp2com.sh - copies the products to COMOUT                          #
#                                                                             #
###############################################################################
set -xa

export PS4='$SECONDS + '

cd $DATA

msg="RTOFS_GLO_FORECAST JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

# --------------------------------------------------------------------------- #
# 0. set some defaults

typeset -Z5 SSSSS

# --------------------------------------------------------------------------- #
# 1. copy in files from parm to top level

#get number of days to run, 
#source $PARMrtofs/rtofs_glo.navy_0.08.config
fcst=fcst1
  fcstdays=1
  PD1=$PDY
  NH=24
  PD2=$PDYp1
  


adate=${PD1:0:4}-${PD1:4:2}-${PD1:6:2}-00000
for pfile in ice_in; do
cp $PARMrtofs/$pfile ./ice_in
  sed -i -e "s/&YMDS.nc/$adate.nc/" -e "s/&adjust_aice/none/" ./ice_in
done

for pfile in data_table datm_in datm.streams diag_table fd_ufs.yaml input.nml noahmptable.tbl ufs.configure
do
  cp $PARMrtofs/$pfile .
done
  sed -i -e "s/&startup_continue/continue/" ./ufs.configure

# model_configure created on the fly
for pfile in model_configure
do
  cp $PARMrtofs/$pfile .
  sed -i -e  "s/&YYYY/${PD1:0:4}/g" -e "s/&MM/${PD1:4:2}/g" -e "s/&DD/${PD1:6:2}/g" -e "s/&HH/00/g" -e "s/&NH/$NH/g" ./model_configure
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
  cp $PARMrtofs/${pfile} .
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
for momres in $(ls $COMIN/RESTART/${PD1}.000000.MOM.res*nc*)
do
   fn=$(basename $momres | cut -d. -f3-)
   ln -s $momres INPUT/$fn
done

# forcing (for this time period)  (change datm.streams when changing)
#temporarily from fix
#ln -s $COMINm1/gfs.2025121400-2025122218_positive.nc INPUT/.
ln -s $FIXrtofs/gfs.2025121400-2025122218_positive.nc INPUT/.

# iced with right date
icedate=$(echo $PD1 | cut -c1-4)-$(echo $PD1 | cut -c5-6)-$(echo $PD1 | cut -c7-8)-00000
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
#for rfile in $(ls RESTART/${PDY}.180000.MOM.res*.nc* RESTART/${PDYp1}.000000.MOM.res*.nc*)
for rfile in $(ls RESTART/${PD2}.000000.MOM.res*.nc*)
do
  echo "cp -p -f $rfile $COMOUT/RESTART" >> cmdfile.cpout
done
idate=$(echo $PD1 | cut -c1-4)-$(echo $PD1 | cut -c5-6)-$(echo $PD1 | cut -c7-8)-00000
fdate=$(echo $PD2 | cut -c1-4)-$(echo $PD2 | cut -c5-6)-$(echo $PD2 | cut -c7-8)-00000
#echo "cp -p -f RESTART/iced.${idate}.nc RESTART/iced.${fdate}.nc $COMOUT/RESTART" >> cmdfile.cpout
#echo "cp -p -f RESTART/datm.gfs.cpl.r.${idate}.nc RESTART/datm.gfs.cpl.r.${fdate}.nc $COMOUT/RESTART" >> cmdfile.cpout 
echo "cp -p -f RESTART/iced.${fdate}.nc $COMOUT/RESTART" >> cmdfile.cpout
echo "cp -p -f RESTART/datm.gfs.cpl.r.${fdate}.nc $COMOUT/RESTART" >> cmdfile.cpout 

#echo "cp -p -f datm.gfs.datm.r.${idate}.nc datm.gfs.datm.r.${fdate}.nc $COMOUT" >> cmdfile.cpout
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
#mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpout
./cmdfile.cpout
err=$? ; export err ; err_chk
date

