#!/bin/ksh

#   this script runs NCODA pre_QC and NCODA QC for HF Radar
#   and drifting buoy velocity observations

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)
set -xa

export run_dir=$DATA
log_dir=$run_dir/logs/vel_qc
mkdir -p $log_dir

cut_dtg=${PDYm1}00
prv_dtg=$( $EXECrtofs/rtofs_dtg -w -h -24 $cut_dtg )

#   set QC environmental variables
export ATM_MODEL_DIR=$COMIN/..
export CODA_CLIM_DIR=$FIXrtofs/codaclim
export CRTM_COEF_DIR=$FIXrtofs/crtmclim
export GDEM_CLIM_DIR=$FIXrtofs/gdem
export HYCOM_FIX_DIR=$FIXrtofs
export HYCOM_FIX_DIR=$DATA
export LSEA_CLIM_DIR=$FIXrtofs/codaclim
export MODAS_CLIM_DIR=$FIXrtofs/modas
export OCN_DATA_DIR=$run_dir/ocnqc
mkdir -p $OCN_DATA_DIR/incoming
mkdir -p $OCN_DATA_DIR/sfc
mkdir -p $OCN_DATA_DIR/velocity

# link in forcing.wndspd so that ncoda programs find it
mkdir -p ./data_${PDYm1}00
if [[ -s $COMINm1/rtofs_glo.anal.t00z.forcing.wndspd.a ]] && \
   [[ -s $COMINm1/rtofs_glo.anal.t00z.forcing.wndspd.b ]]
then
   if [[ ! -s ./data_${PDYm1}00/forcing.wndspd.a ]] && \
      [[ ! -s ./data_${PDYm1}00/forcing.wndspd.b ]] 
   then
      ln -sf $COMINm1/rtofs_glo.anal.t00z.forcing.wndspd.a ./data_${PDYm1}00/forcing.wndspd.a
      ln -sf $COMINm1/rtofs_glo.anal.t00z.forcing.wndspd.b ./data_${PDYm1}00/forcing.wndspd.b
   fi
else
   echo "using uniform 5 m/s wind speed"
fi

# path to location of hfr data
export HFR_DATA_DIR=$DCOMINHFR

#   set path to BUFR dump files
export BUFR_DATA_DIR=$DATA/dump

echo "current date/time is " $( date)
echo "data cut date time group is " $cut_dtg
echo "previous date time group is " $prv_dtg
echo " "
echo "NCODA HF Radar pre_QC"

#   create list of HF Radar netCDF files to process
cd $HFR_DATA_DIR

ymd=${prv_dtg:0:8}
for k in 12 13 14 15 16 17 18 19 20 21 22 23
do
   cmd="$ymd/wgrdbul/ndbc/$ymd$k*hfr*.nc"
   ls $cmd > $log_dir/hfr_$k.${cut_dtg}_prelim
done
ymd=${cut_dtg:0:8}
for k in 00 01 02 03 04 05 06 07 08 09 10 11
do
   cmd="$ymd/wgrdbul/ndbc/$ymd$k*hfr*.nc"
   ls $cmd > $log_dir/hfr_$k.${cut_dtg}_prelim
done

#   change to working directory
cd $log_dir
cat hfr_*.${cut_dtg}_prelim > hfr_files.${cut_dtg}_prelim

# check on readability of hfr files
echo timecheck hfr start ncdump at $(date)
while read line
do
  ncdump -k $HFR_DATA_DIR/$line > /dev/null
  ncrc=$?
  if [ $ncrc -eq 0 ]
  then
     echo $line >> hfr_files.$cut_dtg
  else
     echo "WARNING - file $HFR_DATA_DIR/$line and will not be processed."
  fi
done < hfr_files.${cut_dtg}_prelim
echo timecheck hfr finish ncdump at $(date)

#   execute ncoda pre_qc for HF Radar netCDF files
$EXECrtofs/rtofs_ncoda_hf_radar_nc $cut_dtg > pout1
err=$?; export err ; err_chk
echo " error from rtofs_hf_radar_nc=",$err

#--------------------------------------------------------------------------------------
echo " "
echo "NCODA Drifter pre_QC"

#   execute ncoda pre_qc for BUFR drifter files
$EXECrtofs/rtofs_ncoda_drft_decode $cut_dtg > pout2
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_drft_decode=",$err

if [ -e fort.71 ]
then
  mv -f fort.71 drifter_frames.$cut_dtg.out
fi
cat pout1 pout2 > vel_preqc.$cut_dtg.out

#--------------------------------------------------------------------------------------
echo "  "
echo "NCODA Velocity QC"

#   create prediction namelist file
rm -f prednl
cat << eof1 > prednl
 &prednl
   ocn_modl(0)   = 'CODA',
   ocn2_path(0)  = '${COMINm1}/ncoda/hycom_var/restart'
   ocn2_fcst(0)  = .false.,
   ocn2_nest(0)  = 1,
   ocn2_upd(0)   = 24,
   prd2_use(0)   = 'updt'
   ocn3_path(0)  = '${COMINm1}/ncoda/hycom_var/restart'
   ocn3_fcst(0)  = .true.,
   ocn3_nest(0)  = 1,
   ocn3_upd(0)   = 24,
   prd3_use(0)   = 'stat'
 &end
eof1

#   clear symbolic links
rm -f $OCN_DATA_DIR/incoming/hfr.a
rm -f $OCN_DATA_DIR/incoming/hfr.b
rm -f $OCN_DATA_DIR/incoming/drft.a
rm -f $OCN_DATA_DIR/incoming/drft.b

#   execute ncoda qc
ln -s $OCN_DATA_DIR/incoming/hfr.a.$cut_dtg $OCN_DATA_DIR/incoming/hfr.a
ln -s $OCN_DATA_DIR/incoming/hfr.b.$cut_dtg $OCN_DATA_DIR/incoming/hfr.b
ln -s $OCN_DATA_DIR/incoming/drft.a.$cut_dtg $OCN_DATA_DIR/incoming/drft.a
ln -s $OCN_DATA_DIR/incoming/drft.b.$cut_dtg $OCN_DATA_DIR/incoming/drft.b
$EXECrtofs/rtofs_ncoda_qc $cut_dtg velocity > vel_qc.$cut_dtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_qc=",$err
if [ -e fort.44 ]
then
  mv fort.44 vel_qc.$cut_dtg.rej
fi
if [ -e fort.46 ]
then
  mv fort.46 vel_qc_rpt.$cut_dtg.rej
fi
if [ -e gmeta ]
then
  mv -f gmeta vel_qc.$cut_dtg.gmeta
fi

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)

exit 0

