#!/bin/ksh

#   this script runs NCODA pre_QC and NCODA QC for NPP

echo "*** Started script $0 on hostname "`hostname`' at time '`date`
set -xa

export run_dir=$DATA
log_dir=$run_dir/logs/npp_qc
mkdir -p $log_dir

cut_dtg=${PDYm1}00
prv_dtg=$( $EXECrtofs/rtofs_dtg -w -h -24 $cut_dtg )

#   set QC environmental variables
export ATM_MODEL_DIR=$COMIN
export CODA_CLIM_DIR=$FIXrtofs/codaclim
export CRTM_COEF_DIR=$FIXrtofs/crtmclim
export GDEM_CLIM_DIR=$FIXrtofs/gdem
export HYCOM_FIX_DIR=$FIXrtofs
export LSEA_CLIM_DIR=$FIXrtofs/codaclim
export MODAS_CLIM_DIR=$FIXrtofs/modas
export OCN_DATA_DIR=$run_dir/ocnqc
mkdir -p $OCN_DATA_DIR/incoming
mkdir -p $OCN_DATA_DIR/viirs

#   set paths to NCEP netCDF files
export SST_DATA_DIR=$DCOMINSST

#   set path to BUFR dump files
export BUFR_DATA_DIR=$DATA/dump

echo "current date/time is " $( date)
echo "data cut date time group is " $cut_dtg
echo "previous date time group is " $prv_dtg

#--------------------------------------------------------------------------------------
echo " "
echo "NCODA NPP pre_QC"

#   create list of NPP VIIRS sst netCDF files to process
cd $SST_DATA_DIR

ymd=${prv_dtg:0:8}
for k in 12 13 14 15 16 17 18 19 20 21 22 23
do
   cmd="$ymd/sst/$ymd$k*L2P*VIIRS_NPP*"
   if [ -s $cmd ] ; then
      ls $cmd > $log_dir/npp_$k.$cut_dtg
   else
      echo "WARNING $cmd does not exist"
   fi
done

ymd=${cut_dtg:0:8}
for k in 00 01 02 03 04 05 06 07 08 09 10 11
do
   cmd="$ymd/sst/$ymd$k*L2P*VIIRS_NPP*"
   if [ -s $cmd ] ; then
      ls $cmd > $log_dir/npp_$k.$cut_dtg
   else
      echo "WARNING $cmd does not exist"
   fi
done

#   change to working directory
cd $log_dir
cat npp_*.$cut_dtg > acspo_sst_files.$cut_dtg
if [[ ! -f acspo_sst_files.$cut_dtg || ! -s acspo_sst_files.$cut_dtg ]]; then
   echo "WARNING - acspo_sst_files.$cut_dtg does not exist/is empty. No VIIRS NPP files to process."
   echo "NPP_VIIRS.obs_control file will not be updated"
fi

#   execute ncoda pre_qc for NPP netCDF files
$EXECrtofs/rtofs_ncoda_acspo_sst_nc npp $cut_dtg > npp_preqc.$cut_dtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_acspo_sst_nc=",$err

#--------------------------------------------------------------------------------------
echo "  "
echo "NCODA NPP QC"

#   create prediction namelist file
rm -f prednl
cat << eof1 > prednl
 &prednl
   ocn_modl(0)   = 'CODA',
   ocn2_path(0)  = '${COMINm1}/ncoda/glbl_var/restart'
   ocn2_fcst(0)  = .false.,
   ocn2_nest(0)  = 1,
   ocn2_upd(0)   = 24,
   prd2_use(0)   = 'updt'
   ocn3_path(0)  = '${COMINm1}/ncoda/hycom_var/restart'
   ocn3_fcst(0)  = .true.,
   ocn3_nest(0)  = 1,
   ocn3_upd(0)   = 24,
   prd3_use(0)   = 'stat'

   ocn_modl(1)   = 'CODA',
   ocn2_path(1)  = '${COMINm1}/ncoda/nhem_var/restart'
   ocn2_fcst(1)  = .false.,
   ocn2_nest(1)  = 1,
   ocn2_upd(1)   = 24,
   prd2_use(1)   = 'updt'
   ocn3_path(1)  = 'dummy'
   ocn3_fcst(1)  = .false.,
   ocn3_nest(1)  = 1,
   ocn3_upd(1)   = 24,
   prd3_use(1)   = 'stat'

   ocn_modl(2)   = 'CODA',
   ocn2_path(2)  = '${COMINm1}/ncoda/shem_var/restart'
   ocn2_fcst(2)  = .false.,
   ocn2_nest(2)  = 1,
   ocn2_upd(2)   = 24,
   prd2_use(2)   = 'updt'
   ocn3_path(2)  = 'dummy'
   ocn3_fcst(2)  = .false.,
   ocn3_nest(2)  = 1,
   ocn3_upd(2)   = 24,
   prd3_use(2)   = 'stat'
 &end
eof1

#   clear symbolic links
rm -f $OCN_DATA_DIR/incoming/npp.a
rm -f $OCN_DATA_DIR/incoming/npp.b

#   execute ncoda qc
ln -s $OCN_DATA_DIR/incoming/npp.a.$cut_dtg $OCN_DATA_DIR/incoming/npp.a
ln -s $OCN_DATA_DIR/incoming/npp.b.$cut_dtg $OCN_DATA_DIR/incoming/npp.b
$EXECrtofs/rtofs_ncoda_qc $cut_dtg npp > npp_qc.$cut_dtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_qc=",$err
mv fort.44 npp_qc.$cut_dtg.rej

#   cleanup

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`

exit 0

