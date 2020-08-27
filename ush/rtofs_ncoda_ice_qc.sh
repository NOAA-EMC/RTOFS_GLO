#!/bin/ksh

#   this script runs NCODA pre_QC and NCODA QC for ICE

echo "*** Started script $0 on hostname "`hostname`' at time '`date`
set -xa

export run_dir=$DATA

#   set output directory
log_dir=$run_dir/logs/ice_qc
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
mkdir -p $OCN_DATA_DIR/ice

#   set paths to NCEP netCDF files
export SSMI_ICE_DATA_DIR=$DATA/ice_nc
export AMSR_ICE_DATA_DIR=$DCOMINAMSR

echo "current date/time is " $( date)
echo "data cut time group is " $cut_dtg
echo "previous date time group is " $prv_dtg

#--------------------------------------------------------------------------------------
echo " "
echo "NCODA ICE pre_QC"

#   create lists of SSMI and AMSR ice netCDF files to process
cd $SSMI_ICE_DATA_DIR

# SSMI
ymd=${prv_dtg:0:8}
cmd="l2out*$ymd*"
ls $cmd > $log_dir/ssmi_01.$cut_dtg

ymd=${cut_dtg:0:8}
cmd="l2out*$ymd*"
ls $cmd > $log_dir/ssmi_02.$cut_dtg

# AMSR
cd $AMSR_ICE_DATA_DIR
ymd=${prv_dtg:0:8}
cmd="$ymd/seaice/pda/AMSR2-SEAICE*s$ymd*"
ls $cmd > $log_dir/amsr_01.$cut_dtg

ymd=${cut_dtg:0:8}
cmd="$ymd/seaice/pda/AMSR2-SEAICE*s$ymd*"
ls $cmd > $log_dir/amsr_02.$cut_dtg

#   change to working directory
cd $log_dir
cat ssmi_*.$cut_dtg > ssmi_files.$cut_dtg
if [ ! -s ssmi_files.$cut_dtg ]; then
   echo "WARNING - ssmi_files.$cut_dtg is empty. No SSMI files to process."
   echo "SSMI.obs_control file will not be updated"
fi
cat amsr_*.$cut_dtg > amsr_ice_files.$cut_dtg
if [ ! -s amsr_ice_files.$cut_dtg ]; then
   echo "WARNING - amsr_ice_files.$cut_dtg is empty. No AMSR_ICE files to process."
   echo "AMSR_ICE.obs_control file will not be updated"
fi

#   execute ncoda pre_qc for ICE netCDF files
#SSMI
$EXECrtofs/rtofs_ncoda_ncep_ice_nc ssmi $cut_dtg > ssmi_preqc.$cut_dtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_ncep_ice_nc ssmi=",$err
#AMSR
$EXECrtofs/rtofs_ncoda_ncep_ice_nc amsr_ice $cut_dtg > amsr_ice_preqc.$cut_dtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_ncep_ice_nc amsr_ice=",$err

#--------------------------------------------------------------------------------------
echo "  "
echo "NCODA ICE QC"

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
rm -f $OCN_DATA_DIR/incoming/amsr_ice.a
rm -f $OCN_DATA_DIR/incoming/amsr_ice.b
rm -f $OCN_DATA_DIR/incoming/ssmi.a
rm -f $OCN_DATA_DIR/incoming/ssmi.b

#   execute ncoda qc
#SSMI
ln -s $OCN_DATA_DIR/incoming/ssmi.a.$cut_dtg $OCN_DATA_DIR/incoming/ssmi.a
ln -s $OCN_DATA_DIR/incoming/ssmi.b.$cut_dtg $OCN_DATA_DIR/incoming/ssmi.b
$EXECrtofs/rtofs_ncoda_qc $cut_dtg ssmi > ssmi_qc.$cut_dtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_qc ssmi=",$err
mv fort.44 ssmi_qc.$cut_dtg.rej

#AMSR
ln -s $OCN_DATA_DIR/incoming/amsr_ice.a.$cut_dtg $OCN_DATA_DIR/incoming/amsr_ice.a
ln -s $OCN_DATA_DIR/incoming/amsr_ice.b.$cut_dtg $OCN_DATA_DIR/incoming/amsr_ice.b
$EXECrtofs/rtofs_ncoda_qc $cut_dtg amsr_ice > amsr_ice_qc.$cut_dtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_qc amsr_ice=",$err
mv fort.44 amsr_ice_qc.$cut_dtg.rej

#   cleanup

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`

exit 0
