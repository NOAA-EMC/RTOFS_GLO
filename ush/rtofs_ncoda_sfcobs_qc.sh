#!/bin/ksh

#   this script runs NCODA pre_QC and NCODA QC for SFCOBS
#   it also creates surface velocity observations from
#   drifter tracks

echo "*** Started script $0 on hostname "`hostname`' at time '`date`
set -xa

export run_dir=$DATA

#   set output directory
log_dir=$run_dir/logs/sfc_qc
mkdir -p $log_dir

cut_dtg=${PDYm1}00

#   set QC environmental variables
export ATM_MODEL_DIR=$DATA
export CODA_CLIM_DIR=$FIXrtofs/codaclim
export CRTM_COEF_DIR=$FIXrtofs/crtmclim
export GDEM_CLIM_DIR=$FIXrtofs/gdem
export LSEA_CLIM_DIR=$FIXrtofs/codaclim
export MODAS_CLIM_DIR=$FIXrtofs/modas
export OCN_DATA_DIR=$run_dir/ocnqc
mkdir -p $OCN_DATA_DIR/incoming
mkdir -p $OCN_DATA_DIR/sfc
mkdir -p $OCN_DATA_DIR/velocity

#   set path to BUFR dump files
export BUFR_DATA_DIR=$DATA/dump

echo "current date/time is " $( date)
echo "data cut date time group is " $cut_dtg

#--------------------------------------------------------------------------------------
echo " "
echo "NCODA SFCOBS pre_QC"

#   change to working directory
cd $log_dir

#   execute ncoda pre_qc for SFCOBS bufr files
$EXECncoda/ncoda_bufr_decode sfc $cut_dtg > sfc_preqc.$cut_dtg.out

echo "  "
echo "NCODA SFCOBS QC"

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
rm -f $OCN_DATA_DIR/incoming/sfc.a
rm -f $OCN_DATA_DIR/incoming/sfc.b

#   execute ncoda qc
ln -s $OCN_DATA_DIR/incoming/sfc.a.$cut_dtg $OCN_DATA_DIR/incoming/sfc.a
ln -s $OCN_DATA_DIR/incoming/sfc.b.$cut_dtg $OCN_DATA_DIR/incoming/sfc.b
$EXECncoda/ncoda_qc $cut_dtg sfc velocity > sfc_qc.$cut_dtg.out
mv fort.44 sfc_qc.$cut_dtg.rej
mv fort.46 sfc_qc_vel.$cut_dtg.rej
mv fort.50 sfc_qc_vel.$cut_dtg.rpt

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`

exit 0

