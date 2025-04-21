#!/bin/ksh

#   this script runs NCODA pre_QC and NCODA QC for PROFILEs

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)
set -xa

export run_dir=$DATA

#   set output directory
log_dir=$run_dir/logs/profile_qc
mkdir -p $log_dir

cut_dtg=${PDYm1}00

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
mkdir -p $OCN_DATA_DIR/profile

#   set path to BUFR dump files
export BUFR_DATA_DIR=$DATA/dump

echo "current date/time is " $( date)
echo "data cut date time group is " $cut_dtg

#--------------------------------------------------------------------------------------
echo " "
echo "NCODA PROFILE pre_QC"

#   change to working directory
cd $log_dir

#   execute ncoda pre_qc for PROFILE bufr files
$EXECrtofs/rtofs_ncoda_bufr_decode prof $cut_dtg > prof_preqc.$cut_dtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_bufr_decode=",$err

echo "  "
echo "NCODA PROFILE QC"

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
   ocn3_path(1)  = '${COMINm1}/ncoda/gom_var/restart'
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
rm -f $OCN_DATA_DIR/incoming/profile.a
rm -f $OCN_DATA_DIR/incoming/profile.b

#   execute ncoda qc
ln -s $OCN_DATA_DIR/incoming/profile.a.$cut_dtg $OCN_DATA_DIR/incoming/profile.a
ln -s $OCN_DATA_DIR/incoming/profile.b.$cut_dtg $OCN_DATA_DIR/incoming/profile.b
$EXECrtofs/rtofs_ncoda_qc $cut_dtg profile > prof_qc.$cut_dtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_qc=",$err
[[ -f fort.45 ]] && mv fort.45 prof_qc.$cut_dtg.rpt
[[ -f fort.47 ]] && mv fort.47 prof_qc.$cut_dtg.err
[[ -f fort.48 ]] && mv fort.48 prof_qc.$cut_dtg.vgd
[[ -f fort.49 ]] && mv fort.49 prof_qc.$cut_dtg.inc
[[ -f fort.51 ]] && mv fort.51 prof_qc.$cut_dtg.dup
[[ -f fort.52 ]] && mv fort.52 prof_qc.$cut_dtg.stb
[[ -f fort.53 ]] && mv fort.53 prof_qc.$cut_dtg.arg
#mv gmeta prof_qc.$cut_dtg.gmeta

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)

exit 0

