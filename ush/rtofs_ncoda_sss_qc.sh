#!/bin/ksh

#   this script runs NCODA pre_QC and NCODA QC for SSS

echo "*** Started script $0 on hostname "`hostname`' at time '`date`
set -xa

export run_dir=$DATA
log_dir=$run_dir/logs/sss_qc
mkdir -p $log_dir

cut_dtg=${PDYm1}00

#   set QC environmental variables
export ATM_MODEL_DIR=$COMIN
export CODA_CLIM_DIR=$FIXrtofs/codaclim
export CRTM_COEF_DIR=$FIXrtofs/crtmclim
export GDEM_CLIM_DIR=$FIXrtofs/gdem
export LSEA_CLIM_DIR=$FIXrtofs/codaclim
export MODAS_CLIM_DIR=$FIXrtofs/modas
export OCN_DATA_DIR=$run_dir/ocnqc
mkdir -p $OCN_DATA_DIR/incoming
mkdir -p $OCN_DATA_DIR/sss

#   set paths to NCEP netCDF files
export SSS_DATA_DIR=$DCOMROOT/dev

echo "current date/time is " $( date)
echo "data cut date time group is " $cut_dtg

#--------------------------------------------------------------------------------------
echo " "
echo "NCODA SSS pre_QC"

#   create 2-day list of SMOS and SMAP SSS netCDF files to process
cd $SSS_DATA_DIR
for k in 00 24 48
do
   prv_dtg=$( $EXECncoda/dtg -w -h -$k $cut_dtg )
   ymd=${prv_dtg:0:8}
   cmd=$ymd/wtxtbul/satSSS/SMOS/"SM_OPER_MIR*$ymd*"
   ls $cmd > $log_dir/smos_$k.$cut_dtg
done

for k in 00 24 48
do
   prv_dtg=$( $EXECncoda/dtg -w -h -$k $cut_dtg )
   ymd=${prv_dtg:0:8}
   cmd=$ymd/wtxtbul/satSSS/SMAP/"SMAP_L2B_SSS*$ymd*h5"
   ls $cmd > $log_dir/smap_$k.$cut_dtg
done

#   change to working directory
cd $log_dir
cat smos_*.$cut_dtg > smos_sss_files.$cut_dtg
cat smap_*.$cut_dtg > smap_sss_files.$cut_dtg

#   execute ncoda pre_qc for SSS netCDF files
$EXECncoda/ncoda_sat_sss_nc $cut_dtg > sss_preqc.$cut_dtg.out

#--------------------------------------------------------------------------------------
echo "  "
echo "NCODA SSS QC"

#   create prediction namelist file
rm -f prednl
cat << eof1 > prednl
 &prednl
   ocn_modl(0)   = 'CODA',
   ocn2_path(0)  = '${COMINm1}/ncoda/hycom_var/restart'
   ocn2_fcst(0)  = .true.,
   ocn2_nest(0)  = 1,
   ocn2_upd(0)   = 24,
   prd2_use(0)   = 'updt',
   ocn3_path(0)  = '${COMINm1}/ncoda/hycom_var/restart'
   ocn3_fcst(0)  = .true.,
   ocn3_nest(0)  = 1,
   ocn3_upd(0)   = 24,
   prd3_use(0)   = 'stat',

   ocn_modl(1)   = 'CODA',
   ocn2_path(1)  = '${COMINm1}/ncoda/glbl_var/restart'
   ocn2_fcst(1)  = .false.,
   ocn2_nest(1)  = 1,
   ocn2_upd(1)   = 24,
   prd2_use(1)   = 'updt',
   ocn3_path(1)  = 'dummy',
   ocn3_fcst(1)  = .false.,
   ocn3_nest(1)  = 1,
   ocn3_upd(1)   = 24,
   prd3_use(1)   = 'stat',

   ocn_modl(2)   = 'CODA',
   ocn2_path(2)  = '${COMINm1}/ncoda/nhem_var/restart',
   ocn2_fcst(2)  = .false.,
   ocn2_nest(2)  = 1,
   ocn2_upd(2)   = 24,
   prd2_use(2)   = 'updt',
   ocn3_path(2)  = 'dummy',
   ocn3_fcst(2)  = .false.,
   ocn3_nest(2)  = 1,
   ocn3_upd(2)   = 24,
   prd3_use(2)   = 'stat',
 &end
eof1

#   clear symbolic links
rm -f $OCN_DATA_DIR/incoming/sss.a
rm -f $OCN_DATA_DIR/incoming/sss.b

#   execute ncoda qc
ln -s $OCN_DATA_DIR/incoming/sss.a.$cut_dtg $OCN_DATA_DIR/incoming/sss.a
ln -s $OCN_DATA_DIR/incoming/sss.b.$cut_dtg $OCN_DATA_DIR/incoming/sss.b
$EXECncoda/ncoda_qc $cut_dtg sss > sss_qc.$cut_dtg.out
mv fort.44 sss_qc.$cut_dtg.rej

#   cleanup
#rm -f smos_00.* smos_24.* smos_48.*
#rm -f smap_00.* smap_24.* smap_48.*

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`

exit 0

