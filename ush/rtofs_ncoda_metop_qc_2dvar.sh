#!/bin/ksh

#   this script runs NCODA pre_QC and NCODA QC for METOP

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)
set -xa

export run_dir=$DATA
log_dir=$run_dir/logs/metop_qc
mkdir -p $log_dir

cut_dtg=${PDY}00
prv_dtg=$( $EXECrtofs/rtofs_dtg -w -h -24 $cut_dtg )
ddtg=${PDY}${cyc}
upd=6

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
mkdir -p $OCN_DATA_DIR/metop

#   set paths to NCEP netCDF files
export SST_DATA_DIR=$DCOMINSST

#   set path to BUFR dump files
export BUFR_DATA_DIR=$DATA/dump

echo "current date/time is " $( date)
echo "data cut date time group is " $ddtg

#--------------------------------------------------------------------------------------
echo " "
echo "NCODA METOP pre_QC"

#   create list of MTA, MTB and MTC sst netCDF files to process
cd $SST_DATA_DIR

case $cyc in
  00)
      ymd=${prv_dtg:0:8}
      for k in 18 19 20 21 22 23
      do
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MA*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mta_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MB*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mtb_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MC*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mtc_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
      done
      ymd=${cut_dtg:0:8}
      for k in 00 01 02 03
      do
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MA*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mta_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi 
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MB*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mtb_$k.$cut_dtg
         else 
            echo "WARNING $cmd does not exist"
         fi
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MC*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mtc_$k.$cut_dtg
         else 
            echo "WARNING $cmd does not exist"
         fi
      done
      ;;
  06)
      ymd=${cut_dtg:0:8}
      for k in 00 01 02 03 04 05 06 07 08 09
      do
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MA*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mta_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MB*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mtb_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MC*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mtc_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
      done
      ;;
  12)
      ymd=${cut_dtg:0:8}
      for k in 06 07 08 09 10 11 12 13 14 15
      do
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MA*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mta_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MB*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mtb_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MC*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mtc_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
      done
      ;;
  18)
      ymd=${cut_dtg:0:8}
      for k in 12 13 14 15 16 17 18 19 20 21
      do
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MA*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mta_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MB*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mtb_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
         cmd="$ymd/sst/$ymd$k*L2P*AVHRRF_MC*.nc"
         if [ -s $cmd ] ; then
            ls $cmd > $log_dir/mtc_$k.$cut_dtg
         else
            echo "WARNING $cmd does not exist"
         fi
      done
      ;;
esac

#   change to working directory
cd $log_dir
cat mta_*.$cut_dtg mtb_*.$cut_dtg mtc_*.$cut_dtg > acspo_sst_files.${ddtg}_prelim

echo timecheck metop start ncdump at $(date)
while read line
do
  ncdump -k $SST_DATA_DIR/$line > /dev/null
  ncrc=$?
  if [ $ncrc -eq 0 ]
  then
     echo $line >> acspo_sst_files.$ddtg
  else
     echo "WARNING - file $SST_DATA_DIR/$line and will not be processed."
  fi
done < acspo_sst_files.${ddtg}_prelim
echo timecheck metop finish ncdump at $(date)

if [[ ! -f  acspo_sst_files.$ddtg || ! -s acspo_sst_files.$ddtg ]]; then
   echo "WARNING - acspo_sst_files.$ddtg does not exist/is empty. No METOP files to process."
   echo "METOP.obs_control file will not be updated"
fi

#   execute ncoda pre_qc for METOP netCDF files
$EXECrtofs/rtofs_ncoda_acspo_sst_nc metop $ddtg $upd > metop_preqc.$ddtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_acspo_sst_nc=",$err

#--------------------------------------------------------------------------------------
echo "  "
echo "NCODA METOP QC"

#   create prediction namelist file
rm -f prednl
cat << eof1 > prednl
 &prednl
   ocn_modl(0)   = 'CODA',
   ocn2_path(0)  = '${DATA}/glbl_var/restart'
   ocn2_fcst(0)  = .false.,
   ocn2_nest(0)  = 1,
   ocn2_upd(0)   = $upd,
   prd2_use(0)   = 'updt'
 &end
eof1

#   clear symbolic links
rm -f $OCN_DATA_DIR/incoming/metop.a
rm -f $OCN_DATA_DIR/incoming/metop.b

#   execute ncoda qc
ln -s $OCN_DATA_DIR/incoming/metop.a.$ddtg $OCN_DATA_DIR/incoming/metop.a
ln -s $OCN_DATA_DIR/incoming/metop.b.$ddtg $OCN_DATA_DIR/incoming/metop.b
$EXECrtofs/rtofs_ncoda_qc $ddtg metop > metop_qc.$ddtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_qc=",$err
mv fort.44 metop_qc.$ddtg.rej

#   cleanup

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)

exit 0

