#!/bin/ksh

#   this script runs 2dvar pre_QC and NCODA QC for SSS

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)
set -xa

export run_dir=$DATA
log_dir=$run_dir/logs/sss_qc
mkdir -p $log_dir

cut_dtg=${PDY}00
cut_dtg=${PDYm1}00
ddtg=${PDYm1}${cyc}
upd=6

#   set QC environmental variables
export ATM_MODEL_DIR=$COMIN
export CODA_CLIM_DIR=$FIXrtofs/codaclim
export GDEM_CLIM_DIR=$FIXrtofs/gdem
export HYCOM_FIX_DIR=$FIXrtofs
export LSEA_CLIM_DIR=$FIXrtofs/codaclim
export MODAS_CLIM_DIR=$FIXrtofs/modas
export OCN_DATA_DIR=$run_dir/ocnqc
mkdir -p $OCN_DATA_DIR/incoming
mkdir -p $OCN_DATA_DIR/sss

#   set paths to NCEP netCDF files
export SSS_DATA_DIR=$DCOMINSSS

#  export SMOS HDF5 file locking
export HDF5_USE_FILE_LOCKING=FALSE

echo "current date/time is " $( date)
echo "data cut date time group is " $ddtg

#--------------------------------------------------------------------------------------
echo " "
echo "NCODA SSS pre_QC"

#   create 2-day list of SMOS and SMAP SSS netCDF files to process
cd $SSS_DATA_DIR
for k in 00 24 48
do
   prv_dtg=$( $EXECrtofs/rtofs_dtg -w -h -$k $cut_dtg )
   ymd=${prv_dtg:0:8}
   cmd=$ymd/wtxtbul/satSSS/SMOS/"SM_OPER_MIR*$ymd*nc"
   if [ -s $cmd ] ; then
      ls $cmd > $log_dir/smos_$k.${ddtg}_prelim
   else
      echo "WARNING $cmd does not exist"
   fi
done

for k in 00 24 48
do
   prv_dtg=$( $EXECrtofs/rtofs_dtg -w -h -$k $cut_dtg )
   ymd=${prv_dtg:0:8}
   cmd=$ymd/wtxtbul/satSSS/SMAP/"SMAP_L2B_SSS_NRT*$ymd*h5"
   if [ -s $cmd ] ; then
      ls $cmd > $log_dir/smap_$k.${ddtg}_prelim
   else
      echo "WARNING $cmd does not exist"
   fi
done

#   change to working directory
cd $log_dir
cat smos_*.${ddtg}_prelim > smos_sss_files.${ddtg}_prelim
cat smap_*.${ddtg}_prelim > smap_sss_files.${ddtg}_prelim

# check on readability of smos files
echo timecheck smos start ncdump at $(date)
while read line
do
  ncdump -k $SSS_DATA_DIR/$line > /dev/null
  ncrc=$?
  if [ $ncrc -eq 0 ]
  then
     echo $line >> smos_sss_files.$ddtg
  else
     echo "WARNING - file $SSS_DATA_DIR/$line and will not be processed."
  fi
done < smos_sss_files.${ddtg}_prelim
echo timecheck smos finish ncdump at $(date)

# check on readability of smap files
echo timecheck smap start h5dump at $(date)
while read line
do
  h5dump -H $SSS_DATA_DIR/$line > /dev/null
  h5rc=$?
    if [ $h5rc -eq 0 ]
  then
     echo $line >> smap_sss_files.$ddtg
  else
     echo "WARNING - file $SSS_DATA_DIR/$line and will not be processed."
  fi
done < smap_sss_files.${ddtg}_prelim
echo timecheck smap finish h5dump at $(date)

if [[ ! -f smos_sss_files.$ddtg || ! -s smos_sss_files.$ddtg ]]; then
   echo "WARNING - smos_sss_files.$ddtg does not exist/is empty. No SMOS files to process."
fi
if [[ ! -f smap_sss_files.$ddtg || ! -s smap_sss_files.$ddtg ]]; then
   echo "WARNING - smap_sss_files.$ddtg does not exist/is empty. No SMAP files to process."
fi
if [[ ! -f smos_sss_files.$ddtg || ! -s smos_sss_files.$ddtg ]] && [[ ! -f smap_sss_files.$ddtg || ! -s smap_sss_files.$ddtg ]]; then
   echo "SSS.obs_control file will not be updated"
fi
#   execute ncoda pre_qc for SSS netCDF files
$EXECrtofs/rtofs_ncoda_sat_sss_nc $ddtg $upd > sss_preqc.$ddtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_sat_sss_nc=",$err

#--------------------------------------------------------------------------------------
echo "  "
echo "NCODA SSS QC"

#   create prediction namelist file
rm -f prednl
cat << eof1 > prednl
 &prednl
   ocn_modl(0)   = 'CODA',
   ocn2_path(0)  = '${run_dir}/glbl_var/restart'
   ocn2_fcst(0)  = .true.,
   ocn2_nest(0)  = 1,
   ocn2_upd(0)   = $upd,
   prd2_use(0)   = 'updt'
 &end
eof1

#   clear symbolic links
rm -f $OCN_DATA_DIR/incoming/sss.a
rm -f $OCN_DATA_DIR/incoming/sss.b

#   execute ncoda qc
ln -s $OCN_DATA_DIR/incoming/sss.a.$ddtg $OCN_DATA_DIR/incoming/sss.a
ln -s $OCN_DATA_DIR/incoming/sss.b.$ddtg $OCN_DATA_DIR/incoming/sss.b
$EXECrtofs/rtofs_ncoda_qc $ddtg sss > sss_qc.$ddtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_qc=",$err
mv fort.44 sss_qc.$ddtg.rej

#   cleanup
#rm -f smos_00.* smos_24.* smos_48.*
#rm -f smap_00.* smap_24.* smap_48.*

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)

exit 0

