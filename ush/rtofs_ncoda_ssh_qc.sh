#!/bin/ksh

#   this script runs NCODA pre_QC and NCODA QC for ADT SSH

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)
set -xa

export run_dir=$DATA
log_dir=$run_dir/logs/ssh_qc
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
mkdir -p $OCN_DATA_DIR/ssh

#   set paths to NCEP netCDF files (SSH_DATA_DIR/date/SSH_DATA_DIR_2)
export SSH_DATA_DIR=$DATA
export SSH_DATA_DIR_2=wgrdbul/adt

echo "current date/time is " $(date)
echo "data cut date time group is " $cut_dtg

#--------------------------------------------------------------------------------------
echo " "
echo "NCODA SSH pre_QC"

#   change to working directory
cd $log_dir

echo SSH_DATA_DIR $DCOMINSSH to $SSH_DATA_DIR
echo OCN_DATA_DIR $OCN_DATA_DIR

# check readability of ssh files and copy them to verified directory
echo timecheck ssh start ncdump at $(date)
for apdy in $PDYm7 $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1
do
  verified_location=$DATA/$apdy/$SSH_DATA_DIR_2
  mkdir -p $verified_location
  for file in $(ls $DCOMINSSH/$apdy/$SSH_DATA_DIR_2/rads_adt_ncoda_*.nc)
  do
    ncdump -k $file > /dev/null
    ncrc=$?
    if [ $ncrc -eq 0 ]
    then
       cp -p $file $verified_location
    else
       echo "WARNING - file $file and will not be processed."
    fi
  done
done
echo timecheck ssh finish ncdump at $(date)

#   execute ncoda pre_qc for SSH netCDF files
$EXECrtofs/rtofs_ncoda_adt_ssh_nc $cut_dtg > ssh_preqc.$cut_dtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_adt_ssh_nc=",$err

echo "  "
echo "NCODA SSH QC"

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
rm -f $OCN_DATA_DIR/incoming/ssh.a
rm -f $OCN_DATA_DIR/incoming/ssh.b

#   execute ncoda qc
ln -s $OCN_DATA_DIR/incoming/ssh.a.$cut_dtg $OCN_DATA_DIR/incoming/ssh.a
ln -s $OCN_DATA_DIR/incoming/ssh.b.$cut_dtg $OCN_DATA_DIR/incoming/ssh.b
$EXECrtofs/rtofs_ncoda_qc $cut_dtg ssh > ssh_qc.$cut_dtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_qc=",$err
mv fort.44 ssh_qc.$cut_dtg.rej

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)

exit 0

