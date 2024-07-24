#!/bin/ksh

#   this script runs 2DVAR pre_QC and NCODA QC for SFCOBS

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)
set -xa

export run_dir=$DATA
log_dir=$run_dir/logs/sfc_qc
mkdir -p $log_dir

cut_dtg=${PDY}00
prv_dtg=$( $EXECrtofs/rtofs_dtg -w -h -24 $cut_dtg )
ddtg=${PDY}${cyc}

# set update cycle
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
mkdir -p $OCN_DATA_DIR/sfc

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

#   set path to BUFR dump files
export BUFR_DATA_DIR=$DATA/dump

echo "current date/time is " $( date)
echo "data cut date time group is " $ddtg

#--------------------------------------------------------------------------------------
echo " "
echo "NCODA SFCOBS pre_QC"

#   change to working directory
cd $log_dir

#   execute ncoda pre_qc for SFCOBS bufr files
$EXECrtofs/rtofs_ncoda_bufr_decode sfc $ddtg $upd > sfc_preqc.$ddtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_bufr_decode=",$err

echo "  "
echo "NCODA SFCOBS QC"

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
rm -f $OCN_DATA_DIR/incoming/sfc.a
rm -f $OCN_DATA_DIR/incoming/sfc.b

#   execute ncoda qc
ln -s $OCN_DATA_DIR/incoming/sfc.a.$ddtg $OCN_DATA_DIR/incoming/sfc.a
ln -s $OCN_DATA_DIR/incoming/sfc.b.$ddtg $OCN_DATA_DIR/incoming/sfc.b
$EXECrtofs/rtofs_ncoda_qc $ddtg sfc > sfc_qc.$ddtg.out
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_qc=",$err
if [ -e fort.44 ]
then
  mv fort.44 sfc_qc.$ddtg.rej
fi

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)

exit 0

