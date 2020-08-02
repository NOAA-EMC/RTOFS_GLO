#!/bin/ksh

#   this script runs NCODA AOD QC using JPSS VIIRS
#   1. dump the AOD grib files into direct access files
#   2. decode the JPSS NetCDF files and collocate the AOD
#      fields with the satellite brightness temperatures
#      - the brightness temperatures are used to form split
#        window differences (10.8-12.3 and 8.6-10.8)
#   3. calculate the canonical variates and create a time
#      dependent data base for classifying new ssts as
#      aerosol contaminated

echo "*** Started script $0 on hostname "`hostname`' at time '`date`
set -xa

export run_dir=$DATA
log_dir=$run_dir/logs/aod_qc
mkdir -p $log_dir

cut_dtg=${PDYm1}00
prv_dtg=$( $EXECncoda/dtg -w -h -24 $cut_dtg )

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
mkdir -p $OCN_DATA_DIR/aod
export AOD_DATA_DIR=$OCN_DATA_DIR/aod

#   set paths to NCEP netCDF files
export SST_DATA_DIR=$DCOMROOT/$sst_dataloc

#   set path to BUFR dump files
export BUFR_DATA_DIR=$DATA/dump

echo "current date/time is " $( date)
echo "data cut date time group is " $cut_dtg
echo "previous date time group is " $prv_dtg

#--------------------------------------------------------------------------------------
echo " "
echo "NCODA AOD GRIB DECODER"

echo "Determine if GEFS is operational "

RUNAOD=YES

if [ "$RUNAOD" == "NO" ]
then 
   echo "bypassing running aod "
   exit
fi

#AOD_GRIB_DIR=$(compath.py gefs/prod)
AOD_GRIB_DIR=/gpfs/dell2/ptmp/Li.Pan/o/gefs.v12.0.0/com/gefs/dev/


ymd=${prv_dtg:0:8}
for k in 00 06 12 18
do
   sfx="z.pgrb2a.0p25.f000"
   h=$k
   cmd="/gefs.$ymd/$k/chem/pgrb2ap25_aer/geaer.t$k$sfx"
   $WGRIB2 $AOD_GRIB_DIR$cmd -no_header -ieee $AOD_DATA_DIR/FV3GFS_chem.$ymd$h

   sfx="z.pgrb2a.0p25.f003"
   typeset -Z2 h
   h=$(($k+03))
   cmd="/gefs.$ymd/$k/chem/pgrb2ap25_aer/geaer.t$k$sfx"
   $WGRIB2 $AOD_GRIB_DIR$cmd -no_header -ieee $AOD_DATA_DIR/FV3GFS_chem.$ymd$h
done

ymd=${cut_dtg:0:8}
for k in 00 06 12 18
do
   sfx="z.pgrb2a.0p25.f000"
   h=$k
   cmd="/gefs.$ymd/$k/chem/pgrb2ap25_aer/geaer.t$k$sfx"
   $WGRIB2 $AOD_GRIB_DIR$cmd -no_header -ieee $AOD_DATA_DIR/FV3GFS_chem.$ymd$h

   sfx="z.pgrb2a.0p25.f003"
   typeset -Z2 h
   h=$(($k+03))
   cmd="/gefs.$ymd/$k/chem/pgrb2ap25_aer/geaer.t$k$sfx"
   $WGRIB2 $AOD_GRIB_DIR$cmd -no_header -ieee $AOD_DATA_DIR/FV3GFS_chem.$ymd$h
done

export SST_DATA_DIR=$DCOMROOT/$sst_dataloc
cd $SST_DATA_DIR

ymd=${prv_dtg:0:8}
for k in 12 13 14 15 16 17 18 19 20 21 22 23
do
   cmd="$ymd/sst/$ymd$k*L2P*VIIRS_N20*"
   ls $cmd > $log_dir/n20_$k.$cut_dtg
done

ymd=${cut_dtg:0:8}
for k in 00 01 02 03 04 05 06 07 08 09 10 11
do
   cmd="$ymd/sst/$ymd$k*L2P*VIIRS_N20*"
   ls $cmd > $log_dir/n20_$k.$cut_dtg
done

#   change to working directory
cd $log_dir
cat n20_*.$cut_dtg > acspo_sst_files.$cut_dtg

#   execute ncoda aod pre_qc for JPSS VIIRS netCDF files
$EXECncoda/ncoda_aod_sst_nc jpss $cut_dtg > aod_preqc.$cut_dtg.out

#   cleanup
rm -f n20_*.*

#-------------------------------------------------------------------------------------
echo " "
echo "NCODA AOD Canonical Variate Analysis"

#   set diagnostics namelist
rm -f odiagnl
cat << eof1 > odiagnl
 &odiagnl
   do_cva = .true.,
 &end
eof1

#   change to working directory
cd $log_dir

#   execute ncoda diagnostics program
$EXECncoda/ncoda_diagn $cut_dtg > cva_diagn.$cut_dtg.out
mv gmeta cva_diagn.$cut_dtg.gmeta

#   cleanup

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`

exit 0

