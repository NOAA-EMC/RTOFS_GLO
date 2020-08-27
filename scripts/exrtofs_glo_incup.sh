#!/bin/sh
set -xa
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_incup.sh                                   #
# Script description:                                                         #
#                                                                             #
# Author:        Dan Iredell     Org: NP23         Date: 2020-07-30           #
#                                                                             #
# Abstract: Runs 3hour HYCOM with incremental update
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             #
# Script history log:                                                         #
# 2020-07-30  Dan Iredell                                                     #
#                                                                             #
###############################################################################

export PS4='$SECONDS + '

cd $DATA

msg="RTOFS_GLO_INCUP JOB has begun on `hostname` at `date`"
postmsg "$jlogfile" "$msg"

# --------------------------------------------------------------------------- #

# 1. Set up inputs for run

inc_hours=3

dtg=${PDYm1}00
dtginc=`$EXECrtofs/rtofs_dtg $dtg -h -$inc_hours`
dtgm1=`$EXECrtofs/rtofs_dtg $dtg -d -1`
dtgm2=`$EXECrtofs/rtofs_dtg $dtg -h -$inc_hours`
dtgp1=`$EXECrtofs/rtofs_dtg $dtg -d 1`
dtgp1inc=`$EXECrtofs/rtofs_dtg $dtgp1 -h -$inc_hours` 
cisec=`echo ${dtginc:8:2} 86400 24 | awk '{printf ( "%5.5d", ($1 * $2 / $3))}'`
jday2=`$USHutil/date2jday.sh ${dtgm2:0:8}`
archday2=${jday2:0:4}_${jday2:4:3}_${dtgm2:8:2}

# inc_hours with incremental update
hday12=`$USHrtofs/rtofs_date_normal2hycom.sh $dtg`
hday11=`echo $hday12 $inc_hours 24 | awk '{printf ("%12.3f", ($1 - ($2 / $3)))}'`
export iday=$hday11

echo $hday11 $hday12 > limits

ln -sf $COMIN/rtofs_glo.ssmi.$dtg.r ssmi.r
dtgr0=`$EXECrtofs/rtofs_dtg $dtgm1 -f "%Y-%m-%d"`
dtgr1=`$EXECrtofs/rtofs_dtg $dtg -f "%Y-%m-%d"`

# cp in yesterday's restart file produced at n-03
if [[ ! -e $COMINm1/rtofs_glo.t00z.n-03.restart.a ]] &&  \
   [[ ! -e $COMINm1/rtofs_glo.t00z.n-03.restart.b ]] &&  \
   [[ ! -e $COMINm1/rtofs_glo.t00z.n-03.restart_cice ]]; then
     echo "  $COMINm1/rtofs_glo.t00z.n-03.restart.a $COMINm1/rtofs_glo.t00z.n-03.restart.b \
             $COMINm1/rtofs_glo.t00z.n-03.restart_cice missing, exiting now."
     export err=1;err_chk
else
   ln -sf $COMINm1/rtofs_glo.t00z.n-03.restart.a restart_in.a
   ln -sf $COMINm1/rtofs_glo.t00z.n-03.restart.b restart_in.b
   ln -sf $COMINm1/rtofs_glo.t00z.n-03.restart_cice  cice.restart.${dtgr0}-$cisec
   echo cice.restart.${dtgr0}-$cisec > cice.restart_file
fi

#link forcing
case=anal
pref=rtofs_glo.$case.t00z

ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.cb_11_10mm.a       cb.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.cb_11_10mm.b       cb.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.cice.prec_lanl_12.r   cice.prec_lanl_12.r
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.cice.rhoa_ncar85-88_12.r cice.rhoa_ncar85-88_12.r
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.forcing.chl.a      forcing.chl.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.forcing.chl.b      forcing.chl.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.forcing.offlux.a   forcing.offlux.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.forcing.offlux.b   forcing.offlux.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.forcing.rivers.a   forcing.rivers.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.forcing.rivers.b   forcing.rivers.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.cice.r    regional.cice.r
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.a    regional.grid.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.b    regional.grid.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.a   regional.depth.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.b   regional.depth.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.iso.sigma.a        iso.sigma.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.iso.sigma.b        iso.sigma.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.tbaric.a           tbaric.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.tbaric.b           tbaric.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_int.a        relax.intf.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_int.b        relax.intf.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_rmu.a        relax.rmu.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_rmu.b        relax.rmu.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_sal.a        relax.saln.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_sal.b        relax.saln.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_ssh.a        relax.ssh.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_ssh.b        relax.ssh.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_sss.a        relax.sssrmx.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_sss.b        relax.sssrmx.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_tem.a        relax.temp.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.relax_tem.b        relax.temp.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.thkdf4.a           thkdf4.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.thkdf4.b           thkdf4.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.thkdf4_double.a    thkdf4_double.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.thkdf4_double.b    thkdf4_double.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.veldf2.a           veldf2.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.veldf2.b           veldf2.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.veldf4.a           veldf4.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.veldf4.b           veldf4.b

   for f in airtmp glbrad lwdflx precip presur radflx shwflx surtmp vapmix wndewd wndnwd wndspd; do
      ln -sf $COMINm1/${pref}.forcing.$f.a  forcing.$f.a
      ln -sf $COMINm1/${pref}.forcing.$f.b  forcing.$f.b
   done

   # Prepare sea ice forcing with the start time of the run
   export RUN_MODE=incup
   $USHrtofs/${RUN}_iceforcing.sh

mkdir incup
ln -sf $COMIN/rtofs_glo.incupd.$archday2.a incup/incupd.$archday2.a
ln -sf $COMIN/rtofs_glo.incupd.$archday2.b incup/incupd.$archday2.b

cp $PARMrtofs/${RUN}_${modID}.${inputgrid}.archs.input        ./archs.input
cp $PARMrtofs/${RUN}_${modID}.${inputgrid}.incup.blkdat.input ./blkdat.input 
cp $PARMrtofs/${RUN}_${modID}.${inputgrid}.incup.ice_in       ./ice_in
cp $PARMrtofs/${RUN}_${modID}.${inputgrid}.patch.input        ./patch.input

/bin/rm -f core
touch core

date
mpirun -l $EXECrtofs/rtofs_hycom -procs $NPROCS >> $pgmout 2>errfile
err=$?; export err ; err_chk
echo " error from rtofs_hycom=",$err

date

# cp to COMOUT
cp restart_out.a $COMOUT/rtofs_glo.t00z.n-24.restart.a
cp restart_out.b $COMOUT/rtofs_glo.t00z.n-24.restart.b
cp cice.restart.${dtgr1}-00000 $COMOUT/rtofs_glo.t00z.n-24.restart_cice
mode=n
for afile in `ls ${DATA}/archv.????_???_00.a ${DATA}/archs.????_???_00.a ${DATA}/arche.????_???_00.a` 
do
  cfile=`basename $afile`
  YYYY=`echo $cfile | cut -c7-10`
  DDD=`echo $cfile | cut -c12-14`
  HH=`echo $cfile | cut -c16-17`
  YYYYMMDD=`${USHutil}/date2jday.sh ${YYYY}${DDD}`
  MM=`echo $YYYYMMDD | cut -c5-6`
  DD=`echo $YYYYMMDD | cut -c7-8`
  LEAD=`$NHOUR ${YYYY}${MM}${DD}${HH} ${PDY}${mycyc}`
  arch=`echo $cfile | cut -d. -f1`
  HYCOMarchTplate=${RUN}_${modID}.t${mycyc}z.${mode}${LEAD}.${arch}
  if [ $arch = "archv" ] ; then
    cp -p -f ${afile%.a}.a ${COMOUT}/${HYCOMarchTplate}.a
    cp -p -f ${afile%.a}.b ${COMOUT}/${HYCOMarchTplate}.b
    cp -p -f ${afile%.a}.txt ${COMOUT}/${HYCOMarchTplate}.txt
  fi
  if [ $arch = "archs" ] ; then
    cp -p -f ${afile%.a}.a ${COMOUT}/${HYCOMarchTplate}.a
    cp -p -f ${afile%.a}.b ${COMOUT}/${HYCOMarchTplate}.b
    cp -p -f ${afile%.a}.txt ${COMOUT}/${HYCOMarchTplate}.txt
  fi
  if [ $arch = "arche" ] ; then
    cp -p -f ${afile%.a}.a ${COMOUT}/${HYCOMarchTplate}.a
    cp -p -f ${afile%.a}.b ${COMOUT}/${HYCOMarchTplate}.b
  fi
done

