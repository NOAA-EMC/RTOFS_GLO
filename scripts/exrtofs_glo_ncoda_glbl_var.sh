#!/bin/sh
set -xa
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_ncoda_glbl_var.sh                          #
# Script description:                                                         #
#                                                                             #
# Author:        Dan Iredell     Org: NP23         Date: 2020-07-30           #
#                                                                             #
# Abstract: 
#   this script runs global 2DVAR sea ice, sss and sst analyses
#   the analyses are performed on a near global mercator grid
#   grid resoltuion is 12 km at the equator, 8 km mid-latitude
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             #
# Script history log:                                                         #
# 2020-07-30  Dan Iredell                                                     #
#                                                                             #
###############################################################################

export PS4='$SECONDS + '

msg="RTOFS_GLO_NCODA_GLBL_VAR JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

cd $DATA

# --------------------------------------------------------------------------- #

# 1. Populate DATA with glbl_var files from COMINm1/ncoda
echo timecheck RTOFS_GLO_GLBL start get at $(date)

mkdir -p $DATA/restart
mkdir -p $DATA/work
rm -f cmdfile.cpin
if compgen -G "$COMINm1/ncoda/glbl_var/restart/*" > /dev/null
then
  for gv in $(ls $COMINm1/ncoda/glbl_var/restart/); do
    echo "cp -p -f $COMINm1/ncoda/glbl_var/restart/$gv $DATA/restart" >> cmdfile.cpin
  done
  chmod +x cmdfile.cpin
  mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpin
  err=$? ; export err ; err_chk
  date
else
  echo "cold starting global var!"
fi

ln -sf $COMIN/ncoda/ocnqc $DATA
echo timecheck RTOFS_GLO_GLBL finish get at $(date)

# 2. build namelists

rm -f odsetnl
rm -f ogridnl
rm -f oanl
rm -f omapnl

#  deny metop-a: 110, 111, 112
#  deny metop-b: 115, 116, 117

cat << eof1 > odsetnl
 &dsetnl
  dsoclim = '$FIXrtofs/codaclim'
  dsogdem = '$FIXrtofs/gdem'
  dsorff  = '$DATA/restart'
  dsoudat = '$DATA/ocnqc'
  dsowork = '$DATA/work'
 &end
eof1

cat << eof2 > ogridnl
 &gridnl
  alnnt = 180.,
  delx  = 12355.43554688,
  dely  = 12355.43554688,
  iref  = 1621,
  jref  = 1221,
  kko   = 1,
  m     = 3241,
  n     = 2441,
  nnest = 1,
  nproj = 1,
  phnt1 = 0.,
  phnt2 = 0.,
  rlat  = 0.,
  rlon  = 180.,
 &end
eof2

# using cluster (1,2,6) 1.5 instead of 1 as in hera's run, and set cluster(7) to default (biology)
# mask_opt set to 2D instead of 0D
# rscl(7) changed to rscl(6)
# vscl(7) changed to rscl(6)
cp $PARMrtofs/${RUN}_${modID}.glbl.oanl.in   ./oanl

cat << eof4 > omapnl
 &omapnl
  do_data_raw = .true.,
 &end
eof4

# 3 run global var (NCODA 2D)

ddtg=${PDYm1}00
log_dir=$DATA/logs
mkdir -p $log_dir

#   execute ncoda variational programs
echo timecheck RTOFS_GLO_GLBL start setup at $(date)
#NCODA setup
$EXECrtofs/rtofs_ncoda_setup 2D ncoda ogridnl $ddtg > pout1
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_setup=",$err

#NCODA prep
mpiexec -n 24 --cpu-bind core $EXECrtofs/rtofs_ncoda_prep 2D ncoda ogridnl $ddtg > pout2
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_prep=",$err

#NCODA var
mpiexec -n $NPROCS --cpu-bind core $EXECrtofs/rtofs_ncoda 2D ncoda ogridnl $ddtg > pout3
err=$?; export err ; err_chk
echo " error from rtofs_ncoda=",$err

#NCODA post
mpiexec -n $NPROCS --cpu-bind core $EXECrtofs/rtofs_ncoda_post 2D ncoda ogridnl $ddtg > pout4
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_post=",$err

echo timecheck RTOFS_GLO_GLBL finish post at $(date)

#   rename local files
#mv fort.40 $log_dir/glbl_var.$ddtg.sus
mv fort.67 $log_dir/glbl_var.$ddtg.obs
mv fort.68 $log_dir/glbl_var.$ddtg.grd

#   create graphics
DoGraphics=NO
if [ $DoGraphics = YES ] ; then
  export OCN_OUTPUT_DIR=$DATA/restart
  export OCN_CLIM_DIR=$FIXrtofs/codaclim
  #NCODA map
  $EXECrtofs/rtofs_ncoda_map $ddtg > pout5
  err=$?; export err ; err_chk
  echo " error from rtofs_ncoda_map=",$err
  mv gmeta $log_dir/glbl_var.$ddtg.gmeta
fi

#
#   combine work files
cat pout* > $log_dir/glbl_var.$ddtg.out
cat $log_dir/glbl_var.$ddtg.out >> $pgmout

# 4. Copy last 15 days of data back to COMOUT/ncoda
echo timecheck RTOFS_GLO_GLBL start put at $(date)

mkdir -p $COMOUT/ncoda/glbl_var/restart
rm -f cmdfile.cpout
for d in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
  backymdh=$( $EXECrtofs/rtofs_dtg -d -$d ${PDY}00 )
  backymd=${backymdh:0:8}
  if compgen -G "$DATA/restart/*${backymd}*" > /dev/null
  then
     for gv in $(ls $DATA/restart/*${backymd}*); do
         echo "cp -p -f $gv $COMOUT/ncoda/glbl_var/restart" >> cmdfile.cpout
     done
#     echo "cp -p -f $DATA/restart/*${backymd}* $COMOUT/ncoda/glbl_var/restart" >> cmdfile.cpout
  fi
done

chmod +x cmdfile.cpout
mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpout
err=$? ; export err ; err_chk
date

mkdir -p $COMOUT/ncoda/logs/glbl_var
cp -p -f $DATA/logs/*.$ddtg.* $COMOUT/ncoda/logs/glbl_var
echo timecheck RTOFS_GLO_GLBL finish put at $(date)

#################################################
msg="THE RTOFS_GLO_NCODA_GLBL_VAR JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"

################## END OF SCRIPT #######################

