#!/bin/sh
set -xa
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_ncoda_polar_var.sh                          #
# Script description:                                                         #
#                                                                             #
# Author:        Dan Iredell     Org: NP23         Date: 2020-07-30           #
#                                                                             #
# Abstract: 
#   this script runs a series of polar 2DVAR sea ice and sst analyses
#   the analyses are performed on polar stereographic grids over the
#   northern and southern hemisphere poles
#   grid resolutions are 9 km
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             #
# Script history log:                                                         #
# 2020-07-30  Dan Iredell                                                     #
#                                                                             #
###############################################################################

export PS4='$SECONDS + '

msg="RTOFS_GLO_NCODA_POLAR_VAR JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

cd $DATA

# --------------------------------------------------------------------------- #

# 1. Populate DATA/polar_var with polar_var files from COMINm1/ncoda
echo timecheck RTOFS_GLO_POLAR start get at $(date)

mkdir -p $DATA/nhem_var/restart
mkdir -p $DATA/shem_var/restart
mkdir -p $DATA/nhem_var/work
mkdir -p $DATA/shem_var/work
mkdir -p $DATA/logs/nhem_var
mkdir -p $DATA/logs/shem_var
ddtg=${PDYm1}00

coldstart=0
rm -f cmdfile.cpin
if compgen -G "$COMINm1/ncoda/nhem_var/restart/*" > /dev/null
then
  for nv in $(ls $COMINm1/ncoda/nhem_var/restart/); do
    echo "cp -p -f $COMINm1/ncoda/nhem_var/restart/$nv $DATA/nhem_var/restart" >> cmdfile.cpin
  done
else
  echo "WARNING - Cold starting $job - north hemisphere"
  coldstart=2
fi

if compgen -G "$COMINm1/ncoda/shem_var/restart/*" > /dev/null
then
  for sv in $(ls $COMINm1/ncoda/shem_var/restart/); do
    echo "cp -p -f $COMINm1/ncoda/shem_var/restart/$sv $DATA/shem_var/restart" >> cmdfile.cpin
  done
else
  echo "WARNING - Cold starting $job - south hemisphere"
  let coldstart=coldstart+1
fi
if [ $coldstart -gt 0 ]
then
  echo "WARNING - Cold starting $jobid"
  echo "WARNING - Job $jobid is cold-starting"                                  > $DATA/polar.coldstart.email
  echo "This is an abnormal event."                                            >> $DATA/polar.coldstart.email
  echo "The following directories are empty:"                                  >> $DATA/polar.coldstart.email
  if [[ $coldstart -eq 2 || $coldstart -eq 3 ]]
  then
      echo "$COMINm1/ncoda/nhem_var/restart"                                   >> $DATA/polar.coldstart.email
  fi
  if [[ $coldstart -eq 1 || $coldstart -eq 3 ]]
  then
      echo "$COMINm1/ncoda/shem_var/restart"                                   >> $DATA/polar.coldstart.email
  fi
  echo "This job will continue to run as a cold-start."                        >> $DATA/polar.coldstart.email
  cat $DATA/polar.coldstart.email | mail.py -s "WARNING - Job $job cold started"
fi

if [ -s cmdfile.cpin ]
then
  chmod +x cmdfile.cpin
  mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpin
  err=$? ; export err ; err_chk
  date
fi

ln -sf $COMIN/ncoda/ocnqc $DATA

# 2. Build common namelist

cp $PARMrtofs/${RUN}_${modID}.polar.oanl.in   ./nhem_var/oanl
cp $PARMrtofs/${RUN}_${modID}.polar.oanl.in   ./shem_var/oanl

# 3. Run NHEM (NCODA 2Dvar)

echo timecheck RTOFS_GLO_POLAR start nhem at $(date)
cd $DATA/nhem_var
#   build local nhem namelist files
rm -f odsetnl
rm -f ogridnl
rm -f omapnl

cat << eof2 > ogridnl
 &gridnl
  delx  = 9000.,
  dely  = 9000.,
  iref  = 450,
  jref  = 450,
  kko   = 1,
  m     = 900,
  n     = 900,
  nnest = 1,
  nproj = 3,
  phnt1 = 60.,
  phnt2 = 60.,
  rlat  = 90.,
  rlon  = 0.,
  alnnt = 300.,
 &end
eof2

cat << eof3 > odsetnl
 &dsetnl
  dsoclim = '$FIXrtofs/codaclim'
  dsogdem = '$FIXrtofs/gdem'
  dsorff  = '$DATA/nhem_var/restart'
  dsoudat = '$DATA/ocnqc'
  dsowork = '$DATA/nhem_var/work'
 &end
eof3

cat << eof4 > omapnl
 &omapnl
  do_data_raw = .true.,
 &end
eof4

#NCODA setup
$EXECrtofs/rtofs_ncoda_setup 2D ncoda ogridnl $ddtg > pout1
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_setup=",$err

#NCODA prep
mpiexec -n 1 $EXECrtofs/rtofs_ncoda_prep 2D ncoda ogridnl $ddtg > pout2
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_prep=",$err

#NCODA var
mpiexec -n $NPROCS --cpu-bind core $EXECrtofs/rtofs_ncoda 2D ncoda ogridnl $ddtg > pout3
err=$?; export err ; err_chk
echo " error from rtofs_ncoda=",$err

#NCODA post
mpiexec -n $NPROCS $EXECrtofs/rtofs_ncoda_post 2D ncoda ogridnl $ddtg > pout4
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_post=",$err

#   rename local files
#mv fort.40 $DATA/logs/nhem_var/nhem_var.$ddtg.sus
mv fort.67 $DATA/logs/nhem_var/nhem_var.$ddtg.obs
mv fort.68 $DATA/logs/nhem_var/nhem_var.$ddtg.grd

#   create graphics
DoGraphics=NO
if [ $DoGraphics = YES ] ; then
  echo timecheck RTOFS_GLO_POLAR start ncoda_map at $(date)
  export OCN_OUTPUT_DIR=$DATA/nhem_var/restart
  export OCN_CLIM_DIR=$FIXrtofs/codaclim
  #NCODA map
  $EXECrtofs/rtofs_ncoda_map $ddtg > pout5
  err=$?; export err ; err_chk
  echo " error from rtofs_ncoda_map=",$err
  mv gmeta $DATA/logs/nhem_var/nhem_var.$ddtg.gmeta
fi

cat pout* > $DATA/logs/nhem_var/nhem_var.$ddtg.out
cat $DATA/logs/nhem_var/nhem_var.$ddtg.out >> $DATA/$pgmout

# 4. Run SHEM (NCODA 2Dvar)

echo timecheck RTOFS_GLO_POLAR start shem at $(date)
cd $DATA/shem_var
#   build local shem namelist files
rm -f odsetnl
rm -f ogridnl
rm -f omapnl

cat << eof5 > ogridnl
 &gridnl
  delx  = 9000.,
  dely  = 9000.,
  iref  = 450,
  jref  = 450,
  kko   = 1,
  m     = 900,
  n     = 900,
  nnest = 1,
  nproj = 3,
  phnt1 = -60.,
  phnt2 = -60.,
  rlat  = -90.,
  rlon  = 0.,
  alnnt = 300.,
 &end
eof5

cat << eof6 > odsetnl
 &dsetnl
  dsoclim = '$FIXrtofs/codaclim'
  dsogdem = '$FIXrtofs/gdem'
  dsorff  = '$DATA/shem_var/restart'
  dsoudat = '$DATA/ocnqc'
  dsowork = '$DATA/shem_var/work'
 &end
eof6

#  sss_min = 30.,
cat << eof7 > omapnl
 &omapnl
  do_data_raw = .true.,
 &end
eof7

#NCODA setup
$EXECrtofs/rtofs_ncoda_setup 2D ncoda ogridnl $ddtg > pout1
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_setup=",$err

#NCODA prep
mpiexec -n 1 $EXECrtofs/rtofs_ncoda_prep 2D ncoda ogridnl $ddtg > pout2
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_prep=",$err

#NCODA var
mpiexec -n $NPROCS --cpu-bind core $EXECrtofs/rtofs_ncoda 2D ncoda ogridnl $ddtg > pout3
err=$?; export err ; err_chk
echo " error from rtofs_ncoda=",$err

#NCODA post
mpiexec -n $NPROCS $EXECrtofs/rtofs_ncoda_post 2D ncoda ogridnl $ddtg > pout4
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_post",$err

#   rename local files
#mv fort.40 $DATA/logs/shem_var/shem_var.$ddtg.sus
mv fort.67 $DATA/logs/shem_var/shem_var.$ddtg.obs
mv fort.68 $DATA/logs/shem_var/shem_var.$ddtg.grd

#   create graphics
DoGraphics=NO
if [ $DoGraphics = YES ] ; then
  export OCN_OUTPUT_DIR=$DATA/shem_var/restart
  export OCN_CLIM_DIR=$FIXrtofs/codaclim
  #NCODA map
  $EXECrtofs/rtofs_ncoda_map $ddtg > pout5
  err=$?; export err ; err_chk
  echo " error from rtofs_ncoda_map=",$err
  mv gmeta $DATA/logs/shem_var/shem_var.$ddtg.gmeta
fi

cat pout* > $DATA/logs/shem_var/shem_var.$ddtg.out
cat $DATA/logs/shem_var/shem_var.$ddtg.out >> $DATA/$pgmout

# 5. Copy data back to COMOUT/ncoda

echo timecheck RTOFS_GLO_POLAR start put at $(date)
cd $DATA
mkdir -p $COMOUT/ncoda/nhem_var/restart
mkdir -p $COMOUT/ncoda/shem_var/restart
rm -f cmdfile.cpout
for d in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
  backymdh=$( $EXECrtofs/rtofs_dtg -d -$d ${PDY}00 )
  backymd=${backymdh:0:8}
  if compgen -G "$DATA/nhem_var/restart/*${backymd}*" > /dev/null
  then
     echo "cp -p -f $DATA/nhem_var/restart/*${backymd}*" $COMOUT/ncoda/nhem_var/restart >> cmdfile.cpout
  fi
  if compgen -G "$DATA/shem_var/restart/*${backymd}*" > /dev/null
  then
     echo "cp -p -f $DATA/shem_var/restart/*${backymd}*" $COMOUT/ncoda/shem_var/restart >> cmdfile.cpout
  fi
done

chmod +x cmdfile.cpout
mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpout
err=$? ; export err ; err_chk
date

mkdir -p $COMOUT/ncoda/logs/nhem_var
mkdir -p $COMOUT/ncoda/logs/shem_var
cp -p -f $DATA/logs/nhem_var/*.$ddtg.* $COMOUT/ncoda/logs/nhem_var
cp -p -f $DATA/logs/shem_var/*.$ddtg.* $COMOUT/ncoda/logs/shem_var

echo timecheck RTOFS_GLO_POLAR finish put at $(date)

date

#################################################
msg="THE RTOFS_GLO_NCODA_POLAR_VAR JOB HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"

################## END OF SCRIPT #######################

