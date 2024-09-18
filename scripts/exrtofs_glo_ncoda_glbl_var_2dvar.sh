#!/bin/sh
set -xa
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_ncoda_glbl_var_2dvar.sh                    #
# Script description:                                                         #
#                                                                             #
# Author:        Dan Iredell     Org: NP23         Date: 2020-07-30           #
#                                                                             #
# Abstract: 
#   this script runs global 2DVAR sss and sst analyses for HAFS
#   HYCOM forecasts are read from the NCODA 3DVAR restart directory
#      specified by the dsomrff path in odsetnl
#   the ocean observations are read from a special run of ncoda_qc
#      that is executed in real time using a 6-hr update cycle
#  
#   the 2DVAR analyses are accessed by HAFS
#      requires the oanl namelist variable hafs set .true.
#  
#   the analysis DTG is input as a command line argument
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             #
# Script history log:                                                         #
# 2024-07-30  Dan Iredell                                                     #
#                                                                             #
###############################################################################

export PS4='$SECONDS + '

msg="RTOFS_GLO_NCODA_GLBL_VAR_2dvar JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

cd $DATA

# --------------------------------------------------------------------------- #

# 1. Populate DATA with glbl_var files from COMprev/2dvar
echo timecheck RTOFS_GLO_GLBL start get at $(date)

export previous=$(${NDATE} -6 ${PDY}${cyc})
export prevday=$(echo $previous | cut -c1-8)
export prevcyc=$(echo $previous | cut -c9-10)
export COMprev=$COMROOTrtofs/$RUN.$prevday/2dvar

mkdir -p $DATA/restart
mkdir -p $DATA/work
rm -f cmdfile.cpin
if compgen -G "$COMprev/glbl_var$prevcyc/restart/*" > /dev/null
then
  for gv in $(ls $COMprev/glbl_var$prevcyc/restart/); do
    echo "cp -p -f $COMprev/glbl_var$prevcyc/restart/$gv $DATA/restart" >> cmdfile.cpin
  done
  chmod +x cmdfile.cpin
  mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpin
  err=$? ; export err ; err_chk
  date
else
  echo "WARNING - Cold starting $jobid"
  echo "WARNING - Job $jobid is cold-starting"                                  > $DATA/glbl.coldstart.email
  echo "This is an abnormal event."                                            >> $DATA/glbl.coldstart.email
  echo "The following directory is empty:"                                     >> $DATA/glbl.coldstart.email
  echo "$COMprev/glbl_var$prevcyc/restart"                                     >> $DATA/glbl.coldstart.email
  echo "This job will continue to run as a cold-start."                        >> $DATA/glbl.coldstart.email
  cat $DATA/glbl.coldstart.email | mail.py -s "WARNING - Job $job cold started"
fi

# 2. build namelists and .

ddtg=${PDY}${cyc}      # ncoda hycom_var files do not exist
#ddtg=${PDYm1}${cyc}

log_dir=$DATA/logs
mkdir -p $log_dir

ln -sf $COMIN/2dvar/ocnqc$cyc $DATA/ocnqc

ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.a ${DATA}/regional.grid.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.b ${DATA}/regional.grid.b
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.a ${DATA}/regional.depth.a
ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.b ${DATA}/regional.depth.b
#ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.iso.sigma.a      iso.sigma.a
#ln -f -s ${FIXrtofs}/${RUN}_${modID}.${inputgrid}.iso.sigma.b      iso.sigma.b

echo timecheck RTOFS_GLO_GLBL start setup at $(date)

rm -f odsetnl
rm -f ogridnl
rm -f oanl

cat << eof1 > odsetnl
 &dsetnl
  dsoclim = '$FIXrtofs/codaclim'
  dsogdem = '$FIXrtofs/gdem'
  dsomrff = '$COMROOTrtofs'
  dsomfix = '$DATA'
  dsorff  = '$DATA/restart'
  dsoudat = '$DATA/ocnqc'
  dsowork = '$DATA/work'
 &end
eof1

cat << eof2 > ogridnl
 &gridnl
  delx(1) = 8896.78809,
  dely(1) = 8895.59277,
  kko     = 1,
  m       = 4500,
  n       = 3298,
  nnest   = 1,
  nproj   = -1,
 &end
eof2

cat << eof3 > oanl
 &oanl
  cluster(1)  = 1.,
  cluster(2)  = 1.,
  cluster(3)  = 1.,
  cluster(6)  = 1.,
  debug(4)    = .true.,
  dh_flow     = 'SST',
  fgat_rec    =  8, 16, 17, 13, 14, 1,
  global      = .true.,
  hafs        = .true.,
  ice_asm     = .false.,
  mask_opt    = '2D',
  n_it        = 30,
  nsr_inf     = 1.5,
  over(1)     = 1.,
  over(2)     = 2.,
  over(3)     = 1.,
  over(4)     = 1.,
  over(5)     = 1.,
  over(6)     = 2.,
  rscl(1)     = 0.6,
  rscl(2)     = 1.,
  rscl(3)     = 1.,
  rscl(6)     = 1.,
  rscl_cap    = 150.,
  ssh_asm     = .false.,
  sss_asm     = .true.,
  sss_time    = 'synt',
  sst_asm     = .true.,
  sst_time    = 'synt',
  upd_cyc     = 6,
  vscl(1)     = 16.,
  vscl(2)     = 4.,
  vscl(3)     = 4.,
  vscl(6)     = 4.,
  z_lvl       = 0., 99*0.,
 &end
eof3

# 3 run global var (NCODA 2D)

#   execute ncoda variational programs
#NCODA setup
$EXECrtofs/rtofs_ncoda_setup 2D ncoda ogridnl $ddtg > pout1
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_setup=",$err

#NCODA prep
echo timecheck RTOFS_GLO_GLBL start prep at $(date)
mpiexec -n 24 --cpu-bind core $EXECrtofs/rtofs_ncoda_prep 2D ncoda ogridnl $ddtg > pout2
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_prep=",$err

#NCODA var
echo timecheck RTOFS_GLO_GLBL start ncoda2d at $(date)
mpiexec -n $NPROCS --cpu-bind core $EXECrtofs/rtofs_ncoda 2D ncoda ogridnl $ddtg > pout3
err=$?; export err ; err_chk
echo " error from rtofs_ncoda=",$err

#NCODA post
echo timecheck RTOFS_GLO_GLBL start post at $(date)
mpiexec -n $NPROCS --cpu-bind core $EXECrtofs/rtofs_ncoda_post 2D ncoda ogridnl $ddtg > pout4
err=$?; export err ; err_chk
echo " error from rtofs_ncoda_post=",$err

#   rename local files
[[ -f fort.40 ]] && mv fort.40 $log_dir/glbl_var.$ddtg.sus
[[ -f fort.67 ]] && mv fort.67 $log_dir/glbl_var.$ddtg.obs
[[ -f fort.68 ]] && mv fort.68 $log_dir/glbl_var.$ddtg.grd

#
#   combine work files
cat pout* > $log_dir/glbl_var.$ddtg.out
cat $log_dir/glbl_var.$ddtg.out >> $pgmout

# 4. Copy last 15 days of data back to COMOUTprev
echo timecheck RTOFS_GLO_GLBL start put at $(date)

mkdir -p $COMOUT/2dvar/glbl_var$cyc/restart
rm -f cmdfile.cpout
for d in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
  backymdh=$( $EXECrtofs/rtofs_dtg -d -$d ${PDY}00 )
  backymd=${backymdh:0:8}
  if compgen -G "$DATA/restart/*${backymd}*" > /dev/null
  then
     for gv in $(ls $DATA/restart/*${backymd}*); do
         echo "cp -p -f $gv $COMOUT/2dvar/glbl_var$cyc/restart" >> cmdfile.cpout
     done
#     echo "cp -p -f $DATA/restart/*${backymd}* $COMOUT/2dvar/glbl_var$cyc/restart" >> cmdfile.cpout
  fi
done

chmod +x cmdfile.cpout
mpiexec -np $NPROCS --cpu-bind verbose,core cfp ./cmdfile.cpout
err=$? ; export err ; err_chk
date

mkdir -p $COMOUT/2dvar/logs$cyc/glbl_var
cp -p -f $DATA/logs/*.$ddtg.* $COMOUT/2dvar/logs$cyc/glbl_var
echo timecheck RTOFS_GLO_GLBL finish put at $(date)

#################################################
msg="THE RTOFS_GLO_NCODA_GLBL_VAR JOB_2dvar HAS ENDED NORMALLY on $(hostname) at $(date)"
postmsg "$msg"

################## END OF SCRIPT #######################

