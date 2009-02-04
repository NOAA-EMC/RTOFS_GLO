#!/bin/sh
#########################################################################
# Usage: rtofs_seasforce3.sh                                            #
#                                                                       #
# Description: This script copies the GFS/GDAS flux files and           #
#              interpolate them to generates the seashore forcing files #
#              for the Ocean Forecast Model                             #
#                                                                       #
# History:                                                              #
#    06-16-2005  Dan Iredell                                            #
#    06-01-2006  Ilya Rivin: add sea level pressure forcing             #
#########################################################################
# expected macros exported to this script:
#   DATA - work dir
#   cyc - cycle (not used now)
#   PDY - present day, also PDYmN thru PDYpNN to cover input dates
#   mode - anal (nowcast) or fcst (forecast)
#   XXXXrtofs - root of rtofs directory for fix, exec, etc.
#   FLUXDIR - location of flux files. default is /com/gfs/prod/....
#   utilscript
#   utilexec
#####
# future: use $cyc to grab latest flux files (not always 00z cycle)
set -x

echo "*** Started script $0"

if [ $# -lt 3 ] ; then 
  echo USAGE:  seasforce3.sh start-date end-date interval
  exit 2
fi

sh ${utilscript}/setup.sh

sdate=$1
edate=$2
intvl=$3
diffhours=`${utilexec}/nhour ${edate} ${sdate}`
let diffdays=diffhours/24
let diffdays=diffdays-1
sdate=`${utilexec}/ndate -3 $sdate`
edate=`${utilexec}/ndate 3 $edate`

# to incorporate sea level pressure
sea_lev_pres=PRMSL
atmgds=

tf[1]='gdas1.t18z.sfluxgrbf03'
tf[2]='gdas1.t00z.sfluxgrbf00'
# gets reset to following after 1st pass: tf[2]='gdas1.t18z.sfluxgrbf06'
tf[3]='gdas1.t00z.sfluxgrbf03'
tf[4]='gdas1.t00z.sfluxgrbf06'
tf[5]='gdas1.t06z.sfluxgrbf03'
tf[6]='gdas1.t06z.sfluxgrbf06'
tf[7]='gdas1.t12z.sfluxgrbf03'
tf[8]='gdas1.t12z.sfluxgrbf06'
tf[9]='gdas1.t18z.sfluxgrbf03'

# for sea surface pressure
pf[1]='gdas1.t18z.pgrbf03'
pf[2]='gdas1.t00z.pgrbf00'
# gets reset to following after first pass: pf[2]='gdas1.t18z.pgrbf06'
pf[3]='gdas1.t00z.pgrbf03'
pf[4]='gdas1.t00z.pgrbf06'
pf[5]='gdas1.t06z.pgrbf03'
pf[6]='gdas1.t06z.pgrbf06'
pf[7]='gdas1.t12z.pgrbf03'
pf[8]='gdas1.t12z.pgrbf06'
pf[9]='gdas1.t18z.pgrbf03'

atdirroot=${FLUXDIR:-/com/gfs/prod/gdas.}
atdir[1]=$atdirroot$PDYm2
atdir[2]=$atdirroot$PDYm1
atdir[3]=$atdirroot$PDY

              echo atdir of 1 = ${atdir[1]}
              echo atdir of 2 = ${atdir[2]}
              echo atdir of 3 = ${atdir[3]}
ii=0
while [ $ii -lt $diffdays ]
do
   let ii=ii+1
   let jj=ii+3
   let hh=ii*24
   bb=\$PDYp$ii
   cc=`eval echo $bb`
   atdir[${jj}]=$atdirroot$cc
              echo atdir of $jj = ${atdir[${jj}]}
done

ftdirroot=${FLUXDIR:-/com/gfs/prod/gfs.}
ftdir[1]=$ftdirroot$PDYm1
ftdir[2]=$ftdirroot$PDY

fhr=00
idate=$sdate
NTIME=0

# Copy the surface flux files from GFS/GDAS

rm -rf t.dat
icyc=1     # counts the iterations
while [ $idate -le $edate ]
do
   flxpdy=`echo $idate |cut -c1-8`

# use cycle and iteration for determining flx directory and filename
# icyc - iteration count of do loop
# dcyc - day number based on iteration / 8 (8 is number of flx files produced daily)
# rcyc - count per day (cycles 1 thru 8)

   dcyc=`expr $icyc / 8`
   let dcyc=dcyc+1
   rcyc=`expr $icyc % 8`

   if [ $rcyc -eq 0 ]; then 
     rcyc=8
     let dcyc=dcyc-1
   fi

   if [ $mode = "anal" ]
   then
       if [[ $rcyc -le 2 && $icyc -ne 2 ]] ; then
         index=$dcyc
         COMFLX=${atdir[${index}]}
       else 
         let index=dcyc+1
         COMFLX=${atdir[${index}]}
       fi

       if [ $dcyc -ge 2 ] ; then
         tf[2]='gdas1.t18z.sfluxgrbf06'
         pf[2]='gdas1.t18z.pgrbf06'
       fi

       hh=`echo ${tf[${rcyc}]} |cut -c21-22`
       tt=`echo ${tf[${rcyc}]} |cut -c8-9`

      # re-calculate flxpdy here:
       flxpdy=`echo $COMFLX |awk -F"." '{print $2}'`

       flxfile=flxf${hh}.${flxpdy}t${tt}z
       cp -p $COMFLX/${tf[${rcyc}]}  $flxfile
       ${utilexec}/grbindex $flxfile $flxfile.idx
       
       #add atmospheric pressure
       # this test assumes that atm grid resolution is constant during the run
       if [ -z "$atmgds" ]
       then
          export GRBFILE=$flxfile
          export IDXFILE=$flxfile.idx
          $EXECrtofs/rtofs_getkpds >>$pgmout 2>errfile
          export err=$?; err_chk
          atmgds='255 '`cat kpds.dat`
         export err=$?; err_chk
       fi
       pgrbfile=$COMFLX/${pf[${rcyc}]}
       rec_number=`${utilexec}/wgrib -v ${pgrbfile} | grep ${sea_lev_pres} | cut -c1-3`
       ${utilexec}/wgrib -d ${rec_number} -grib ${pgrbfile} -o dump.grb 
       ${utilexec}/copygb -g"$atmgds" -x -a -i0 dump.grb $flxfile 
       rm $flxfile.idx

       ${utilexec}/grbindex $flxfile $flxfile.idx

       echo $idate $COMFLX/${tf[${rcyc}]}
       echo $idate $flxfile >>t.dat 

   else
       case $icyc in
       1)
         index=$dcyc
         COMFLX=${ftdir[${index}]}
         ftt=18
         let flxhh=icyc*$intvl
         ;;
       2)
         COMFLX=${ftdir[2]}
         ftt=00
         flxhh=0
         ;;
       *) 
         COMFLX=${ftdir[2]}
         ftt=00
         let index=icyc-2
         let flxhh=index*$intvl
         ;;
       esac
       if [ $flxhh -lt 10 ]; then flxhh=0$flxhh; fi
       flxfile=flxf${flxhh}.${flxpdy}t${ftt}z
       cp -p $COMFLX/gfs.t${ftt}z.sfluxgrbf${flxhh}  $flxfile
       ${utilexec}/grbindex $flxfile $flxfile.idx

       #add atmospheric pressure
       # this test assumes that atm grid resolution is constant during the run
       if [ -z "$atmgds" ]
       then
          export GRBFILE=$flxfile
          export IDXFILE=$flxfile.idx
          $EXECrtofs/rtofs_getkpds >>$pgmout 2>errfile
          export err=$?; err_chk
          atmgds='255 '`cat kpds.dat`
        fi
       pgrbfile=$COMFLX/gfs.t${ftt}z.pgrbf${flxhh}
       rec_number=`${utilexec}/wgrib -v ${pgrbfile} | grep ${sea_lev_pres} | cut -c1-3`
       ${utilexec}/wgrib -d ${rec_number} -grib ${pgrbfile} -o dump.grb 
       ${utilexec}/copygb -g"$atmgds" -x -a -i0 dump.grb $flxfile 
       rm $flxfile.idx
       ${utilexec}/grbindex $flxfile $flxfile.idx

       echo $idate $COMFLX/gfs.t${ftt}z.sfluxgrbf${flxhh}
       echo $idate $flxfile >>t.dat 
   fi
    
   let icyc=icyc+1
   NTIME=`expr $NTIME + 1`
   idate=`${utilexec}/ndate $intvl $idate` 
   fhr=`expr $fhr + $intvl`
   if [ $fhr -lt 10 ]; then fhr=0$fhr; fi
done

#################################################################
# Interpolate to RTOFS grid
#################################################################
#- UNIT  7  - FILE intp_pars.dat, COTROL RUN PARAMETRS
#- UNIT  8  - FILE regional.grid.b, DESCRIPTOR FOR HYCOM GRID 
#-            (READ IN mod_geom.f90)
#- UNIT  9  - FILE regional.grid.a, HYCOM GRID 
#-            (READ IN mod_geom.f90)
#- UNIT 33  - FILE listflx.dat, LIST OF DATES AND MRF FLUS FILES TO 
#-            BE USED IN INTERPOLATION.
#- UNIT 59  - FILE regional.depth.a, HYCOM BATHIMETRY 
#-            (READ IN mod_geom.f90)
#- UNIT 61  - FILE regional.mask.a, HYCOM mask
#-            (READ IN mod_geom.f90)
#- UNIT 81  - MRF GRIBBED FLUXES FILES WITH THE NAMES FROM THE LIST 
#-            SPECIFIED IN listflx.dat.
#- UNIT 82  - THE SAME      
#################################################################
pgm=rtofs_gfs2rtofs
. prep_step

msg=" `date`  -- $pgm for $adate started "
postmsg "$jlogfile" "$msg"

echo ${NTIME} > listflx.dat 
cat t.dat >> listflx.dat 
rm -f t.dat

export XLFRTEOPTS="unit_vars=yes:buffering=disable_all"
export XLFUNIT_7=intp_pars.dat
export XLFUNIT_8=regional.grid.b
export XLFUNIT_9=regional.grid.a
export XLFUNIT_33=listflx.dat
export XLFUNIT_59=regional.depth.a
export XLFUNIT_61=regional.mask.a

dbx $EXECrtofs/rtofs_gfs2rtofs <<EOF
run
where
quit
EOF

#$EXECrtofs/rtofs_gfs2rtofs >>$pgmout 2>errfile
#export err=$?; err_chk

# End of genrating the forcing files

echo "*** Finished script $0"
