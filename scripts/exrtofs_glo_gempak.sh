#!/bin/sh

###############################################################################
######################  UNIX Script Documentation Block  ######################
#                                                                             #
# Script name:         exrtofs_glo_post_gempak.sh                             #
# Script description:  generate the whole set of GEMPAK files (hours          #
#        00, 01, 02, ..., 72) for one region                                  #
#                                                                             #
# Authors: Wojciech Cencek Org: PMB      Date: 2015-07-28                     #
#                                                                             #
# Abstract: This is the GEMPAK script for RTOFS_GLO                           #
#                                                                             #
# Imported variables:                                                         #
#                                                                             #
# instr (from ecFlow):  alaska, bering, or west_atl                           #
# outstr (from ecFlow): alaska, bering, or watl                               #
#                                                                             #
###############################################################################

set -xa

export PS4='$SECONDS + '

cd $DATA

msg="$job JOB has begun on `hostname` at `date`"
postmsg "$msg"

yymmdd=${PDY:2}

cpfs $GEMPAKrtofs/g2varswmo2_Grtofs.tbl .
cpfs $GEMPAKrtofs/gempak.sh .

echo "Getting GRTOFS grib 2 files"
# JY only creates up to n024 in this upgrade, n048 is not created.
#for hr in n048 f024 f048 f072 ; do
for hr in n024 f024 f048 f072 ; do
file=$COMIN/rtofs_glo.t00z.${hr}_${instr}_std.grb2
[  -s $file ] || ( echo "FATAL ERROR: $file not found"; export err=1; err_chk )
ln -s $file .
done


#Convert Grib 2 files into GEMPAK grids for each time-step from f001 to f072
> mpirun.log
for last in 24 48 72 ; do
> mpirun.dat
    for stepnum in `seq -w $((last-23)) $last` # 01..24, 25..48, 49..72
    do
	echo "./gempak.sh $last $stepnum $instr $outstr $COMOUTgempak > $instr.$stepnum.log" >> mpirun.dat
    done
    chmod 775 mpirun.dat
    mpirun cfp mpirun.dat >> mpirun.log
    export err=$?; err_chk
done
cat mpirun.log

################################################################################
#... The rest is just to generate the f000 GEMPAK file with correct timestamp...
################################################################################


gemfile=grtofs_${outstr}_${PDY}00f000

##Get 0 hour nowcast from 48 hour time-step from rtofs_glo.t00z.n048_${instr}_std.grb2 file
# n048 not exist, change to n024, is it valid? - JY
# JY $WGRIB2 rtofs_glo.t00z.n048_${instr}_std.grb2 -for 162:166 -grib grtofs_${instr}_${PDY}00f000.grb2
if [ ${instr} = 'alaska' -o ${instr} = 'bering' ]; then
$WGRIB2 rtofs_glo.t00z.n024_${instr}_std.grb2 -for 208:212 -grib grtofs_${instr}_${PDY}00f000.grb2
else
$WGRIB2 rtofs_glo.t00z.n024_${instr}_std.grb2 -for 162:166 -grib grtofs_${instr}_${PDY}00f000.grb2
fi


echo "Converting F000 grib 2 file to Temporary grid"

nagrib2 <<GEM_nagrib2
GBFILE   = grtofs_${instr}_${PDY}00f000.grb2
 GDOUTF   = ${gemfile}.temp
 PROJ     =
 GRDAREA  = grid
 KXKY     =
 MAXGRD   = 200
 CPYFIL   = gds
 GAREA    = grid
 OUTPUT   = T
 G2TBLS   = g2varswmo2_Grtofs.tbl;g2varswmo2_Grtofs.tbl
 G2DIAG   = all
 OVERWR   = yes
 PDSEXT   = NO
l
r
e
GEM_nagrib2
gpend

echo "Create empty F000 grid to copy grids to with fixed time-stamp"
gdcfil <<GEM_gdcfil
GDOUTF   = $gemfile
 PROJ     =  
 GRDAREA  = grid
 KXKY     =  
 MAXGRD   = 200
 CPYFIL   = grtofs_${outstr}_${PDY}00f024
 ANLYSS   = 4/2;2;2;2
l
r
e
GEM_gdcfil
gpend

for var in WTMP SALTY UOGRD VOGRD SSHG ; do
    echo "Copy $var variable to F000 grid with fixed time-stamp"
    gddiag <<GEM_gddiag
 GDFILE   = ${gemfile}.temp 
 GDOUTF   = $gemfile
 GFUNC    = $var
 GDATTIM  = LAST
 GLEVEL   = 0
 GVCORD   = dpth
 GRDNAM   = ${var}^${yymmdd}/0000F000
 GRDTYP   = S
 GPACK    =
 GRDHDR   =
 PROJ     =
 GRDAREA  = 
 KXKY     =
 MAXGRD   = 200
 CPYFIL   = grtofs_${outstr}_${PDY}00f024  
 ANLYSS   = 4/2;2;2;2
l
r

e
GEM_gddiag
    gpend
done

[  -s $gemfile ] || ( echo "FATAL ERROR: $gemfile was not generated"; export err=1; err_chk )
cpfs $gemfile  $COMOUTgempak
if [ $SENDDBN = YES ] ; then
    ${DBNROOT}/bin/dbn_alert MODEL $DBN_ALERT_TYPE $job $COMOUTgempak/$gemfile
fi

#################################################
msg='THE $job JOB HAS ENDED NORMALLY.'
postmsg "$msg"
################## END OF SCRIPT #######################
