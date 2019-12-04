#!/bin/bash
######################  UNIX Script Documentation Block  ######################
#                                                                             #
# Script name:         gempak.sh                                              #
# Script description: extract records corresponding to hour $stepnum          #
#        from RTOFS wgrib2 file with forecast up to hour $last                #
#        and convert them to GEMPAK                                           #
#        The GEMPAK files are alerted and copied to $gdir                     #
#                                                                             #
# Authors: Wojciech Cencek Org: PMB      Date: 2015-07-28                     #
#                                                                             #
# Abstract: This is the GEMPAK script for RTOFS_GLO                           #
#                                                                             #
# Imported variables:                                                         #
#                                                                             #
# instr (from ecFlow): input file name string (alaska, bering, west_atl)      #
# outstr (from ecFlow): output file name string (alaska, bering, watl)        #
# PDY                                                                         #
# WGRIB2                                                                      #
# job                                                                         #
###############################################################################
#
set -x
#
last=$1
stepnum=$2
instr=$3
outstr=$4
gdir=$5
#
tmpdir=$instr.$stepnum
mkdir -p $tmpdir
cd $tmpdir
#
num=$(( 10#${stepnum}+24-last )) # can be 1,2,...,24
firstrec=$(( 10#${num}*7 - 6 ))  # can be 1, 8,15,...,162
lastrec=$(( 10#${num}*7 - 2 ))       # can be 5,12,19,...,166
stamp="${firstrec}:${lastrec}"
step=f0${stepnum}
echo "Splitting grib2 files into time-steps (5 records in each: WTMP, SALTY, UOGRD, VOGRD, SSHG) using WGRIB2"
$WGRIB2 ../rtofs_glo.t00z.f0${last}_${instr}_std.grb2 -for ${stamp} -grib grtofs_${instr}_${PDY}00${step}.grb2
echo "Converting grib2 files into GEMPAK grids for each time-step"
gemfile=grtofs_${outstr}_${PDY}00${step}
nagrib2 <<GEM_nagrib2
 GBFILE   = grtofs_${instr}_${PDY}00${step}.grb2
 GDOUTF   = $gemfile
 PROJ     =  
 GRDAREA  = grid
 KXKY     =  
 MAXGRD   = 200
 CPYFIL   = gds
 GAREA    = grid
 OUTPUT   = T
 G2TBLS   = ../g2varswmo2_Grtofs.tbl;../g2varswmo2_Grtofs.tbl
 G2DIAG   = all
 OVERWR   = yes
 PDSEXT   = NO
l
r
e
GEM_nagrib2
gpend

echo
[  -s $gemfile ] || ( echo "FATAL ERROR: $gemfile was not generated"; exit 1 )
cpfs $gemfile  $gdir
if [ $SENDDBN = YES ] ; then
    ${DBNROOT}/bin/dbn_alert MODEL $DBN_ALERT_TYPE $job $gdir/$gemfile
fi
mv $gemfile ..


