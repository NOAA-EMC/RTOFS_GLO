#!/bin/sh
set -x
#
#########################################################################
# Usage: rtofs_atmforcing_correct.sh                                    #
#                                                                       #
# Description: Correct GFS near-surface air temperature                 #
#              Originally, air temperature is in the file               #
#              forcing.airtmp.[ab]It is moved to forcing.airtm1.[ab]    #
#              GFS surface temperature is read from forcing.surtmp.[    #
#              ab] and air temperature is corrected where surface       #
#              temperature is below freezing. The result is written to  #
#              forcing.airtmp.[ab]                                      #
#                                                                       #
# History:                                                              #
#    02-02-2007 Ilya Rivin                                              #
#########################################################################

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)

# It is assumed that forcing.imput files
# forcing.airtmp.[ab] forcing.surtmp.[ab] and regional.mask.[ab]
# are already in the working directory $DATA

cd $DATA

# prepare input file 
blines=$(cat ${DATA}/forcing.surtmp.b | wc -l)
num_frames=$(grep 'surtmp: date,span,range' ${DATA}/forcing.surtmp.b | wc -l)
ihead=$(expr ${blines} \- ${num_frames})
sname=$(grep span ${DATA}/forcing.surtmp.b |head -1 |cut -c1-8)
aname=$(grep span ${DATA}/forcing.airtmp.b |head -1 |cut -c1-8)

cat > atmforcing_correct.in <<EOF
$ihead
$num_frames
$aname
$sname
EOF

export pgm="${RUN}_atmforcing_correct"
. prep_step
startmsg
$EXECrtofs/${RUN}_atmforcing_correct < atmforcing_correct.in >>$pgmout 2>errfile
err=$?
if [ $err -ne 0 ]
then
  $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job" "return code $err" $err
fi
echo " error from ${RUN}_atmforcing_correct=",$err

mv ${DATA}/forcing.airtmp.a ${DATA}/forcing.airtm1.a 
mv ${DATA}/forcing.airtmp.b ${DATA}/forcing.airtm1.b 
mv ${DATA}/forcing.airtem.a ${DATA}/forcing.airtmp.a 
mv ${DATA}/forcing.airtem.b ${DATA}/forcing.airtmp.b 
# dbgz: Ilya
#cp ${DATA}/forcing.airtm1.a ${DATA}/forcing.airtmp.a 
#cp ${DATA}/forcing.airtm1.b ${DATA}/forcing.airtmp.b

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)
