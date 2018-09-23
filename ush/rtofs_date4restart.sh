#!/bin/sh
#########################################################################
# Usage: rtofs_date4restart.sh  <restart file>                          #
#                                                                       #
# Description: Date from restart                                        #
#                                                                       #
# History:                                                              #
#    07-01-2010  Ilya RIvin                                             #
#########################################################################

##>  dont !!!
##> echo "*** Started script $0 on hostname "`hostname`' at time '`date`

if [ $# -ne 1 ] 
then
  echo "USAGE: $0 <restart file>"
  exit 2
fi

basetime=1900123100   ## 1901010100
cdate=`awk '{  if (NR==2) { print $5 } }'  < $1 `
r1=`echo $cdate | cut -d. -f1 `
r2=${cdate##${r1}}
r2=`echo $r2 | cut -c2-`
r2=${r2:=0}
z=${#r2}
dv=1
while [ $z -gt 0 ]
do
  let dv=dv*10
  let z=z-1
done 
let r1=r1*24
let dv2=dv/2
let r2=r2*24+dv2
let r2=r2/dv
let r=r1+r2
normaldate=`${EXECutil}/ndate \` expr $r \`  ${basetime}`
echo $normaldate

##>  dont !!!
##> echo "*** Finished script $0 on hostname "`hostname`' at time '`date`

