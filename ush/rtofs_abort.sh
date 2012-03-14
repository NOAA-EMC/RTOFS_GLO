#!/bin/sh
#########################################################################
# Usage: rtofs_abort.sh msg1 msg2 err                                   #
#                                                                       #
# Description: Abnormal exit                                            #
#                                                                       #
# History:                                                              #
#    07-01-2010  Ilya Rivin                                             #
#########################################################################

if [ $# -ne 3 ] 
then
  echo "USAGE: $0 msg1 msg2 err"
  echo 'UNCLEAN EXIT'
  exit -7
fi

msg1=$1
msg2=$2
err=$3

postmsg "$jlogfile" "$msg1"' '"$msg2"
err=2;
echo "$msg1" "$msg2"
export err;err_chk

exit -${err}      
