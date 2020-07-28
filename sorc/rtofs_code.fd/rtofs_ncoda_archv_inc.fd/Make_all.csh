#!/bin/csh
#
#set echo
#
# --- Usage:  ./Make_all.csh >& Make_all.log
#
# --- make all archive executables (except netCDF)
#
# --- set ARCH to the correct value for this machine.
#
#
#
#
# --- archive modifying programs
#
foreach m ( ncoda_archv_inc )
  make ${m} ARCH=intelIF >&! Make_${m}.log
  if ($status) then
    echo "Make failed:" ${m} " - see Make_${m}.log"
#   cat Make_${m}.log
  else
    echo "Make worked:" ${m}
  endif
  if (-e /usr/bin/ldedit) then
#   try to set medium pages on POWER5+
    /usr/bin/ldedit -bdatapsize=64K -bstackpsize=64K ${m}
  endif
  mv $m ../../exec/
end
