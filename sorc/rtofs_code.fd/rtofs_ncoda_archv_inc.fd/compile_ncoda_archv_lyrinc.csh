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
set echo
setenv DEXEC '../../exec'

echo "Compiling updated ncoda_archv_inc "
make clean
foreach m ( ncoda_archv_lyrinc )
  setenv flog Make_${m}.log
  touch $flog
  /bin/rm $flog
  make ${m} ARCH=intelIF >&! $flog
  if ($status) then
    echo "Make failed:" ${m} " - see $flog"
#   cat Make_${m}.log
  else
    echo " -------------- "
    echo "  Make worked:" ${m}
    echo "Moving executable ${m} ---> ${DEXEC}"
    /bin/mv ${m} ${DEXEC}/.
  endif
  if (-e /usr/bin/ldedit) then
#   try to set medium pages on POWER5+
    /usr/bin/ldedit -bdatapsize=64K -bstackpsize=64K ${m}
  endif
#  /bin/ln -sf ${DR}/${flog} ${WDR}/.
#  mv $m ../../exec/
end
