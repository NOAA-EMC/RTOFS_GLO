#
#set echo
#
# --- Usage:  ./Make_all.csh >& Make_all.log
#
# --- make all meanstd executables
#
# --- set ARCH to the correct value for this machine.
#
#
#
#
# --- mnanstd programs
#
setenv ARCH intelIF
foreach m ( hycom_diff  )
  make ${m} ARCH=${ARCH} >&! Make_${m}.log
  if ($status) then
    echo "Make failed:" ${m} " - see Make_${m}.log"
  else
    echo "Make worked:" ${m}
  endif
  if (-e /usr/bin/ldedit) then
#   try to set medium pages on POWER5+ and POWER6
    /usr/bin/ldedit -bdatapsize=64K -bstackpsize=64K ${m}
  endif
  mv $m ../../exec
end
