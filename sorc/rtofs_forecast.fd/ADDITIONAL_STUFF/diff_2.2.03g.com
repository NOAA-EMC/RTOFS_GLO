#
set echo
#
foreach f ( READ* Makefile Make.com *.h *.f *.F )
  echo "*****     *****     *****     *****     *****     *****     *****"
  diff -ibw $f ../../GLBt0.72/src_2.2.03g_32_mpi
end
#
foreach n ( blkdat geopar )
  echo "*****     *****     *****     *****     *****     *****     *****"
  diff -ibw ${n}.F ../../GLBt0.72/src_2.2.03g_32_mpi/${n}.f
end
