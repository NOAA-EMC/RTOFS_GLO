#
set echo
#
foreach f ( READ* Makefile Make.com *.h *.f *.F )
  echo "*****     *****     *****     *****     *****     *****     *****"
  diff -ibw $f ../../GLBgx1v3/src_2.2.06m_32_mpi
end
