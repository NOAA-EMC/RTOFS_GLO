#
set echo
#
foreach f ( READ* Makefile Make.com *.h *.f *.F )
  echo "*****     *****     *****     *****     *****     *****     *****"
# diff -ibw $f ../../GLBgx1v3/src_2.2.06n_32_mpi
  diff -ibw $f ../../GLBt0.72/src_2.2.03n_32_mpi
end
