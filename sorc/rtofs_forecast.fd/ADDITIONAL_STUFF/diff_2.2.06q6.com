#
set echo
#
foreach f ( READ* Makefile Make.com *.h *.f *.F *.c )
  echo "*****     *****     *****     *****     *****     *****     *****"
# diff -ibw $f ../../GLBgx1v3/src_2.2.06q6_32_mpi
# diff -ibw $f ../../GOMd0.08/src_2.2.06q6_20_mpi
  diff -ibw $f ../../GLBt0.72/src_2.2.03q6_32_mpi
end
