#
set echo
#
foreach f ( READ* Makefile Make.com *.h *.f *.F *.c )
  echo "*****     *****     *****     *****     *****     *****     *****"
# diff -ibw $f ../../GLBt0.72/src_2.2.03r_32_mpi
  diff -ibw $f ../../GLBa0.08/src_2.2.03r_32_mpi
end
