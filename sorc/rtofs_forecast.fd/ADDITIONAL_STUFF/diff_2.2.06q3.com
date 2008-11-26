#
set echo
#
foreach f ( READ* Makefile Make.com *.h *.f *.F *.c )
  echo "*****     *****     *****     *****     *****     *****     *****"
# diff -ibw $f ../../GLBgx1v3/src_2.2.06q3_32_mpi
# diff -ibw $f ../../GOMd0.08/src_2.2.06q3_20_mpi
  diff -ibw $f /u/home/smedstad/hycom/BERa0.08/src_2.2.06q3_28_cice
end
