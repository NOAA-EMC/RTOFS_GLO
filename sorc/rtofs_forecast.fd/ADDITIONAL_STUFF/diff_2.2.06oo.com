#
set echo
#
foreach f ( READ* Makefile Make.com *.h *.f *.F )
  echo "*****     *****     *****     *****     *****     *****     *****"
# diff -ibw $f ../../GLBgx1v3/src_2.2.06oo_32_mpi
  diff -ibw $f ../../GOMd0.08/src_2.2.06oo_20_mpi
end
