#
set echo
#
foreach f ( READ* Makefile Make.com *.h *.f *.F )
  echo "*****     *****     *****     *****     *****     *****     *****"
# diff -ibw $f ../../GOMd0.08/src_2.2.06q_20_mpi
  diff -ibw $f ../../MSBc0.02/src_2.2.06q_20_mpi
end
