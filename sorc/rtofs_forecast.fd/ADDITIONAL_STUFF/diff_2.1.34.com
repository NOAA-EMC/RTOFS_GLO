#
set echo
#
foreach f ( READ* Makefile Make.com *.h *.f *.F )
  echo "*****     *****     *****     *****     *****     *****     *****"
  diff -ibw $f ../../ATLb2.00/src_2.1.34_22_one
end
