#
set echo
#
foreach f ( READ* Makefile Make.com *.h *.f *.F )
  echo "*****     *****     *****     *****     *****     *****     *****"
# diff -ibw $f ../../ARCc0.72/src_2.2.05_32_cice
  diff -ibw $f ../../BERa0.08/src_2.2.05_28_cice
end
#
foreach f ( blkdat )
  echo "*****     *****     *****     *****     *****     *****     *****"
# diff -ibw ${f}.F ../../ARCc0.72/src_2.2.05_32_cice/${f}.f
  diff -ibw ${f}.F ../../BERa0.08/src_2.2.05_28_cice/${f}.f
end
