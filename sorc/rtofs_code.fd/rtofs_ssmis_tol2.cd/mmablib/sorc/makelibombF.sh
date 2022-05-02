#!/bin/sh
###############################################################
#
#   AUTHOR:    Vuong - W/NP11
#
#   DATE:      02/07/2000
#
#   PURPOSE:   This script uses the make utility to update the libomb 
#              archive library.
#              It first reads a list of the source files in the library and
#              then generates a makefile used to update the archive
#              library.  The make command is then executed,
#              where the archive library name and 
#              compilation flags are passed to the makefile through 
#              environment variables.
#
#   REMARKS:   Only source files that have been modified since the last
#              library update are recompiled and replaced in the object
#              archive libraries.  The make utility determines this
#              from the file modification times.
#
#              New source files are also compiled and added to the object 
#              archive libraries.
#
# Maintenance: Robert Grumbine
#
###############################################################


cd sorc

if [ -f mapxy.c ]
then
  mv mapxy.c   mapxy.c2
fi

#
#     Generate a list of object files that corresponds to the
#     list of Fortran ( .f ) files in the current directory
#
for i in `ls *.f`
do
  fobj=`basename $i .f`
  FOBJS="$FOBJS ${fobj}.o"
done

#
#     Remove make file, if it exists.  May need a new make file
#     with an updated object file list.
#
if [ -f make.libomb ] 
then
  rm -f make.libomb
fi
#
#     Generate a new make file ( make.libomb), with the updated object list,
#     from this HERE file.
#
cat > make.libomb << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( ${FOBJS} )

.f.a:
	echo using Fortran compiler: `which ftn`
	ftn -c \$(FFLAGS) \$<
	ar -ruv  \$@ \$*.o
	rm -f \$*.o

EOF
#
#     Update 4-byte version of libombf_4.a
#
export LIB="libombf_4.a"
#export FFLAGS=" -O3 -qnosave"
export FFLAGS=" -O3 "
make -f make.libomb
mv $LIB ..

rm -f make.libomb
rm -f *.o

if [ -f mapxy.c2 ]
then
  mv mapxy.c2   mapxy.c
fi
