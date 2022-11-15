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

if [ -f mapxy.f ]
then
  mv mapxy.f   mapxy.f2
fi


#
#     Generate a list of object files that corresponds to the
#     list of C ( .c ) files in the current directory
#
for i in `ls *.c`
do
  cobj=`basename $i .c`
  COBJS="$COBJS ${cobj}.o"
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

\$(LIB):	\$(LIB)( ${COBJS} )

#cc is an alias on wcoss2 systems

.c.a:
	echo using C compiler: `which cc`
	cc -c \$(CFLAGS) \$<
	ar -ruv \$@ \$*.o
	rm -f \$*.o

EOF
#
#     Update 4-byte version of libombc_4.a
#
export LIB="libombc_4.a"
export CFLAGS=" -O3 -Wall"
make -f make.libomb

mv $LIB ..

rm -f make.libomb
rm -f *.o

if [ -f mapxy.f2 ]
then
  mv mapxy.f2   mapxy.f
fi
