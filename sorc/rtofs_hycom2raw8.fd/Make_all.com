#!/bin/csh
#
# --- create HYCOM related executables.
#
#
set echo
## cd ~/hycom/GLBb0.08/forcing-1.1.0/hycom2raw8
setenv OS `uname`
if ($OS == "Linux") then
  if (`uname -m` == "x86_64") then
        setenv OS Linux64
  endif
  setenv OS LinuxICE
endif
#
# --- the following are extracted from hycom/ALL/config/*_setup
#
switch ($OS)
case 'LinuxICE':
#       compile for SGI Altix ICE, Intel compiler
        setenv FC       "ifort"
        setenv FFLAGS   "-g -O3 -fp-model source -convert big_endian  -assume byterecl -mcmodel=medium -fPIC"
        setenv FLIBS    "-shared-intel"
        setenv CC       "icc"
        setenv CFLAGS   "-O -mcmodel=medium -fPIC"
        breaksw
endsw
# clean
rm -f parse.o hycom_endian_io.o hycom2raw8_linuxICE hycom2raw8

#compile
$FC $FFLAGS -c hycom_endian_io.F
$CC $CFLAGS -c parse.c

#ifort -g -O3 -fp-model source -convert big_endian -assume byterecl -mcmodel=medium -fPIC hycom2raw8.F -shared-intel hycom_endian_io.o parse.o -o hycom2raw8_LinuxICE
#ln -s hycom2raw8_LinuxICE hycom2raw8

foreach f (hycom2raw8)

$FC $FFLAGS ${f}.F $FLIBS hycom_endian_io.o parse.o -o ${f}_${OS}
ln -s ${f}_${OS} $f
