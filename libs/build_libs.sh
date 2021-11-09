#!/bin/sh 
set -x 

module purge
module use ../modulefiles
module load build_rtofs_libs_wcoss2.module
module list

 mkdir -p incmod
 rm -f incmod/rtofs*/*.mod
 rm -f librtofs*.a
 cd ./sorc/rtofs_mpi_mods
 rm -f makefile dimensions.h *.mod *.o
 cp makefile_serial makefile
 make ser
 rm -f makefile dimensions.h *.mod *.o
 cp makefile_mpi makefile
 make mpi
 rm -f makefile dimensions.h *.mod *.o
 cd ../rtofs_hycomiot
 make install
