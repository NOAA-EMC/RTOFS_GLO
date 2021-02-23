#!/bin/sh 
set -x 

module purge
module use ../modulefiles
module load build_rtofs_libs.module
module list

 mkdir -p incmod
 rm -f incmod/rtofs*/*.mod
 rm -f librtofs*.a
 cd ./sorc/rtofs_mpi_mods
 rm makefile dimensions.h *.mod *.o
 cp makefile_serial makefile
 make ser
 rm makefile dimensions.h *.mod *.o
 cp makefile_mpi makefile
 make mpi
 rm makefile dimensions.h *.mod *.o
 cd ../rtofs_hycomiot
 make install
