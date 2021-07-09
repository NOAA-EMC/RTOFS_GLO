#!/bin/sh

. $MODULESHOME/init/sh

mkdir -p ../../exec

module purge
module use ../../modulefiles 
module load build_hera_rtofs_code.module
module list

make > rtofsglo.compile.code.log 2>&1

