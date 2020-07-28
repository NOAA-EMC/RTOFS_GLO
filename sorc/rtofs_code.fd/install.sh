#!/bin/sh
set -x

mkdir -p ../../exec

module purge
module use ../../modulefiles 
module load build_rtofs_codes.module

make > rtofsglo.compile.log 2>&1
