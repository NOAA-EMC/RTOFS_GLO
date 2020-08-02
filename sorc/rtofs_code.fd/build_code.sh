#!/bin/sh

mkdir -p ../../exec

module purge
module use ../../modulefiles 
module load build_rtofs_code.module

make > rtofsglo.compile.code.log 2>&1

