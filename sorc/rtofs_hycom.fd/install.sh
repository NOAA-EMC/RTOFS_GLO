#!/bin/sh
set -x

mkdir -p ../../exec

module purge
module use ../../modulefiles 
module load build_rtofs_hycom.module

cd src_2.2.99DHMTi-dist2B_relo_cice_v4.0e
csh clean_ice.csh > ../rtofsglo.compile.hycom.log 2>&1
csh comp_ice.csh >> ../rtofsglo.compile.hycom.log 2>&1

cp hycom_cice ../../../exec

