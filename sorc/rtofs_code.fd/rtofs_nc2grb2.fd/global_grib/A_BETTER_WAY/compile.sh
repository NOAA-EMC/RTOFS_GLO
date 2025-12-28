#!/bin/bash

# --- 1. Environment Setup ---
module purge
module load craype-x86-rome libfabric/1.20.1 craype-network-ofi envvar/1.0
module load intel/19.1.3.304 PrgEnv-intel/8.3.3 craype/2.7.17
module load bacio/2.4.1 bufr/12.2.0 g2/3.4.5 sigio/2.3.2 sp/2.4.0 w3nco/2.4.1
module load jasper/2.0.25 libpng/1.6.37 zlib/1.2.11 netcdf/4.7.4 hdf5/1.10.6

# --- 2. Build Variables ---
FC="ftn"
FFLAGS="-O3 -convert big_endian"
SRC="rtofs_nc_to_grib2.f90"
CMD="rtofs_nc_to_grib2"

# --- 3. Path & Library Mapping ---
# G2 Paths
G2_INC_PATH="/apps/ops/prod/libs/intel/19.1.3.304/g2/3.4.5/include_4"
G2_LIB_DIR="/apps/ops/prod/libs/intel/19.1.3.304/g2/3.4.5/lib64"

# NetCDF Paths (Derived from your successful include output)
NC_INC_PATH="/apps/prod/hpc-stack/intel-19.1.3.304/netcdf/4.7.4/include"
NC_LIB_DIR="/apps/prod/hpc-stack/intel-19.1.3.304/netcdf/4.7.4/lib"

INC="-I${G2_INC_PATH} -I${NC_INC_PATH}"

# Constructing the library string manually to be safe
# Note: we add -L for both G2 and NetCDF directories
LIBS="-L${G2_LIB_DIR} -lg2_4 -L${NC_LIB_DIR} -lnetcdff -lnetcdf ${W3NCO_LIB4} ${BACIO_LIB4} -ljasper -lpng -lz"

echo "-----------------------------------------------"
echo "Include Path: $INC"
echo "G2 Lib Path:  -L${G2_LIB_DIR}"
echo "NC Lib Path:  -L${NC_LIB_DIR}"
echo "-----------------------------------------------"

rm -f $CMD *.o *.mod

# --- 4. Execution ---
$FC $FFLAGS $INC $SRC -o $CMD $LIBS

# --- 5. Status Check ---
if [ $? -eq 0 ]; then
    echo "-----------------------------------------------"
    echo "SUCCESS: $CMD built."
    chmod +x $CMD
    rm -f *.o *.mod
else
    echo "-----------------------------------------------"
    echo "FAILURE: Linking error. Still cannot find NetCDF libraries."
    echo "Check if this directory exists: ls -d $NC_LIB_DIR"
    exit 1
fi
