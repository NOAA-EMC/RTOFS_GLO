#!/bin/bash

# --- 1. Environment Setup (CRITICAL for Shared Libraries) ---
module purge
module load craype-x86-rome
module load libfabric/1.20.1
module load craype-network-ofi
module load envvar/1.0
module load intel/19.1.3.304
module load PrgEnv-intel/8.3.3
module load craype/2.7.17
module load bacio/2.4.1
module load bufr/12.2.0
module load g2/3.4.5
module load sigio/2.3.2
module load sp/2.4.0
module load w3nco/2.4.1
module load jasper/2.0.25
module load libpng/1.6.37
module load zlib/1.2.11
module load netcdf/4.7.4
module load hdf5/1.10.6

# --- 2. Configuration ---
EXECUTABLE="./rtofs_nc_to_grib2"
NC_INPUT="/lfs/h1/ops/prod/com/rtofs/v2.5/rtofs.20251227/rtofs_glo_2ds_f000_prog.nc"
VAR_NAME="sst"
GRIB_OUTPUT="rtofs_glo_sst_f000.grb2"

# Metadata
IYR=2025; IMO=12; IDAY=27; ICYCLE=00; FCSTHR=0
PARM=0; P_CAT=3; LON0=0.0; LAT0=-90.0; DLAT=0.083; DLON=0.083; DEPTH=0; GEN_ID=0

# Clear old output
rm -f "$GRIB_OUTPUT"

echo "-----------------------------------------------"
echo "Executing: $EXECUTABLE"
echo "-----------------------------------------------"

# --- 3. Execution ---
# Each string is on its own line to match the Fortran read(*,'(A)')
$EXECUTABLE << EOF
$NC_INPUT
$VAR_NAME
$GRIB_OUTPUT
$IYR $IMO $IDAY $ICYCLE $FCSTHR
$PARM $P_CAT $LON0 $LAT0 $DLAT $DLON $DEPTH $GEN_ID
EOF

# --- 4. Final Check ---
if [ -s "$GRIB_OUTPUT" ]; then
    echo "-----------------------------------------------"
    echo "SUCCESS: $GRIB_OUTPUT generated."
    ls -lh "$GRIB_OUTPUT"
    # Optional: Display grid info if wgrib2 is available
    if command -v wgrib2 >/dev/null 2>&1; then
       wgrib2 "$GRIB_OUTPUT" -grid -v
    fi
else
    echo "-----------------------------------------------"
    echo "FAILURE: Check compiler/library compatibility."
    exit 1
fi
