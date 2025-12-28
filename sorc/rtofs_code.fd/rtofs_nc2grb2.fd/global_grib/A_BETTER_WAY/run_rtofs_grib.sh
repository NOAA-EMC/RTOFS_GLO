#!/bin/bash

# --- 1. Help Menu Function ---
usage() {
    echo "=========================================================="
    echo "RTOFS NetCDF to GRIB2 Converter"
    echo "Usage: $0 [variable_name]"
    echo "----------------------------------------------------------"
    echo "Supported variables:"
    echo "  sst           - Sea Surface Temperature (Default)"
    echo "  sss           - Sea Surface Salinity"
    echo "  ssh           - Sea Surface Height"
    echo "  u_velocity    - Ocean U-Velocity"
    echo "  v_velocity    - Ocean V-Velocity"
    echo ""
    echo "Example: $0 sss"
    echo "=========================================================="
    exit 1
}

# Check for help flags
if [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
    usage
fi

# --- 2. Environment Setup ---
module purge
module load craype-x86-rome libfabric/1.20.1 craype-network-ofi envvar/1.0
module load intel/19.1.3.304 PrgEnv-intel/8.3.3 craype/2.7.17
module load bacio/2.4.1 g2/3.4.5 w3nco/2.4.1 jasper/2.0.25 libpng/1.6.37 zlib/1.2.11 netcdf/4.7.4 hdf5/1.10.6

# --- 3. Input Arguments ---
VAR_NAME=${1:-"sst"}

# --- 4. GRIB2 Code Lookup Table ---
case $VAR_NAME in
    "sst")
        PARM=0; P_CAT=3; LONG_NAME="Sea Surface Temp" ;;
    "sss")
        PARM=5; P_CAT=3; LONG_NAME="Sea Surface Salinity" ;;
    "ssh")
        PARM=1; P_CAT=3; LONG_NAME="Sea Surface Height" ;;
    "u_velocity")
        PARM=2; P_CAT=1; LONG_NAME="U-Velocity" ;;
    "v_velocity")
        PARM=3; P_CAT=1; LONG_NAME="V-Velocity" ;;
    *)
        echo "Error: Variable '$VAR_NAME' is not supported."
        usage ;;
esac

# --- 5. Configuration ---
EXECUTABLE="./rtofs_nc_to_grib2"
NC_INPUT="/lfs/h1/ops/prod/com/rtofs/v2.5/rtofs.20251227/rtofs_glo_2ds_f000_prog.nc"
GRIB_OUTPUT="rtofs_glo_${VAR_NAME}_f000.grb2"

# Metadata (Base)
IYR=2025; IMO=12; IDAY=27; ICYCLE=00; FCSTHR=0
LON0=0.0; LAT0=-90.0; DLAT=0.083; DLON=0.083; DEPTH=0; GEN_ID=0

echo "-----------------------------------------------"
echo "Processing: $LONG_NAME ($VAR_NAME)"
echo "-----------------------------------------------"

# --- 6. Execution ---
$EXECUTABLE << EOF
$NC_INPUT
$VAR_NAME
$GRIB_OUTPUT
$IYR $IMO $IDAY $ICYCLE $FCSTHR
$PARM $P_CAT $LON0 $LAT0 $DLAT $DLON $DEPTH $GEN_ID
EOF

# --- 7. Verification ---
if [ -s "$GRIB_OUTPUT" ]; then
    echo "SUCCESS: Created $GRIB_OUTPUT"
    if command -v wgrib2 >/dev/null 2>&1; then
        wgrib2 "$GRIB_OUTPUT" -v
    fi
else
    echo "FAILURE: Check if $VAR_NAME exists in the NetCDF file."
    exit 1
fi
