#!/bin/bash

# --- 1. Configuration & Executable Path ---
EXECUTABLE="./rtofs_nc2grb2"
VAR_NAME="sst"
NC_FILE="rtofs_data.nc"

# Ensure output environment variable is set for the BACIO library
export XLFUNIT_50="rtofs_output.grb2"

# --- 2. Grid Dimensions ---
# Note: You can uncomment the Python call if you want to generate fort.20 dynamically
# ./nc_to_fort20.py "$NC_FILE" "$VAR_NAME"
# For now, using hardcoded global dimensions:
PY_OUTPUT="GRID_DIM 4500 3298"

IMAX=$(echo "$PY_OUTPUT" | grep "GRID_DIM" | awk '{print $2}')
JMAX=$(echo "$PY_OUTPUT" | grep "GRID_DIM" | awk '{print $3}')

if [ -z "$IMAX" ] || [ -z "$JMAX" ]; then
    echo "Error: Could not determine grid dimensions."
    exit 1
fi

echo "Detected Grid: IMAX=$IMAX, JMAX=$JMAX"

# --- 3. Create Variable Name File (Unit 30) ---
# This tells the Fortran code whether to convert Celsius to Kelvin
echo "$VAR_NAME" > fort.30

# --- 4. Define GRIB2 Metadata ---
# Ensure these match the variable names used in the EOF block below
IDAY=$(date +%d)
IYR=$(date +%Y)
IMO=$(date +%m)
FCSTHR=0
ICYCLE=12
PARM=0      # 0 = Water Temperature
P_CAT=3     # 3 = Surface Properties
LON0=-80.0
LAT0=25.0
DLAT=0.1
DLON=0.1
DEPTH=0
GEN_PRO=0

# --- 5. Run the Fortran Executable ---
echo "Step 2: Encoding to GRIB2..."

# The Fortran 'read(*,*)' expects exactly 15 values.
# We pass them all on one line to ensure the input stream is complete.
$EXECUTABLE << EOF
$IMAX $JMAX $IDAY $IYR $IMO $FCSTHR $ICYCLE $PARM $P_CAT $LON0 $LAT0 $DLAT $DLON $DEPTH $GEN_PRO
EOF

# Capture the exit status
STATUS=$?

if [ $STATUS -eq 0 ]; then
    echo "----------------------------------------------"
    echo "Success! GRIB2 file created: $XLFUNIT_50"
    ls -lh "$XLFUNIT_50"
else
    echo "----------------------------------------------"
    echo "Fortran execution failed with error code: $STATUS"
fi
