#!/bin/bash

# Goal: Rename variables in the 2D ice netcdf file
# Why?  CICE is writing variables with "old_name".
#       Renaming maintains continuity (for sake of downstream users) with RTOFS v2.5.
# ----

# Define variable renaming map ( [old_name]="new_name" )
# Change these values anytime without altering the logic below
declare -A var_map=(
    ["hi_h"]="ice_thickness"
    ["Tsfc_h"]="ice_temperature"
    ["aice_h"]="ice_coverage"
    ["uvel_h"]="ice_uvelocity"
    ["vvel_h"]="ice_vvelocity"
)

# 1. Check if exactly 4 arguments are provided
if [[ $# -ne 4 ]]; then
    echo "ERROR: Missing required arguments."
    echo "Usage: $0 <input_path> <hour_str> <output_path> <output_filename>"
    exit 1
fi

# 2. Input Arguments
IN_PATH=$1
HOUR_STR=$2
OUT_PATH=$3
OUT_FNAME=$4

# 3. File Template Configuration
prefix="rtofs_glo_2ds."
suffix=".ice.nc"
infile="${prefix}${HOUR_STR}${suffix}"
full_inpath="${IN_PATH}/${infile}"

# 4. Verification Step
echo ">>> Verifying input file in $IN_PATH..."
if [[ ! -s "$full_inpath" ]]; then
    echo "ERROR: File $full_inpath is missing or 0 bytes."
    exit 1
fi

# 5. Output Setup
mkdir -p "$OUT_PATH"
final_output="${OUT_PATH}/${OUT_FNAME}"

echo ">>> Copying to base file: $final_output"
cp "$full_inpath" "$final_output"
if [[ $? -ne 0 ]]; then
    echo "ERROR: Failed to copy input file to output path."
    exit 1
fi

# 6. Renaming Block
echo ">>> Finalizing Variable Names..."

# Build the ncrename arguments dynamically from the map
rename_args=""
for old_var in "${!var_map[@]}"; do
    rename_args+="-v ${old_var},${var_map[$old_var]} "
done

# Execute ncrename with the dynamically built flags
ncrename ${rename_args} "$final_output"

if [[ $? -eq 0 ]]; then 
    echo "  [RENAME] Ice variables renamed successfully."
else
    echo "ERROR: ncrename failed."
    exit 1
fi

# Final verification to ensure the output file is not empty
if [[ ! -s "$final_output" ]]; then
    echo "ERROR: Output file $final_output is missing or 0 bytes after renaming."
    exit 1
fi

echo ">>> SUCCESS! Renamed file saved: $final_output"
exit 0
