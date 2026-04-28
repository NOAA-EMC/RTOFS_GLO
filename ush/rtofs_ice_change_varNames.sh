#!/bin/bash

# Goal: Rename variables in the 2D ice netcdf file.
# Why?  CICE is writing variables with "old_name".
#       Renaming maintains continuity (for sake of downstream users) with RTOFS v2.5.
#
# Note: Input file is modified "in-place", i.e., there is NO NEW OUTPUT file.
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

# 1. Check if exactly 1 arguments are provided
if [[ $# -ne 1 ]]; then
    echo "ERROR: Missing required arguments."
    echo "Usage: $0 <input_name>"
    exit 1
fi

# 2. Input Arguments
#IN_PATH=$1
#HOUR_STR=$2
full_inpath=$1

# 3. File Template Configuration
#prefix="rtofs_glo_2ds."
#suffix=".ice.nc"
#infile="${prefix}${HOUR_STR}${suffix}"
#full_inpath="${IN_PATH}/${infile}"

# 4. Verification Step
echo ">>> Verifying input file in $IN_PATH..."
if [[ ! -s "$full_inpath" ]]; then
    echo "ERROR: File $full_inpath is missing or 0 bytes."
    exit 1
fi

# 5. Renaming Block (In-Place)
echo ">>> Finalizing Variable Names in-place..."

# Build the ncrename arguments dynamically from the map
rename_args=""
for old_var in "${!var_map[@]}"; do
    rename_args+="-v ${old_var},${var_map[$old_var]} "
done

# Execute ncrename with the dynamically built flags directly on the input file
ncrename ${rename_args} "$full_inpath"

if [[ $? -eq 0 ]]; then 
    echo "  [RENAME] Ice variables finished successfully."
else
    echo "  [RENAME] Ice variables failed in ncrename."
    exit 1
fi

# Final verification to ensure the file is not empty
if [[ ! -s "$full_inpath" ]]; then
    echo "ERROR: File $full_inpath is missing or 0 bytes after renaming."
    exit 1
fi

echo ">>> SUCCESS! In-place renamed file: $full_inpath"
exit 0
