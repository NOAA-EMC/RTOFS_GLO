#!/bin/bash

# Goal: combine/concatenate 4 netcdf files containing:
# 1. h: layer thickness
# 2. t: temperature
# 3. s: salinity
# 4. u: u-velocity
# 5. v: v-velocity
#  into a single netcdf file
# ----

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
vars=("h" "s" "t" "u" "v")
prefix1="rtofs_glo_3dz."
prefix2=".daily.3z"
# Concatenate
prefix="${prefix1}${HOUR_STR}${prefix2}"
suffix="io.nc"

# 4. Verification Step
echo ">>> Verifying input files in $IN_PATH..."
for v in "${vars[@]}"; do
    file="${IN_PATH}/${prefix}${v}${suffix}"
    if [[ ! -s "$file" ]]; then
        echo "ERROR: File $file is missing or 0 bytes."
        exit 1
    fi
done

# 5. Combination Logic
mkdir -p "$OUT_PATH"
final_output="${OUT_PATH}/${OUT_FNAME}"

# Start with the first file (h)
first_file="${IN_PATH}/${prefix}h${suffix}"
echo ">>> Creating base file: $OUT_FNAME"
cp "$first_file" "$final_output"

# Append the rest (s, t, u, v)
# Note: Since the previous script didn't rename internal vars yet, 
# they are still 'salt', 'potT', 'u', 'v' inside those files.
for v_tag in "s" "t" "u" "v"; do
    current_file="${IN_PATH}/${prefix}${v_tag}${suffix}"
    echo "  -> Appending $v_tag..."
    ncks -A "$current_file" "$final_output"
done

# 6. Renaming Block
# Synthesized names often need to match downstream post-processing
echo ">>> Finalizing Variable Names..."

# Rename 'potT' to 't'
#ncrename -v potT,t "$final_output" 2>/dev/null
#if [[ $? -eq 0 ]]; then echo "  [RENAME] potT -> t"; fi

# Rename 'salt' to 's'
#ncrename -v salt,s "$final_output" 2>/dev/null
#if [[ $? -eq 0 ]]; then echo "  [RENAME] salt -> s"; fi

# If you want 'u' and 'v' to be 'uz' and 'vz' or similar:
# ncrename -v u,uz -v v,vz "$final_output"

echo ">>> SUCCESS! Combined and renamed file: $final_output"
