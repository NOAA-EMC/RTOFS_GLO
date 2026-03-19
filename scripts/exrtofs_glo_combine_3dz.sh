#!/bin/bash

# 1. Environment Setup
module load intel/19.1.3.304 udunits/2.2.28 nco/5.2.4

# 2. Input Arguments
IN_PATH=$1
OUT_PATH=$2
OUT_FNAME=$3

if [[ -z "$OUT_FNAME" ]]; then
    echo "Usage: $0 <input_path> <output_path> <output_filename>"
    exit 1
fi

# 3. File Template Configuration
vars=("h" "s" "t" "u" "v")
prefix="rtofs_glo_3dz.tm000.daily.3z"
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
