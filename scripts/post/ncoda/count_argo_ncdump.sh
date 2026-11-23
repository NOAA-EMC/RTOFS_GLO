#!/bin/bash
# count_argo_ncdump.sh
# Purpose: Count Argo profiles across converted RTOFS profile NetCDF files using ncdump + awk.
# Usage:   ./count_argo_ncdump.sh [target_dir] [file_glob]

target_dir=${1:-"/lfs/h2/emc/stmp/${USER}/qc_decode"}
file_glob=${2:-"rtofs.*_*.profile.nc"}

if [[ ! -d "${target_dir}" ]]; then
    echo "ERROR: Directory not found: ${target_dir}" >&2
    exit 1
fi

module load intel netcdf >/dev/null 2>&1

shopt -s nullglob
files=( "${target_dir}"/${file_glob} )
shopt -u nullglob

if [[ ${#files[@]} -eq 0 ]]; then
    echo "ERROR: No files found matching ${target_dir}/${file_glob}" >&2
    exit 1
fi

# Resolve Argo IDs dynamically from data_type_name in the first file (expects 36 & 37)
read -r argo_temp_id argo_salt_id < <(
    ncdump -v data_type_name "${files[0]}" | awk -F'"' '
        /data_type_name =/ { in_data=1 }
        in_data && NF >= 2 {
            lbl = $2
            gsub(/^[ \t]+|[ \t]+$/, "", lbl)
            if (lbl == "Argo Float Temp") t_id = idx
            if (lbl == "Argo Float Salt") s_id = idx
            idx++
        }
        END { printf "%d %d\n", t_id, s_id }
    '
)

echo "--------------------------------------------------------------------------------"
printf "Resolved from data_type_name -> Argo Float Temp ID: %s | Argo Float Salt ID: %s\n" \
    "${argo_temp_id}" "${argo_salt_id}"
echo "--------------------------------------------------------------------------------"
printf "%-36s | %8s | %10s | %10s | %10s\n" \
    "File" "Total" "Argo_Any" "Argo_Temp" "Argo_Salt"
echo "--------------------------------------------------------------------------------"

for nc_file in "${files[@]}"; do
    fname=$(basename "${nc_file}")

    # Dump only temp_type and sal_type (fast even on 1.4 GB files)
    read -r total_nobs argo_any argo_t argo_s < <(
        ncdump -v temp_type,sal_type "${nc_file}" | awk -v tid="${argo_temp_id}" -v sid="${argo_salt_id}" '
            /^data:/ { in_data=1; next }
            !in_data { next }
            /temp_type =/ { mode="t"; sub(/.*temp_type =/, "") }
            /sal_type =/  { mode="s"; sub(/.*sal_type =/, "") }
            {
                n = split($0, a, ",")
                for (i = 1; i <= n; i++) {
                    gsub(/[^0-9-]/, "", a[i])
                    if (a[i] != "") {
                        if (mode == "t") t[++nt] = a[i] + 0
                        if (mode == "s") s[++ns] = a[i] + 0
                    }
                }
            }
            END {
                any_c = 0; t_c = 0; s_c = 0
                for (i = 1; i <= nt; i++) {
                    is_t = (t[i] == tid)
                    is_s = (s[i] == sid)
                    if (is_t) t_c++
                    if (is_s) s_c++
                    if (is_t || is_s) any_c++
                }
                printf "%d %d %d %d\n", nt, any_c, t_c, s_c
            }
        '
    )

    printf "%-36s | %8d | %10d | %10d | %10d\n" \
        "${fname}" "${total_nobs}" "${argo_any}" "${argo_t}" "${argo_s}"
done

echo "--------------------------------------------------------------------------------"
