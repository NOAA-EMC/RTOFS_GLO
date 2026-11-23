#!/bin/bash
# Usage: ./decode_cycle_profiles.sh <cycle_date> [obs_type] [out_dir]
# Example: ./decode_cycle_profiles.sh 20261123 profile

cycle_date=${1:?Usage: $0 <cycle_date> [obs_type] [out_dir]}
obs_type=${2:-"profile"}
out_dir=${3:-"/lfs/h2/emc/stmp/${USER}/qc_decode"}

exec_bin="/lfs/h2/emc/stmp/${USER}/RTOFS_GLO/sorc/rtofs_da.fd/exec/read_binary_qc_obs.x"
in_dir="/lfs/h1/ops/prod/com/rtofs/v2.5/rtofs.${cycle_date}/ncoda/ocnqc/${obs_type}"

if [[ ! -d "${in_dir}" ]]; then
    echo "ERROR: Input directory not found: ${in_dir}"
    exit 1
fi

mkdir -p "${out_dir}"
module load intel netcdf >/dev/null 2>&1

for f in $(ls "${in_dir}"/*.${obs_type} 2>/dev/null | sort); do
    fname=$(basename "$f")
    obs_date="${fname:0:8}"
    out_file="rtofs.${cycle_date}_${obs_date}.${obs_type}.nc"

    echo ">>> Converting ${fname} -> ${out_file}"
    "${exec_bin}" "${f}" "${obs_type}" "${out_dir}" "${out_file}" || exit 2
done

echo ">>> Done. Files written to ${out_dir}"
