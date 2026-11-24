#!/bin/bash
# collect_argo_lookback_counts.sh
# Auto-discovers available rtofs.YYYYMMDD cycles in COM, processes all ~15 lookback
# profile files per cycle on the fly, logs Argo counts to CSV, and cleans up temp NetCDF.
#
# Usage:
#   Cron / auto-discover all cycles in COM: ./collect_argo_lookback_counts.sh
#   Explicit range:                        ./collect_argo_lookback_counts.sh 20261118 20261124 [out_csv]

set -u

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
CHECK_ENV_PATH="/lfs/h2/emc/couple/noscrub/${USER}/RTOFS_GLO/scripts/post/check_machine_env.sh"

# Exit cleanly if running on backup WCOSS2 machine
if [[ -f "${CHECK_ENV_PATH}" ]]; then
    source "${CHECK_ENV_PATH}" || exit 0
fi

com_base="/lfs/h1/ops/prod/com/rtofs/v2.5"

# 1. Discover available rtofs.YYYYMMDD cycles on disk
shopt -s nullglob
rtofs_dirs=( "${com_base}"/rtofs.[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9] )
shopt -u nullglob

if [[ ${#rtofs_dirs[@]} -eq 0 ]]; then
    echo "ERROR: No rtofs.YYYYMMDD directories found in ${com_base}" >&2
    exit 1
fi

avail_cycles=()
for d in "${rtofs_dirs[@]}"; do
    cyc="${d##*rtofs.}"
    if [[ -d "${d}/ncoda/ocnqc/profile" ]]; then
        avail_cycles+=( "${cyc}" )
    fi
done

if [[ ${#avail_cycles[@]} -eq 0 ]]; then
    echo "ERROR: No cycles with ncoda/ocnqc/profile found in ${com_base}" >&2
    exit 1
fi

# Sort cycles chronologically
IFS=$'\n' avail_cycles=($(sort <<<"${avail_cycles[*]}"))
unset IFS

# Optional CLI range filter; defaults to full discovered COM range
start_cycle=${1:-${avail_cycles[0]}}
end_cycle=${2:-${avail_cycles[-1]}}

cycles_to_run=()
for cyc in "${avail_cycles[@]}"; do
    if [[ "${cyc}" -ge "${start_cycle}" && "${cyc}" -le "${end_cycle}" ]]; then
        cycles_to_run+=( "${cyc}" )
    fi
done

if [[ ${#cycles_to_run[@]} -eq 0 ]]; then
    echo "ERROR: No cycles found between ${start_cycle} and ${end_cycle}" >&2
    exit 1
fi

first_cyc="${cycles_to_run[0]}"
last_cyc="${cycles_to_run[-1]}"

if [[ -n "${3:-}" ]]; then
    out_csv="$3"
else
    out_csv="${SCRIPT_DIR}/argo_counts_${first_cyc}_${last_cyc}.csv"
fi

exec_bin="/lfs/h2/emc/couple/noscrub/${USER}/RTOFS_GLO/sorc/rtofs_da.fd/exec/read_binary_qc_obs.x"
if [[ ! -x "${exec_bin}" ]]; then
    exec_bin="/lfs/h2/emc/stmp/${USER}/RTOFS_GLO/sorc/rtofs_da.fd/exec/read_binary_qc_obs.x"
fi

tmp_dir="/lfs/h2/emc/stmp/${USER}/tmp_argo_count_$$"
mkdir -p "${tmp_dir}"
trap 'rm -rf "${tmp_dir}"' EXIT

module load intel netcdf prod_util >/dev/null 2>&1

echo ">>> Discovered ${#cycles_to_run[@]} cycles: ${first_cyc} .. ${last_cyc}"
echo "cycle_date,obs_date,lag_days,total_profiles,argo_any,argo_temp,argo_salt" > "${out_csv}"

argo_temp_id=""
argo_salt_id=""

for curr in "${cycles_to_run[@]}"; do
    in_dir="${com_base}/rtofs.${curr}/ncoda/ocnqc/profile"
    echo ">>> Processing cycle rtofs.${curr}..."
    cycle_epoch=$(date -u -d "${curr}" +%s)

    for bin_file in $(ls "${in_dir}"/*.profile 2>/dev/null | sort); do
        fname=$(basename "${bin_file}")
        obs_date="${fname:0:8}"
        obs_epoch=$(date -u -d "${obs_date}" +%s)
        lag_days=$(( (obs_epoch - cycle_epoch) / 86400 ))

        tmp_nc="tmp_${curr}_${obs_date}.nc"
        "${exec_bin}" "${bin_file}" "profile" "${tmp_dir}" "${tmp_nc}" >/dev/null

        nc_path="${tmp_dir}/${tmp_nc}"
        if [[ ! -s "${nc_path}" ]]; then
            echo "WARNING: Failed to convert ${bin_file}" >&2
            continue
        fi

        if [[ -z "${argo_temp_id}" ]]; then
            read -r argo_temp_id argo_salt_id < <(
                ncdump -v data_type_name "${nc_path}" | awk -F'"' '
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
            echo "    Resolved IDs -> Argo Float Temp: ${argo_temp_id}, Argo Float Salt: ${argo_salt_id}"
        fi

        read -r total_nobs argo_any argo_t argo_s < <(
            ncdump -v temp_type,sal_type "${nc_path}" | awk -v tid="${argo_temp_id}" -v sid="${argo_salt_id}" '
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

        echo "${curr},${obs_date},${lag_days},${total_nobs},${argo_any},${argo_t},${argo_s}" >> "${out_csv}"
        rm -f "${nc_path}"
    done
done

echo ">>> Complete. Saved CSV to: ${out_csv}"

# --- Generate updated lookback plot ---
module reset >/dev/null 2>&1
module load intel ve/rtofs >/dev/null 2>&1
echo ">>> Plotting... "
"${SCRIPT_DIR}/plot_argo_lookback.py" "${SCRIPT_DIR}" || echo "WARNING: Plot generation failed."

exit 0
