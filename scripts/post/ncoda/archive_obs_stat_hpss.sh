#!/bin/bash
# archive_obs_stat_hpss.sh
# Purpose: Submit a dev_transfer PBS job to archive daily RTOFS observation NetCDF,
#          CSV, and log files to HPSS.
# Usage:   ./archive_obs_stat_hpss.sh [YYYYMMDD]  (defaults to today's cycle date)

set -u

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
CHECK_ENV_PATH="$(readlink -f "${SCRIPT_DIR}/../check_machine_env.sh")"

# 1. Machine check (only run on active primary WCOSS2 host)
if [[ -f "${CHECK_ENV_PATH}" ]]; then
    source "${CHECK_ENV_PATH}" || exit 3
else
    echo "ERROR: Machine check missing at ${CHECK_ENV_PATH}"
    exit 2
fi

# 2. Configuration
rtofs_version="v2.5"
rtofs_date="${1:-$(date +%Y%m%d)}"

src_base="/lfs/h2/emc/couple/noscrub/${USER}/RTOFS_OM/${rtofs_version}/obs_stat"
src_dir="${src_base}/${rtofs_date}"
hpss_dir="/NCEPDEV/emc-ocean/5year/Santha.Akella/rtofs.v2p5_obs"

if [[ ! -d "${src_dir}" ]]; then
    echo "ERROR: Source directory does not exist: ${src_dir}"
    exit 4
fi

# 3. Build and submit PBS dev_transfer job
tmpdir="/lfs/h2/emc/ptmp/${LOGNAME}/archive.obs_stat"
mkdir -p "${tmpdir}"

job_script="${tmpdir}/obs_stat_hpss.${rtofs_date}.$$.pbs"

cat << EOF > "${job_script}"
#!/bin/bash
#PBS -N jobs_stat_hpss_${rtofs_date}
#PBS -j oe
#PBS -o ${tmpdir}/obs_stat_hpss.${rtofs_date}.log
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1
#PBS -q dev_transfer
#PBS -l walltime=02:00:00

module purge
module load envvar prod_envir prod_util PrgEnv-intel intel

set -x

hsi mkd -p ${hpss_dir}

cd ${src_base} || exit 1
htar -cvf ${hpss_dir}/obs_stat.${rtofs_date}.tar ${rtofs_date}

# Also refresh summary CSV/PNG index tarball at top level
htar -cvf ${hpss_dir}/obs_stat_summary.tar timeseries_*.png 2>/dev/null || true
EOF

chmod +x "${job_script}"
echo ">>> Submitting HPSS archive job for ${rtofs_date} -> ${hpss_dir}/obs_stat.${rtofs_date}.tar"
qsub "${job_script}"
