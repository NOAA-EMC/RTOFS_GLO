#!/bin/bash
set -e

# Capture the scripts directory before we move to the sandbox
SCRIPT_DIR="$PWD"

# Source configurations and path definitions
source ./forecast_config.sh
source ./set_path_to_FIX.sh
source "./get_ic_paths_${OCNRES}.sh"
source "./get_forcing_paths_${OCNRES}.sh"

echo ">>> Detected Machine: $MACHINE_ID ($SCHEDULER)"
echo ">>> Building sandbox in: $SANDBOX_DIR"

# Capture the absolute path to the executable before changing directories
EXEC_FILE=$(readlink -f "../../exec/ufs_model.x")

# ==========================================
# 1. Build Sandbox Tree & Static Inputs
# ==========================================

# DROP DEAD: Ensure sandbox does not already exist
if [[ -d "$SANDBOX_DIR" ]]; then
    echo "FATAL: Sandbox directory already exists: $SANDBOX_DIR"
    echo "Please remove it or change SANDBOX_DIR in forecast_config.sh before running."
    exit 1
fi

mkdir -p "$SANDBOX_DIR"/{MOM6_OUTPUT,RESTART,history,INPUT}
cd "$SANDBOX_DIR"

echo ">>> Symlinking DATM forcings..."
if [[ -z "${FORCING_FILE:-}" ]]; then
    echo "ERROR: FORCING_FILE is empty! Check get_forcing_paths_${OCNRES}.sh"
    exit 1
fi

ln -sf "${DATM_FIX_DIR}/${MESH_ATM}" "./INPUT/${MESH_ATM}"
ln -sf "${FORCING_FILE}" "./INPUT/$(basename "${FORCING_FILE}")"

echo ">>> Symlinking explicit MOM6 fixed inputs for resolution ${OCNRES}..."
if [[ "$OCNRES" == "025" ]]; then
    MOM6_FIX_FILES=(
        "All_edits.nc" "geothermal_davies2013_v1.nc" "hycom1_75_800m.nc"
        "interpolate_zgrid_30L.nc" "interpolate_zgrid_32L.nc" "interpolate_zgrid_40L.nc"
        "layer_coord.nc" "MOM_channels_global_025" "ocean_hgrid.nc"
        "ocean_mask.nc" "ocean_mosaic.nc" "ocean_topog.nc"
        "oceanda_zgrid_75L.nc" "runoff.daitren.clim.1440x1080.v20180328.nc"
        "seawifs-clim-1997-2010.1440x1080.v20180328.nc" "tidal_amplitude.v20140616.nc" "topog.nc"
    )
    for f in "${MOM6_FIX_FILES[@]}"; do ln -sf "${MOM6_FIX_DIR}/$f" ./INPUT/; done
    ln -sf "${DATM_FIX_DIR}/mom6/025/grid_spec.nc" ./INPUT/

elif [[ "$OCNRES" == "008" ]]; then
    MOM6_FIX_FILES=(
        "chl_mom6.nc" "mom6_vgrid.nc" "ocean_hgrid.nc"
        "ocean_mask.nc" "ocean_mosaic.nc" "ocean_topog.nc"
        "runoff.daitren.clim.0.08deg.nc" "sss_mom6.nc" "tidal_amplitude.nc"
    )
    for f in "${MOM6_FIX_FILES[@]}"; do ln -sf "${MOM6_FIX_DIR}/$f" ./INPUT/; done
    ln -sf "${DATM_FIX_DIR}/mom6/008/grid_spec.nc" ./INPUT/

else
    echo "ERROR: Unknown OCNRES: $OCNRES."
    exit 1
fi

echo ">>> Copying Initial Conditions..."
# Find all MOM restart files and copy them to INPUT/, stripping any date prefixes
for ic_file in "${MOM6_IC_DIR}/"*MOM.res*.nc; do
    if [[ -f "$ic_file" ]]; then
        base_name=$(basename "$ic_file")
        # Strips everything up to and including 'MOM.res' to get the suffix, then prepends 'MOM.res'
        target_name=${base_name#*MOM.res}
        cp "$ic_file" "./INPUT/MOM.res${target_name}"
    fi
done

# Use the dynamically defined target filename from get_ic_paths for CICE
cp "${CICE_IC_FILE}" "${CICE_IC_TARGET}"

echo ">>> Building module environment..."
mkdir -p ./modulefiles
cp "${UFSsrc}/tests/module-setup.sh" .
cp "${UFSsrc}/modulefiles/ufs_${MACHINE_ID}.intel.lua" ./modulefiles/modules.fv3.lua
cp "${UFSsrc}/modulefiles/ufs_common.lua" ./modulefiles/

echo ">>> Symlinking CICE fixed inputs..."
ln -sf "${CICE_FIX_DIR}/grid_cice_NEMS_mx${OCNRES}.nc" ./
ln -sf "${CICE_FIX_DIR}/kmtu_cice_NEMS_mx${OCNRES}.nc" ./
ln -sf "${CICE_FIX_DIR}/mesh.mx${OCNRES}.nc" ./

# ==========================================
# 2. Executable & Configurations
# ==========================================
echo ">>> Symlinking executable..."
ln -sf "$EXEC_FILE" ./fv3.exe

echo ">>> Copying custom developer configurations for p${OCNRES}..."
CUSTOM_CONF="${SCRIPT_DIR}/../../parm/configs/p${OCNRES}"

if [[ ! -d "$CUSTOM_CONF" ]]; then
    echo "ERROR: Custom configuration directory not found: $CUSTOM_CONF"
    exit 1
fi

cp -r "${CUSTOM_CONF}/UFS/"* ./
cp -r "${CUSTOM_CONF}/CICE6/"* ./
cp -r "${CUSTOM_CONF}/MOM6/data_table" ./
cp -r "${CUSTOM_CONF}/MOM6/diag_table" ./
cp -r "${CUSTOM_CONF}/MOM6/input.nml" ./
cp -r "${CUSTOM_CONF}/MOM6/MOM_input" ./INPUT/
cp -r "${CUSTOM_CONF}/MOM6/MOM_layout" ./INPUT/
cp -r "${CUSTOM_CONF}/MOM6/MOM_override" ./INPUT/

# ==========================================
# 3. Generate the Job Card
# ==========================================
echo ">>> Generating job_card..."
cat << EOF > job_card
#!/bin/bash
EOF

if [[ "$SCHEDULER" == "PBS" ]]; then
cat << EOF >> job_card
#PBS -o out
#PBS -e err
#PBS -N $JOB_NAME
#PBS -A $ACCOUNT
#PBS -q $QUEUE
#PBS -l select=${NODES}:ncpus=${TASKS_PER_NODE}:mpiprocs=${TASKS_PER_NODE}:ompthreads=1
#PBS -l place=vscatter:exclhost
#PBS -l walltime=$WALLTIME

set -eux
cd \$PBS_O_WORKDIR
EOF

elif [[ "$SCHEDULER" == "SLURM" ]]; then
cat << EOF >> job_card
#SBATCH -e err
#SBATCH -o out
#SBATCH --account=$ACCOUNT
#SBATCH --qos=$QUEUE
#SBATCH --partition=$PARTITION
#SBATCH --nodes=$NODES
#SBATCH --ntasks-per-node=$TASKS_PER_NODE
#SBATCH --mem=0
#SBATCH --time=$WALLTIME
#SBATCH --job-name="$JOB_NAME"
#SBATCH --exclusive

set -eux
EOF
fi

# Append Common Job Body
cat << EOF >> job_card
echo -n " \$( date +%s )," > job_timestamp.txt
set +x

module() {
    if [[ "\$1" == "reset" ]]; then command module purge; else command module "\$@"; fi
}
export -f module

MACHINE_ID="${MACHINE_ID}"
source ./module-setup.sh
module use \$PWD/modulefiles
module load modules.fv3

if [[ "${MACHINE_ID}" == "wcoss2" ]]; then
    module load cray-pals craype-network-ucx cray-mpich-ucx
elif [[ "${MACHINE_ID}" == "acorn" ]]; then
    module load cray-pals
fi

module list
set -x

echo "Model started:  " \`date\`

export OMP_NUM_THREADS=1
export ESMF_RUNTIME_PROFILE=ON
export ESMF_RUNTIME_PROFILE_OUTPUT="SUMMARY"
EOF

# Ursa (Mellanox InfiniBand) specific environment tuning
if [[ "${MACHINE_ID}" == "ursa" ]]; then
cat << EOF >> job_card
export OMP_STACKSIZE=512M
export KMP_AFFINITY=scatter
export PMI2=""
export ESMF_RUNTIME_COMPLIANCECHECK=OFF:depth=4
export MPI_TYPE_DEPTH=20
export I_MPI_EXTRA_FILESYSTEM=ON
export FI_MLX_INJECT_LIMIT=0
export FI_MR_CACHE_MONITOR=kdreg2
export MPICH_SMP_SINGLE_COPY_MODE=XPMEM
EOF
fi

# Scheduler-specific Execution Command
if [[ "$SCHEDULER" == "PBS" ]]; then
cat << EOF >> job_card
export OMP_PLACES=cores
export OMP_STACKSIZE=2048M
export MPICH_MPIIO_HINTS="*:romio_cb_write=enable"
export FI_OFI_RXM_RX_SIZE=40000
export FI_OFI_RXM_TX_SIZE=40000
export FI_OFI_RXM_SAR_LIMIT=3145728

if [ "\${JOB_SHOULD_FAIL:-NO}" = WHEN_RUNNING ] ; then
    echo "The job should abort now." 1>&2; false
fi

mpiexec -n $TOTAL_TASKS -ppn $TASKS_PER_NODE --cpu-bind depth --depth 1 ./fv3.exe
EOF

elif [[ "$SCHEDULER" == "SLURM" ]]; then
cat << EOF >> job_card
if [ "\${JOB_SHOULD_FAIL:-NO}" = WHEN_RUNNING ] ; then
    echo "The job should abort now." 1>&2; false
fi

sync && sleep 1
srun \${PMI2:-} --label --distribution=block:block -n $TOTAL_TASKS ./fv3.exe
EOF
fi

cat << 'EOF' >> job_card

echo "Model ended:    " `date`
echo -n " $( date +%s )," >> job_timestamp.txt
EOF

# ==========================================
# 4. Pre-Flight Checks
# ==========================================
echo ">>> Performing Pre-Flight Checks..."
MISSING=0
for f in fv3.exe input.nml model_configure job_card INPUT module-setup.sh datm_in; do
    if [[ ! -e "$f" ]]; then 
        echo "ERROR: Missing $f"
        MISSING=1
    fi
done

if [[ $MISSING -eq 1 ]]; then
    echo ">>> PRE-FLIGHT CHECK FAILED."
    exit 1
else
    echo ">>> Sandbox built successfully! Run 'cd $SANDBOX_DIR' and submit 'job_card'."
fi
