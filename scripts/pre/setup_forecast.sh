#!/bin/bash
set -e

# Load configuration (handles machine detection, component resolutions, and UFSsrc)
source ./forecast_config.sh

# Fetch parsed root directory paths (INPUTDATA_ROOT, DISKNM)
source ./parse_rt_paths.sh

echo ">>> Detected Machine: $MACHINE_ID ($SCHEDULER)"
echo ">>> Sourcing static inputs from: $INPUTDATA_ROOT"
echo ">>> Building sandbox in: $SANDBOX_DIR"

# Capture the absolute path to the executable before changing directories
EXEC_FILE=$(readlink -f "../../exec/ufs_model.x")

# ==========================================
# 1. Build Sandbox Tree & Static Inputs
# ==========================================
mkdir -p "$SANDBOX_DIR"/{MOM6_OUTPUT,RESTART,history,INPUT}
cd "$SANDBOX_DIR"

echo ">>> Building INPUT directory..."

# DATM fixed input
ln -sf "${INPUTDATA_ROOT}/DATM_CDEPS/${MESH_ATM}" ./INPUT/
ln -sf "${INPUTDATA_ROOT}/DATM_CDEPS/${DATM_SRC}/201110/"*201110*.nc ./INPUT/ 2>/dev/null || true

# MOM6 fixed input
cp "${INPUTDATA_ROOT}/MOM6_FIX/${OCNRES}/"* ./INPUT/
cp "${INPUTDATA_ROOT}/MOM6_FIX_DATM/${OCNRES}/"* ./INPUT/

# CICE fixed input
cp "${INPUTDATA_ROOT}/CICE_FIX/${OCNRES}/grid_cice_NEMS_mx${OCNRES}.nc" ./INPUT/
cp "${INPUTDATA_ROOT}/CICE_FIX/${OCNRES}/kmtu_cice_NEMS_mx${OCNRES}.nc" ./INPUT/
cp "${INPUTDATA_ROOT}/CICE_FIX/${OCNRES}/mesh.mx${OCNRES}.nc" ./INPUT/

# IC / Restarts (Cold start)
cp "${INPUTDATA_ROOT}/MOM6_IC/${OCNRES}/2011100100/MOM"*.nc ./INPUT/
cp "${INPUTDATA_ROOT}/CICE_IC/${OCNRES}/cice_model_${ICERES}.cpc.res_2011100100.nc" ./cice_model.res.nc

# Build modulefiles dynamically from UFS source
echo ">>> Building module environment..."
mkdir -p ./modulefiles
cp "${UFSsrc}/tests/module-setup.sh" .
cp "${UFSsrc}/modulefiles/ufs_${MACHINE_ID}.intel.lua" ./modulefiles/modules.fv3.lua
cp "${UFSsrc}/modulefiles/ufs_common.lua" ./modulefiles/

# ==========================================
# 2. Executable & Configurations
# ==========================================
echo ">>> Symlinking executable..."
ln -sf "$EXEC_FILE" ./fv3.exe

echo ">>> Generating dynamic configuration files via atparse..."
source "${UFSsrc}/tests/atparse.bash"

# Export variables required by the parm templates
export IATM=1536
export JATM=768
export ATM_NX_GLB=$IATM
export ATM_NY_GLB=$JATM
export ATMRES="${IATM}x${JATM}"
export DATM_SRC="GEFS_NEW"
export FILEBASE_DATM="gefs"
export stream_files="INPUT/${FILEBASE_DATM}.201110.nc"
export STREAM_OFFSET="-21600"
export RESTART_N="12"
export eps_imesh="2.5e-1"
export MOM6_TOPOEDITS="ufs.topo_edits_011818.nc"
export MOM6_ALLOW_LANDMASK_CHANGES="True"

# Variables specifically for input.mom6.nml.IN
export MOM6_OUTPUT_DIR="MOM6_OUTPUT"
export MOM6_RESTART_DIR="RESTART"
export MOM6_RESTART_SETTING="n"  # 'n' = cold start, 'r' = warm start
export OCN_SPPT=".false."
export EPBL=".false."

PARM="${UFSsrc}/tests/parm"

# Disable undefined variable abortion temporarily in case templates have non-critical missing vars
set +u

# Parse .IN templates and map to their final filenames
atparse < "$PARM/datm_in.IN" > datm_in
atparse < "$PARM/datm.streams.IN" > datm.streams
atparse < "$PARM/ice_in.IN" > ice_in
atparse < "$PARM/input.mom6.nml.IN" > input.nml
atparse < "$PARM/datm_cdeps_configure.IN" > model_configure
atparse < "$PARM/ufs.configure.datm_cdeps.IN" > ufs.configure
atparse < "$PARM/MOM6_data_table.IN" > data_table
atparse < "$PARM/MOM_input_025.IN" > INPUT/MOM_input
atparse < "$PARM/stream.config_mom6.IN" > stream.config_mom6

# Copy un-templated files directly
cp "$PARM/fd_ufs.yaml" .
cp "$PARM/diag_table" .
cp "$PARM/MOM_override" ./INPUT/

set -e # Re-enable error catching

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
#SBATCH --time=30
#SBATCH --job-name="$JOB_NAME"
#SBATCH --exclusive

set -eux
EOF
fi

# Append Common Job Body
cat << EOF >> job_card
echo -n " \$( date +%s )," > job_timestamp.txt
set +x

# Override 'module reset' if it exists in setup scripts to prevent WCOSS2 crash
module() {
    if [[ "\$1" == "reset" ]]; then command module purge; else command module "\$@"; fi
}
export -f module

MACHINE_ID="${MACHINE_ID}"
source ./module-setup.sh
module use \$PWD/modulefiles
module load modules.fv3

if [[ "${MACHINE_ID}" == "wcoss2" || "${MACHINE_ID}" == "acorn" ]]; then
    module load cray-pals craype-network-ucx cray-mpich-ucx
fi

module list
set -x

echo "Model started:  " \`date\`

export OMP_NUM_THREADS=1
export ESMF_RUNTIME_PROFILE=ON
export ESMF_RUNTIME_PROFILE_OUTPUT="SUMMARY"
EOF

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
export OMP_STACKSIZE=512M
export KMP_AFFINITY=scatter
export PMI2=""

export I_MPI_EXTRA_FILESYSTEM=ON
export FI_MLX_INJECT_LIMIT=0

if [ "\${JOB_SHOULD_FAIL:-NO}" = WHEN_RUNNING ] ; then
    echo "The job should abort now." 1>&2; false
fi

sync && sleep 1
srun \${PMI2} --label --distribution=block:block -n $TOTAL_TASKS ./fv3.exe
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
