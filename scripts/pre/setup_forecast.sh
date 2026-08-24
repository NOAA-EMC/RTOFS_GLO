#!/bin/bash
set -e

# Load configuration (which now handles machine detection)
source ./run_config.sh

echo ">>> Detected Machine: $MACHINE_ID ($SCHEDULER)"
echo ">>> Building sandbox in: $SANDBOX_DIR"

# ==========================================
# 1. Build Sandbox Tree
# ==========================================
mkdir -p "$SANDBOX_DIR"/{MOM6_OUTPUT,RESTART,history}
cd "$SANDBOX_DIR"

echo ">>> Symlinking static files..."
ln -sf "$SOURCE_DIR/INPUT" ./INPUT
ln -sf "$SOURCE_DIR/fv3.exe" ./fv3.exe
ln -sf "$SOURCE_DIR/"*mx025.nc . 

echo ">>> Copying dynamic/configuration files..."
cp -r "$SOURCE_DIR/modulefiles" ./modulefiles
cp "$SOURCE_DIR/module-setup.sh" .
cp "$SOURCE_DIR"/{datm_in,datm.streams,diag_table,fd_ufs.yaml,ice_in,input.nml,model_configure,ufs.configure,data_table,ice.restart_file} .

echo ">>> Copying restart files..."
cp -r "$SOURCE_DIR/RESTART/"* ./RESTART/
cp "$SOURCE_DIR/"*.res.nc .
cp "$SOURCE_DIR/DATM_GEFS"*.nc .

# ==========================================
# 2. Generate the Job Card
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

# Assuming intel compiler based on your setup
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
# 3. Pre-Flight Checks
# ==========================================
echo ">>> Performing Pre-Flight Checks..."
MISSING=0
for f in fv3.exe input.nml model_configure job_card INPUT module-setup.sh; do
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
