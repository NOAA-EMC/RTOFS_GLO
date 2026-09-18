# What's here?
  - Scripts for pre-processing of inputs, datasets, executables,
  - Sandbox deployment for high-resolution UFS-DATM-MOM6-CICE6 runs.

# Brief description:

> **Note on Model Resolutions:** This framework is engineered and actively supported for
  high-resolution **0.08°** deployments (`OCNRES="008"` in `forecast_config.sh`).
  While 0.25° helper scripts are included in this repository, they are provided strictly "as-is"
  for lightweight developer debugging and testing.
  For all standard workflows and support, please ensure you are using the 0.08° configuration.

| File name | Brief description |
| :--       | :-- |
| `compile_ufs.sh` | To build the UFS in Data Atmosphere (DATM) mode. |
| `forecast_config.sh` | Sets global configurations (resolution, sandbox paths) and calculates machine-specific node undersubscription. |
| `get_forcing_paths_008.sh` | Resolves machine-specific paths for 0.08° DATM forcing files. |
| `get_forcing_paths_025.sh` | Resolves machine-specific paths for 0.25° DATM forcing files (debug only). |
| `get_ic_paths_008.sh` | Resolves machine-specific paths for 0.08° MOM6 and CICE6 restart files. |
| `get_ic_paths_025.sh` | Resolves machine-specific paths for 0.25° MOM6 and CICE6 restart files (debug only). |
| `set_path_to_FIX.sh` | Resolves machine-specific paths for static FIX datasets (MOM6/CICE grid, topography, meshes). |
| `setup_forecast.sh` | Main deployment script. Builds the sandbox, symlinks inputs, and dynamically injects machine-specific MPI/IO tunings into the `job_card`. |

# Example usage:

- `./compile_ufs.sh`
  Note: No arguments are needed.

- `./setup_forecast.sh`
  Note: Run after editing `forecast_config.sh` for your target resolution.
  It will automatically detect the host machine (WCOSS2, Ursa, Orion, Hercules), build the sandbox, and generate the ready-to-submit `job_card`.
  **Note**:
  - You need to pay attention to which queues you submit your job.
  - Change settings as needed, for example requested walltime.

# Architecture & I/O Tunings (0.08° Deployments):
Due to the massive 18GB PIO read requirement for the 1/12-degree ice restart arrays (`iced.yyyy-mm-dd-hhmmss.nc`),
default node configurations on RDHPCS platforms will segmentation fault in the MPI wrapper.
The `setup_forecast.sh` script automatically injects the following workarounds:

* **Global Optimizations (All platforms)**:
  * `export NC_BLKSZ=1M` to aggregate NetCDF chunking and prevent metadata thrashing.
  * `ulimit -s unlimited`, `ulimit -l unlimited`, `ulimit -c 0`.
  * `export I_MPI_SHM_HEAP_VSIZE=8192` to increase shared memory heap.

* **WCOSS2 / Acorn (Cray Slingshot)**:
  * Fully subscribed (128 tasks/node). Handled natively by Cray MPICH.

* **Orion (Skylake / Mellanox InfiniBand)**:
  * Undersubscribed to **32 tasks per node** (8 cores left idle per socket) to widen memory bandwidth.
  * UCX memory caching disabled (`UCX_MEM_CACHE=n`, `UCX_MEM_MALLOC_HOOKS=no`) to bypass memory pinning crashes during `MPI_Alltoallw`.

* **Hercules (Ice Lake / Mellanox InfiniBand)**:
  * Undersubscribed to **64 tasks per node** (16 cores left idle per socket).
  * Inherits the identical UCX memory cache bypass used on Orion.
