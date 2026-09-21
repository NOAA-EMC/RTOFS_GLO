# What's here?
  - Compilation scripts for the RTOFS global forecast model (UFS and UFS utilities).
  - Compilation scripts for NCODA and RTOFS-specific data assimilation utilities.
  - Source code directories (`.fd`) encompassing the model, DA, HYCOM, and utility components.

# Brief description:

> **Note on Machine Support:** The UFS forecast model compilation script (`build_forecast.sh`) 
    supports cross-platform builds (`wcoss2`, `ursa`, `orion`, `gaeac6`). 
    However, the NCODA and RTOFS utility build scripts are currently strictly enforced 
    for execution on **wcoss2** only. Ensure you are on the correct machine before executing the DA/Utility builds.

| File / Dir name | Brief description |
| :--        | :-- |
| `build_forecast.sh` | Builds the UFS model and `ufs_utils` (specifically `mppnccombine`), pushing verified executables to the top-level `exec/` directory. |
| `build_ncoda_and_rtofs_utils.sh` | Builds prerequisite libraries, clones the NCODA repo, symlinks the FIX directory, and executes the RTOFS build/install process. |
| `build_rtofs_ncoda.sh` | Sub-script to exclusively clone NCODA, symlink FIX datasets, and build the NCODA executables for RTOFS. |
| `build_rtofs.sh` | Top-level wrapper script to `compile`, `install`, `debug`, or `clean` the binaries in `rtofs_code.fd` and `rtofs_ncoda.fd`. |
| `*.fd/` (Directories) | Source code repositories containing the UFS model, UFS utilities, RTOFS source, HYCOM, and Data Assimilation (DA) code. |

# Example usage:

- `./build_forecast.sh emc wcoss2`
  Note: Requires `<RUN_ENVIR>` and `<machine_name>` arguments.

- `./build_ncoda_and_rtofs_utils.sh wcoss2`
  Note: Requires `<machine_name>` argument. Fails if machine is not `wcoss2`.

- `./build_rtofs_ncoda.sh wcoss2`
  Note: Requires `<machine_name>` argument. Executes logic only if the machine is `wcoss2`.

- `./build_rtofs.sh clean`
  Note: Accepts optional arguments `[compile|debug|install|clean]`. Defaults to `compile` if no argument is passed.
