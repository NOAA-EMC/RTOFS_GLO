# `hycom_utils.fd`
  - HYCOM Archive Diagnostics.
    - Standalone utilities for verifying RTOFS/HYCOM archive files against static bathymetry.
  - Generation of z-spaced fields (for MOM6 initialization).

## Background & Provenance

- Selected source files were extracted from [HYCOM-tools.](https://github.com/HYCOM/HYCOM-tools/tree/master) (Nov 09, 2026)
  - It was done to avoid pulling in a large submodule.
  - Those legacy sources (`getdat.f`, `horout_nc.f`, `mod_plot.F`, `stmt_fns_*.h`, etc.) were
    subsequently retired and replaced with a self-contained codebase.

### Features:
1. **Eliminate full-archive sequential I/O:**
   - Read only the requested variable, layer thicknesses (`thknss`), and `srfhgt`, etc,
     via direct access rather than scanning every 2D/3D record in `archv.a`.
2. **Accelerate NetCDF output:**
   - Write 64-bit offset NetCDF files with `NF90_NOFILL`.
   - Single-call array writes on native MOM6 symmetric tripole coordinates (`xh`, `xq`, `yh`, `yq`, `z_l`, `Time`).
3. **Consolidate preprocessing:**
   - Fold bathymetry consistency checks adapted from [hycom_archive_sea_ok.F](https://github.com/HYCOM/HYCOM-tools/blob/master/bin/hycom_archive_sea_ok.F)
   - Z-level remapping (`layer2z.f`), MOM6 grid mapping (`archv2mom6res.f`),
     - Z* layer thickness generation (adapted from [build_thickness.py](https://github.com/NOAA-EMC/rtofs-tools/issues/21#issue-6357718250)
     - Into a single library module without dependencies on `bacio`, `w3nco`, or C (as in [HYCOM-tools.](https://github.com/HYCOM/HYCOM-tools/))

## Directory Contents

| File | Description |
| :--- | :--- |
| `hycom_io_utils.F90` | Core Fortran 90 module providing direct-access `.a` I/O, `.b` metadata/record lookup, bathymetry consistency checks, masked 2D smoothing, `layer2z`/`layer2c` vertical remapping, z* thickness calculation, MOM6 symmetric grid mappers (`h2m_p`, `h2m_u`, `h2m_v`), and NetCDF writers. |
| `check_hycom_archive_bathymetry.F90` | Diagnostic program that compares archive layer thicknesses against `regional.depth.[ab]`, writes binary (`.a`) and NetCDF (`.nc`) depth-anomaly maps, and optionally writes corrected `regional.depth.[ab]` files. |
| `archv2mom6z.F90` | Selective variable extractor of `ssh`, `dz`, `u`, `v`, `temp`, `saln`, or any 2D archive field) on the symmetric tripole grid (`nx = idm`, `ny = jdm - 1`, `nxp = idm + 1`, `nyp = jdm`). |
| `rtofs_glo_glorys_depths.in` | Vertical grid and interpolation namelist template configured with the 50 nominal GLORYS12 z* cell-center depths. |
| `CMakeLists.txt` | CMake build configuration targeting Intel Fortran and NetCDF (`-convert big_endian -assume byterecl`). |
| `build_all.sh` | Build wrapper script that loads required (WCOSS2) modules, compiles via CMake, and installs executables into `../../exec/`. |

## Build Instructions

From `sorc/hycom_utils.fd/`:

```bash
./build_all.sh
```

This compiles and installs `check_hycom_archive_bathymetry` and `archv2mom6z` into `../../exec/`.

## Usage

### 1. Bathymetry Consistency Check & Correction

```bash
check_hycom_archive_bathymetry <archv.a> <depth.a> <anom.a> [corrected_depth.a]
```

* Writes `<anom.a>` and `<anom.nc>` containing `depth_anom`, `depth_orig`, and `depth_corr`.
* If `[corrected_depth.a]` is supplied, also writes the corrected `.a` and companion `.b` bathymetry files where `|depth_anom| > 0.01 m`.

### 2. Single-Variable Extraction to z-Grid

```bash
archv2mom6z <var> <archv.a> <depth.a> <grid.a> <depths.in> <out.nc>
```

Supported `<var>` arguments:

* `ssh` — Sea surface height on `h`-grid (`nx=4500, ny=3297`, coords `xh, yh`)
* `dz` (or `h`) — 3D z* layer thickness with partial bottom steps on `h`-grid (`nx=4500, ny=3297, z_l=50`, coords `xh, yh, z_l`)
* `uvl` (or `u`) — Total un-rotated zonal velocity on `u`-grid (`nxp=4501, ny=3297, z_l=50`, coords `xq, yh, z_l`)
* `vvl` (or `v`) — Total un-rotated meridional velocity on `v`-grid (`nx=4500, nyp=3298, z_l=50`, coords `xh, yq, z_l`)
* `tem` (or `temp`) — Potential temperature on `h`-grid (`nx=4500, ny=3297, z_l=50`, coords `xh, yh, z_l`)
* `sal` (or `saln`) — Salinity on `h`-grid (`nx=4500, ny=3297, z_l=50`, coords `xh, yh, z_l`)
* Any 8-character 2D field name present in `<archv.b>` (e.g., `bl_dpth`, `mix_dpth`, `steric`)
