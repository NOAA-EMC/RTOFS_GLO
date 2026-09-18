# Real-Time Ocean Forecast System (RTOFS)
The Global RTOFS system at NCEP based on HYCOM-CICE and NCODA

The global-workflow depends on the following prerequisities to be available on the system:

* modules - NCEPLIBS (various), hdf5, intel/ips v18, wgrib2, netcdf v4.7.4, gempak (see files under /versions for additional details)

RTOFS-GLO current supports the following machines:

* WCOSS2
* [NOAA RDHPCS](https://docs.rdhpcs.noaa.gov/systems/index.html#): Gaea C6, MSU-HPC, Ursa for forecast model only.

## Building RTOFS:

### 1. Base check out

```
git clone git@github.com:NOAA-EMC/RTOFS_GLO.git
cd RTOFS_GLO/sorc
```

**Note**:
- If you have no intention to develop anything and want to merely use RTOFS, see below.
- Instead of clone using: `git clone git@github.com:NOAA-EMC/RTOFS_GLO.git`,
- Do: `git clone https://github.com/NOAA-EMC/RTOFS_GLO.git`.

### 2. Build components

1. Forecast model:
   - Use `./build_forecast.sh` <RUN_ENVIR> <machine_name>
   - Notes:
     - Allowed `RUN_ENVIR`= nco, emc.
     - `machine_name` = wcoss2, ursa, orion, gaeac6.
     - For all development on WCOSS-2: `./build_forecast.sh emc wcoss2`
       - Other than WCOSS-2: `./build_forecast.sh emc <machine_name>`

2. Rest of the utilities, including NCODA (**requires access to NCODA repository**), on wcoss2 only:
   - Use `build_ncoda_and_rtofs_utils.sh <machine_name>`
   - Notes:
     - Allowed `machine_name`= wcoss2 (only).
     - Relies on `sorc/build_rtofs.sh`.

### 3. Experiment set up

1. Forecast only:
   - After (above) step 2, `cd scripts/pre`
   - `./setup_forecast.sh` will set up a UFS-DATM p008 test case.
     Works on: wcoss2 (incl acorn), Ursa, Orion, and Hercules.
   - See `README.md` for details

**Remarks**:
- If building NCODA only, use: `sorc/build_rtofs_ncoda.sh`

