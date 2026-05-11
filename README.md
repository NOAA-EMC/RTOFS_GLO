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
- Do: `git clone https://github.com/NOAA-EMC/RTOFS_GLO.git`

### 2. Build components

- Forecast model:
  - Use `./build_forecast.sh` <RUN_ENVIR> <machine_name>
  - Notes:
    - Allowed `RUN_ENVIR`= nco, emc.
    - `machine_name` = wcoss2, ursa, orion, gaeac6.
    - For all development on WCOSS-2: `./build_forecast.sh emc wcoss2`


- NCODA DA (**requires access to NCODA repository**):
    
```
$ git clone git@github.com:NOAA-EMC/NCODA.git rtofs_ncoda.fd
```

While in /libs folder:
```
$ ./build_libs.sh
```

While in /sorc folder:
```
$ ./build_rtofs.sh
$ ./build_rtofs.sh install
```

