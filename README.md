# Real-Time Ocean Forecast System (RTOFS)
The Global RTOFS system at NCEP based on HYCOM-CICE and NCODA

The global-workflow depends on the following prerequisities to be available on the system:

* modules - NCEPLIBS (various), hdf5, intel/ips v18, wgrib2, netcdf v4.7.4, gempak (see files under /versions for additional details)

RTOFS-GLO current supports the following machines:

* WCOSS-Cray

## Building RTOFS:

### 1. Check out components

$ git clone git@github.com:NOAA-EMC/RTOFS_GLO.git

While in /sorc folder (require access to NCODA repository):
```
$ git clone git@github.com:NOAA-EMC/NCODA.git rtofs_ncoda.fd
```
### 2. Build components

While in /libs folder:
```
$ ./build_libs.sh
```

While in /sorc folder:
```
$ ./build_rtofs.sh
$ ./build_rtofs.sh install
```

