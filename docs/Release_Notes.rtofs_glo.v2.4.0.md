RTOFS_GLO V2.4.0 RELEASE NOTES

-------
PRELUDE
-------

The RTOFS V2.3.4 is upgraded with the following changes: 

* GOES-18 processing
* Fix bugzillas
* Verify all netCDF and HDF5 data to avoid crashes on corrupt or invalid data
* Two HYCOM changes that mitigate HYCOM crashes with respect to negative thicknesses
* Optimization, script clean-up

IMPLEMENTATION INSTRUCTIONS
---------------------------

```bash
cd $PACKAGEROOT
mkdir rtofs.v2.4.0
cd rtofs.v2.4.0
git clone -b rtofs.v2.4.0 https://github.com/NOAA-EMC/RTOFS_GLO.git .
cd sorc
git clone -b rtofs.v2.4.0 https://github.com/NOAA-EMC/NCODA.git rtofs_ncoda.fd
```

To build the RTOFS:
```bash
cd libs
./build_libs.sh
cd sorc
./build_rtofs.sh
./build_rtofs.sh install
```
The `build_rtofs.sh` script compiles all RTOFS executables. Compile logs for each directory is in each directory. 
To build an individual directory:

```bash
cd rtofs_code.fd
make clean
./build_code.sh
make install
```
```bash
cd rtofs_hycom.fd
./build_hycom.sh clean
./build_hycom.sh esmf
./build_hycom.sh
./build_hycom.sh install
```
```bash
cd rtofs_ncoda.fd
make clean
./build_ncoda.sh
make install
```

Copy fix files to their final respective locations by executing:
```bash
cd /lfs/h2/emc/eib/noscrub/dan.iredell/RTOFSFIX/20230615
tar cf - ./fix | (cd <topdir>/rtofs.v2.4.0; tar xf -)
```

VERSION FILE CHANGES
--------------------

* `versions/build.ver` - change PrgEnv-intel=8.3.3, craype=2.7.17, cray-mpich=8.1.19, cray-libsci=22.08.1.1
* `versions/run.ver` - change PrgEnv-intel=8.3.3, craype=2.7.17, cray-mpich=8.1.19, prod_envir=2.0.6, prod_util=2.0.14

SORC CHANGES
------------

* HYCOM
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e`
    * HYCOM stability
      * `hycom/hybgen.F`
      * `hycom/mxkprf.F`
      * `config/Aintelsse-impi-relo_cice`
    * Restartability
      * `hycom/restart.f`
      * `source/ice_calendar.F90`

* NCODA
  * `sorc/rtofs_ncoda.fd`
    * GOES-18 processing
      * `ncoda_qc/include/coda_types.h`
      * `ncoda_qc/libsrc/util/goes_code.f`
      * `ncoda_var/include/coda_types.h`

VERSION FILE CHANGES
--------------------


RESOURCE CHANGES
----------------

* Change cores for QC job
  * `ecf/da/jrtofs_global_ncoda_qc.ecf`


JOBS/SCRIPTS/USH CHANGES
------------------------

* Script cleanup, restartability, bugzilla fixes
  * `jobs/JRTOFS_GLO_ANALYSIS`
  * `jobs/JRTOFS_GLO_ANALYSIS_GRIB2_POST`
  * `jobs/JRTOFS_GLO_ANALYSIS_POST`
  * `jobs/JRTOFS_GLO_ANALYSIS_PRE`
  * `jobs/JRTOFS_GLO_FORECAST_GRIB2_POST`
  * `jobs/JRTOFS_GLO_FORECAST_POST`
  * `jobs/JRTOFS_GLO_FORECAST_POST_2`
  * `jobs/JRTOFS_GLO_FORECAST_STEP1`
  * `jobs/JRTOFS_GLO_FORECAST_STEP1_PRE`
  * `jobs/JRTOFS_GLO_FORECAST_STEP2`
  * `jobs/JRTOFS_GLO_FORECAST_STEP2_PRE`
  * `jobs/JRTOFS_GLO_GEMPAK`
  * `jobs/JRTOFS_GLO_GZIP`
  * `jobs/JRTOFS_GLO_INCUP`
  * `jobs/JRTOFS_GLO_NCODA_GLBL_VAR`
  * `jobs/JRTOFS_GLO_NCODA_HYCOM_VAR`
  * `jobs/JRTOFS_GLO_NCODA_INC`
  * `jobs/JRTOFS_GLO_NCODA_POLAR_VAR`
  * `jobs/JRTOFS_GLO_NCODA_QC`
  * `parm/rtofs_glo.navy_0.08.anal.blkdat.input`
  * `parm/rtofs_glo.navy_0.08.anal.ice_in`
  * `parm/rtofs_glo.navy_0.08.fcst.blkdat.input`
  * `parm/rtofs_glo.navy_0.08.fcst.ice_in`
  * `parm/rtofs_glo.navy_0.08.incup.blkdat.input`
  * `scripts/exrtofs_glo_analysis_pre.sh`
  * `scripts/exrtofs_glo_analysis.sh`
  * `scripts/exrtofs_glo_forecast_pre.sh
  * `scripts/exrtofs_glo_forecast.sh`
  * `scripts/exrtofs_glo_gempak.sh`
  * `scripts/exrtofs_glo_grib2_post.sh`
  * `scripts/exrtofs_glo_gzip.sh`
  * `scripts/exrtofs_glo_incup.sh`
  * `scripts/exrtofs_glo_ncoda_glbl_var.sh`
  * `scripts/exrtofs_glo_ncoda_hycom_var.sh`
  * `scripts/exrtofs_glo_ncoda_inc.sh`
  * `scripts/exrtofs_glo_ncoda_polar_var.sh`
  * `scripts/exrtofs_glo_ncoda_qc.sh`
  * `scripts/exrtofs_glo_post_2.sh`
  * `scripts/exrtofs_glo_post.sh`
  * `ush/rtofs_atmforcing_correct.sh`
  * `ush/rtofs_atmforcing_extract.sh`
  * `ush/rtofs_atmforcing_getges.sh`
  * `ush/rtofs_atmforcing.sh`
  * `ush/rtofs_atmforcing_stage.sh`
  * `ush/rtofs_create_regions_mpmd_weights.sh`
  * `ush/rtofs_date4restart.sh`
  * `ush/rtofs_date_normal2hycom.sh`
  * `ush/rtofs_glo2d_ice.sh`
  * `ush/rtofs_glo2d.sh`
  * `ush/rtofs_glo3z_6hrly.sh`
  * `ush/rtofs_glo3z_daily.sh`
  * `ush/rtofs_iceforcing.sh`
  * `ush/rtofs_nc2grib2.sh`
  * `ush/rtofs_ncoda_amsr_qc.sh`
  * `ush/rtofs_ncoda_goes_qc.sh`
  * `ush/rtofs_ncoda_himawari_qc.sh`
  * `ush/rtofs_ncoda_ice_qc.sh`
  * `ush/rtofs_ncoda_jpss_qc.sh`
  * `ush/rtofs_ncoda_metop_qc.sh`
  * `ush/rtofs_ncoda_npp_qc.sh`
  * `ush/rtofs_ncoda_prep_ice.sh`
  * `ush/rtofs_ncoda_prep_sfc_sfcr_prof.sh`
  * `ush/rtofs_ncoda_profile_qc.sh`
  * `ush/rtofs_ncodaqc2com.sh`
  * `ush/rtofs_ncoda_sfcobs_qc.sh`
  * `ush/rtofs_ncoda_ssh_qc.sh`
  * `ush/rtofs_ncoda_sss_qc.sh`
  * `ush/rtofs_ncoda_vel_qc.sh`
  * `ush/rtofs_prestaging.sh`
  * `ush/rtofs_runstaging.sh`
  * `ush/rtofs_submit.sh`
  * `ush/rtofs_surface_day1-3.sh`
  * `ush/rtofs_surface_day4-5.sh`
  * `ush/rtofs_surface_hcasts.sh`
  * `ush/rtofs_tmp2com.sh`


FIX CHANGES
-----------

* GOES-18 and Himawari-09 processing (new fix files)
  * `fix/codaclim/ABI_G18.loc`
  * `fix/codaclim/AHI_H09.loc`

CHANGES TO FILE NAMES
---------------------

* `rtofs_glo.t00z.n00.cice_inst` to `rtofs_glo.t00z.n00.cice_inst.nc`
* `rtofs_glo.t00z.f24.cice_inst` to `rtofs_glo.t00z.f24.cice_inst.nc`
* `rtofs_glo.t00z.f48.cice_inst` to `rtofs_glo.t00z.f48.cice_inst.nc`
* `rtofs_glo.t00z.f72.cice_inst` to `rtofs_glo.t00z.f72.cice_inst.nc`
* `rtofs_glo.t00z.f96.cice_inst` to `rtofs_glo.t00z.f96.cice_inst.nc`
* `rtofs_glo.t00z.f120.cice_inst` to `rtofs_glo.t00z.f120.cice_inst.nc`
* `rtofs_glo.t00z.f144.cice_inst` to `rtofs_glo.t00z.f144.cice_inst.nc`
* `rtofs_glo.t00z.f168.cice_inst` to `rtofs_glo.t00z.f168.cice_inst.nc`
* `rtofs_glo.t00z.f192.cice_inst` to `rtofs_glo.t00z.f192.cice_inst.nc`

CHANGES TO HPSS TARBALLS
------------------------
* TBD


PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * The entire RTOFS v2.4.0 package needs to be installed and tested on WCOSS-2
* Does this change require a 30-day evaluation?
  * Yes


JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes

DOCUMENTATION
-------------

* RTOFS V2.4.0 Implementation Kick-off Meeting Slides: https://docs.google.com/presentation/d/1axYq2Vr-FMt5wBx7pIBt5XAHZA-1MRt8T7v5H0cS6RA/edit#slide=id.g29ffeee96c9_0_1006

PREPARED BY
-----------
Dan.Iredell@noaa.gov
