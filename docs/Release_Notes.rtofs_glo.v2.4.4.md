RTOFS_GLO V2.4.4 RELEASE NOTES

-------
PRELUDE
-------

The RTOFS v2.4.0 is upgraded to RTOFS v2.4.4 with the following changes: 

* Add KEEPDATA logic in job scripts
* Add restart capability for either hardware or software failures. Archive and restart data is
saved in persistent directories and a restart will use this data to restart at the appropriate time.
* Remove obsolete codes (ncoda_graph)
* Script clean up for KEEPDATA, shebangs, etc

IMPLEMENTATION INSTRUCTIONS
---------------------------

```bash
cd $PACKAGEROOT
mkdir rtofs.v2.4.4
cd rtofs.v2.4.4
git clone -b release/v2.4.4 https://github.com/NOAA-EMC/RTOFS_GLO.git .
cd sorc
git clone -b release/v2.4.4 https://github.com/NOAA-EMC/NCODA.git rtofs_ncoda.fd
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

RUN INSTRUCTIONS
----------------

This implementation continues directly from rtofs.v2.3.4. There isn't a parallel run to start from.

VERSION FILE CHANGES
--------------------

* `versions/build.ver` - change PrgEnv-intel=8.3.3, craype=2.7.17, cray-mpich=8.1.19, cray-libsci=22.08.1.1
* `versions/run.ver` - change PrgEnv-intel=8.3.3, craype=2.7.17, cray-mpich=8.1.19, prod_envir=2.0.6, prod_util=2.0.14

SORC CHANGES
------------

* CODES
  * `rtofs_archv2netCDF.fd/bigrid.f`
  * `rtofs_ssmis_tol2.cd/mmablib/sorc/w3ft01.f` (removed - in w3 library)

* HYCOM
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e`
    * `hycom/blkdat.F`
    * `hycom/mod_hycom.F`

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

* None


JOBS/SCRIPTS/USH CHANGES
------------------------

* Script cleanup, restartability, bugzilla fixes
* Restart capability
  * `jobs/JRTOFS_GLO_ANALYSIS`
  * `jobs/JRTOFS_GLO_FORECAST_STEP1`
  * `jobs/JRTOFS_GLO_FORECAST_STEP2`
  * `jobs/JRTOFS_GLO_INCUP`
  * `parm/rtofs_glo.navy_0.08.anal.ice_in`
  * `parm/rtofs_glo.navy_0.08.fcst.ice_in`
  * `parm/rtofs_glo.navy_0.08.incup.ice_in`
  * `scripts/exrtofs_glo_analysis.sh`
  * `scripts/exrtofs_glo_forecast.sh`
  * `scripts/exrtofs_glo_incup.sh`
  * `ush/rtofs_tmp2com.sh`

* Script cleanup, bugzillas
  * `jobs/JRTOFS_GLO_ANALYSIS_PRE`
  * `jobs/JRTOFS_GLO_FORECAST_GRIB2_POST`
  * `jobs/JRTOFS_GLO_FORECAST_POST`
  * `jobs/JRTOFS_GLO_FORECAST_POST_2`
  * `jobs/JRTOFS_GLO_FORECAST_STEP1_PRE`
  * `jobs/JRTOFS_GLO_FORECAST_STEP2_PRE`
  * `jobs/JRTOFS_GLO_GZIP`
  * `jobs/JRTOFS_GLO_NCODA_GLBL_VAR`
  * `jobs/JRTOFS_GLO_NCODA_HYCOM_VAR`
  * `jobs/JRTOFS_GLO_NCODA_INC`
  * `jobs/JRTOFS_GLO_NCODA_POLAR_VAR`
  * `scripts/exrtofs_glo_post.sh`
  * `scripts/exrtofs_glo_post_2.sh`
  * `ush/rtofs_atmforcing_getges.sh`
  * `ush/rtofs_glo3z_6hrly.sh`
  * `ush/rtofs_glo3z_daily.sh`
  * `ush/rtofs_ncoda_amsr_qc.sh`
  * `ush/rtofs_ncoda_goes_qc.sh`
  * `ush/rtofs_ncoda_himawari_qc.sh`
  * `ush/rtofs_ncoda_ice_qc.sh`
  * `ush/rtofs_ncoda_jpss_qc.sh`
  * `ush/rtofs_ncoda_metop_qc.sh`
  * `ush/rtofs_ncoda_npp_qc.sh`
  * `ush/rtofs_ncoda_prep_ice.sh`
  * `ush/rtofs_ncoda_profile_qc.sh`
  * `ush/rtofs_ncoda_sfcobs_qc.sh`
  * `ush/rtofs_ncoda_ssh_qc.sh`
  * `ush/rtofs_ncoda_sss_qc.sh`
  * `ush/rtofs_ncoda_vel_qc.sh`
  * `ush/rtofs_surface_day1-3.sh`
  * `ush/rtofs_surface_day4-5.sh`
  * `ush/rtofs_surface_hcasts.sh`


FIX CHANGES
-----------

* None.

CHANGES TO FILE NAMES
---------------------

* None.

CHANGES TO HPSS TARBALLS
------------------------

* TBD


PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * The entire RTOFS v2.4.4 package needs to be installed and tested on WCOSS-2
* Does this change require a 30-day evaluation?
  * Yes


JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes


DCOM DEPENDENCIES
-----------------
All dcom data is data of opportunity.

DOCUMENTATION
-------------

* RTOFS V2.4.0 Implementation Kick-off Meeting Slides: https://docs.google.com/presentation/d/1axYq2Vr-FMt5wBx7pIBt5XAHZA-1MRt8T7v5H0cS6RA/edit#slide=id.g29ffeee96c9_0_1006

PREPARED BY
-----------
Dan.Iredell@noaa.gov
