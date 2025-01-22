RTOFS_GLO V2.5.0 RELEASE NOTES

-------
PRELUDE
-------

The RTOFS v2.4.4 is upgraded to RTOFS v2.5.0 with the following changes:

* Added climatological constraints for temperature and salinity for SSH assimilation in the analysis (to reduce subsurface negative salinity bias in the Caribbean Sea)
* Modifications in post-processing of the assimilation to provide to the ocean model increments on HYCOM hybrid layers versus the depth-fixed layers, to reduce the error during mapping of the increments onto the HYCOM vertical grid
* Added 2DVAR assimilation of SSH for verification of ocean front and eddy feature locations
* Added capability for automatic rejection of problematic buoy data in realtime
* New data sources MeteoSat (2nd and 3rd generation), NOAA-21, SWOT
* Bugzilla fixed, most notably cleaning up crashes with debug flags on


IMPLEMENTATION INSTRUCTIONS
---------------------------

```bash
cd $PACKAGEROOT
mkdir rtofs.v2.5.0
cd rtofs.v2.5.0
git clone -b release/v2.5.0 https://github.com/NOAA-EMC/RTOFS_GLO.git .
cd sorc
git clone -b release/v2.5.0 https://github.com/NOAA-EMC/NCODA.git rtofs_ncoda.fd
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
cd /lfs/h2/emc/eib/noscrub/dan.iredell/RTOFSFIX/20241112
tar cf - ./fix | (cd <topdir>/rtofs.v2.4.0; tar xf -)
```

RUN INSTRUCTIONS
----------------

The current parallel is in /lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR1/prod/com/rtofs/v2.5. Repace the current
production with the run in this directory.

VERSION FILE CHANGES
--------------------

* `versions/build.ver` - update to bufr/12.0.1, g2/3.5.1
* `versions/run.ver` - update to bufr_dump/1.2.1, grib_util/1.2.4

SORC CHANGES
------------

* CODES
  * `sorc/rtofs_code.fd/build_code.sh`
  * `sorc/rtofs_code.fd/rtofs_archv2netCDF.fd/zebra.f`
  * `sorc/rtofs_code.fd/rtofs_archv2netCDF.fd/zh_sun.f`
  * `sorc/rtofs_code.fd/rtofs_hycom2raw8.fd/makefile`
  * `sorc/rtofs_code.fd/rtofs_hycom_diff.fd/bigrid.f`
  * `sorc/rtofs_code.fd/rtofs_hycom_diff.fd/makefile`
  * `sorc/rtofs_code.fd/rtofs_hycom_expr.fd/makefile`
  * `sorc/rtofs_code.fd/rtofs_hycom_extract.fd/makefile`
  * `sorc/rtofs_code.fd/rtofs_ncoda_archv_inc.fd/Makefile`
  * `sorc/rtofs_code.fd/rtofs_ncoda_archv_inc.fd/makefile`
  * `sorc/rtofs_code.fd/rtofs_ncoda_archv_inc.fd/ncoda_archv_lyrinc.f`
  * `sorc/rtofs_code.fd/rtofs_ncoda_archv_inc.fd/zebra.f`
  * `sorc/rtofs_code.fd/rtofs_raw2hycom.fd/makefile`
  * `sorc/rtofs_code.fd/rtofs_ssmis_tol2.cd/ssmisu_decode.f`

* HYCOM
  * `sorc/rtofs_hycom.fd/build_hycom.sh`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/comp_ice.debug.csh`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/hycom/geopar.F`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/hycom/mod_cb_arrays.F`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/hycom/mod_momtum.F`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/hycom/mod_tsadvc.F`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/hycom/prtmsk.f`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/source/ice_forcing.F90`

* NCODA
  * `from v3.10 to v3.20`

SCRIPTS/USH/PARM CHANGES
------------------------

  * `gempak/gempak.sh`
  * `scripts/exrtofs_glo_gempak.sh`
  * `scripts/exrtofs_glo_incup.sh`
  * `scripts/exrtofs_glo_ncoda_glbl_var.sh`
  * `scripts/exrtofs_glo_ncoda_hycom_var.sh`
  * `scripts/exrtofs_glo_ncoda_inc.sh`
  * `scripts/exrtofs_glo_ncoda_polar_var.sh`
  * `scripts/exrtofs_glo_ncoda_qc.sh`
  * `ush/rtofs_ncoda_amsr_qc.sh`
  * `ush/rtofs_ncoda_goes_qc.sh`
  * `ush/rtofs_ncoda_himawari_qc.sh`
  * `ush/rtofs_ncoda_ice_qc.sh`
  * `ush/rtofs_ncoda_jpss_qc.sh`
  * `ush/rtofs_ncoda_metop_qc.sh`
  * `ush/rtofs_ncoda_msg_qc.sh`
  * `ush/rtofs_ncoda_npp_qc.sh`
  * `ush/rtofs_ncoda_profile_qc.sh`
  * `ush/rtofs_ncoda_sfcobs_qc.sh`
  * `ush/rtofs_ncoda_ssh_qc.sh`
  * `ush/rtofs_ncoda_sss_qc.sh`
  * `ush/rtofs_ncoda_vel_qc.sh`
  * `parm/rtofs_glo.glbl.oanl.in`
  * `parm/rtofs_glo.hycom.oanl.in`
  * `parm/rtofs_glo.navy_0.08.anal.blkdat.input`
  * `parm/rtofs_glo.navy_0.08.anal.ice_in`
  * `parm/rtofs_glo.navy_0.08.fcst.blkdat.input`
  * `parm/rtofs_glo.navy_0.08.fcst.ice_in`
  * `parm/rtofs_glo.navy_0.08.incup.blkdat.input`
  * `parm/rtofs_glo.navy_0.08.incup.ice_in`
  * `parm/rtofs_glo.ncoda_archv_lyr.input`
  * `parm/rtofs_glo.polar.oanl.in`


FIX CHANGES
-----------

* new file codaclim/HYCOM.node_mask to mask out Sulu Sea
* updated file codaclim/ABI_G18.loc to remove NaNs

CHANGES TO FILE NAMES
---------------------

* None.

CHANGES TO HPSS TARBALLS
------------------------

* None.


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
