# RTOFS_GLO V2.5.5 RELEASE NOTES

## Why `v2.5.5`? 

- On June 06, 2026, the forecast step-2 of RTOFS failed in a new way, 
  see [this issue](https://github.com/NOAA-EMC/RTOFS_GLO/issues/140) for details.
  - The sea ice model, namely CICE4 failed to converge and hence the run failed to satisfy
    preset (hardcoded) criteria.
  - It happened during a period of rapid melt over very thin ice (less than 10 cm thick) with no snow.
- What is provided in v2.5.5 is an emergency gateway that would lift the strict criteria for the 
  ice thermodynamics, due to the following reasons:
  - The development of this version of the sea ice model (CICE4) ceased more than a decade ago.
  - RTOFS v3 is in the works, which will use the latest components from the UFS-Weather-Model,
    specifically CICE6.
  - If the RTOFS job fails for the same reason (see below), then follow the special instructions (provided below)
    to let it finish the job and then, one can revert to the strict checks.

## Methodology
1. Encode a namelist boolen flag: `loose_ferrmax`, whose default is .false.

   **Special instructions**
   ---
2. Whenever the RTOFS jobs (jrtofs_global_analysis, jrtofs_global_forecast_step1, jrtofs_global_forecast_step2) 
   fail with error message that contains both: `Thermo iteration does not converge` **and** `Flux conservation error`:
   - Clean up any restarts (from failed run), follow [item 2 of these instructions.](https://www2.pmb.ncep.noaa.gov/wiki/index.php/Special_Procedures#RTOFS)
   - Modify the parameter file: 
     pkg_dir=`/lfs/h1/ops/prod/packages/rtofs_glo.${current_version}/parm/`,
     ${pkg_dir}/rtofs_glo.navy_0.08.anal.ice_in **or** ${pkg_dir}/rtofs_glo.navy_0.08.fcst.ice_in,
     depending on whether it is analysis or forecast_step (1 or 2) that failed.
     Of course, save the original versions before modification.
   - The modification is to simply add the boolen flag set to **.true** as shown below **no** other changes:
```
...
...
&ice_nml
...
...
  , loose_ferrmax = .true.
/
&icefields_nml
    f_tmask     = .false.
...
...
```
3. There should be **no** need to change wallclock time for the job.
4. After the job finishes successfully, restore the parameter files (in the above step 2).


## What has changed?

Everything is same as in 
the [RTOFS_GLO V2.5.0 RELEASE NOTES,](https://github.com/NOAA-EMC/RTOFS_GLO/blob/develop/docs/Release_Notes.rtofs_glo.v2.5.0.md)
except the following:

1. Substitue `v2.5.0` with `v2.5.5`.
   (SORC CHANGES)
2. `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/source/ice_init.F9`
3. `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/source/ice_therm_vertical.F90`
4. `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/bufr/ prof_decode.f` 
    - This item (number 4): changes the value of `parameter MX_LVL` from `6000` to `10000`.
    - It address cases when the (subsurface) profile data has > 6000 values.


## Building RTOFS:

### 1. Check out components

$ git clone -b release/v2.5.5 git@github.com:NOAA-EMC/RTOFS_GLO.git

While in /sorc folder (require access to NCODA repository):
```
$ git clone -b release/v2.5.0 git@github.com:NOAA-EMC/NCODA.git rtofs_ncoda.fd
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
