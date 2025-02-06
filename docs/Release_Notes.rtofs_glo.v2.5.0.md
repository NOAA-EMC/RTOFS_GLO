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
* Bugzillas fixed, most notably cleaning up crashes when the debug flags are on (e.g. check all, ftrapuv)


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

* LIBS
  * `libs/build_libs.sh`
  * `libs/sorc/rtofs_hycomiot/makefile`
  * `libs/sorc/rtofs_mpi_mods/Makefile`

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
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/comp_ice.csh`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/comp_ice.debug.csh`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/hycom/geopar.F`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/hycom/mod_cb_arrays.F`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/hycom/mod_momtum.F`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/hycom/mod_tsadvc.F`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/hycom/prtmsk.f`
  * `sorc/rtofs_hycom.fd/src_2.2.99DHMTi-dist2B_relo_cice_v4.0e/source/ice_forcing.F90`

* NCODA
  * `sorc/rtofs_ncoda.fd/ncoda_decode/config.user`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/include/coda_types.h`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/alarm/rd_ice_alrm.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/alarm/rd_prf_alrm.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/alarm/rd_sfc_alrm.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/alarm/rd_ssh_alrm.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/alarm/rd_sss_alrm.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/alarm/rd_sst_alrm.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/alarm/rd_vel_alrm.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/bufr/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/bufr/prof_decode.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/bufr/saild_decode.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/bufr/saild_hdr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/bufr/saild_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/bufr/saldrn_decode.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/bufr/saldrn_hdr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/bufr/saldrn_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/ssh/rd_adt.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/sss/rd_smap_hdf.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/sst/acspo_bin.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/sst/acspo_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/sst/rd_acspo.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/libsrc/sst/rd_geo_loc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/src/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/src/ncoda_acspo_sst_nc/ncoda_acspo_sst_nc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/src/ncoda_adt_ssh_nc/ncoda_adt_ssh_nc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/src/ncoda_alarm/ncoda_alarm.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/src/ncoda_bufr_decode/ncoda_bufr_decode.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/src/ncoda_drft_decode/ncoda_drft_decode.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/src/ncoda_hf_radar_nc/ncoda_hf_radar_nc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/src/ncoda_ncep_ice_nc/ncoda_ncep_ice_nc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_decode/src/ncoda_sat_sss_nc/ncoda_sat_sss_nc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/config.user`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/include/coda_types.h`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/graph/lbl_axis.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/graph/map_buoy_vel.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/graph/map_qc_hst.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/graph/map_qc_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/graph/map_qc_prof.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/graph/map_qc_vel.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/ice/ice_report.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/modas/modas_prof.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/modas/modas_qc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/modas/modas_salt.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/cr_vol.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/ld_raw_prof.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/prof_argo.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/prof_dupchk.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/prof_repl.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/prof_report.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/prof_salt.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/prof_sign.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/prof_wmo.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/qc_prof.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/rd_prof_db.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/wr_prof_appnd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/wr_qc_prof.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/prfobs/xval_anl.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/ld_raw_saild.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/ld_raw_saldrn.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/qc_saild.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/qc_saldrn.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/saild_qc_dma.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/saild_report.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/saldrn_qc_dma.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/saldrn_report.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/wr_qc_saild.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/wr_qc_saldrn.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/wr_saild_appnd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sfcobs/wr_saldrn_appnd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/ssh/ld_raw_ssh.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/ssh/qc_ssh.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/ssh/rd_ssh_db.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/ssh/rd_ssh_mean.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/ssh/ssh_report.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/ssh/wr_qc_ssh.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/ssh/wr_ssh_appnd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sss/sss_report.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sst/ld_raw_sst.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sst/qc_sst.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sst/sst_report.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/sst/wr_qc_sst.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/util/goes_code.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/util/init_ocnqc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/util/msg_code.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/util/viirs_code.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/libsrc/velobs/vel_report.f`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/src/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_qc/src/ncoda_qc/ncoda_qc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/config.user`
  * `sorc/rtofs_ncoda.fd/ncoda_var/include/adjnl.h`
  * `sorc/rtofs_ncoda.fd/ncoda_var/include/coda_types.h`
  * `sorc/rtofs_ncoda.fd/ncoda_var/include/ensmnl.h`
  * `sorc/rtofs_ncoda.fd/ncoda_var/include/oanl.h`
  * `sorc/rtofs_ncoda.fd/ncoda_var/include/stmt_fns_SIGM2_17term.h`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/adj_map.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/coamps_grid.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/coda_adj.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/coda_adj_post.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/coda_adj_var.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/grd_adj_fld.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/grd_adj_ice.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/grd_adj_sfc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/grd_adj_vol.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/mdl_adj.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/pre_mult.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/prep_adj_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/rd_adj_grd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/rd_rossby.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/set_adj_hcorr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/vol_adj_rpt.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/adj/wr_adj_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/archv/`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/archv/grid_mod.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/archv/hycom_archv.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/archv/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/archv/rd_archv.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/coda/coda_driver.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/coda/coda.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/coda/coda_var.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/coda/corr_model.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/coda/covar_grd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/coda/jmin_diag.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/coda/post_mult.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/coda/rd_inv_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/coda/rms_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/coda/rsd_vctr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/coda/wr_incr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/asn_vol.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/coda_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/covar_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/covar_grd_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/cr_vol_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/cr_vol.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/cr_vol_init.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/cr_vol_ovr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/driver_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/err_anl.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/err_conf.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/err_fld.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/grd_vol.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/ncoda_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/post_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/rd_hdr_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/rd_inv_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/reduce_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/set_hcr_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/solve_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/split_vols_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/vol_def_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/vol_save_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/vol_wgts.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/err/wr_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/coda_et.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/ensm_et.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/ensm_et_nc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/et_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/et.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/et_lyr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/et_lyr_inc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/et_model_dp.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/et_model.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/etmpi.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/et_mpio.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/et_proc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/et_prs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/et_smth.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/model_et.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/model_glb.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/model_mem.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/rw_ens_mem.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/et/set_mem_name.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/hybrid/coda_hyb.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/eq_bias.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/hycom_dp.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/hycom_fcst.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/hycom_ice_upd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/hycom_lyr_inc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/hycom_lyr_prs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/hycom_node_msk.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/hycom_upd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/lvl_lyr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/lyr_prs_smth.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/model_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/mom_fcst.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/mom_ice_upd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/mom_lyr_inc.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/mom_lyr_prs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/mom_upd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/ncoda_upd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/post_analysis.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/postmpi.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/post_mpio.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/smth_lyr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/vrfy_anl.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/post/vrfy_fcst.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/coda_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/cr_ssh_file.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/drct_baln.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/drct_fcst.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/drct_fld.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/drct_lyr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/drct_mld_adj.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/drct_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/drct_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/drct_trck.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/ensm_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/ensm_grd.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/ensm_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/ensm_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/geoptl_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/grdnt_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/ice_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/innov_chk.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/innov_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/lyrp_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/mass_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/mv_derive.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/mv_geoptl.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/mv_innov.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/mv_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/mv_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/mv_vcorr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/ocn_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/ocn_sfc_ht.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/prepmpi.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/prof_fld.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/prof_moor.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/prof_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/prof_pool.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/prs_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/rd_mv_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/rd_obs_data.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/rd_prof.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/rd_ssh_anl.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/rd_ssh.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/rd_ssh_mean.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/rd_vel_anl.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/rd_vel.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/remove_vel.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/salt_baln.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/sfc_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/ssh_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/sss_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/sst_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/super_ob.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/ts_corr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/ts_static.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/velc_obs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/prep/wr_inv_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/chk_hafs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/chk_ssh.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/dyn_hght.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/grd_clim.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/hycom_fcst.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/hycom_fgat.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/hycom_flds.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/hycom_init.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/hycom_priors.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/hycom_prs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/mom_fgat.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/mom_flds.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/mom_grid.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/mom_init.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/mom_priors.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/mom_prs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/ncoda_flds.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/ncoda_init.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/ncoda_priors.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/setup/rd_vsgm.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/share/datao_init.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/share/fld1_trp.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/share/hycom_prs.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/share/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/share/rd_lnd_dst.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/share/salt_corr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/share/ts_static.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/share/uv_stagger.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/libsrc/share/uv_stgr.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/src/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/src/ncoda_adj/ncoda_adj.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/src/ncoda_archv/`
  * `sorc/rtofs_ncoda.fd/ncoda_var/src/ncoda_archv/Makefile`
  * `sorc/rtofs_ncoda.fd/ncoda_var/src/ncoda_archv/ncoda_archv.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/src/ncoda_err/ncoda_err.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/src/ncoda_et/ncoda_et.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/src/ncoda_post/ncoda_post.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/src/ncoda_prep/ncoda_prep.f`
  * `sorc/rtofs_ncoda.fd/ncoda_var/src/ncoda_setup/ncoda_setup.f`


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
  * The entire RTOFS v2.5.0 package needs to be installed and tested on WCOSS-2
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
