# Two approaches to revive [HYCOM](https://github.com/HYCOM/HYCOM-src) ocean are provided here (in this directory).

  A. A restart from [ESPC-D output.](https://data.hycom.org/datasets/ESPC-D-V02/data/archm/2025/)

  B. An archive to apply an increment that has been created from the [ESPC-D output.](https://data.hycom.org/datasets/ESPC-D-V02/data/archm/2025/)

## Both of the above rely on [hycom-tools](https://github.com/HYCOM/HYCOM-tools) 
- Therefore, first step is to build it; if needed, 
  see [these instructions.](https://github.com/NOAA-EMC/RTOFS_GLO/wiki/Build-instructions#to-build-hycom-tools)

- Download ESPC-D daily mean archive for e.g., 2025/04/01: 
  - [.a file](https://data.hycom.org/datasets/ESPC-D-V02/data/archm/2025/US058GCOM-OPSnce.espc-d-031-hycom_fcst_glby008_2025040112_M0000_archm.a). **Note**: it is about `15GB`. 
  - [Corresponding .b file](https://data.hycom.org/datasets/ESPC-D-V02/data/archm/2025/US058GCOM-OPSnce.espc-d-031-hycom_fcst_glby008_2025040112_M0000_archm.b)

# A. Create a restart
  1. Use: `run_isubaregion.csh` to convert daily mean archive from ESPC-D to RTOFS (GLBb0.08) bathymetry (coastline), grid.
     - If not already, build `HYCOM_tools.fd/subregion/src/isubaregion` by editing: `subregion/src/Make_ncdf.csh`, add `isubaregion` to what gets built.
       ```
       foreach m ( isubs_field isubs_count isubaregion)
       ```
     - Same as above, but for: `hycom_wind_date_LinuxAIF` and `hycom_ymdh_wind_LinuxAIF` by editing: `bin/Make_ncdf.csh`, e.g.:
       ```
       foreach f ( wind_stat_nc wind_stat_range_nc hycom_ymdh_wind hycom_wind_date)
       ```
     - Edit settings in `run_isubaregion.csh`, lines below `# -- Edit following --`
     - Run this script on a node: `qsub run_isubaregion.csh`.
       - Check error, output logfiles: `convert_GLB_y_b_0.08.e` and `convert_GLB_y_b_0.08.o` respectively.
       - In the output dir (set in `run_isubaregion.csh`), check if `*_archm_*` files have been created.


 
