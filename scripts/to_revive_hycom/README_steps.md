# Two approaches to revive [HYCOM](https://github.com/HYCOM/HYCOM-src) ocean are provided here. Create:
  A. A restart from [ESPC-D output.](https://data.hycom.org/datasets/ESPC-D-V02/data/archm/2025/)
  B. An archive to apply an increment that has been created from the [ESPC-D output.](https://data.hycom.org/datasets/ESPC-D-V02/data/archm/2025/)

## For either option to work, 
- First build [hycom-tools;](https://github.com/HYCOM/HYCOM-tools) 
  [following these instructions.](https://github.com/NOAA-EMC/RTOFS_GLO/wiki/Build-instructions#to-build-hycom-tools)

- Download ESPC-D daily mean archive for e.g., 2025/04/01: 
  - [.a file](https://data.hycom.org/datasets/ESPC-D-V02/data/archm/2025/US058GCOM-OPSnce.espc-d-031-hycom_fcst_glby008_2025040112_M0000_archm.a); it is about `15GB`. 
  - [corresponding .b file](https://data.hycom.org/datasets/ESPC-D-V02/data/archm/2025/US058GCOM-OPSnce.espc-d-031-hycom_fcst_glby008_2025040112_M0000_archm.b)

# A. Create a restart
  1. Convert daily mean archive from ESPC-D to RTOFS (GLBb0.08) bathymetry, grid, coastline using: `run_isubaregion.csh`
     - If not already, build `HYCOM_tools.fd/subregion/src/isubaregion`. Edit `subregion/src/Make_ncdf.csh`, add `isubaregion` to what gets built.
       ```
       foreach m ( isubs_field isubs_count isubaregion)
       ```
     - Edit settings in `run_isubaregion.csh`, lines below `# -- Edit following --`


 
