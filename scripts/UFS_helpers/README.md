# What's here?
  Scripts that aid in transitioing from RTOFS **v2.5** that uses:
  - [HYCOM.](https://github.com/NOAA-EMC/HYCOM-src)
  - [CICE4.](https://www2.cesm.ucar.edu/models/ccsm4.0/cice/doc/index.html)  

to a _future_ version (**v3.0**) that would use:

  - [MOM6.](https://github.com/NOAA-EMC/MOM6)
  - [CICE6.](https://github.com/NOAA-EMC/CICE)

# Brief Description:

| File name | Brief description |
| :--       | --: |
| `bin2nc_inc_fld.sh` | Main script to convert binary formatted increment file or full field to netcdf |
| `convert_2d_archive_2nc.py` | Script to convert archive (2d fields: archs) to netcdf formatted file |
| `config_archive_to_nc.yaml` | Example configuration for `2d_archive_nc_plot.py` |
| `convert_bin_inc_to_nc.py` | Script that is called by the bin2nc_inc_fld.sh |
| `set_py_modules.sh` | Set up python modules (on wcoss-2, those maintained within [EVS](https://github.com/NOAA-EMC/EVS)) |
| `rtofs_hpss_path.sh` | Function that returns path to RTOFS output on HPSS (tape archive) | 
| `get_data_from_hpss.sh` | Script to fetch (archive) files from HPSS |
| `hycom_wind_ymdh.py` | To Convert time stamp in hycom archive [b-file] to yyyy/mm/dd:hh |
| `utils*.py` | Functions that do (all the) work |
| | |
| `diagnostics_global.py` | Calculate global mean and standard deviation of a 2-d field, optionally save plot. |

# Example usage:

- `./bin2nc_inc_fld.sh`: Echoes example usage.

- `./convert_bin_inc_to_nc.py -h`: Echoes example usage; must have python modules loaded (use `set_py_modules.sh`)
  - Note that it has some defaults and required fields.

- `convert_2d_archive_2nc.py -h`: Echoes example usage; must have python modules loaded (use `set_py_modules.sh`).
  - Note that the configuration is set via yaml file, for e.g., `config_archive_to_nc.yaml`

- To convert `ice temperature` and `field`:
  `./bin2nc_inc_fld.sh dwood /lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR1/prod/com/rtofs/v2.5/ 20250331 icetmp fld`

- To convert `ice coverage` and `increment`:
  `./bin2nc_inc_fld.sh dwood /lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR1/prod/com/rtofs/v2.5/ 20250331 icecov inc`

- Same as above, but from RTOFS v2.4:
  `./bin2nc_inc_fld.sh dwood /lfs/h1/ops/prod/com/rtofs/v2.4/ 20250401 icecov fld`

- To get files from HPSS:
  `./get_data_from_hpss.sh 2025-04-03 2 v2p4  /lfs/h2/emc/ptmp/santha.akella/data/rtofs rtofs_glo.t00z.n00.archs.`

- To calculate global mean and standard deviation and optionally plot:
  - Statistics will be saved to an ASCII file in the output path, see defauls:
    - `./diagnostics_global.py -h`
    - `./diagnostics_global.py --data_file /lfs/h2/emc/ptmp/santha.akella/data/arch2nc/v2p4_SSS_2025-04-01T00\:00.nc --varName SSS`
  - To save plot (default is not to save plot): 
    - `./diagnostics_global.py --data_file /lfs/h2/emc/ptmp/santha.akella/data/arch2nc/v2p4_SSS_2025-04-01T00\:00.nc --varName SSS --gen_plot`

**Note**:
  - On WCOSS-2, plotting with cartopy if failed once, will keep on failing. Unfortunately there is no way to install software!
    - Work around: `scp` data elsewhere, the same script will work!
