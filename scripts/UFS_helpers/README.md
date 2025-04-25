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
| `convert_bin_inc_to_nc.py` | Script that is called by the bin2nc_inc_fld.sh |
| `set_py_modules.sh` | Set up python modules (on wcoss-2, those maintained within [EVS](https://github.com/NOAA-EMC/EVS)) |
| `hycom_wind_ymdh.py` | To Convert time stamp in hycom archive [b-file] to yyyy/mm/dd:hh |
| `utils.py` | Functions that do (all the) work |

# Example usage:

- `./bin2nc_inc_fld.sh`: Echoes example usage.

- `./convert_bin_inc_to_nc.py -h`: Echoes example usage; must have python modules loaded (use `set_py_modules.sh`)
  - Note that it has some defaults and required fields.

- To convert `ice temperature` and `field`:
  `./bin2nc_inc_fld.sh dwood /lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR1/prod/com/rtofs/v2.5/ 20250331 icetmp fld`

- To convert `ice coverage` and `increment`:
  `./bin2nc_inc_fld.sh dwood /lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR1/prod/com/rtofs/v2.5/ 20250331 icecov inc`

- Same as above, but from RTOFS v2.4:
  `./bin2nc_inc_fld.sh dwood /lfs/h1/ops/prod/com/rtofs/v2.4/ 20250401 icecov fld`
