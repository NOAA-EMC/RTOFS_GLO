# What's here?
  - Scripts that process NCODA output used in RTOFS.


# Brief Description:

| File name | Brief description | Notes |
| :-- | :-- | :-- |
| `obs_stat.sh` | Main driver for daily RTOFS observation processing | Orchestrates COM directory scanning, output archiving, and cleanup. |
| `convert_ncoda_binary_qc_obs.sh` | Converts NCODA binary QC files to NetCDF | Interfaces with Python utilities to generate standardized ocean obs. |
| `audit_ncoda_obs.sh` | Generates CSV summary of observation counts | Parses filenames for platform (Field 2/3) and extracts `nobs` from NetCDF. |
| `bin2nc_inc_fld.sh` | Low-level binary conversion utility | Used by the conversion driver to interface with NCODA binaries. |
| `convert_bin_inc_to_nc.py` | Python back-end for NetCDF generation | Handles the mapping of NCODA binary structures to NetCDF4; called by the `bin2nc_inc_fld.sh`. |

# Example usage:

- `./obs_stat.sh`
   - Notes: Main entry point. Uses `current_date` (internal) to trigger the full pipeline.
     - `00 00 * * * /path/to/obs_stat.sh`
     - Automated via Crontab (requires `SHELL=/bin/bash -l`).

- `./convert_ncoda_binary_qc_obs.sh 20260321 /path/to/output/20260321`  
   - Notes: Arguments are `<rtofs_date>` and `<oPath>`.
     - It iterates through obs types (SST, SSS, etc.) and uses `read_binary_qc_obs.x` converter for each binary found in the COM directory.
     - Can be run standalone.

- `./audit_ncoda_obs.sh 20260321 /path/to/output/20260321`  
   - Notes: Arguments are `<rtofs_date>` and `<oPath>`. Generates the `obs_counts_YYYYMMDD.csv` summary.
     - Can be run standalone to re-generate statistics for an existing directory.

- `./convert_bin_inc_to_nc.py -h`: Echoes example usage; must have python modules loaded (use `set_py_modules.sh`)
  - Note that it has some defaults and required fields.

---

- `./bin2nc_inc_fld.sh dwood /lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR1/prod/com/rtofs/v2.5/ 20250331 icetmp fld`


# Housekeeping:
- The `obs_stat.sh` script automatically removes archive directories older than 30 days 
  using the `$oPath` parent directory.
