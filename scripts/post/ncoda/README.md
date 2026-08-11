# What's here?
  - Scripts that process NCODA output used in RTOFS.
  - NCODA Observation Statistics Dashboard
    - Automates the extraction and visualization of cost function minimums (`Jmin`) and observation counts (`N`).
    - From NCODA `hycom_var` log files. It generates a multi-panel time-series dashboard for a specified date range.
  - NCODA Verification (Innovation) Statistics Dashboard
    - Automates the extraction and visualization of forecast/analysis RMS Errors and Mean Bias metrics.
    - Parses "Analysis Verification" blocks from `ncoda_hycom_var.OUTPUT.*` logs to generate time-series and mean vertical profile (`oanl` grid) plots.

# Brief Description:

| File name | Brief description | Notes |
| :-- | :-- | :-- |
| `obs_stat.sh` | Main driver for daily RTOFS observation processing | Orchestrates COM directory scanning, output archiving, and cleanup. |
| `convert_ncoda_binary_qc_obs.sh` | Converts NCODA binary QC files to NetCDF | Interfaces with Python utilities to generate standardized ocean obs. |
| `audit_ncoda_obs.sh` | Generates CSV summary of observation counts | Parses filenames for platform (Field 2/3) and extracts `nobs` from NetCDF. |
| `bin2nc_inc_fld.sh` | Low-level binary conversion utility | Used by the conversion driver to interface with NCODA binaries. |
| `convert_bin_inc_to_nc.py` | Python back-end for NetCDF generation | Handles the mapping of NCODA binary structures to NetCDF4; called by the `bin2nc_inc_fld.sh`. |
| `plot_obs_stat.py` | Generates time-series plots from observation count CSV files | Plots counts over time for a specified platform (e.g., PROFILE) and outputs a PNG image. Requires Python, pandas, and matplotlib. |
| `driver_jmin_stats.sh` | Top-level execution script. Iterates through the requested date range, calls the extraction script, loads the WCOSS2 Python environment, and triggers the plotter. | |
| `jmin_stats.sh` | Core extraction engine. Parses daily `hycom_var.<YYYYMMDD00>.out` logs, extracts `Jmin` and `N` values, and outputs daily CSV files (`jmin_<YYYYMMDD>.csv`). | |
| `plot_jmin_comp.py` | Python visualization script. Reads the CSVs and the YAML config to generate `dashboard_jmin_stats.png` with dynamically scaled axes. | |
| `plot_jmin_config.yaml` | User-defined configuration file. | Specifies the `Category` and `Metric` combinations to be plotted. Also holds the `comparison` block mapping experiment names to data paths for `compare_jmin_stats.py`. |
| `compare_jmin_stats.py` | Python visualization script for multi-experiment comparisons. | Reads CSVs from multiple experiment paths defined in the YAML and plots them together for R&D evaluation. |
| `innov_stats.sh` | Top-level execution wrapper for daily NCODA verification stats. | Calls the extraction script for the current date, loads the WCOSS2 Python environment, and triggers the innovation plotter. Designed for crontab. |
| `get_innov_stats.sh` | Core verification extraction engine. | Uses `awk` to parse "Analysis Verification" blocks from `OUTPUT` files, generating daily CSVs for variables (ice, temp, salt, geo) and depth profiles (`t_z`, `s_z`, `geo_z`). |
| `plot_innov.py` | Python visualization script for innovation stats. | Reads verification CSVs and the YAML config to generate time-series plots (RMS & Bias) and time-mean vertical depth profiles. |
| `plot_innov_config.yaml` | User-defined configuration file for verification plots. | Specifies variable prefixes, target columns (`ObsType` vs `Depth`), and specific platform/depth targets to plot. |

# Example usage:

- `./obs_stat.sh`
   - Notes: Main entry point. Uses `current_date` (internal) to trigger the full pipeline.
     - `00 00 * * * /path/to/obs_stat.sh`
     - Automated via Crontab (requires `SHELL=/bin/bash -l`).
     - Housekeeping:
       - Automatically removes archive directories older than 30 days using the `$oPath` parent directory, retains all else but `csv` files.

- `./convert_ncoda_binary_qc_obs.sh 20260321 /path/to/output/20260321`  
   - Notes: Arguments are `<rtofs_date>` and `<oPath>`.
     - It iterates through obs types (SST, SSS, etc.) and uses `read_binary_qc_obs.x` converter for each binary found in the COM directory.
     - Can be run standalone.

- `./audit_ncoda_obs.sh 20260321 /path/to/output/20260321`  
   - Notes: Arguments are `<rtofs_date>` and `<oPath>`. Generates the `obs_counts_YYYYMMDD.csv` summary.
     - Can be run standalone to re-generate statistics for an existing directory.

- `./convert_bin_inc_to_nc.py -h`: Echoes example usage; must have python modules loaded (use `set_py_modules.sh`)
  - Note that it has some defaults and required fields.

- `./plot_obs_stat.py /path/to/archive_base_path PROFILE`
  - Notes: Arguments are `<archive_base_path>` and `<obs_plat>`.
    - The `<archive_base_path>` must be the root directory containing the `YYYYMMDD` subdirectories with the CSVs.
    - The `<obs_plat>` is the specific platform you want to plot (e.g., `GOES`, `METOP`, `PROFILE`) and is case-insensitive.
    - Saves a time-series plot as a PNG image (e.g., `timeseries_goes_sst_count.png`) directly in the `<archive_base_path>`.
    - Must have Python modules loaded before running (e.g., `module load intel ve/hafs`).

- `./driver_jmin_stats.sh <dir> <start_date_YYYYMMDD> <end_date_YYYYMMDD> <output_path>`
  - Notes: Run the driver script with the base RTOFS directory, start date, end date, and your desired output path for the CSVs and PNG.
    - Example, filled with values: `./driver_jmin_stats.sh /lfs/h1/ops/prod/com/rtofs/v2.5 20260324 20260330 /lfs/h2/emc/stmp/${USER}/ops`

- `./compare_jmin_stats.py -c plot_jmin_config.yaml -o /path/to/save/comparisons/`
  - Notes: Generates multi-experiment overlay plots for development.
    - Requires the `comparison` block in `plot_jmin_config.yaml` to be populated with `- experiment: <name>` and `  path: <dir_path>`.
    - Must have Python modules loaded before running (e.g., `module load intel ve/hafs`).

- `./innov_stats.sh`
  - Notes: Main wrapper for verification stats. Pulls today's date automatically and processes everything.
    - Example: `./innov_stats.sh`

- `./get_innov_stats.sh [rtofs_version] [run_dir_date] [output_path]`
  - Notes: Can be run standalone to extract CSVs for a specific date/version.
    - Example: `./get_innov_stats.sh v2.5 20260811 /path/to/output`

- `./plot_innov.py plot_innov_config.yaml`
  - Notes: Generates individual PNG time-series plots for RMS and Bias based on YAML config targets, plus a time-mean vertical profile plot.
    - Must have Python modules loaded before running (e.g., `module load intel ve/rtofs`).

---

- `./bin2nc_inc_fld.sh dwood /lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR1/prod/com/rtofs/v2.5/ 20250331 icetmp fld`
