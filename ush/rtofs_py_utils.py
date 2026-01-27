#!/usr/bin/env python3

"""
Helper python utilities
"""

import os
import sys
import xarray as xr
import numpy as np

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

# --------------------------------------------------------------------------- #
# GLOBAL CONFIGURATION
# --------------------------------------------------------------------------- #
FILL_VALUE = -9.99e+33
NCODA_SPVAL_THRESHOLD = -100.0
PLOT_DPI = 60  # Low DPI for faster diagnostic previews

def var_atts(vName):
    """
    Returns (scale_factor, units, long_name, cmap) based on variable name.
    """
    scale = 1.0
    units = "m"
    long_name = vName
    cmap = "viridis"

    if vName == "icecov":
        scale = 0.01
        units = "fraction"
        long_name = "Ice Concentration"
        cmap = "Blues"
    elif vName == "icethk":
        units = "m"
        long_name = "Ice Thickness"
        cmap = "YlGnBu_r"
    elif vName == "icetmp":
        units = "degC"
        long_name = "Ice Temperature"
        cmap = "RdYlBu_r"
    elif vName == "mixlyr":
        units = "m"
        long_name = "Mixed Layer Depth"
        cmap = "plasma"

    return scale, units, long_name, cmap

def read_ncoda_2d(inFile_path, ni, nj):
    fName = os.path.basename(inFile_path)
    parts = fName.split('_')

    if len(parts) < 2:
        sys.exit(f"ERROR: Filename {fName} does not match expected format.")

    date_str = parts[1]
    fDate = f"{date_str[0:4]}-{date_str[4:6]}-{date_str[6:8]}"
    fTime = np.array([fDate], dtype='datetime64[D]')

    try:
        with open(inFile_path, 'rb') as f:
            vals = np.fromfile(f, dtype='>f4', count=ni*nj)
            if vals.size != (ni * nj):
                sys.exit(f"ERROR: File {fName} size mismatch. Expected {ni*nj}, got {vals.size}")
            vals = vals.reshape((nj, ni))
    except Exception as e:
        sys.exit(f"ERROR: Failed to read or reshape binary file: {e}")

    return fTime, vals

def plot_var(nc_file, vName, outPath):
    """
    Diagnostic plotting. Note: vName here is the output name (e.g., 'sic').
    """
    if not os.path.exists(nc_file):
        return

    ds = xr.open_dataset(nc_file)
    plot_data = ds[vName].squeeze()
    plot_data = plot_data.where(plot_data != FILL_VALUE)

    # Map back to original name if needed for cmap lookup
    lookup_name = "icecov" if vName == "sic" else vName
    _, _, _, v_cmap = var_atts(lookup_name)

    plt.figure(figsize=(10, 6))
    plot_data.plot(cmap=v_cmap, robust=True)
    #plt.title(f"Diagnostic Plot: {vName}\n{os.path.basename(nc_file)}")

    plot_name = nc_file.replace('.nc', '.png')
    plt.savefig(plot_name, dpi=PLOT_DPI, bbox_inches='tight')
    plt.close('all')
    print(f"DIAGNOSTIC PLOT SAVED: {plot_name}")

def bin_to_nc_2d(inFile, vName, outPath, topo_fName):
    # 1. Grid & Mask
    try:
        with xr.open_dataset(topo_fName, decode_times=False) as ds_topo:
            nj, ni = ds_topo.depth.shape
            ocean_mask = ~np.isnan(ds_topo.depth.values)
    except Exception as e:
        sys.exit(f"ERROR: Problem reading topography file {topo_fName}: {e}")

    # 2. Read Binary
    val_date, vals = read_ncoda_2d(inFile, ni, nj)

    # 3. Clean Special Values
    vals = np.where(vals < NCODA_SPVAL_THRESHOLD, FILL_VALUE, vals)

    # 4. Scaling
    scale_factor, v_units, v_long_name, _ = var_atts(vName)
    vals = np.where(vals != FILL_VALUE, vals * scale_factor, FILL_VALUE)

    # 5. Masking
    vals = np.where(ocean_mask, vals, FILL_VALUE)

    # 6. Construct Dataset
    # Handle the "sic" renaming logic here
    out_var_name = "sic" if vName == "icecov" else vName

    ds_out = xr.Dataset(
        data_vars={out_var_name: (("ny", "nx"), vals.astype(np.float32))},
        coords={"time": (("time"), val_date)}
    )

    # 7. Metadata & Encoding
    ds_out[out_var_name].encoding = {"_FillValue": FILL_VALUE}
    ds_out[out_var_name].attrs = {
        "units": v_units,
        "long_name": v_long_name,
        "scale_factor_applied": scale_factor
    }

    # 8. Stats
    valid_data = vals[vals != FILL_VALUE]
    if valid_data.size > 0:
        v_min, v_max = valid_data.min(), valid_data.max()
        print(f"Summary for {out_var_name}: Min={v_min:.4f}, Max={v_max:.4f}")
        if vName == "icecov" and (v_max > 1.01 or v_min < -0.01):
            print(f"CRITICAL WARNING: {out_var_name} out of bounds! [Min: {v_min:.4f}, Max: {v_max:.4f}]")
    else:
        print(f"WARNING: No valid ocean data found for {out_var_name}")

    # 9. Save
    out_name = os.path.join(outPath, f"{os.path.basename(inFile)}.nc")
    try:
        ds_out.to_netcdf(out_name)
        print(f"SUCCESS: Created NetCDF [{out_var_name}] at {out_name}")
    except Exception as e:
        sys.exit(f"ERROR: Failed to write NetCDF {out_name}: {e}")

    return ds_out
