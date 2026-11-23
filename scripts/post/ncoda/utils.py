#!/usr/bin/env python3

"""
Helper utilities to process RTOFS analysis increments and QC observation files.

Questions:
1. In each day's analysis, past 18 days of increments are saved.
     Much of the date (if not all?) is same and is being repeated over and over!
     Why are we duplicating files and seemingly wasting disk space?
"""

import os
import glob as glob
from itertools import combinations_with_replacement
from pathlib import Path
import xarray as xr
import numpy as np

def gather_fNames(data_path_pref, data_path_suff, proc_date, varName, fType, varType="sfc"):

  if fType == "inc":
   ncoda_fType = "analinc"
  else:
   ncoda_fType = "analfld"

  data_path = data_path_pref + "rtofs.{}/".format(proc_date) + data_path_suff
  print(f'\nLooking for {varName}_{varType}_*{ncoda_fType} files at path:\n{data_path}')
  fNames = sorted( glob.glob( data_path + varName + "_" + varType + "*" + ncoda_fType))
  #print(fNames)

  return data_path, fNames

def read_ncoda_increment_2d(data_path, fName_full):

  fName = fName_full.replace(data_path, "") # get rid of path from _full_ file name

  vName = fName.split('_')[0]
  vName_full =vName + ' ' + fName.split('_')[-1]
  im, jm = [int(fName.split('_')[2][2:6]), int(fName.split('_')[2][7:11])]
  fDate = fName.split('_')[3]
  fDate = fDate[0:4] + '-' + fDate[4:6] + '-' + fDate[6:8]# + ':' + fDate[8:10] # Always at 00 UTC
  fTime = np.array([str(fDate)], dtype='datetime64')

  print(f'\nReading RTOFS DA {vName_full} on\n{fTime} with [x,y] dim = {im,jm}.')

  f = open(data_path + fName, 'rb')
  vals = []
  f.seek(0)
  dummy = np.fromfile(f, dtype='>f',count=jm*im) # read 2d file (1 layer)
  #dummy = dummy.reshape((jm,im))
  #vals = np.copy(dummy)
  vals = dummy.reshape((jm,im))
  f.close()

  return vName, fName, vName_full, fTime, vals

def land_sea_mask(topo_fName, save_forLater=False):

  """
  Create a land-sea mask from bathymetry:
- land: 0
- sea: 1
  """

  ds_topo = xr.open_dataset(topo_fName, decode_times=False)
  ls_mask = ds_topo.copy(deep=True)
  ls_mask['depth'] = xr.where(np.isnan(ls_mask.depth), 0, 1)
  ls_mask = ls_mask.rename({'depth':'mask'}) # rename 

  if (save_forLater):
    fName = 'ls_mask_' + topo_fName
    ls_mask.to_netcdf(topo_fName) # This will work only for @sanAkel!

  return ls_mask

def bin_to_nc_2d_incr(data_path, fName, outPath, topo_fName):

  # Read binary increment file
  vName, fName_bin, vName_full, incDate, vals = read_ncoda_increment_2d(data_path, fName)

  # Create a dataset using topography file as a template
  ds_inc= xr.open_dataset(topo_fName, decode_times=False)
  ds_inc[vName] = (('Y', 'X'), vals)

  ls_mask = land_sea_mask(topo_fName)
  # Make sure concentration over land = 0.
  # Land values will be made to nan anyway, so this is done only for sanity sake!
  ds_inc[vName] = ds_inc[vName] * ls_mask.mask.squeeze()

  # Apply the land sea mask created above
  ds_inc[vName] = ds_inc[vName].where(ls_mask.mask == 1, np.nan)

  # Delete depth, fix attributes
  ds_inc = ds_inc.drop_vars(['depth', 'Date']) # drop bathymetry

  ds_inc=ds_inc.rename({'MT': 'time'}) # rename MT to time
  ds_inc['time'] = incDate # add time value

  # fix attributes
  #ds_inc = ds_inc.drop_attrs() # Won't work unless additional packages are install- can't on wcoss!
  ds_inc[vName].attrs['units'] = '1'
  ds_inc[vName].attrs['standard_name'] = vName
  ds_inc[vName].attrs['description'] = vName_full
  ds_inc.attrs['source'] = 'NCEP RTOFS v2.5'

  #print(ds_inc[vName].attrs)

  ds_inc.to_netcdf(outPath+'/'+fName_bin+'.nc')
  return ds_inc


def build_composite_qc_map(qc_map: dict[int, str]) -> dict[int, str]:
    """Precompute exact + additive composite QC flag combinations from qc_codes."""
    resolved = dict(qc_map)
    nonzero_codes = sorted(k for k in qc_map if k > 0)

    # Profile & SSS flags can sum up to 4 check codes (including repeated codes like DERIVED_SAL=12)
    for r in range(2, 5):
        for combo in combinations_with_replacement(nonzero_codes, r):
            total = sum(combo)
            if total not in resolved:
                resolved[total] = "+".join(qc_map[c] for c in combo)

    return resolved


def decode_qc_flag(flag_val: int, resolved_qc_map: dict[int, str]) -> str:
    """Decode a single or composite QC flag integer."""
    val = int(flag_val)
    if val in resolved_qc_map:
        return resolved_qc_map[val]
    return f"UNKNOWN({val})"


def load_obs_dataset(nc_path: str | Path) -> tuple[xr.Dataset, dict[int, str]]:
    """Open an observation NetCDF file and attach decoded type, WMO ID, and date strings."""
    ds = xr.open_dataset(nc_path)

    # 1. Decode data type lookup table (0..209)
    decoded_type_vars = []
    if "data_type_name" in ds.data_vars:
        type_labels = ds["data_type_name"].str.decode("utf-8").str.strip()
        for var_name in list(ds.data_vars):
            if ds[var_name].attrs.get("lookup_table") == "data_type_name":
                out_name = f"{var_name}_name"
                ds[out_name] = type_labels.isel(data_types=ds[var_name])
                decoded_type_vars.append(out_name)

    ds.attrs["_decoded_type_vars"] = decoded_type_vars

    # 2. Build QC code lookup dictionary (including composite sums)
    qc_map: dict[int, str] = {}
    if "qc_code_id" in ds.data_vars and "qc_code_name" in ds.data_vars:
        base_qc_map = dict(
            zip(
                ds["qc_code_id"].values.astype(int),
                ds["qc_code_name"].str.decode("utf-8").str.strip().values,
            )
        )
        qc_map = build_composite_qc_map(base_qc_map)

    # 3. Decode WMO ID and date strings if present
    if "wmo_id" in ds.data_vars:
        ds["wmo_id_str"] = ds["wmo_id"].str.decode("utf-8").str.strip()

    if "date" in ds.data_vars and ds["date"].dtype.kind == "S":
        ds["date_str"] = ds["date"].str.decode("utf-8").str.strip()

    return ds, qc_map


def summarize_obs_nc(nc_path: str | Path) -> tuple[int, int, str]:
    """Return (nobs, good_count, semicolon-separated data_type_names) for audit CSVs."""
    ds, _ = load_obs_dataset(nc_path)
    nobs = int(ds.sizes.get("nobs", 0))

    if "flag" in ds.data_vars:
        flg = ds["flag"].values
        if flg.ndim == 2:
            # Profile: count profiles that have at least one GOOD (flag == 0) level
            good_count = int((flg == 0).any(axis=1).sum())
        else:
            good_count = int((flg == 0).sum())
    else:
        good_count = nobs

    labels = []
    for var_name in ds.attrs.get("_decoded_type_vars", []):
        labels.extend(sorted(set(ds[var_name].values.tolist())))
    unique_labels = ";".join(dict.fromkeys(labels))

    ds.close()
    return nobs, good_count, unique_labels
