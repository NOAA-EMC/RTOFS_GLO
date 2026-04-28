#!/usr/bin/env python3

"""
Helper utilities to process RTOFS analysis increments.

Questions:
  1. In each day's analysis, past 18 days of increments are saved.
     Much of the date (if not all?) is same and is being repeated over and over!
     Why are we duplicating files and seemingly wasting disk space?
"""

import os
import glob as glob
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
