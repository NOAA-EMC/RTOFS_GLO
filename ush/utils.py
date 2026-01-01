#!/usr/bin/env python3

"""
Helper utilities to process NCODA output
"""

import os
import glob as glob
import xarray as xr
import numpy as np

def read_ncoda_2d(fName):

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
  vals = dummy.reshape((jm,im))
  f.close()

  return vName, fName, vName_full, fTime, vals

def land_sea_mask(topo_fName):

  """
  Create a land-sea mask from bathymetry:
  - land: 0
  - sea: 1
  """

  ds_topo = xr.open_dataset(topo_fName, decode_times=False)
  ls_mask = ds_topo.copy(deep=True)
  ls_mask['depth'] = xr.where(np.isnan(ls_mask.depth), 0, 1)
  ls_mask = ls_mask.rename({'depth':'mask'}) # rename 

  return ls_mask

def bin_to_nc_2d(inFile, vName, outPath, topo_fName):

  # Read binary file
  val_date, vals = read_ncoda_2d(inFile)

  # Create a dataset using topography file as a template
  ds_out= xr.open_dataset(topo_fName, decode_times=False)
  ds_out[vName] = (('Y', 'X'), vals)

  ls_mask = land_sea_mask(topo_fName)
  # Make sure concentration over land = 0.
  # Land values will be made to nan, so this is done only for sanity sake!
  ds_out[vName] = ds_out[vName] * ls_mask.mask.squeeze()

  # Apply the land sea mask created above
  ds_out[vName] = ds_out[vName].where(ls_mask.mask == 1, np.nan)

  # Delete depth, fix attributes
  ds_out = ds_out.drop_vars(['depth', 'Date']) # drop bathymetry

  ds_out =ds_out.rename({'MT': 'time'}) # rename MT to time
  ds_out['time'] = val_date # add time value

  # fix attributes
  #ds_inc = ds_inc.drop_attrs() # Won't work unless additional packages are install- can't on wcoss!
  ds_out[vName].attrs['units'] = '1'
  ds_out[vName].attrs['standard_name'] = vName
  ds_out[vName].attrs['description'] = XX
  ds_out.attrs['source'] = 'NCEP RTOFS'

  #print(ds_out[vName].attrs)

  ds_out.to_netcdf(outPath+'/'+inFile+'.nc')
  return ds_out
