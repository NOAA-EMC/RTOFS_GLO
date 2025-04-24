#!/usr/bin/env python3

"""
Utilities to:
- Read Hycom archive (i.e., archs, arche) files.
  Support for archv is not (yet) available.
  Following functions are based on those at 
  pyhycom: https://github.com/uwincm/pyhycom/blob/master/pyhycom.py
- Write out/return xarray dataset.

"""

import numpy as np
import xarray as xr

var_long_name = {
        "SSH": "sea_surface_height",
        "SSS": "sea_surface_salinity",
        "SST": "sea_surface_temperature", 
        "SSU": "sea_surface_u_current",
        "SSV": "sea_surface_v_current"}

var_units = {
        "SSH": "[m]",
        "SSS": "[PSU]",
        "SST": "[degC]", 
        "SSU": "[m/s]",
        "SSV": "[m/s]"}

# return file handle
def open_a_file(filename, mode):
    file = open(filename[:-1]+'a',mode=mode)
    return file

#Return the name of the corresponding HYCOM "b" file.
def get_b_filename(fName):
    bfilename = fName[:-1]+'b'
    return bfilename

#Return a list where each element contains text from each line of `b file`
def getTextFile(fName):
    return [line.rstrip() for line in open(fName,'r').readlines()]

# get dimensions of an archive from .b file
def getDims(fName, topo_file=False):

  f = getTextFile(get_b_filename(fName))
  idmFound, jdmFound = [False, False]

  if topo_file:
    for line in f:
        if 'i/jdm' in line:
          xx = line.split()[3]; jdm = xx[0:4]
          idm = line.split()[2]
          idmFound = True
          jdmFound = True
        if idmFound and jdmFound:break
  else:
    for line in f:
        if 'idm' in line:
          idm = line.split()[0]
          idmFound = True
        if 'jdm' in line:
          jdm = line.split()[0]
          jdmFound = True
        if idmFound and jdmFound:break

  return int(jdm), int(idm)

def getFieldIndex(field, fName):
    f = getTextFile(get_b_filename(fName))
    if 'arch' in fName.split('/')[-1]:f = f[10:] # skip first 10 lines
    if 'grid' in fName.split('/')[-1]:f = f[3:] # skip first 3 lines
    fieldIndex = []
    for line in f:
      if field == line.split()[0].replace('.','').replace(':',''):
        fieldIndex.append(f.index(line))
    return fieldIndex

def getField(field, fName, undef=np.nan, x_range=None, y_range=None):

  dims = getDims(fName)
  if dims.__len__() == 2:
    jdm, idm = dims
  else:
    jdm, idm, kdm = dims
    print("\n-- CAUTION! Read 3d archive is not yet ready!\n")

  reclen = 4*idm*jdm # Record length in bytes
  # HYCOM binary data is written out in chunks/"words" of multiples of 4096*4 bytes.
  # Length of one level of one variable (reclen) will be between
  # consecutive multiples of the wordlen. Data is padded to bring the volume
  # up to the next multiple. The "pad" value below is equal to the bytes that are needed to do this.
  wordlen = 4096*4
  pad = wordlen * np.ceil(reclen / wordlen) - reclen   # Pad size in bytes
  fieldRecords = getFieldIndex(field,fName)         # Get field record indices
  fieldAddresses = np.array(fieldRecords)*(reclen+pad) # Address in bytes

  file = open_a_file(fName,mode='rb') # Open file
  if dims.__len__() == 2: # 2-d field
    field = np.zeros((jdm,idm))
    file.seek(int(fieldAddresses[0]),0) # Move to address of the field
    data = file.read(idm*jdm*4)
    field = np.reshape(np.frombuffer(data, dtype='float32', count=idm*jdm),(jdm,idm)).byteswap()

    if not x_range is None:
      field = field[:,:,x_range]
    if not y_range is None:
      field = field[:,y_range,:]

  #field = field.byteswap() # Convert to little-endian
  file.close()
  field[field == np.float32(2**100)] = undef

  return field

# Number of records in the binary file, read from .b
def getNumberOfRecords(fName):
  f = getTextFile(get_b_filename(fName))
  if 'arch' in fName:
      f = f[10:]; return len(f)
  if 'grid' in fName:
      f = f[3:]; return len(f)
  if 'depth' in fName:
      return 1
  if 'restart' in fName:
      f = f[2:]; return len(f)

def getBathymetry(grid_fName, topog_fName, undef=np.nan):

  jdm,idm = getDims(grid_fName)

  file = open_a_file(topog_fName, mode='rb')
  #Data is in float32, which has 4 bytes/value
  data = file.read(idm*jdm*4)
  field = np.reshape(np.frombuffer(data,dtype='float32',count=idm*jdm).byteswap(),(jdm,idm))
  file.close()

  print(f"field.shape={field.shape}")
  field[field>2**99] = undef

  return field

def get_model_day(fName, varName):
  bFile = getTextFile( get_b_filename(fName))
  for line in bFile:
    if varName in line:
      model_day = line.split()[3]
  #print(model_day)
  return model_day

def arch_bin_dataset(lat, lon, data_date, var, vName, write_to_nc=True):

  ds = xr.Dataset()

  ds['Latitude'] = xr.DataArray(lat, dims=("Y", "X"),\
    coords={"Y": np.arange(1, lat.shape[0]+1), "X": np.arange(1, lat.shape[1]+1)},\
    name="Latitude",\
    attrs={"units": "degrees_north", "long_name": "latitude"})

  ds['Longitude'] = xr.DataArray(lon, dims=("Y", "X"),\
    coords={"Y": np.arange(1, lon.shape[0]+1), "X": np.arange(1, lon.shape[1]+1)},\
    name="Longitude",\
    attrs={"units": "degrees_east", "long_name": "longitude", "modulo": "360 degrees"})

  ds[vName] = xr.DataArray(var, dims=("Y", "X"),\
    coords={"Y": np.arange(1, var.shape[0]+1), "X": np.arange(1, var.shape[1]+1)},
    name=vName,\
    attrs={"units": var_units[vName], "long_name": var_long_name[vName]})

  ds['time'] = np.datetime64(data_date)

  ds.attrs = {"source": "NCEP RTOFS v2.5",\
              "history": "converted archive to netcdf",\
              "using": "https://github.com/NOAA-EMC/RTOFS_GLO"}

  ds=ds.set_coords(["Latitude", "Longitude", "time"])
 
  if (write_to_nc):
   fName = f"scratch/{vName}_{data_date}.nc"
   ds.to_netcdf(fName)
   print(f"Saved data to file name:\t{fName}\n")

  return ds
