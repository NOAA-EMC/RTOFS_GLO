#!/usr/bin/env python3

"""

Convert analysis increments or fields from NCODA analysis that from
binary to netcdf format.

"""

from utils import gather_fNames, bin_to_nc_2d_incr

# -- arg parse
proc_date = "20250331"
data_path_pref = "/lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR1/prod/com/rtofs/v2.5/"
data_path_suff = "ncoda/hycom_var/restart/"

varName = "icecov" #"icethk" # "icetmp" # "mixlyr"
varType = "sfc"
fType = "inc" # "fld" or "inc"

topo_fName = "/lfs/h2/emc/couple/noscrub/santha.akella/data_files/v2.5/topog/depth_GLBb0.08_09m11.nc"

# -- arg parse

#--------------------------------------------------------------------------------------------------
data_path, fNames = gather_fNames(data_path_pref, data_path_suff, proc_date, varName, fType, varType="sfc")
print(f'\nFound following [{len(fNames)}] files of [{varName}].')

# convert each binary formatted file to netcdf
for iF, fName in enumerate(fNames):
  bin_to_nc_2d_incr(data_path, fName, topo_fName)
#--------------------------------------------------------------------------------------------------
