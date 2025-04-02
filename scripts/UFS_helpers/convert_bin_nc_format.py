#!/usr/bin/env python3

"""

Convert analysis increments or fields from NCODA analysis that from
binary to netcdf format.

"""

import argparse
from utils import gather_fNames, bin_to_nc_2d_incr


# user inputs
get_inputs = argparse.ArgumentParser(prog='\nconvert_bin_nc_format.py',\
          description='To convert RTOFS-DA output from binary to netcdf format.',\
          usage='%(prog)s [options]',\
          formatter_class=argparse.ArgumentDefaultsHelpFormatter)

get_inputs.add_argument('--proc_date', type=str,\
          help='RTOFS-DA output date', metavar='example: 20250331',\
          required=True)

get_inputs.add_argument('--data_path_pref', type=str,\
          help='Path to experiment',\
          default="/lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR1/prod/com/rtofs/v2.5/")

get_inputs.add_argument('--data_path_suff', type=str,\
          help='Path to NCODA output within data_path_pref',\
          default="ncoda/hycom_var/restart/")

get_inputs.add_argument('--var_name', type=str,\
          help='2-d variable name: icecov or icethk or icetmp or mixlyr',\
          default="icecov")

get_inputs.add_argument('--var_type', type=str,\
          help='type of variable: sfc or pre or lyr',\
          default="sfc")

get_inputs.add_argument('--file_type', type=str,\
          help='(inc) increment or (fld) field',\
          default="inc")

get_inputs.add_argument('--output_path', type=str,\
          help='Path to where output files are to be saved',\
          required=True)

get_inputs.add_argument('--topography_file', type=str,\
          help='Path to topography file',\
          default="/lfs/h2/emc/couple/noscrub/santha.akella/data_files/v2.5/topog/depth_GLBb0.08_09m11.nc")

args = get_inputs.parse_args()
# --

proc_date = args.proc_date
data_path_pref = args.data_path_pref
data_path_suff = args.data_path_suff

varName = args.var_name
varType = args.var_type
fType = args.file_type
output_path = args.output_path

topo_fName = args.topography_file

# Get names of binary files
data_path, fNames = gather_fNames(data_path_pref, data_path_suff, proc_date, varName, fType, varType="sfc")
print(f'\nFound following [{len(fNames)}] files of [{varName}].')

# convert each binary formatted file to netcdf
for iF, fName in enumerate(fNames):
  bin_to_nc_2d_incr(data_path, fName, output_path, topo_fName)
