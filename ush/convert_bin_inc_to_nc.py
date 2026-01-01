#!/usr/bin/env python3

"""

Convert analysis increments or (2d) fields from NCODA analysis
from binary to netcdf format.

"""

import argparse

# user inputs
get_inputs = argparse.ArgumentParser(prog='\nconvert_bin_inc_to_nc.py',\
          description='To convert RTOFS-DA output from binary to netcdf format.',\
          usage='%(prog)s [options]',\
          formatter_class=argparse.ArgumentDefaultsHelpFormatter)

get_inputs.add_argument('--topography_file', type=str,\
          help='Path to topography file',\
          default="/lfs/h2/emc/couple/noscrub/santha.akella/data_files/v2.5/topog/depth_GLBb0.08_09m11.nc")

get_inputs.add_argument('--input_file', type=str,\
          help='Name of NCODA binary output, including path',\
          required=True)

get_inputs.add_argument('--var_name', type=str,\
          help='2-d variable name: icecov or icethk or icetmp or mixlyr',\
          default="icecov")

get_inputs.add_argument('--output_path', type=str,\
          help='Path to where output file is to be saved',\
          required=True)

args = get_inputs.parse_args()
# --

topo_file = args.topography_file
input_file = args.input_file
var_name = args.var_name
output_path = args.output_path

# convert file format: binary to netcdf
#bin_to_nc_2d(input_file, var_name, output_path, topo_file)
