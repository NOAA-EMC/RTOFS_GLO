#!/usr/bin/env python3

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import yaml
import glob as glob
import numpy as np
import pandas as pd
from utils_proc_arch import *
from hycom_wind_ymdh import hycom_wind_ymdh

output_var_names = {
  "srfhgt":"SSH",
  "salin": "SSS",
  "temp":  "SST",
  "u-vel": "SSU",
  "v-vel": "SSV",
  "curr":  "CURR"}

# user inputs
get_inputs = ArgumentParser(description="\
           Convert hycom output 2d archive [a,b] to netcdf format",\
           usage='%(prog)s [options]',
           formatter_class=ArgumentDefaultsHelpFormatter)

get_inputs.add_argument('--config_file', type=str,\
           help='yaml file that sets configuration, see provided yaml file for an example',\
           default=\
           '/lfs/h2/emc/couple/noscrub/santha.akella/src/rtofs_17Apr2025/scripts/UFS_helpers/config_archive_to_nc.yaml')
args = get_inputs.parse_args()
# --

config_file = args.config_file
print(f'\nReading configuration options from:\n{config_file}\n')
config = yaml.load( open( config_file, "r"), Loader=yaml.FullLoader)

rtofs_system = config['system_name']
grid_file=config['grid_file']
input_path = config['input_path']

start_date, end_date = [pd.to_datetime(config['start_date']), pd.to_datetime(config['end_date'])]
arch_type = config['arch_type']
varNames = config['arch_variables']

do_extraVars = False # default is no extra variables to output.
if config['do_extraVars']:
  do_extraVars = True
  extra_variables = config['extra_variables']

output_path = config['output_path']
# --

for dd in pd.date_range(start_date, end_date):
  if dd == start_date:
    print(f'Reading grid info from:\n[{grid_file}] on start date')
    plon = getField("plon", grid_file)
    plat = getField("plat", grid_file)

    # sanitize any longitude (i.e. plon) > 360.:
    plon = np.where(plon <= 360., plon, plon-360.)

  data_path=input_path+'{}'.format(dd.strftime('%Y%m%d'))
  #print(data_path)
  input_archive = glob.glob(data_path+"/"+arch_type+"*a")[0]
  #print(input_archive)

  for varName in varNames:
    # time stamp in the archive file
    [year, month, day, hour]= hycom_wind_ymdh( float(get_model_day(input_archive, varName)))
    MM = 0 # assumed to be at 0 minutes
    var = getField(varName, input_archive)

    data_date = "{}-{}-{}T{}:{}".\
              format(year, str(month).zfill(2), str(day).zfill(2), str(hour).zfill(2), str(MM).zfill(2))
    print(f"\nConverting {varName} on.. {data_date}")
    output_fName = f"{output_path}/{rtofs_system}_"
    output_fName = output_fName + "{}_{}.nc".format(output_var_names[varName], data_date)
    #print(output_fName)
    ds = arch_bin_dataset(plat, plon, data_date, var, output_var_names[varName], output_fName, True)

  if do_extraVars:
    for varName in extra_variables:
      if varName == 'curr':
        fName_pref = f"{output_path}/{rtofs_system}_"
        fName_u = fName_pref + "SSU_{}.nc".format(data_date)
        fName_v = fName_pref + "SSV_{}.nc".format(data_date)

        print(f'\nWorking on {varName} using {fName_u} and {fName_v}\n')
        ds_ssu = xr.open_dataset( fName_u)
        ds_ssv = xr.open_dataset( fName_v)
        ds = ds_ssv # use SSV dataset as a container
        ds['%s'%(output_var_names[varName])] = xr.DataArray(np.sqrt(ds_ssu['SSU']**2 + ds_ssv['SSV']**2),\
          coords=ds_ssv['SSV'].coords, dims=ds_ssv['SSV'].dims, name=output_var_names[varName], attrs={'units':'m/s'})
        ds = ds.drop_vars('SSV') # get rid of SSV (variable)
        output_fName = fName_pref + "{}_{}.nc".format(output_var_names[varName], data_date)
        ds.to_netcdf(output_fName)
        print(f"Saved data to file name:\t{output_fName}\n")

print("\nAll Done!\n")
