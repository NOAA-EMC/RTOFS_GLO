#!/usr/bin/env python3

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import yaml

import xarray as xr
import numpy as np

import cartopy.crs as ccrs
import cartopy.feature as cfeature
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER

import matplotlib
matplotlib.use('Agg')
from matplotlib import pyplot as plt
# --

var_units = {
  'SSH': 'm',
  'SST': 'degC',
  'SSS': 'PSU',
  'SSU': 'm/s',
  'SSV': 'm/s',
  'CURR': 'm/s'
}

# user inputs
get_inputs = ArgumentParser(description="\
           Calculate mean and std dev of a 2-d (globally defined) field \
           and optionally save a plot of it.", usage='%(prog)s [options]',\
           formatter_class=ArgumentDefaultsHelpFormatter)

get_inputs.add_argument('--config_file', type=str,\
           help='yaml file that sets configuration, see provided yaml file for an example',\
           default=\
           '/u/santha.akella/tmp/plot_converted_nc/scripts/UFS_helpers/plot_config_2d_field.yaml')

get_inputs.add_argument('--data_file', type=str,\
           help='file name (full path) that has the 2-d data.', required=True)

get_inputs.add_argument('--varName', type=str,\
           help='Name of the 2-d variable in the specified file name.', required=True)

get_inputs.add_argument('--gen_plot', action="store_true",\
           help='Whether to generate and save a plot, none if this argument is excluded.')

get_inputs.add_argument('--output_path', type=str,\
          help='Path to where output files are to be saved',\
          default="/u/santha.akella/tmp/plot_converted_nc/scripts/UFS_helpers/scratch/")

get_inputs.add_argument('--modelName', type=str,\
          help='Name of the ocean model', default="hycom")

args = get_inputs.parse_args()
# --

region = 'Global'  # This script is meant for GLOBAL diagnostics and plots.

ds = xr.open_dataset(args.data_file)

if (args.modelName == 'hycom') and (args.varName == 'SSH'):
  Grav = 9.81 # units: ms^{-2}; used for scaling SSH.
  ds[args.varName] = ds[args.varName]/Grav
# --

# form date string - used in saving above stats to a file.
yyyymmdd = ds.time.values.astype("str")
dStr = yyyymmdd.split('T')[0]+'T'+yyyymmdd.split('T')[1].split(':')[0]

# Statistics (mean and standard deviation)
var_av= ds[args.varName].mean(skipna=True).values
var_std= ds[args.varName].std(skipna=True).values
var_stats = np.asarray( [dStr, var_av, var_std])
#print(f"\n{region} Mean and standard deviation of {args.varName}: {var_av}, {var_std}")

stats_fName= args.output_path + '/' + args.varName + '_' + region + '_stats_' + dStr + '.txt'
np.savetxt(stats_fName, var_stats.flatten(), newline=' ', fmt='%s')
# --

if args.gen_plot:
  print(f'\nReading configurations for plotting from:\n{args.config_file}\n')
  #with open(args.config_file, 'r', encoding='utf-8') as fh:
  #  config = yaml.load(fh, Loader=yaml.SafeLoader)
  config = yaml.load( open( args.config_file, "r"), Loader=yaml.FullLoader)

  # Attributes in netcdf file
  latName = config['latName']
  lonName = config['lonName']

  vMin, vMax, cMap, cLon = config['%s'%(args.varName)]['%s'%(region)]
  #print(vMin, vMax, cMap, cLon)

  #print(f'\nPlotting {args.varName}...\n')

  plot_width, plot_height, plot_dpi = [8, 6, 120]
  cbar_orientation = 'vertical'

  fig = plt.figure(figsize=(plot_width, plot_height))
  ax = fig.add_subplot(1,1,1, projection=ccrs.PlateCarree(central_longitude=cLon))

  im= ds[args.varName].plot(ax=ax, x=lonName, y=latName,\
                            transform=ccrs.PlateCarree(),\
                            vmin=vMin, vmax=vMax, cmap=cMap,\
                            add_labels=False, add_colorbar=False)

  ax.add_feature(cfeature.LAND, zorder=0, edgecolor='k', facecolor=("lightgray"), alpha=0.2)
  ax.coastlines(color='k', alpha=0.4)
  ax.set_title("{}".format(dStr))

  cbar=plt.colorbar(im, ax=ax, pad=0.01, orientation=cbar_orientation, shrink=0.5)
  cbar.set_label("{} [{}]".format(args.varName, var_units[args.varName]))

  gl = ax.gridlines(draw_labels=True)
  gl.top_labels = False
  gl.right_labels = False
  gl.xformatter = LONGITUDE_FORMATTER
  gl.yformatter = LATITUDE_FORMATTER
  gl.xlabel_style = {'size': 6}
  gl.ylabel_style = {'size': 6}

  figName= args.output_path + '/' + args.varName + '_' + region + '_' + dStr + '.png'
  plt.savefig(figName, bbox_inches='tight', dpi=plot_dpi)
  print(f'Saved\n{figName}')
  plt.close()
