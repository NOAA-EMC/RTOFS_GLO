#!/usr/bin/env python3

"""
- To plot following regions (zoom-in).
- N Pole (Arctic).
- Globe.
- S Pole (Antarctic).
- Summary statistics (or dashboards).

- To get array indices given coordinates (on hycom grid).
- Ease data gathering
"""

import xarray as xr
import numpy as np
import pandas as pd

import cartopy.crs as ccrs
import cartopy.feature as cfeature

import matplotlib.pyplot as plt

arc_ssh_ticks = np.asarray([-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.])
arc_sst_ticks = np.asarray([-3., -2., -1., -0.5, 0, 0.5, 1, 2, 3, 5])
# --

def read_stats(stats_fName):
  stats = np.loadtxt(stats_fName, dtype=str)
  stats_date = pd.to_datetime( str(stats[0]))
  stats_mean = float( stats[1])
  stats_sdev = float( stats[2])
  return [stats_date, stats_mean, stats_sdev]

def gather_data(fNames):
  dates = []; averages = []; sigmas = []
  for iF, fName in enumerate( fNames):
    #print(fName)
    [date, mean, sdev] = read_stats(fName)
    dates.append( date)
    averages.append( mean)
    sigmas.append( sdev)
  region_stats = {'Date': dates, 'mean': averages, 'std_dev': sigmas}
  df = pd.DataFrame(region_stats)
  #print(df)
  return df
# --

def get_index(lat_array, lon_array, lat0, lon0):
  # First, find the index of the grid point nearest a specific lat/lon.
  abslat = np.abs(lat_array-lat0)
  abslon = np.abs(lon_array-lon0)
  c = np.maximum(abslon, abslat)

  ([xloc], [yloc]) = np.where(c == np.min(c))

  #point_ds = ds.sel(X=xloc, Y=yloc)
  #print(f'yIndex= {yloc}, xIndex= {xloc}')
  return [yloc, xloc]
# --

def get_cutOut(lat_array, lon_array, lon_s, lon_e, lat_s, lat_e):
  # Lower left
  [y1, x1] = get_index(lat_array, lon_array, lat_s, lon_s)
  # Lower right
  [y2, x2] = get_index(lat_array, lon_array, lat_s, lon_e)
  # x1 will be same as x2
  # ----------------------
  # Upper left
  [y3, x3] = get_index(lat_array, lon_array, lat_e, lon_s)
  # Upper right
  [y4, x4] = get_index(lat_array, lon_array, lat_e, lon_e)
  # x3 will be same as x4

  #ds_cutOut=ds.sel(X=slice(y1, y2), Y=slice(x1, x3))
  return [y1, y2, x1, x3]
# --

def plot_individual_exp_stats(exp_name, region, var_name, units, df, output_path):
  av_mean, av_sdev = [df['mean'].mean(), df['std_dev'].mean()] # average over all dates

  plot_width, plot_height, plot_dpi = [8, 6, 120]
  fig = plt.figure(figsize=(plot_width, plot_height))
  ax = fig.add_subplot(111)

  # keys should match those in above gather_data
  im=df.plot(ax=ax, x='Date', y='mean', yerr='std_dev',\
             kind='line', capsize=2, label='{} {} {}[{}]\nMean: {:.2f}, Std: {:.2f}'.\
             format(exp_name, region, var_name, units, av_mean, av_sdev))

  ax.set_ylabel('Mean +/- 1 std dev [%s]'%(var_name))
  ax.legend(loc=1)
  #ax.set_title('%s'%(region))
  figName = output_path + "{}_{}_mean_1sdev_{}.png".format(exp_name, region, var_name)
  plt.savefig(figName, bbox_inches='tight', dpi=plot_dpi)
  #print(f'Saved:\t[{figName}]')
  plt.close('all')
# --

def compare_stats(region, var_name, units, expNames, dFrames, output_path):

# Summarized stats: average over all dates
  nExp = len(expNames)
  av_of_mean = np.zeros((nExp))
  av_of_sdev = np.zeros_like(av_of_mean)

  for iExp, expName in enumerate(expNames):
    av_of_mean[iExp]= dFrames[iExp]['mean'].mean()
    av_of_sdev[iExp]= dFrames[iExp]['std_dev'].mean()
  stats_summary_df = pd.DataFrame({'systems': expNames, 'av(mean)': av_of_mean, 'av(std dev)': av_of_sdev})
  # format float to .2f before displaying 
  stats_summary_df = stats_summary_df.round(3)
# --

  plot_width, plot_height, plot_dpi = [8, 8, 120]
  fig, axes = plt.subplots(2, 1, figsize=(plot_width, plot_height))

  for iExp, expName in enumerate(expNames):
    axes[0].plot(dFrames[iExp]['Date'], dFrames[iExp]['std_dev'], label='{}'.format(expName))
  axes[0].legend(loc=1)
  axes[0].set_ylabel('1 std dev')
  axes[0].set_xlabel('Date')
  axes[0].set_xlabel('')
  axes[0].set_xticks([])
  axes[0].set_frame_on(False)
  axes[0].set_title('Region: {}. Variable: {}[{}]'.format(region, var_name, units))

  for iExp, expName in enumerate(expNames):
    dFrames[iExp].plot(ax=axes[1], x='Date', y='mean', yerr='std_dev', kind='line', capsize=2,\
         label='{}'.format(expName))
  axes[1].set_ylabel('mean +/- 1 sigma')
  #axes[1].set_xlabel('')
  #axes[1].set_xticks([])
  #axes[1].set_frame_on(False)

  table = pd.plotting.table(axes[1], stats_summary_df,\
                            loc='bottom', cellLoc='center',\
                            bbox=[0, -1.0, 1, 0.3])

  plt.tight_layout()
  figName = output_path + "{}_{}_summary.png".format(region, var_name)
  plt.savefig(figName, bbox_inches='tight', dpi=plot_dpi)
  #print(f'Saved:\t[{figName}]')
  plt.close('all')
# --

def plot_arctic(input_ds, vName, data_date, cMin, cMax, cMap, cLon=-30, DPI=120):
  fig = plt.figure(figsize=[8,6])

  ax = fig.add_subplot(1,1,1, projection=ccrs.NorthPolarStereo(central_longitude=cLon))
  ax.add_feature(cfeature.LAND, facecolor='grey', alpha=0.2)
  ax.coastlines(color='k', alpha=0.2)
  ax.set_extent([-300, 60, 50, 90], ccrs.PlateCarree())

  im = input_ds[vName].plot(ax=ax, x='Longitude', y='Latitude',\
            vmin=cMin, vmax=cMax, cmap=cMap,\
            transform=ccrs.PlateCarree(),\
            add_labels=False, add_colorbar=False)

  im.axes.gridlines(color='black', alpha=0.5, linestyle='--', draw_labels=True)

  cax = ax.inset_axes([0.8, 0.32, 0.03, 0.6])
  if vName == 'SSH':
    Ticks = arc_ssh_ticks
  else:
    Ticks = arc_sst_ticks
  fig.colorbar(im, cax=cax, orientation='vertical', ticks=Ticks)

  cax.tick_params(labelsize=10, rotation=0)
  cax.set_title('{}'.format(input_ds.time.values))

  figName = f"scratch/{vName}_{data_date}.png"
  plt.savefig(figName, dpi=DPI, bbox_inches='tight')
  print(f"Saved figure to file name:\t{figName}\n")

