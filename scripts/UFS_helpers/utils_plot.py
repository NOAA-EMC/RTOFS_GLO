#!/usr/bin/env python3

"""
- To plot following regions (zoom-in).
- N Pole (Arctic).
- Globe.
- S Pole (Antarctic).

- To get array indices given coordinates (on hycom grid).
"""

import xarray as xr
import numpy as np

import cartopy.crs as ccrs
import cartopy.feature as cfeature

import matplotlib.pyplot as plt

arc_ssh_ticks = np.asarray([-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.])
arc_sst_ticks = np.asarray([-3., -2., -1., -0.5, 0, 0.5, 1, 2, 3, 5])
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

