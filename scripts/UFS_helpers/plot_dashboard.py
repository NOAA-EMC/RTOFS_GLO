#!/usr/bin/env python3

import sys
import glob as glob

import numpy as np
import pandas as pd

import matplotlib
matplotlib.use('Agg')
from matplotlib import pyplot as plt
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

def plot_control(system_name, region, var_name, df, plot_mean=True):
  plot_width, plot_height, plot_dpi = [8, 6, 120]
  fig = plt.figure(figsize=(plot_width, plot_height))
  ax = fig.add_subplot(111)
  if (plot_mean):
    im=df.plot(x='Date', y='mean', kind='line', label='%s'%(sys_name))
    plt.ylabel('Mean %s'%(var_name))
    figName = "{}_{}_mean_{}.png".format(system_name, region, var_name)
  else:
    im=df.plot(x='Date', y='std_dev', kind='line', label='%s'%(sys_name))
    plt.ylabel('Std. Dev %s'%(var_name))
    figName = "{}_{}_sdev_{}.png".format(system_name, region, var_name)
  plt.legend()
  plt.title('%s'%(region))

  plt.savefig(figName, bbox_inches='tight', dpi=plot_dpi)
  print(f'Saved:\t[{figName}]')
  plt.close('all') 

def compare_with_control(systems, region, var_name, df_ctl, df_exp, plot_mean=True):
  plot_width, plot_height, plot_dpi = [8, 6, 120]
  fig = plt.figure(figsize=(plot_width, plot_height))
  ax = fig.add_subplot(111)
  if (plot_mean):
    im1=df_ctl.plot(ax=ax, x='Date', y='mean', kind='line', label='%s'%(systems[0]))
    im2=df_exp.plot(ax=ax, x='Date', y='mean', kind='line', label='%s'%(systems[1]))
    plt.ylabel('Mean %s'%(var_name))
    figName = "compare_{}_{}_{}_{}_mean.png".format(systems[0], systems[1], region, var_name)
  else:
    im1=df_ctl.plot(ax=ax, x='Date', y='std_dev', kind='line', label='%s'%(systems[0]))
    im2=df_exp.plot(ax=ax, x='Date', y='std_dev', kind='line', label='%s'%(systems[1]))
    plt.ylabel('Std. Dev %s'%(var_name))
    figName = "compare_{}_{}_{}_{}_sdev.png".format(systems[0], systems[1], region, var_name)
  plt.legend()
  plt.title('%s'%(region))

  plt.savefig(figName, bbox_inches='tight', dpi=plot_dpi)
  print(f'Saved:\t[{figName}]')
  plt.close('all') 
# --

# inputs
base_dir="/u/santha.akella/tmp/plot_converted_nc/scripts/UFS_helpers/"
systems=["v2p4", "v2p5"]
var_name="SSH"
# --

#regions = ["Global", "Arctic", "Antarctic", "Eq_Pac", "Atlantic", "Trop"] # follows those set in "get_dashboard_data.sh"
regions = ["e_Pac"]

for region in regions:
  for iS, sys_name in enumerate(systems):
    data_path=base_dir + "scratch_" + sys_name + "/" + var_name + "/"
    fNames=sorted( glob.glob("{}/{}_{}_stats_*.txt".format(data_path, var_name, region)))
    df = gather_data( fNames)

    print(f'RTOFS system: {sys_name},\t Region: {region}')
    if iS==0:
      control_df = df
      plot_control(sys_name, region, var_name, control_df) # plot mean
      plot_control(sys_name, region, var_name, control_df, False) # plot standard deviations
    elif iS==1:
      exp_df = df
    else:
      print(f'System number= {iS}. Trying to compare > 2 systems, it is not coded.')
      sys.exit('Exiting!')

  compare_with_control(systems, region, var_name, control_df, exp_df) # plot mean
  compare_with_control(systems, region, var_name, control_df, exp_df, False) # plot standard deviations
