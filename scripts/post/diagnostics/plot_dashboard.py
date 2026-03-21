#!/usr/bin/env python3

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import yaml
import glob as glob

import numpy as np
import pandas as pd

import matplotlib
matplotlib.use('Agg')
from matplotlib import pyplot as plt

from utils_plot import gather_data, plot_individual_exp_stats, compare_stats
# --

# user inputs
get_inputs = ArgumentParser(description="\
           Plot summary of statistics or a dashboard",\
           usage='%(prog)s [options]',
           formatter_class=ArgumentDefaultsHelpFormatter)
  
get_inputs.add_argument('--config_file', type=str,\
           help='yaml file that sets configuration, see provided yaml file for an example',\
           default='/u/santha.akella/tmp/plot_converted_nc/scripts/UFS_helpers/config_dashboard.yaml')
args = get_inputs.parse_args() 
# --

# Get configuration information
config = yaml.load( open( args.config_file, "r"), Loader=yaml.FullLoader)
experiments = config['systems']
variables = config['variables']
units = config['units']
regions = config['regions']

plot_each = config['plot_each']
compare_all = config['compare_all']

for iVar, varName in enumerate(variables): # expected to be few
  print(f'\n\n\t{varName}\tDashboard will display following:\n\n')
  for iReg, region in enumerate(regions):  # many are expected
    df_collection = {} 

    for iExp, expName in enumerate(experiments):
      print(f'{config['%s'%(expName)]['name'].upper()}\t\t {region}')
      data_path= config['%s'%(expName)]['data_path'] + "/" + varName + "/"
      fNames=sorted( glob.glob("{}/{}_{}_stats_*.txt".format(data_path, varName, region)))
      #print(f'Found: {fNames}')
      df = gather_data( fNames)
      df_collection[iExp] = df
      if (plot_each):
        plot_individual_exp_stats(expName, region, varName, units[iVar], df, config['output_path'])
    print("\n")

    if (compare_all):
      compare_stats(region, varName, units[iVar], experiments, df_collection, config['output_path'])
