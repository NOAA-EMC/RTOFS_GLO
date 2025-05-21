#!/usr/bin/env python3

import os
import argparse
import glob as glob

get_inputs = argparse.ArgumentParser(prog='\nrename_forcings.py',\
          description='get forcings and strip prefix before running hycom.',\
          usage='%(prog)s [options]',\
          formatter_class=argparse.ArgumentDefaultsHelpFormatter)

get_inputs.add_argument('--forcing_path', type=str,\
          help='Path to staged forcing files', \
          metavar='/lfs/h2/emc/eib/noscrub/dan.iredell/forcing/20250320/',\
          required=True)

get_inputs.add_argument('--exp_dir', type=str,\
          help='Path to exp_dir', \
          metavar='/lfs/h2/emc/ptmp/santha.akella/rtofs.20250320/',\
          required=True)

args = get_inputs.parse_args()
# --

inPath = args.forcing_path
outPath = args.exp_dir

fPref = 'rtofs_glo.fcst1.t00z.' # default; hard coded
#
fNames = sorted( glob.glob( inPath+fPref+'*'))

print(f"Stripping prefix:\t{fPref}\n")
for fName in fNames:
  fName_out = outPath+"/"+fName.replace(inPath+fPref, "")
  #print(f"Link: [{fName}] to [{fName_out}]")
  #print("\n")
  stat1 = os.system("ln -s " + fName + " " + fName_out)
  if stat1 == 0:
    print(f"Linked {fName} to: {fName_out}")
    print(" ")
