#!/usr/bin/env python3

# needs py modules
#module use /apps/dev/modulefiles/
#module load ve/evs/2.0_py312

import argparse
import sys
import os
from netCDF4 import Dataset
from datetime import datetime

def format_iso_date(iso_str):
    """
    Parses ISO string (e.g., 2025-12-24T23:27:54.123)
    Returns string in format: yyyy/mm/dd\t hh
    """
    try:
        # 1. Remove fractional seconds/decimals to ensure strptime works cleanly
        #    "2025-12-24T23:27:54.123" becomes "2025-12-24T23:27:54"
        clean_iso = iso_str.split('.')[0]

        # 2. Parse into a datetime object
        dt_obj = datetime.strptime(clean_iso, "%Y-%m-%dT%H:%M:%S")

        # 3. Format to "yyyy/mm/dd <tab> hh"
        return dt_obj.strftime("%Y%m%d%H%M")
    except ValueError:
        return iso_str  # Return original if parsing fails

def extract_smos_times(file_path):
    if not os.path.exists(file_path):
        print(f"Error: The file '{file_path}' does not exist.")
        sys.exit(1)

    try:
        nc = Dataset(file_path, mode='r')

        raw_start = getattr(nc, 'FH:Validity_Period:Validity_Start')
        raw_end   = getattr(nc, 'FH:Validity_Period:Validity_Stop')

        # Basic cleaning (remove 'UTC=')
        start_clean = raw_start.replace('UTC=', '').strip()
        end_clean   = raw_end.replace('UTC=', '').strip()

        # print("\n-- Try either of these formats --")
        # Print original variables
        # print(f"START_TIME={start_clean} -- END_TIME={end_clean}\n")

        # Print NEW reformatted variables (yyyymmddhhmm)
        # We assign them to new keys so they can be captured if needed
        print(f"{format_iso_date(start_clean)} {format_iso_date(end_clean)}")

        nc.close()

    except AttributeError as e:
        print(f"Error: Required attribute not found in {file_path}. Details: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error reading file: {e}")
        sys.exit(1)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("filename", help="Full path to the SMOS NetCDF file")
    args = parser.parse_args()

    extract_smos_times(args.filename)
