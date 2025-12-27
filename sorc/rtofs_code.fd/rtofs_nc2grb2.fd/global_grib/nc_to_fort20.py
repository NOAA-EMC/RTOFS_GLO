#!/usr/bin/env python3
import numpy as np
from netCDF4 import Dataset
import argparse
import sys

def main():
    parser = argparse.ArgumentParser(
        description="Convert NetCDF variable to ASCII format (F8.4) for rtofs_nc2grb2."
    )
    
    # Positional Arguments
    parser.add_argument("input_nc", help="Path to the source NetCDF file")
    parser.add_argument("variable", help="Name of the variable to extract (e.g., sst, ice_con)")
    
    # Optional Arguments
    parser.add_argument("-o", "--output", default="fort.20", help="Output filename (default: fort.20)")
    parser.add_argument("-f", "--fill", type=float, default=100.0, help="Fill value (>99.0 triggers Fortran mask)")
    parser.add_argument("--transpose", action="store_true", help="Transpose the matrix before flattening")

    args = parser.parse_args()

    try:
        # 1. Open the NetCDF file
        ds = Dataset(args.input_nc, mode='r')
        
        if args.variable not in ds.variables:
            print(f"Error: Variable '{args.variable}' not found in {args.input_nc}")
            sys.exit(1)
            
        # 2. Extract the variable
        data = ds.variables[args.variable][:]
        
        # Handle multi-dimensional data (Slice to 2D)
        # Standard RTOFS NetCDF is often (time, lat, lon)
        if data.ndim == 3:
            data = data[0, :, :]
        elif data.ndim == 4:
            data = data[0, 0, :, :]
        elif data.ndim != 2:
            print(f"Error: Variable must be 2D (after slicing). Found dimensions: {data.shape}")
            sys.exit(1)

        # 3. Handle Transposition if requested
        if args.transpose:
            data = data.T

        # 4. Handle Missing Values / Masked Arrays
        if hasattr(data, 'filled'):
            data = data.filled(fill_value=args.fill)
        else:
            data[np.isnan(data)] = args.fill

        # 5. Write to ASCII in F8.4 format
        # Using order='F' (Fortran/Column-major) to match the Fortran read loop
        # which fills the array by columns first.
        with open(args.output, 'w') as f:
            for val in data.flatten(order='F'):
                f.write(f"{val:8.4f}\n")
                
        print(f"Success: Wrote {data.size} points to {args.output}")
        print(f"GRID_DIM {data.shape[1]} {data.shape[0]}")
        ds.close()
        
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
