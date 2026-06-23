#!/usr/bin/env python3
"""
plot_jmin.py
Purpose: Reads Jmin CSV files and a YAML config. Generates an individual
         time-series plot for each Category defined in the config.
"""

import os
import sys
import glob
import argparse
import yaml
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.ticker as ticker

def load_all_csvs(data_dir):
    files = glob.glob(os.path.join(data_dir, "*", "jmin_*.csv"))
    if not files:
        print(f"FATAL ERROR: No jmin_*.csv files found in {data_dir}/*/", file=sys.stderr)
        sys.exit(1)

    df_list = []
    for f in sorted(files):
        basename = os.path.basename(f)
        date_str = basename.replace("jmin_", "").replace(".csv", "")
        try:
            df = pd.read_csv(f)
            df['Date'] = pd.to_datetime(date_str, format='%Y%m%d')
            df_list.append(df)
        except Exception as e:
            print(f"Warning: Issue reading {f}: {e}", file=sys.stderr)

    if not df_list:
        print("FATAL ERROR: No valid data could be loaded.", file=sys.stderr)
        sys.exit(2)

    return pd.concat(df_list, ignore_index=True)

def main():
    parser = argparse.ArgumentParser(description="Plot Jmin statistics based on YAML config.")
    parser.add_argument("yaml_config", help="Path to YAML configuration file")
    args = parser.parse_args()

    # 1. Load YAML Configuration
    try:
        with open(args.yaml_config, 'r') as yf:
            config = yaml.safe_load(yf)
    except Exception as e:
        print(f"FATAL ERROR: Failed to read YAML config {args.yaml_config}: {e}", file=sys.stderr)
        sys.exit(3)

    plots_config = config.get('plots', [])
    data_dir = config.get('data_dir', '.')
    
    if not plots_config:
        print("WARNING: No 'plots' section found in YAML config. Exiting.", file=sys.stderr)
        sys.exit(0)

    # 2. Load and Clean Data
    df = load_all_csvs(data_dir)
    df['Category'] = df['Category'].astype(str).str.strip()
    df['ObsType'] = df['ObsType'].astype(str).str.strip()

    df['Jmin'] = pd.to_numeric(df['Jmin'], errors='coerce')
    df['N'] = pd.to_numeric(df['N'], errors='coerce')

    color_cycle = plt.rcParams['axes.prop_cycle'].by_key()['color']

    # 3. Generate Individual Plots
    for p in plots_config:
        category = p.get('Category')  
        obs_types = p.get('ObsType', []) 
        
        # Initialize a new figure for this specific category
        fig, ax1 = plt.subplots(figsize=(10, 4))
        ax2 = ax1.twinx()
        
        lines_for_legend = []
        labels_for_legend = []

        for j, obs in enumerate(obs_types):
            mask = (df['Category'] == category) & (df['ObsType'] == obs)
            subset = df[mask].copy()

            if subset.empty:
                print(f"  -> Skipping {category} - {obs}: No data found.")
                continue

            subset.sort_values('Date', inplace=True)
            c = color_cycle[j % len(color_cycle)]

            l1, = ax1.plot(subset['Date'], subset['Jmin'], marker='o', linestyle='-', color=c, linewidth=2)
            l2, = ax2.plot(subset['Date'], subset['N'], marker='s', linestyle='--', color=c, linewidth=1.5, alpha=0.6)
            
            lines_for_legend.append(l1)
            labels_for_legend.append(f"{obs}")

        # Set N axis to log scale if variation is large
        ymin, ymax = ax2.get_ylim()
        if ymax > 0 and ymin > 0 and (ymax / ymin) > 10:
            ax2.set_yscale('log')
            ax2.yaxis.set_major_formatter(ticker.ScalarFormatter())         

        # Plot Formatting
        ax1.set_title(f"NCODA Jmin: {category}", fontweight='bold')
        ax1.set_ylabel("Jmin Value")
        ax2.set_ylabel("Observation Count (N)")
        ax1.grid(True, linestyle='--', alpha=0.7)
        ax1.set_xlabel("Date", fontweight='bold')
        
        if lines_for_legend:
            ax1.legend(lines_for_legend, labels_for_legend, loc='best', frameon=False)

        # X-axis dates
        ax1.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
        fig.autofmt_xdate()

        # Save and close
        plt.tight_layout()
        safe_category_name = category.replace(" ", "_")
        out_filename = f"jmin_{safe_category_name}.png"
        out_filepath = os.path.join(data_dir, out_filename)
        fig.savefig(out_filepath, dpi=120, bbox_inches='tight')
        plt.close(fig)

        print(f"  -> SUCCESS: Created plot: {out_filepath}")

if __name__ == "__main__":
    main()
