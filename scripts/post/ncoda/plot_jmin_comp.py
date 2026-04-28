#!/usr/bin/env python3
"""
plot_jmin_comp.py
Purpose: Reads Jmin CSV files and a YAML config. Generates a single Figure 
         with stacked subplots (one subplot per Category).
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
    files = glob.glob(os.path.join(data_dir, "jmin_*.csv"))
    if not files:
        print(f"FATAL ERROR: No jmin_*.csv files found in {data_dir}", file=sys.stderr)
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
    parser.add_argument("out_path", help="Directory containing CSVs and where plots will be saved")
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
    num_cats = len(plots_config)
    
    if num_cats == 0:
        print("WARNING: No 'plots' section found in YAML config. Exiting.", file=sys.stderr)
        sys.exit(0)

    # 2. Load and Clean Data
    df = load_all_csvs(args.out_path)
    df['Category'] = df['Category'].astype(str).str.strip()
    df['Metric'] = df['Metric'].astype(str).str.strip()

    # NEW: Force Jmin and N to be true numbers, not strings!
    df['Jmin'] = pd.to_numeric(df['Jmin'], errors='coerce')
    df['N'] = pd.to_numeric(df['N'], errors='coerce')

    # 3. Setup Figure (Stacked subplots, shared X-axis)
    # Dynamic height based on the number of categories
    fig, axes = plt.subplots(nrows=num_cats, ncols=1, figsize=(12, 4 * num_cats), sharex=True)
    
    # Ensure axes is iterable even if there's only 1 category
    if num_cats == 1:
        axes = [axes]

    # Colors for iterating through multiple metrics in the same subplot
    color_cycle = plt.rcParams['axes.prop_cycle'].by_key()['color']

    # 4. Generate Subplots
    for i, p in enumerate(plots_config):
        ax1 = axes[i]
        category = p.get('category')
        metrics = p.get('metrics', [])
        
        ax2 = ax1.twinx()
        #ax2.set_yscale('symlog')
        # Get the current limits of the N axis
        ymin, ymax = ax2.get_ylim()
        # If the highest N is more than 10 times larger than the lowest N, use log scale
        if ymax > 0 and ymin > 0 and (ymax / ymin) > 10:
            ax2.set_yscale('log')
            # Optional: force readable labels instead of scientific notation
            ax2.yaxis.set_major_formatter(ticker.ScalarFormatter())         
         
        lines_for_legend = []
        labels_for_legend = []

        for j, metric in enumerate(metrics):
            mask = (df['Category'] == category) & (df['Metric'] == metric)
            subset = df[mask].copy()

            if subset.empty:
                print(f"  -> Skipping {category} - {metric}: No data found.")
                continue

            subset.sort_values('Date', inplace=True)
            c = color_cycle[j % len(color_cycle)]

            # Plot Jmin (Solid line, circles, left Y-axis)
            l1, = ax1.plot(subset['Date'], subset['Jmin'], marker='o', linestyle='-', color=c, linewidth=2)
            # Plot N (Dashed line, squares, right Y-axis, slightly lighter alpha)
            l2, = ax2.plot(subset['Date'], subset['N'], marker='s', linestyle='--', color=c, linewidth=1.5, alpha=0.6)
            
            # Only add the primary Jmin line to the legend to reduce clutter
            lines_for_legend.append(l1)
            labels_for_legend.append(f"{metric}")

        # Subplot Formatting
        ax1.set_title(f"{category}", fontweight='bold')
        ax1.set_ylabel("Jmin Value")
        ax2.set_ylabel("Observation Count (N)")
        ax1.grid(True, linestyle='--', alpha=0.7)
        
        if lines_for_legend:
            # Put the legend outside the plot box so it doesn't cover data
            ax1.legend(lines_for_legend, labels_for_legend, loc='center left', bbox_to_anchor=(1.08, 0.5))

    # Format Bottom X-axis dates
    axes[-1].set_xlabel("Date", fontweight='bold')
    axes[-1].xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
    fig.autofmt_xdate()

    # Save
    plt.tight_layout()
    out_filename = "dashboard_jmin_stats.png"
    out_filepath = os.path.join(args.out_path, out_filename)
    fig.savefig(out_filepath, dpi=120, bbox_inches='tight')
    plt.close(fig)
    print(f"  -> SUCCESS: Created dashboard plot: {out_filepath}")

if __name__ == "__main__":
    main()
