#!/usr/bin/env python3
"""
compare_jmin_stats.py
Purpose: Compares Jmin and N statistics across multiple RTOFS experiments.
         Reads both plot structure and experiment paths from a YAML config.
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

def load_exp_data(data_dir, label):
    """Loads all CSVs from a directory and tags them with an experiment label."""
    files = glob.glob(os.path.join(data_dir, "jmin_*.csv"))
    if not files:
        print(f"WARNING: No jmin_*.csv files found in {data_dir}. Skipping experiment '{label}'.", file=sys.stderr)
        return pd.DataFrame()

    df_list = []
    for f in sorted(files):
        basename = os.path.basename(f)
        date_str = basename.replace("jmin_", "").replace(".csv", "")
        try:
            df = pd.read_csv(f)
            df['Date'] = pd.to_datetime(date_str, format='%Y%m%d')
            df['Experiment'] = label
            df_list.append(df)
        except Exception as e:
            print(f"Warning: Issue reading {f}: {e}", file=sys.stderr)

    if not df_list:
        return pd.DataFrame()

    combined_df = pd.concat(df_list, ignore_index=True)
    
    # Strip whitespace from categories/metrics
    combined_df['Category'] = combined_df['Category'].astype(str).str.strip()
    combined_df['Metric'] = combined_df['Metric'].astype(str).str.strip()
    
    # Force Jmin and N to be true numeric values
    combined_df['Jmin'] = pd.to_numeric(combined_df['Jmin'], errors='coerce')
    combined_df['N'] = pd.to_numeric(combined_df['N'], errors='coerce')
    
    return combined_df

def main():
    parser = argparse.ArgumentParser(description="Compare Jmin statistics across multiple experiments.")
    parser.add_argument("-c", "--config", required=True, help="Path to YAML configuration file")
    parser.add_argument("-o", "--outdir", required=True, help="Output directory to save comparison plots")
    args = parser.parse_args()

    # 1. Ensure output directory exists
    os.makedirs(args.outdir, exist_ok=True)

    # 2. Load YAML Config
    try:
        with open(args.config, 'r') as yf:
            config = yaml.safe_load(yf)
    except Exception as e:
        print(f"FATAL ERROR: Failed to read YAML config {args.config}: {e}", file=sys.stderr)
        sys.exit(1)

    plots_config = config.get('plots', [])
    exp_config = config.get('comparison', [])

    if not plots_config:
        print("WARNING: No 'plots' section found in YAML config. Exiting.", file=sys.stderr)
        sys.exit(0)
        
    if not exp_config:
        print("FATAL ERROR: No 'comparison' section found in YAML config. Exiting.", file=sys.stderr)
        sys.exit(2)

    # 3. Load Data for All Experiments defined in YAML
    all_dfs = []
    for exp in exp_config:
        label = exp.get('experiment')
        dir_path = exp.get('path')
        
        if not label or not dir_path:
            print(f"WARNING: Experiment missing 'experiment' or 'path' in YAML. Skipping: {exp}", file=sys.stderr)
            continue
            
        print(f"Loading data for experiment: {label} from {dir_path}")
        df = load_exp_data(dir_path, label)
        if not df.empty:
            all_dfs.append(df)

    if not all_dfs:
        print("FATAL ERROR: No valid data loaded from any experiment.", file=sys.stderr)
        sys.exit(3)

    master_df = pd.concat(all_dfs, ignore_index=True)
    exp_labels = master_df['Experiment'].unique()
    
    # Set up consistent colors and markers for experiments
    color_cycle = plt.rcParams['axes.prop_cycle'].by_key()['color']
    marker_cycle = ['o', 's', '^', 'D', 'v', '<', '>']
    exp_style = {label: {'color': color_cycle[i % len(color_cycle)], 
                         'marker': marker_cycle[i % len(marker_cycle)]} 
                 for i, label in enumerate(exp_labels)}

    # 4. Generate Comparison Plots
    for p in plots_config:
        category = p.get('category')
        metrics = p.get('metrics', [])

        for metric in metrics:
            mask = (master_df['Category'] == category) & (master_df['Metric'] == metric)
            subset = master_df[mask].copy()

            if subset.empty:
                print(f"  -> Skipping {category} - {metric}: No data found across experiments.")
                continue

            # Setup Figure: 2 subplots (Top: Jmin, Bottom: N)
            fig, (ax1, ax2) = plt.subplots(nrows=2, ncols=1, figsize=(10, 8), sharex=True)
            
            # Track min/max of N to dynamically apply log scale
            n_min, n_max = float('inf'), float('-inf')

            for label in exp_labels:
                exp_data = subset[subset['Experiment'] == label].copy()
                if exp_data.empty:
                    continue
                
                exp_data.sort_values('Date', inplace=True)
                c = exp_style[label]['color']
                m = exp_style[label]['marker']

                # Plot Jmin
                ax1.plot(exp_data['Date'], exp_data['Jmin'], marker=m, linestyle='-', color=c, linewidth=2, label=label)
                
                # Plot N
                ax2.plot(exp_data['Date'], exp_data['N'], marker=m, linestyle='--', color=c, linewidth=2, label=label)
                
                # Update N min/max for scaling logic
                if exp_data['N'].min() < n_min: n_min = exp_data['N'].min()
                if exp_data['N'].max() > n_max: n_max = exp_data['N'].max()

            # Formatting Jmin Panel
            ax1.set_title(f"Comparison: {category} ({metric})", fontweight='bold')
            ax1.set_ylabel("Jmin Value")
            ax1.grid(True, linestyle='--', alpha=0.7)
            ax1.legend(loc='best')

            # Formatting N Panel
            ax2.set_ylabel("Observation Count (N)")
            ax2.grid(True, linestyle='--', alpha=0.7)
            ax2.legend(loc='best')
            
            # Apply dynamic log scale to N if data spans more than 1 order of magnitude
            if n_max > 0 and n_min > 0 and (n_max / n_min) > 10:
                ax2.set_yscale('log')

            # Format X-axis
            ax2.set_xlabel("Date", fontweight='bold')
            ax2.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
            fig.autofmt_xdate()

            # Save Plot
            safe_cat = category.replace(" ", "_").lower()
            safe_met = metric.replace(" ", "_").lower()
            out_filename = f"compare_jmin_{safe_cat}_{safe_met}.png"
            out_filepath = os.path.join(args.outdir, out_filename)

            plt.tight_layout()
            fig.savefig(out_filepath, dpi=120, bbox_inches='tight')
            plt.close(fig)
            print(f"  -> Created comparison plot: {out_filepath}")

if __name__ == "__main__":
    main()
