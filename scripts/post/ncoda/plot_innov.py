#!/usr/bin/env python3
"""
plot_innov.py
Purpose: Reads verification (innov) CSV files and a YAML config. Generates an 
         individual time-series plot (RMS and Bias) for each prefix defined,
         and a combined mean vertical profile plot for t_z and s_z.
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

def load_all_csvs(data_dir, prefix):
    files = glob.glob(os.path.join(data_dir, "*", f"{prefix}_*.csv"))
    if not files:
        print(f"WARNING: No files found for {prefix} in {data_dir}/*/", file=sys.stderr)
        return pd.DataFrame()

    df_list = []
    for f in sorted(files):
        basename = os.path.basename(f)
        date_str = basename.replace(f"{prefix}_", "").replace(".csv", "")
        try:
            df = pd.read_csv(f)
            df['Date'] = pd.to_datetime(date_str, format='%Y%m%d')
            df_list.append(df)
        except Exception as e:
            print(f"Warning: Issue reading {f}: {e}", file=sys.stderr)

    if not df_list:
        return pd.DataFrame()

    return pd.concat(df_list, ignore_index=True)

def plot_mean_vertical_profiles(data_dir):
    df_t = load_all_csvs(data_dir, 't_z')
    df_s = load_all_csvs(data_dir, 's_z')

    if df_t.empty and df_s.empty:
        return

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(10, 8), sharey=True)

    for df, ax, title in zip([df_t, df_s], [ax1, ax2], ['Temperature', 'Salinity']):
        if df.empty:
            continue
            
        df['Depth'] = pd.to_numeric(df['Depth'], errors='coerce')
        for col in ['Bias_Fcst', 'Bias_Anl', 'RMS_Fcst', 'RMS_Anl']:
            df[col] = pd.to_numeric(df[col], errors='coerce')

        mean_df = df.groupby('Depth').mean().reset_index()
        mean_df.sort_values('Depth', inplace=True)

        ax.plot(mean_df['RMS_Anl'], mean_df['Depth'], 'r-', lw=2, label='RMS (Anl)')
        ax.plot(mean_df['RMS_Fcst'], mean_df['Depth'], 'r--', lw=2, label='RMS (Fcst)')
        ax.plot(mean_df['Bias_Anl'], mean_df['Depth'], 'b-', lw=2, label='Bias (Anl)')
        ax.plot(mean_df['Bias_Fcst'], mean_df['Depth'], 'b--', lw=2, label='Bias (Fcst)')

        ax.set_title(f"Time-Mean {title} Profile", fontweight='bold')
        ax.set_xlabel("Value")
        ax.grid(True, linestyle='--', alpha=0.7)
        ax.axvline(0, color='k', lw=1)

    ax1.set_ylabel("Depth (m)")
    if not df_t.empty or not df_s.empty:
        ax1.invert_yaxis()
        ax1.legend(loc='best', frameon=False)

    plt.tight_layout()
    out_filepath = os.path.join(data_dir, "innov_mean_vertical_profiles.png")
    fig.savefig(out_filepath, dpi=120, bbox_inches='tight')
    plt.close(fig)
    print(f"  -> SUCCESS: Created mean profile plot: {out_filepath}")

def main():
    parser = argparse.ArgumentParser(description="Plot Innov statistics based on YAML config.")
    parser.add_argument("yaml_config", help="Path to YAML configuration file")
    args = parser.parse_args()

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

    color_cycle = plt.rcParams['axes.prop_cycle'].by_key()['color']

    # 1. Generate standard time-series plots
    for p in plots_config:
        prefix = p.get('prefix')
        target_col = p.get('target_column', 'ObsType')
        targets = p.get('targets', [])
        
        df = load_all_csvs(data_dir, prefix)
        if df.empty:
            continue

        df[target_col] = df[target_col].astype(str).str.strip()
        for col in ['Bias_Fcst', 'Bias_Anl', 'RMS_Fcst', 'RMS_Anl']:
            df[col] = pd.to_numeric(df[col], errors='coerce')

        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8), sharex=True)
        
        lines_legend = []
        labels_legend = []

        for j, tgt in enumerate(targets):
            tgt_str = str(tgt).strip()
            mask = df[target_col] == tgt_str
            subset = df[mask].copy()

            if subset.empty:
                print(f"  -> Skipping {prefix} - {tgt_str}: No data found.")
                continue

            subset.sort_values('Date', inplace=True)
            c = color_cycle[j % len(color_cycle)]

            # RMS Plot (Top)
            l1, = ax1.plot(subset['Date'], subset['RMS_Anl'], marker='o', linestyle='-', color=c, linewidth=2)
            ax1.plot(subset['Date'], subset['RMS_Fcst'], marker='s', linestyle='--', color=c, linewidth=1.5, alpha=0.7)

            # Bias Plot (Bottom)
            ax2.plot(subset['Date'], subset['Bias_Anl'], marker='o', linestyle='-', color=c, linewidth=2)
            ax2.plot(subset['Date'], subset['Bias_Fcst'], marker='s', linestyle='--', color=c, linewidth=1.5, alpha=0.7)
            
            # Store only one line per target for the clean legend
            lines_legend.append(l1)
            if target_col == 'Depth':
                labels_legend.append(f"{tgt_str} m")
            else:
                labels_legend.append(tgt_str)

        # Formatting RMS Subplot
        title_str = f"RTOFS DA Verification: {prefix.replace('_', ' ').title()}\n(Solid/Circle = Analysis | Dashed/Square = Forecast)"
        ax1.set_title(title_str, fontweight='bold')
        ax1.set_ylabel("RMS Error")
        ax1.grid(True, linestyle='--', alpha=0.7)
        if lines_legend:
            ax1.legend(lines_legend, labels_legend, loc='best', frameon=False, fontsize='small', ncol=2)

        # Formatting Bias Subplot
        ax2.set_ylabel("Mean Bias")
        ax2.set_xlabel("Date", fontweight='bold')
        ax2.grid(True, linestyle='--', alpha=0.7)
        ax2.axhline(0, color='black', linewidth=1, linestyle='-')
        
        # X-axis dates
        ax2.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
        fig.autofmt_xdate()

        plt.tight_layout()
        out_filename = f"innov_{prefix}.png"
        out_filepath = os.path.join(data_dir, out_filename)
        fig.savefig(out_filepath, dpi=120, bbox_inches='tight')
        plt.close(fig)

        print(f"  -> SUCCESS: Created time-series plot: {out_filepath}")

    # 2. Generate the single mean vertical profile plot
    plot_mean_vertical_profiles(data_dir)

if __name__ == "__main__":
    main()
