#!/usr/bin/env python3
"""Plot Argo profile counts across RTOFS cycles from a CSV file or directory of daily CSVs."""

import argparse
from pathlib import Path
import sys
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.dates as mdates


def load_all_csvs(target: Path) -> pd.DataFrame:
    if target.is_file():
        files = [target]
    elif target.is_dir():
        files = sorted(target.glob("argo_counts_*.csv"))
    else:
        print(f"ERROR: Path not found: {target}", file=sys.stderr)
        sys.exit(1)

    if not files:
        print(f"ERROR: No argo_counts_*.csv files found in {target}", file=sys.stderr)
        sys.exit(1)

    frames = []
    for f in files:
        try:
            df_part = pd.read_csv(f)
            if not df_part.empty:
                frames.append(df_part)
        except Exception as exc:
            print(f"WARNING: Skipping {f}: {exc}", file=sys.stderr)

    if not frames:
        print("ERROR: No valid CSV rows loaded.", file=sys.stderr)
        sys.exit(1)

    df = pd.concat(frames, ignore_index=True)
    df = df.drop_duplicates(subset=["cycle_date", "obs_date"], keep="last")
    return df


def main() -> int:
    parser = argparse.ArgumentParser(description="Plot Argo lookback counts across RTOFS cycles.")
    parser.add_argument(
        "input_path", nargs="?", type=Path, default=Path("."),
        help="CSV file or directory containing argo_counts_*.csv (default: current directory)",
    )
    parser.add_argument(
        "-n", "--last-n-cycles", type=int, default=10,
        help="Number of most recent cycles to display (default: 10; 0 = all)",
    )
    parser.add_argument("-o", "--output", type=Path, default=None, help="Output PNG path")
    args = parser.parse_args()

    df = load_all_csvs(args.input_path)
    df["cycle_dt"] = pd.to_datetime(df["cycle_date"].astype(str), format="%Y%m%d")
    df["obs_dt"] = pd.to_datetime(df["obs_date"].astype(str), format="%Y%m%d")

    all_cycles = sorted(df["cycle_date"].unique())
    if args.last_n_cycles > 0 and len(all_cycles) > args.last_n_cycles:
        cycles = all_cycles[-args.last_n_cycles:]
        df = df[df["cycle_date"].isin(cycles)].copy()
    else:
        cycles = all_cycles

    latest_cycle = cycles[-1]
    cmap = plt.get_cmap("viridis", max(len(cycles) - 1, 1))

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 9.5))

    # Panel A: Count vs. Observation Date
    for idx, cyc in enumerate(cycles):
        sub = df[df["cycle_date"] == cyc].sort_values("obs_dt")
        is_latest = (cyc == latest_cycle)
        ax1.plot(
            sub["obs_dt"], sub["argo_any"],
            marker="o",
            linewidth=2.4 if is_latest else 1.3,
            markersize=6 if is_latest else 4,
            alpha=1.0 if is_latest else 0.75,
            color="crimson" if is_latest else cmap(idx),
            zorder=10 if is_latest else 2,
            label=f"rtofs.{cyc}" + (" (latest)" if is_latest else ""),
        )

    ax1.set_title(f"Argo Profile Counts Across RTOFS Lookback Window (Last {len(cycles)} Cycles)")
    ax1.set_ylabel("Argo Profile Count")
    ax1.grid(True, linestyle="--", alpha=0.5)
    ax1.spines["top"].set_visible(False)
    ax1.spines["right"].set_visible(False)
    ax1.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))
    ax1.xaxis.set_major_locator(mdates.DayLocator(interval=2))
    ax1.legend(title="RTOFS Cycle", bbox_to_anchor=(1.01, 1), loc="upper left", fontsize=9)

    # Panel B: Count vs. Relative Lookback Lag
    for idx, cyc in enumerate(cycles):
        sub = df[df["cycle_date"] == cyc].sort_values("lag_days")
        is_latest = (cyc == latest_cycle)
        ax2.plot(
            sub["lag_days"], sub["argo_any"],
            marker="o",
            linewidth=2.4 if is_latest else 1.3,
            markersize=6 if is_latest else 4,
            alpha=1.0 if is_latest else 0.75,
            color="crimson" if is_latest else cmap(idx),
            zorder=10 if is_latest else 2,
            label=f"rtofs.{cyc}",
        )

    ax2.set_title("Argo Profile Counts by Relative Lookback Lag (obs_date - cycle_date)")
    ax2.set_xlabel("Days Relative to Cycle Date (-1 = Most Recent Day)")
    ax2.set_ylabel("Argo Profile Count")
    ax2.set_xticks(range(-15, 0))
    ax2.grid(True, linestyle="--", alpha=0.5)
    ax2.spines["top"].set_visible(False)
    ax2.spines["right"].set_visible(False)

    plt.tight_layout()
    base_dir = args.input_path if args.input_path.is_dir() else args.input_path.parent
    out_png = args.output or (base_dir / "argo_lookback_evolution.png")
    fig.savefig(out_png, dpi=150)
    print(f"Saved plot: {out_png}")

    # Print full lag table (-15 to -1) across cycles
    piv = df.pivot(index="cycle_date", columns="lag_days", values="argo_any")
    all_lags = sorted(piv.columns)
    print("\nArgo Profile Count by Cycle and Lag Days (-15 to -1):")
    print(piv[all_lags].to_string())

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
