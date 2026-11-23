#!/usr/bin/env python3
"""Plot platform profile locations configured via YAML from a converted RTOFS profile NetCDF file."""

import argparse
from pathlib import Path
import sys
import yaml
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

try:
    import cartopy.crs as ccrs
    import cartopy.feature as cfeature
    HAS_CARTOPY = True
except ImportError:
    HAS_CARTOPY = False

sys.path.insert(0, str(Path(__file__).resolve().parent))
from utils import load_obs_dataset


DEFAULT_CONFIG = Path(__file__).resolve().with_name("plot_argo_config.yaml")


def load_config(config_path: Path) -> dict:
    if not config_path.is_file():
        print(f"ERROR: Config file not found: {config_path}", file=sys.stderr)
        sys.exit(1)
    with open(config_path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def setup_map_axes(figsize: tuple):
    """Create a Cartopy PlateCarree map if available, otherwise a standard lat/lon axis."""
    if HAS_CARTOPY:
        try:
            fig, ax = plt.subplots(
                figsize=figsize,
                subplot_kw={"projection": ccrs.PlateCarree()},
            )
            ax.set_global()
            ax.add_feature(cfeature.LAND, facecolor="lightgray", edgecolor="none", zorder=1)
            ax.coastlines(resolution="110m", linewidth=0.6, color="dimgray", zorder=2)
            gl = ax.gridlines(draw_labels=True, linestyle="--", alpha=0.5)
            gl.top_labels = False
            gl.right_labels = False
            return fig, ax, {"transform": ccrs.PlateCarree()}
        except Exception as exc:
            plt.close("all")
            print(f"WARNING: Cartopy map features unavailable ({exc}); falling back to plain axes.")

    fig, ax = plt.subplots(figsize=figsize)
    ax.set_xlim(-180, 180)
    ax.set_ylim(-90, 90)
    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")
    ax.grid(True, linestyle="--", alpha=0.5)
    return fig, ax, {}


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Plot platform locations from profile NetCDF using a YAML config."
    )
    parser.add_argument("profile_nc", type=Path, help="Path to YYYYMMDD.profile.nc")
    parser.add_argument(
        "-c", "--config", type=Path, default=DEFAULT_CONFIG,
        help=f"Path to YAML config (default: {DEFAULT_CONFIG.name})",
    )
    parser.add_argument("-o", "--output", type=Path, default=None, help="Output PNG path")
    args = parser.parse_args()

    if not args.profile_nc.is_file():
        print(f"ERROR: File not found: {args.profile_nc}", file=sys.stderr)
        return 1

    cfg = load_config(args.config)
    group_name = cfg.get("platform_group", "Platform")
    target_types = set(cfg.get("data_type_names", []))
    plot_cfg = cfg.get("plot", {})

    if not target_types:
        print(f"ERROR: No data_type_names specified in {args.config}", file=sys.stderr)
        return 1

    ds, _ = load_obs_dataset(args.profile_nc)

    sal_match = ds["sal_type_name"].isin(list(target_types)) if "sal_type_name" in ds.data_vars else False
    temp_match = ds["temp_type_name"].isin(list(target_types)) if "temp_type_name" in ds.data_vars else False
    subset = ds.where(sal_match | temp_match, drop=True)

    # Wrap longitudes to [-180, 180]
    lons = ((subset["lon"].values + 180.0) % 360.0) - 180.0
    lats = subset["lat"].values

    good_mask = (subset["flag"].values == 0).any(axis=1)

    figsize = tuple(plot_cfg.get("figsize", [12, 6]))
    fig, ax, plot_kwargs = setup_map_axes(figsize)

    ax.scatter(
        lons[good_mask], lats[good_mask],
        s=plot_cfg.get("good_size", 14),
        c=plot_cfg.get("good_color", "tab:blue"),
        marker=plot_cfg.get("good_marker", "o"),
        alpha=0.8,
        zorder=3,
        label=f"Passed QC ({good_mask.sum()})",
        **plot_kwargs,
    )
    if (~good_mask).any():
        ax.scatter(
            lons[~good_mask], lats[~good_mask],
            s=plot_cfg.get("flagged_size", 18),
            c=plot_cfg.get("flagged_color", "tab:red"),
            marker=plot_cfg.get("flagged_marker", "x"),
            zorder=4,
            label=f"Flagged ({(~good_mask).sum()})",
            **plot_kwargs,
        )

    obs_date = args.profile_nc.name.split(".")[0]
    ax.set_title(f"RTOFS {group_name} Profiles — {obs_date} (Total: {subset.sizes['nobs']})")
    ax.legend(loc="lower left", framealpha=0.9)

    out_png = args.output or args.profile_nc.with_name(f"{obs_date}_{group_name.lower()}_locations.png")
    plt.tight_layout()
    fig.savefig(out_png, dpi=plot_cfg.get("dpi", 150))
    print(f"Saved: {out_png}")

    ds.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
