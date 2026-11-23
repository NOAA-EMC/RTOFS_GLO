#!/usr/bin/env python3
"""Read converted RTOFS QC observation NetCDF files and decode type/QC/WMO IDs."""

import argparse
from itertools import combinations
from pathlib import Path
import sys
import xarray as xr


def build_composite_qc_map(qc_map: dict[int, str]) -> dict[int, str]:
    """Precompute exact + additive composite QC flag combinations from qc_codes."""
    resolved = dict(qc_map)
    nonzero_codes = sorted(k for k in qc_map if k > 0)

    # Profile & SSS flags can sum 2 to 4 individual check codes together
    for r in range(2, 5):
        for combo in combinations(nonzero_codes, r):
            total = sum(combo)
            if total not in resolved:
                resolved[total] = "+".join(qc_map[c] for c in combo)

    return resolved


def decode_qc_flag(flag_val: int, resolved_qc_map: dict[int, str]) -> str:
    """Decode a single or composite QC flag integer."""
    val = int(flag_val)
    if val in resolved_qc_map:
        return resolved_qc_map[val]

    # Fallback greedy decomposition (e.g. if a flag like DERIVED_SAL=12 was added twice)
    rem = val
    parts = []
    for code in sorted((k for k in resolved_qc_map if k > 0), reverse=True):
        while rem >= code and "+" not in resolved_qc_map[code]:
            parts.append(resolved_qc_map[code])
            rem -= code
    if rem == 0 and parts:
        return "+".join(reversed(parts))

    return f"UNKNOWN({val})"


def load_obs_dataset(nc_path: Path) -> tuple[xr.Dataset, dict[int, str]]:
    """Open an observation NetCDF file and attach decoded type, WMO ID, and date strings."""
    ds = xr.open_dataset(nc_path)

    # 1. Decode data type lookup table (0..209)
    type_labels = ds["data_type_name"].str.decode("utf-8").str.strip()
    decoded_type_vars = []
    for var_name in list(ds.data_vars):
        if ds[var_name].attrs.get("lookup_table") == "data_type_name":
            out_name = f"{var_name}_name"
            ds[out_name] = type_labels.isel(data_types=ds[var_name])
            decoded_type_vars.append(out_name)

    ds.attrs["_decoded_type_vars"] = decoded_type_vars

    # 2. Build QC code lookup dictionary (including composite sums)
    base_qc_map = dict(
        zip(
            ds["qc_code_id"].values.astype(int),
            ds["qc_code_name"].str.decode("utf-8").str.strip().values,
        )
    )
    qc_map = build_composite_qc_map(base_qc_map)

    # 3. Decode WMO ID and date strings if present
    if "wmo_id" in ds.data_vars:
        ds["wmo_id_str"] = ds["wmo_id"].str.decode("utf-8").str.strip()

    if "date" in ds.data_vars and ds["date"].dtype.kind == "S":
        ds["date_str"] = ds["date"].str.decode("utf-8").str.strip()

    return ds, qc_map


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Read RTOFS QC observation NetCDF file and decode metadata."
    )
    parser.add_argument(
        "input_file",
        type=Path,
        help="Path to the converted observation NetCDF file (e.g., 20261122.profile.nc)",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()

    if not args.input_file.is_file():
        print(f"ERROR: Input file not found: {args.input_file}", file=sys.stderr)
        return 1

    ds, qc_map = load_obs_dataset(args.input_file)

    print(f"Loaded: {args.input_file} (nobs={ds.sizes.get('nobs', 0)})")
    for var_name in ds.attrs.get("_decoded_type_vars", []):
        unique_types = sorted(set(ds[var_name].values.tolist()))
        print(f"  {var_name}: {unique_types}")

    if "wmo_id_str" in ds.data_vars:
        sample_wmo = [w for w in ds["wmo_id_str"].values[:5] if w]
        print(f"  Sample wmo_id: {sample_wmo}")

    if "flag" in ds.data_vars:
        unique_flags = sorted(set(ds["flag"].values.ravel().tolist()))
        decoded_flags = {f: decode_qc_flag(f, qc_map) for f in unique_flags}
        print(f"  QC flags present: {decoded_flags}")

    ds.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
