# RTOFS NetCDF to GRIB2 Conversion Tool

## 📌 Overview
This tool provides a modernized, high-performance solution for converting Global RTOFS (Real-Time Ocean Forecast System) NetCDF production files into NCEP-standard GRIB2 format. It replaces the legacy `rtofs_nc2grb2` shell-processing workflow with a single, optimized Fortran 90 binary.



---

## 🚀 Why This Process is Superior

The legacy `rtofs_nc2grb2` process relied on a fragmented, multi-step pipeline (NetCDF → ncdump → ASCII text → GRIB Packer). This new consolidated approach offers significant advantages:

### 1. Direct Memory Pipeline (Zero Disk Bloat)
* **Legacy:** Dumped massive NetCDF arrays into intermediate ASCII files (like `fort.20`). For a $4320 \times 3298$ grid, this created gigabytes of temporary disk waste.
* **Modern:** Uses the `NetCDF-Fortran` interface to stream data directly into memory. Disk I/O is reduced by nearly **95%**, significantly lowering the load on the `/lfs` filesystem.

### 2. Numerical Integrity
* **Legacy:** Converting floating-point data to ASCII text and back to binary introduced "precision crawl" and rounding errors.
* **Modern:** Maintains native 32-bit floating-point precision throughout. Mathematical operations (like Celsius-to-Kelvin) are performed using high-speed Fortran array syntax.

### 3. Native HPC Optimization
* **Legacy:** Relied on `sed`, `awk`, and shell loops which are slow and difficult to scale.
* **Modern:** A compiled binary using the Intel 19.1 compiler (`-O3`) and the Cray Programming Environment (`ftn`). It is specifically tuned for the **WCOSS2** architecture.



---

## 🛠 Technical Comparison

| Feature | Legacy Shell Process | New Consolidated Binary |
| :--- | :--- | :--- |
| **Logic Flow** | NetCDF ➜ ASCII ➜ GRIB2 | NetCDF ➜ Memory ➜ GRIB2 |
| **Intermediate Files** | Massive `.txt` / `fort.XX` | **None** |
| **Unit Conversion** | External scripts/NCO | Integrated Fortran `WHERE` blocks |
| **Speed** | Slow (minutes/file) | **Fast (seconds/file)** |
| **Error Handling** | Silent shell failures | Strict Fortran I/O exception handling |

---

## 📂 Components

| File | Description |
| :--- | :--- |
| `rtofs_nc_to_grib2.f90` | The core Fortran 90 source code. |
| `compile.sh` | Shell script to build the binary with correct library links. |
| `run_rtofs_grib.sh` | Execution script that handles environment modules and input. |
| `run_all_rtofs.sh` | Batch script to loop through a full forecast directory. |

---

## ⚙️ Quick Start

### 1. Build the Binary
```bash
./compile.sh
