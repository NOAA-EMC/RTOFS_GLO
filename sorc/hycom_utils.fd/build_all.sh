#!/bin/bash
# Stop on error and print commands as they run
set -ex

echo "================================================="
echo " Building RTOFS HYCOM Utilities via CMake"
echo "================================================="

# 1. Reset environment safely (preserves critical Cray modules)
module reset
module load intel netcdf bacio w3nco

# Show loaded modules in the log for reproducibility
module list

# 2. Move into the out-of-source build directory
mkdir -p build
cd build

# 3. Purge old CMake cache if rebuilding
rm -rf CMakeCache.txt CMakeFiles/

# 4. Configure the build
cmake ..

# 5. Compile using 4 parallel jobs
make -j 4

# 6. Copy the executable to the top-level exec directory manually
echo "Moving executable to top-level exec directory..."
mkdir -p ../../../exec
cp rtofs_archv2ncdf3z_2023 ../../../exec/

echo "================================================="
echo " Build and Install Completed Successfully!"
echo "================================================="
