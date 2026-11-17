#!/bin/bash
set -ex

echo "================================================="
echo " Building RTOFS HYCOM Utilities via CMake"
echo "================================================="

# Set environment/modules (targeted for WCOSS2)
module reset
module load intel netcdf
module list

# Move into the out-of-source build directory
mkdir -p build
cd build

# Purge old CMake cache if rebuilding
rm -rf CMakeCache.txt CMakeFiles/

# Configure and compile
cmake ..
make -j 4

# Install executables
echo "Copying executables to top-level exec directory..."
mkdir -p ../../../exec
cp check_hycom_archive_bathymetry ../../../exec/
cp archv2mom6z ../../../exec/

echo "================================================="
echo " Build and Install Completed Successfully!"
echo "================================================="
