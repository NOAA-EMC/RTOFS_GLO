#!/usr/bin/sh
# build script for the cdo system
#

# modules
module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163
module load HDF5-serial/1.10.1
module load NetCDF/4.5.0
# HDF5-serial/1.8.20
# NetCDF/4.5.0

# Set up build environment (simplifies things)
export OBJECT_MODE=64  #this should have been set as default, but just in case...
export RTOFS_CDO=$PWD  #build back into the source directory
mkdir -p $RTOFS_CDO/bin

#Build szip
cd $RTOFS_CDO
# gtar -zxvf szip-2.1.tar.gz
cd szip-2.1
./configure --prefix=$PWD --disable-shared SCC=icc CXX=icc SFC=ifort FFLAGS="-convert LITTLE_ENDIAN"
make clean
make
make install

# Build Proj
cd $RTOFS_CDO
# gtar -zxvf proj-4.7.0.tar.gz
cd proj-4.7.0
./configure --prefix=$PWD --without-mutex --without-jni --disable-shared SCC=icc CXX=icc SFC=ifort FFLAGS="-convert LITTLE_ENDIAN"
make clean
make
make install

# Build Jasper
cd $RTOFS_CDO
# gtar -zxvf jasper-1.900.1.tar.gz
cd jasper-1.900.1
./configure --prefix=$PWD --without-x --disable-opengl --disable-shared SCC=icc CXX=icc SFC=ifort FFLAGS="-convert LITTLE_ENDIAN"
make clean
make
make install

# Build grib_api
cd $RTOFS_CDO
# gtar -zxvf grib_api-1.9.8.tar.gz
cd grib_api-1.9.8
./configure --prefix=$PWD --with-jasper=${RTOFS_CDO}/jasper-1.900.1 --disable-python SCC=icc CXX=icc SFC=ifort FFLAGS="-convert LITTLE_ENDIAN"
make clean
make
make install

# Build cdo
cd $RTOFS_CDO
# gtar -zxvf cdo-1.6.0.tar.gz
cd cdo-1.6.0
./configure --prefix=$RTOFS_CDO \
--with-szlib=$RTOFS_CDO/szip-2.1 \
--with-hdf5=$HDF5 \
--with-netcdf=$NETCDF \
--with-proj=$RTOFS_CDO/proj-4.7.0  \
--with-jasper=$RTOFS_CDO/jasper-1.900.1 \
--with-grib_api=$RTOFS_CDO/grib_api-1.9.8 \
--enable-grib --enable-netcdf --enable-cgribex --enable-shared \
SCC=icc CXX=icc
make clean
make
make install


echo "All done!"
echo "The cdo executable should be in $RTOFS_CDO/bin"

