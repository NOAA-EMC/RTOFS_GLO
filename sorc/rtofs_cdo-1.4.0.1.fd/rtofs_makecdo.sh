#!/usr/bin/sh
# build script for the cdo system
#

# All source code in /nwpara/sorc/rtofs_cdo.fd
# CDO executable in /nwpara/exec/rtofs_cdo

# Set up build environment (simplifies things)
export OBJECT_MODE=64  #this should have been set as default, but just in case...
export RTOFS_CDO=$PWD  #build back into the source directory

#  Build szip
cd $RTOFS_CDO
gtar -zxvf szip-2.1.tar.gz
cd szip-2.1
./configure --prefix=$RTOFS_CDO --disable-shared \
SCC=icc CXX=icc SFC=ifort FFLAGS="-convert LITTLE_ENDIAN"
make
make install

# Build Jasper
cd $RTOFS_CDO
gtar -zxvf jasper-1.900.1.tar.gz
cd jasper-1.900.1
./configure --prefix=$RTOFS_CDO \
--without-x --disable-opengl --disable-shared \
SCC=icc CXX=icc SFC=ifort FFLAGS="-convert LITTLE_ENDIAN"
make
make install

# Build Proj
cd $RTOFS_CDO
gtar -zxvf proj-4.7.0.tar.gz
cd proj-4.7.0
./configure --prefix=$RTOFS_CDO --without-mutex --without-jni --disable-shared \
SCC=icc CXX=icc SFC=ifort FFLAGS="-convert LITTLE_ENDIAN"
make
make install

# Build grib_api
cd $RTOFS_CDO
gtar -zxvf grib_api-1.9.8.tar.gz
cd grib_api-1.9.8
./configure --prefix=$RTOFS_CDO --with-jasper=$RTOFS_CDO --disable-python \
SCC=icc CXX=icc SFC=ifort FFLAGS="-convert LITTLE_ENDIAN"
make
make install

#  Build CDO
cd $RTOFS_CDO
gtar -zxvf cdo-1.5.0.tar.gz
cd cdo-1.5.0
./configure --prefix=$RTOFS_CDO \
--with-zlib=/usrx/local/lib \
--with-szlib=$RTOFS_CDO \
--with-hdf5=$HDF5 \
--with-netcdf=$NETCDF \
--with-proj=$RTOFS_CDO  \
--with-jasper=$RTOFS_CDO \
--with-grib_api=$RTOFS_CDO \
--enable-grib --enable-cgribex --disable-shared \
SCC=icc CXX=icc
make
make install

#--with-netcdf=/usrx/local/netcdf-3.6.3 \
echo 'Cleaning up'

cd $RTOFS_CDO
#rm -rf szip-2.1
#rm -rf jasper-1.900.1
#rm -rf proj-4.7.0
#rm -rf grib_api-1.9.8
#rm -rf cdo-1.5.0

echo "All done!"
echo "The cdo executable should be in $RTOFS_CDO/bin"

