#=======================================================================
# CMake script for RTOFS HYCOM Utilities 
#=======================================================================
cmake_minimum_required(VERSION 3.15)
project(HYCOM_UTILS LANGUAGES Fortran)

# Module environment variables
include_directories($ENV{NETCDF_INCLUDES})
link_directories($ENV{NETCDF_LIBRARIES})

# Fortran flags for reading HYCOM (.a) binary files
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(HYCOM_INTEL_FLAGS "-traceback -xHost -O3 -fp-model precise -ftz -align array64byte -convert big_endian -assume byterecl -warn nogeneral -diag-disable 10212")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${HYCOM_INTEL_FLAGS}")
endif()

# Executable targets
add_executable(check_hycom_archive_bathymetry 
    hycom_io_utils.F90
    check_hycom_archive_bathymetry.F90
)
target_link_libraries(check_hycom_archive_bathymetry PRIVATE netcdff netcdf)

add_executable(archv2mom6z
    hycom_io_utils.F90
    archv2mom6z.F90
)
target_link_libraries(archv2mom6z PRIVATE netcdff netcdf)
