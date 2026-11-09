#=======================================================================
# CMake script for RTOFS HYCOM Utilities 
#=======================================================================
cmake_minimum_required(VERSION 3.15)
project(HYCOM_UTILS LANGUAGES C Fortran)

# 1. Use WCOSS2 module environment variables directly instead of find_package
include_directories($ENV{NETCDF_INCLUDES})
link_directories($ENV{NETCDF_LIBRARIES})

# 2. Critical Intel Fortran flags for reading HYCOM .a binary files
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -traceback -convert big_endian -fp-model precise -assume byterecl")
endif()

# 3. Define all the common HYCOM utility sources (Removed obsolete files)
set(HYCOM_COMMON_SRCS
    mod_ncio.f90 mod_plot.F mod_xc.F mod_za.F mod_zb.F wtime.F
    bigrid.f blkin.f buoflx.f extrct.f fordate.f
    getdat.f getdepth.f horout_nc.f indxi.f indxj.f layer2z.f mixlay.f
    psmoo.f zebra.f zh.F dum_gks.f
)

# 4. Create a static "core" library from the common files. 
add_library(hycom_core STATIC ${HYCOM_COMMON_SRCS})

# Link the standard NetCDF Fortran and C libraries found in $NETCDF_LIBRARIES
target_link_libraries(hycom_core PUBLIC netcdff netcdf)

# 5. Add our updated executable target 
add_executable(rtofs_archv2ncdf3z_2023 
    mod_ppsw.F 
    archv2data3z_2023.f
)

# 6. Link the executable to the core library and the WCOSS2 NCEP libraries
target_link_libraries(rtofs_archv2ncdf3z_2023 
    PRIVATE 
    hycom_core
    $ENV{W3NCO_LIB4} 
    $ENV{BACIO_LIB4}
)
