# Makefile for compiling ncodaz_inc2mom6nc_glb_lyr

# Add netCDF library paths and flags
####NETCDF_PATH := $(NETCDF)
####NETCDF_INC := $(NETCDF_PATH)/include
####NETCDF_LIB := $(NETCDF_PATH)/lib

# Compiler and flags
FC = ftn
FFLAGS      = -fixed -assume byterecl -convert big_endian -I$(NETCDF_INCLUDES)
DEBUG         = -g -O0 -fixed -assume byterecl -convert big_endian -check all -check noarg_temp_created -check nopointer -warn -warn noerrors -nogen-interfaces -fp-stack-check -fstack-protector-all -fpe0 -debug -traceback -ftrapuv -I$(NETCDF_INCLUDES)

CC            = cc
CCFLAGS       =
CPP           = cpp -P
CPPFLAGS      = -P -traditional-cpp -DREAL4
LD            = $(FC)
LDFLAGS       =
EXTRALIBS     = -L$(NETCDF_LIBRARIES) -lnetcdff
BINDIR= ../../../exec

####CRAY_INTEL_FFLAGS      = -O1 -fpp -pc80 -traceback -WB -DCRAY
####CRAY_INTEL_INTSIZE_4   = -i4
####CRAY_INTEL_REALSIZE_4  = -real-size 32
####CRAY_INTEL_SWAPIO      = -convert big_endian -assume byterecl

# Combined FFLAGS (Base flags for all files)
####FFLAGS   = $(CRAY_INTEL_FFLAGS) $(CRAY_INTEL_SWAPIO) \
####           $(CRAY_INTEL_REALSIZE_4) $(CRAY_INTEL_INTSIZE_4) -I$(NETCDF_INC)

# Linker flags - include netCDF libraries
LDFLAGS = -L$(NETCDF_LIBRARIES) -lnetcdff

# Source files (For reference)
SOURCES = ncodaz_inc2mom6nc_glb_lyr.f mod_mom6.F mod_za.F mod_ncio.f90 \
          mod_xc.F blkin.f zh.F getdat.f wtime.F
#SOURCES = ncodaz_inc2mom6nc_glb_lyr.f mod_mom6.F mod_ncio.f90 blkin.f getdat.f
####          mod_xc.F blkin.f zh.F getdat.f wtime.F

# ALL objects must be listed here to satisfy the linker and pattern rules
#OBJECTS = ncodaz_inc2mom6nc_glb_lyr.o mod_mom6.o mod_za.o mod_ncio.o \
#          mod_xc.o blkin.o zh.o getdat.o wtime.o
OBJECTS = mod_xc.o mod_za.o mod_mom6.o mod_ncio.o blkin.o zh.o getdat.o wtime.o ncodaz_inc2mom6nc_glb_lyr.o
#OBJECTS = mod_mom6.o mod_ncio.o blkin.o ncodaz_inc2mom6nc_glb_lyr.o

CMD = rtofs_ncodaz_inc2mom6nc_glb_lyr

# Default target
all:	$(CMD)

# Link object files to create executable
$(CMD):	$(OBJECTS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

# --- Pattern Rules for different extensions ---

# 1. Rule for legacy Fixed Form files (e.g., .f)
# This handles getdat.f, blkin.f, and ncodaz_inc2mom6nc_glb_lyr.f
%.o: %.f
	$(FC) $(FFLAGS) -c $<

# 2. Rule for Preprocessed / Free Form files (e.g., .F)
# Forces -free flag for mod_xc.F, mod_za.F, wtime.F, mod_mom6.F
%.o: %.F
	$(FC) $(FFLAGS) -c $<
#	$(FC) $(FFLAGS) -free -c $<

# 3. Rule for modern f90 files (e.g., .f90 / .F90)
%.o: %.f90
	$(FC) $(FFLAGS) -free -c $<

%.o: %.F90
	$(FC) $(FFLAGS) -free -c $<

# Clean up object files and executable
clean:
	rm -f $(OBJECTS) *.mod $(EXECUTABLE)

# Remove only object files
clean-obj:
	rm -f $(OBJECTS)

install:
	-mv $(CMD) ${BINDIR}

# Phony targets
.PHONY: all clean clean-obj
