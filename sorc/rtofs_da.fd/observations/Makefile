# Makefile for compiling *.F and *.F90

# Add netCDF library paths and flags
NETCDF_PATH := $(NETCDF)
NETCDF_INC := $(NETCDF_PATH)/include
NETCDF_LIB := $(NETCDF_PATH)/lib

# Compiler and flags
FC = ftn

CRAY_INTEL_BASE_FFLAGS  = -O1 -fpp -pc80 -traceback -WB -DCRAY
CRAY_INTEL_FFLAGS       = $(CRAY_INTEL_BASE_FFLAGS) -free
CRAY_INTEL_FIXED_FFLAGS = $(CRAY_INTEL_BASE_FFLAGS) -fixed -132
CRAY_INTEL_INTSIZE_4    = -i4
CRAY_INTEL_REALSIZE_4   = -real-size 32
CRAY_INTEL_SWAPIO       = -convert big_endian -assume byterecl

FFLAGS       = $(CRAY_INTEL_FFLAGS) $(CRAY_INTEL_SWAPIO) \
               $(CRAY_INTEL_REALSIZE_4) $(CRAY_INTEL_INTSIZE_4) \
               -I$(NETCDF_INC)

FFLAGS_FIXED = $(CRAY_INTEL_FIXED_FFLAGS) $(CRAY_INTEL_SWAPIO) \
               $(CRAY_INTEL_REALSIZE_4) $(CRAY_INTEL_INTSIZE_4)

# Linker flags - include netCDF libraries
LDFLAGS := -L$(NETCDF_LIB) -lnetcdf -lnetcdff

# Source files (obs_tables_mod.F must compile first)
OBJS = obs_tables_mod.o read_binary_write_nc_mod.o read_binary_qc_obs.o
EXECUTABLE = read_binary_qc_obs.x

# Default target
all: $(EXECUTABLE)

# Link object files to create executable
$(EXECUTABLE): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

# Fixed-form Fortran source (includes coda_types.h and qc_codes.h in same dir)
%.o: %.F
	$(FC) $(FFLAGS_FIXED) -c $<

# Free-form Fortran source files
%.o: %.F90
	$(FC) $(FFLAGS) -c $<

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

# Clean up object files and executable
clean:
	rm -f $(OBJS) *.mod $(EXECUTABLE)

# Remove only object files
clean-obj:
	rm -f $(OBJS) *.mod

.PHONY: all clean clean-obj
