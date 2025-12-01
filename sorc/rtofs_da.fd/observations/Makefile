# Makefile for compiling *.F90

# Compiler and flags
FC = ftn

CRAY_INTEL_FFLAGS      = -O1 -fpp -pc80 -traceback -WB -DCRAY -free
CRAY_INTEL_DEBUG_FFLAGS      = -O0 -fpp -pc80 -g -traceback -WB -DCRAY -check all,nooutput_conversion -ftrapuv
CRAY_INTEL_INTSIZE_4   = -i4
CRAY_INTEL_REALSIZE_4  = -real-size 32
CRAY_INTEL_SWAPIO      = -convert big_endian -assume byterecl

#FFLAGS = -O2 -fPIC
FFLAGS   = $(CRAY_INTEL_FFLAGS) $(CRAY_INTEL_FIXED) $(CRAY_INTEL_SWAPIO) \
           $(CRAY_INTEL_REALSIZE_4) $(CRAY_INTEL_INTSIZE_4)

LDFLAGS = 

# Source files
SOURCES = read_bin_qc_obs.F90
OBJECTS = $(SOURCES:.F90=.o)
EXECUTABLE = read_bin_qc_obs.x

# Default target
all: $(EXECUTABLE)

# Link object files to create executable
$(EXECUTABLE): $(OBJECTS)
	$(FC) $(LDFLAGS) -o $@ $^

# Compile Fortran source files
%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

# Clean up object files and executable
clean:
	rm -f $(OBJECTS) $(EXECUTABLE)

# Remove only object files
clean-obj:
	rm -f $(OBJECTS)

# Phony targets
.PHONY: all clean clean-obj
