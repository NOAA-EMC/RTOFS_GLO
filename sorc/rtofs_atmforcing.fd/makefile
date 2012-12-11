SHELL=/bin/ksh
#
# This makefile was produced by /usr/bin/fmgen at 04:12:32 PM on 05/01/95
# If it is invoked by the command line
#	make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named swathps.
#
# If it is invoked by the command line
#	make -f makefile swathps.prof
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable which profiles
# named swathps.prof.
#
# To remove all the objects but leave the executables use the command line
#	make -f makefile clean
#
# To remove everything but the source files use the command line
#	make -f makefile clobber
#
# To remove the source files created by /usr/bin/fmgen and this makefile
# use the command line
#	make -f makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#
SRCS=   constants.f90 mod_flags.f90 mod_geom.f90 atmforcing.f90
OBJS=	${SRCS:.f90=.o}

# Tunable parameters
#
# CF		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
# 
CF =		ifort

INC1   = ../../lib/incmod/rtofs_serial_mods_4 
INC2   = ../../lib/incmod/rtofs_hycomiot_mods_4
LIBS  = -L../../lib -lrtofs_serial_mods_4 -lrtofs_hycomiot_4 \
        -L/nwprod/lib -lw3nco_v2.0.3_4 -lbacio_v2.0.1_4
CMD   =		rtofs_atmforcing
PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
FFLAGS = -free -O3  
LDFLAGS = -O3 

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build swathps.
#
all:	$(CMD)

$(CMD):	$(OBJS) 
	$(CF) $(LDFLAGS) -o $(CMD) $(OBJS) $(LIBS)

#$(CMD):		$(OBJS) 
#	$(CF) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)
#
#
#(OBJS):	$(SRCS) $(INC1) $(INC2)
#	$(CF) $(FFLAGS) -c $< -I$(INC1) -I$(INC2)

# Make the profiled version of the command and call it swathps.prof
#
$(CMD).prof:	$(OBJS)
	$(CF) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS) *.mod

copy_summ:
	cp summary.o.hold summary.o

allclean:	clean all

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile


atmforcing.o: atmforcing.f90 constants.o mod_flags.o mod_geom.o 

################################
.IGNORE:
.SUFFIXES: .f90 .o

#	There is probably no reason to modify these rules

.f90.o:
	$(CF) $(FFLAGS) -c $< -I$(INC1) -I$(INC2)

#.f90.o:
#	rm $@ $*.mod
#	$(CF) $(FFLAGS) -c $< -I$(INC1) -I$(INC2)
