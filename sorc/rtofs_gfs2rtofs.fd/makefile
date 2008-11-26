SHELL=/usr/bin/ksh
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
SRCS=   constants.f90 mod_flags.f90 mod_geom.f90 intp.f90 cd.f90
        

OBJS=	${SRCS:.f90=.o}

# Tunable parameters
#
# CF		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
# 
CF =		ncepxlf95

INC1   = ../../lib/rtofs_serial_mods_4 
INC2   = ../../lib/rtofs_hycomiot_mods_4
LIBS  = -L/nwprod/lib -lw3_4 -lbacio_4 \
        -L../../lib -lrtofs_serial_mods_4 -lrtofs_hycomiot_4
CMD   =		rtofs_gfs2rtofs
PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
FFLAGS = -qsuffix=f=f90:cpp=F90 -qfree=f90 -O3  
LDFLAGS = -O3 

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build swathps.
#
all:	$(CMD)

$(CMD):		$(OBJS) 
	$(CF) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

$(OBJS):	$(SRCS) $(INC1) $(INC2)
	$(CF) $(FFLAGS) -c $< -I$(INC1) -I$(INC2)

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
