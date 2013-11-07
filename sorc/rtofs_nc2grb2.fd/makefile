SHELL=  /bin/sh
#
#
SRCS=   nc2grib.f

OBJS=   nc2grib.o

# Tunable parameters
#
# FC        Name of the Fortran compiling system to use
# LDFLAGS   Options of the loader
# FFLAGS    Options of the compiler
# DEBUG     Options of the compiler included for debugging
# LIBS      List of libraries
# CMD       Name of the executable
#
FC            =   ifort
LDFLAGS = 	 -O  -convert big_endian
BINDIR= ../../exec
#INC = /nwprod/lib/incmod/g2_4
#LIBS = -L/nwprod/lib -lg2_v1.4.0_4 -lw3nco_v2.0.3_4 -lbacio_v2.0.1_4 -ljasper -lpng -lz
INC = ${G2_INC4}
LIBS = ${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB4} ${PNG_LIB4} ${Z_LIB4}
CMD =      rtofs_nc2grib
#DEBUG =    
DEBUG =     
FFLAGS =   -O3  -I $(INC) $(DEBUG)

# Lines from here down should not need to be changes.  They are
# the actual rules which make uses to build CMD.
#
all:        $(CMD)
install:: clean $(CMD) moveexe finish

$(CMD):      $(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS) *.mod $(CMD)

finish:
	-rm ${OBJS} # *.mod 

moveexe:
	-mv $(CMD) ${BINDIR}

clobber:        clean
	-rm -f $(CMD) $(CMD).prof

void:   clobber
	-rm -f $(SRCS) makefile

