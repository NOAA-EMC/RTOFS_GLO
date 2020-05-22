#
# Makefile for archive processing
#

.SUFFIXES: 
.SUFFIXES: .c .F .f .o

.F:
	@echo "Must have an explicit rule for" $*
	@exit 1
.f:
	@echo "Must have an explicit rule for" $*
	@exit 1
.c:
	@echo "Must have an explicit rule for" $*
	@exit 1



#
# --- generic make definitions
#
SHELL         = /bin/sh
RM            = \rm -f

#
# rules.
#
# intelICE_setup incorportated below:

.c.o:
	$(CC) $(CPPFLAGS) $(CCFLAGS)  -c $*.c

.f.o:
	$(FC)             $(FCFFLAGS) -c $*.f

.F.o:
	$(FC) $(CPPFLAGS) $(FCFFLAGS) -c $*.F
FC            =	ifort
FCFFLAGS      =	-g -O3 -fp-model source -convert big_endian
FCFFLAGS      =	-traceback -xHost -g -O3 -fp-model precise -ftz -align array64byte -convert big_endian -assume byterecl
CC            =	icc
CCFLAGS       =	-O
CPP           =	cpp -P
CPPFLAGS      =	-DIA32 -DREAL4
LD            =	$(FC)
LDFLAGS       =	-V $(FCFFLAGS) -shared-intel
EXTRALIBS     =
CMD           = rtofs_hycom_diff
BINDIR        = ../../../exec
#
# ---------------------------------------------------------------------
# Standard part of the Makefile starts here
# ---------------------------------------------------------------------
#

MODS     =	mod_mean.o mod_mean_esmf.o mod_xc.o mod_za.o wtime.o zh.o

MEAN_OBJ =	bigrid.o blkin.o \
		extrct.o \
		getdat.o putdat.o

# --- executables.
#

all:	$(CMD)
install:: clean $(CMD)  moveexe finish

${CMD}:      $(MODS) hycom_diff.o $(MEAN_OBJ)
	$(LD) $(LDFLAGS) -o ${CMD} hycom_diff.o $(MODS) $(MEAN_OBJ)


clean:
	/bin/rm -f *.o *.a *.mod ${CMD} 

finish:
	-rm ${OBJS} *.mod 

moveexe:
	-mv $(CMD) ${BINDIR}

#
# --- explicit dependencies for each .[fF] file.
#

hycom_diff.o: hycom_diff.f mod_mean.o                 mod_za.o
mod_mean.o: mod_mean.F
mod_mean_esmf.o: mod_mean_esmf.F
mod_xc.o:   mod_xc.F
mod_za.o:   mod_za.F    mod_xc.o
zh.o:       zh.F zh_sun.f zh_t3e.f zh_ymp.f

