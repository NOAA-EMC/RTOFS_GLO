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
CC            =	icc
CCFLAGS       =	-O
CPP           =	cpp -P
CPPFLAGS      =	-DIA32 -DREAL4
LD            =	$(FC)
LDFLAGS       =	-V $(FCFFLAGS) -shared-intel
EXTRALIBS     =
CMD           = rtofs_restart2archv
BINDIR        = ../../exec
#
# ---------------------------------------------------------------------
# Standard part of the Makefile starts here
# ---------------------------------------------------------------------
#


MODR     =	mod_plot.o mod_xc.o mod_za.o mod_restart.o wtime.o

DATAX_OBJ =     bigrid.o           blkin.o    \
                buoflx.o                      \
                extrct.o  extrot.o fordate.o  \
                getdat.o  getdtm.o getdepth.o \
                          indxi.o  indxj.o    \
                pakk.o             psmoo.o    \
                          zebra.o  zh.o       \
                dum_gks.o

DATAR_OBJ = 	$(DATAX_OBJ) putdat.o


# --- executables.
#

all:	$(CMD)
install:: clean $(CMD)  moveexe finish

${CMD}:      $(MODR) restart2archv.o $(DATAR_OBJ)
	$(LD) $(LDFLAGS) -o ${CMD} restart2archv.o $(MODR) $(DATAR_OBJ)


clean:
	/bin/rm -f *.o *.a *.mod M*log ${CMD} 

finish:
	-rm ${OBJS} *.mod 

moveexe:
	-mv $(CMD) ${BINDIR}

#
# --- explicit dependencies for each .[fF] file.
#

restart2archv.o: restart2archv.f mod_plot.o mod_za.o mod_restart.o

bigrid.o:   bigrid.f      mod_plot.o
blkin.o:    blkin.f
buoflx.o:   buoflx.f
dum_gks.o:  dum_gks.f
extrct.o:   extrct.f
extrot.o:   extrot.f
fordate.o:  fordate.f
getdat.o:   getdat.f      mod_plot.o mod_za.o
getdepth.o: getdepth.f    mod_plot.o mod_za.o
getdtm.o:   getdtm.f      mod_plot.o mod_za.o
indxi.o:    indxi.f       mod_plot.o
indxj.o:    indxj.f       mod_plot.o
pakk.o:     pakk.f
psmoo.o:    psmoo.f       mod_plot.o
putdat.o:   putdat.f      mod_plot.o mod_za.o
wtime.o:    wtime.F

mod_plot.o: mod_plot.F
mod_xc.o:   mod_xc.F   
mod_za.o:   mod_za.F   mod_xc.o

zh.o:       zh.F zh_sun.f zh_t3e.f zh_ymp.f
