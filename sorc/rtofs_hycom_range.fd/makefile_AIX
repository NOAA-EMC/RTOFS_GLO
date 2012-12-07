
#####################################################################
###  Set utilities and flags:  					  ###
#####################################################################
#
AR=ar
CPP=/usr/lib/cpp -P 
FC=ncepxlf95
LD=ncepxlf95
ARFLAGS=-rv
LIBS=
CPPFLAGS=
FFLAGS=-qfixed -O3 -qstrict -qarch=pwr3 -qtune=pwr3 -qcache=auto -qspillsize=32000 -q64 # -qrealsize=4 -qintsize=4 
FCFLAGS= -c -qfixed -qstrict -qspillsize=32000 -qwarn64 
LDFLAGS=-O3 -qarch=auto
#
BINDIR= ../../exec
CMD   =		rtofs_hycom_range
PROFLIB =	-lprof
#
#
#
SRC = hycom_range.f
#
OBJS = ${SRC:.f=.o}
#
#####################################################################
###  Programs to be compiled by this makefile:			  ###
#####################################################################
#
all: $(CMD)
install:: clean $(CMD) finish
#
####################################################################
###  Compilation instructions for main programs:		  ###
#####################################################################
#
 $(CMD): ${OBJS}
	${LD} ${LDFLAGS} -o   $(CMD) ${OBJS} $(LIBS) 
#
#
clean:
	-rm ${OBJS} *.x *.mod *.F.f $(CMD) 
        
finish:
	-rm ${OBJS} *.mod 

moveexe:
	-mv $(CMD) ${BINDIR}

#
makedep:
	makedepend -f makefike ${SRC}

#
########################
.SUFFIXES:
.SUFFIXES: .o .f .l .sh .h  .F .F90 .f90
#
#SINGLE SUFFIX RULES
#
.f:
	$(FC) $(FCFLAGS)  $< -o $@
.F:
	$(CPP) $(CPPFLAGS) -P $< > $*.F.f
	$(FC) $(FCFLAGS) $*.f -o $*
	-rm -f $*.f
.f90:
	$(FC) $(F90FLAGS) $< -o $@
.F90:
	$(CPP) $(CPPFLAGS)  -P $< > $*.f90
	$(FC) $(F90FLAGS) $*.f90 -o $*
#	-rm -f $*.f90
.sh:
	cp $< $@; chmod a+x $@
#
#DOUBLE SUFFIX RULES
#
.F.f:
	$(CPP) $(CPPFLAGS) -P $< > $*.f
	$(FC) $(FCFLAGS) -c $*.f
.F.o:
	$(CPP) $(CPPFLAGS) -P $< > $*.f
	$(FC) $(FCFLAGS)  -c  $*.f 
	-rm -f $*.f
.f90.o:
	$(FC) $(F90FLAGS)  -c  $*.f90 
.F90.f90:
	$(CPP) $(CPPFLAGS) -P $< > $*.f90
	$(FC) $(F90FLAGS) -c $*.f90
.F90.o:
	$(CPP) $(CPPFLAGS) -P $< > $*.f90
	$(FC) $(F90FLAGS)  -c  $*.f90 
#	-rm -f $*.f90
.f90.a:
	$(FC) -c $(F90FLAGS) $<
	$(AR) $(ARFLAGS) $@ $*.o
	rm -f $*.o
#
# DO NOT DELETE THIS LINE - make depend depends on it.
