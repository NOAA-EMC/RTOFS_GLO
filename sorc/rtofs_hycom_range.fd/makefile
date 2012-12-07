
#####################################################################
###  Set utilities and flags:  					  ###
#####################################################################
#
AR=ar
CPP=/usr/bin/cpp -P 
FC=ifort
LD=ifort
ARFLAGS=-rv
LIBS=
CPPFLAGS=
FCFLAGS= -c -assume byterecl -cm -vec_report0 -w -O2 -xSSE4.2 -ip -tpp7 -xW -r8 -lmpi -O -fixed -fp-model strict
LDFLAGS= -assume byterecl -cm -vec_report0 -w -O2 -xSSE4.2 -ip -tpp7 -xW -r8 -O -fixed -fp-model strict
#-AIX CPP=/usr/lib/cpp -P 
#-AIX FC=ncepxlf95
#-AIX LD=ncepxlf95
#-AIX ARFLAGS=-rv
#-AIX LIBS=
#-AIX CPPFLAGS=
#-AIX FFLAGS=-qfixed -O3 -qstrict -qarch=pwr3 -qtune=pwr3 -qcache=auto -qspillsize=32000 -q64 # -qrealsize=4 -qintsize=4 
#-AIX FCFLAGS= -c -qfixed -qstrict -qspillsize=32000 -qwarn64 
#-AIX LDFLAGS=-O3 -qarch=auto
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
