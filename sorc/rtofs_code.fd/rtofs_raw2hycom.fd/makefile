
#####################################################################
###  Set utilities and flags:  					  ###
#####################################################################
#
AR=ar
CPP=/usr/bin/cpp
CC=icc
FC=ifort
LD=ifort
ARFLAGS=-rv
LIBS=
CPPFLAGS= 
##CPPFLAGS=-DENDIAN_IO 
FLIBS=-shared-intel
CCFLAGS= -O -mcmodel=medium -fPIC -c 
FCFLAGS= -g -O3 -fp-model source -convert big_endian -assume byterecl -mcmodel=medium -fPIC -c 
LDFLAGS= -g -O3 -fp-model source -convert big_endian -assume byterecl -mcmodel=medium -fPIC  
BINDIR= ../../../exec
CMD   =		rtofs_raw2hycom
PROFLIB =	-lprof
#
#
#
SRC = raw2hycom.F hycom_endian_io.F parse.c
#
#OBJS = ${SRC:.f=.o} ${SRC:.F=.o} ${SRC:.c=.o}
OBJS = raw2hycom.o hycom_endian_io.o parse.o
#
#####################################################################
###  Programs to be compiled by this makefile:			  ###
#####################################################################
#
all: $(CMD)
install:: clean $(CMD) moveexe finish 
#
####################################################################
###  Compilation instructions for main programs:		  ###
#####################################################################
#
 $(CMD): ${OBJS}
	${LD} ${LDFLAGS} -o   $(CMD)  ${OBJS} $(FLIBS) 
#
#
clean:
	-rm -f ${OBJS} *.mod *.F.f $(CMD) 
        
finish:
	-rm ${OBJS} *.mod *.F.f 

moveexe:
	-mv $(CMD) ${BINDIR}

#
makedep:
	makedepend -f makefike ${SRC}

#
########################
.SUFFIXES:
.SUFFIXES: .o .f .l .sh .h  .F .F90 .f90 .c
#
#SINGLE SUFFIX RULES
#
.c:
	$(CC) $(CCFLAGS)  $< -o $@
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
parse.o:  parse.c
hycom_endian_io.o:   hycom_endian_io.F parse.o
raw2hycom.o:  raw2hycom.F parse.o  hycom_endian_io.o
