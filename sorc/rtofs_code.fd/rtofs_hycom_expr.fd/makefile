
#####################################################################
###  Set utilities and flags:  					  ###
#####################################################################
#
AR=ar
CPP=/usr/bin/cpp
CC=cc
FC=ftn
LD=ftn
ARFLAGS=-rv
LIBS=
CPPFLAGS= 
##CPPFLAGS=-DENDIAN_IO 
FLIBS=-shared-intel
CCFLAGS= -O -mcmodel=medium -fPIC -c 
FCFLAGS= -g -O3 -fp-model source -convert big_endian -assume byterecl -mcmodel=medium -fPIC -c 
DEBUGFC= -g -O0 -check all -check noarg_temp_created -check nopointer -warn -warn noerrors -nogen-interfaces -fp-stack-check -fstack-protector-all -fpe0 -debug -traceback -ftrapuv -gen_interfaces_nosource
LDFLAGS= -g -O3 -fp-model source -convert big_endian -assume byterecl -mcmodel=medium -fPIC  
BINDIR= ../../../exec
CMD   =		rtofs_hycom_expr
PROFLIB =	-lprof
#
#
#
SRC = hycom_expr.F
## SRC = hycom2raw8.F hycom_endian_io.F parse.c
#
#OBJS = ${SRC:.f=.o} ${SRC:.F=.o} ${SRC:.c=.o}
OBJS = hycom_expr.o
#
#####################################################################
###  Programs to be compiled by this makefile:			  ###
#####################################################################
#
all: $(CMD)
everything: clean $(CMD)  install finish
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
	-rm -f ${OBJS} *.mod *.f $(CMD) 
        
finish:
	-rm ${OBJS} *.mod *.F.f 

install:
	-mv $(CMD) ${BINDIR}

debug: FCFLAGS = $(DEBUGFC)
debug: all

test:
	$(CMD) < input.txt > output.txt
	diff output.txt valid_output.txt

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
hycom_expr.o:  hycom_expr.F
## parse.o:  parse.c
## hycom_endian_io.o:   hycom_endian_io.F parse.o
## hycom2raw8.o:  hycom2raw8.F parse.o  hycom_endian_io.o
