
#####################################################################
###  Set utilities and flags:  					  ###
#####################################################################
#
AR=ar
CPP=/usr/bin/cpp -P 
FC=ifort
LD=ifort
ARFLAGS=-rv
RTOFS_LIB_INC=../../../libs/incmod
RTOFS_LIB=../../../libs
INC1   = ${RTOFS_LIB_INC}/rtofs_serial_mods_4
INC2   = ${RTOFS_LIB_INC}/rtofs_hycomiot_mods_4
LIBS=-L${RTOFS_LIB} -lrtofs_serial_mods_4 -lrtofs_hycomiot_4
CPPFLAGS=-DIA32 -DREAL4 -DARCTIC
FFLAGS=-O -fixed  -assume byterecl -convert big_endian -I$(INC1) -I$(INC2) # -03
F90FLAGS=-free -O -xHost -assume byterecl -convert big_endian -I$(INC1) -I$(INC2) # -03
FCFLAGS= -c -fixed -fp-model strict -assume byterecl -convert big_endian -I$(INC1) -I$(INC2)
LDFLAGS=-O3 -xHost
#-AIX AR=ar
#-AIX CPP=/usr/lib/cpp -P 
#-AIX FC=ncepxlf95
#-AIX LD=ncepxlf95
#-AIX ARFLAGS=-rv
#-AIX INC1   = ../../lib/incmod/rtofs_serial_mods_4
#-AIX INC2   = ../../lib/incmod/rtofs_hycomiot_mods_4
#-AIX LIBS=-L../../lib -lrtofs_serial_mods_4 -lrtofs_hycomiot_4
#-AIX CPPFLAGS=-DAIX -DREAL4 -DARCTIC
#-AIX FFLAGS=-O -qarch=auto -qfixed=72 -g  -I$(INC1) -I$(INC2) # -03
#-AIX F90FLAGS=-qsuffix=f=f90:cpp=F90 -qfree=f90 -O -qarch=auto -g -I$(INC1) -I$(INC2) # -03
#-AIX FCFLAGS= -c -qfixed -qstrict -qspillsize=32000 -qwarn64 -I$(INC1) -I$(INC2)
#-AIX LDFLAGS=-O3 -qarch=auto
#
BINDIR= ../../../exec
SRC_F90 = atmforcing_correct.f90
CMD   =		rtofs_atmforcing_correct
PROFLIB =	-lprof
#
#
#
SRC_F = get_abfld1.f

SRC =  ${SRC_F90} ${SRC_F}
#

#
OBJS_F90 = ${SRC_F90:.f90=.o}
OBJS_F = ${SRC_F:.f=.o}
OBJS = ${OBJS_F90} ${OBJS_F}
#
#####################################################################
###  Programs to be compiled by this makefile:			  ###
#####################################################################
#
all: $(CMD)
install: clean $(CMD)  moveexe finish
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
	-rm -f ${OBJS} *.x *.mod *.F.f $(CMD) 
        
finish:
	-rm -f ${OBJS} *.mod 

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
	$(CPP) $(CPPFLAGS) -P $< > $*.f
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
