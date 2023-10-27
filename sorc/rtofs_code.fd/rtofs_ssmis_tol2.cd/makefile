SHELL=/bin/sh

### location of omb library
MMAB_INC=./mmablib/include
MMAB_LIBF4=./mmablib/libombf_4.a
###
        
# include ../makeall.mk
CMD = rtofs_ssmis_tol2
NCEPLIB=$(BUFR_LIB4) $(W3EMC_LIB4) $(W3NCO_LIB4)
FC=ftn
FOPTS=-c -O2 -I$(MMAB_INC) -I$(NETCDF_INCLUDES)
DEBUG=-c -g -O0 -check all -check noarg_temp_created -check nopointer -warn -warn noerrors -fp-stack-check -fstack-protector-all -fpe0 -debug -traceback -ftrapuv -I$(MMAB_INC) -I$(NETCDF_INCLUDES)

LIBS=$(MMAB_LIBF4)
CPP=g++
CPPOPTS= -c -O2 -DLINUX -DCPLUS -I$(MMAB_INC) -I$(NETCDF_INCLUDES)
CPPLD=g++
CPPLDFLAGS=-lombf_4 -lombc_4

BINDIR= ../../../exec

CC=cc
COPTS=-c -Wall -ansi -O2 -DLINUX -I$(MMAB_INC) -I$(NETCDF_INCLUDES)

#Building elements
%.o: %.C
	$(CPP) $(CPPOPTS) $< -o $(@)
%.o: %.c
	$(CC) $(COPTS) $< -o $(@)

%.o: %.f
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.F
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.F90
	$(FC) $(FOPTS) $< -o $(@)



#Should be no changes below this point---------------------------------

all : buildmmablib ssmis_tol2 

bayes : bayes.o
	g++ $< $(LIBS) -o $(@)

demo : demo.o
	g++ $< $(LIBS) -o $(@)

buildmmablib : mmablib
	cd mmablib; make

ssmis_tol2 : algorithm.o filter.o bufr.o skel.o ssmisu_decode.o
	$(FC) ssmisu_decode.o filter.o skel.o bufr.o algorithm.o -L$(NETCDF_LIBRARIES) -lnetcdf  $(NCEPLIB) $(LIBS) -o $(@)

install: 
	 -mv ssmis_tol2 ${BINDIR}/$(CMD)

debug: FOPTS = $(DEBUG)
debug: all

test:
	$(CMD) < input.txt > output.txt
	diff output.txt valid_output.txt

# ---------------------------------------------------------------------

clean :
	rm -f *.o ssmis_tol2 mmablib/*.a

distclean : clean
	rm ssmis_tol2 bayes demo
