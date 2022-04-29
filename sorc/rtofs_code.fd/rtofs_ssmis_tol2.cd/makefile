#Robert Grumbine 27 May 2016
SHELL=/bin/sh

###
#MMAB_INC=/lfs/h2/emc/couple/noscrub/Robert.Grumbine/mmablib/include
#MMAB_LIBF4=/lfs/h2/emc/couple/noscrub/Robert.Grumbine/mmablib/libombf_4.a
MMAB_INC=../../mmablib/include
MMAB_LIBF4=../../mmablib/libombf_4.a
###
        
# include ../makeall.mk
#work
#MMAB_LIB=-L/u/Robert.Grumbine/rgdev/mmablib/v3.5.0/
#MMAB_INC=/u/Robert.Grumbine/rgdev/mmablib/v3.5.0/include
#module load NetCDF
#module load bufr 
#module load w3nco 
#module load w3emc
NCEPLIB=$(BUFR_LIB4) $(W3EMC_LIB4) $(W3NCO_LIB4)
FC=ftn
#FOPTS=-c -std90 -O2 $(MMAB_INC)
FOPTS=-c -O2 $(MMAB_INC)

#home:
##MMAB_LIB=-L/Volumes/ncep/mmablib/v3.5.0/
##MMAB_INC=-I/Volumes/ncep/mmablib/v3.5.0/include
#NETCDF_INCLUDE=-I /Users/rmg3/usrlocal/include/
#NETCDF_LDFLAGS_C=-L /Users/rmg3/usrlocal/lib/
#NCEPLIB=/Volumes/ncep/nceplibs/*4.a
#FC=gfortran

#FOPTS=-c -std=f95 -O2 $(MMAB_INC) $(NETCDF_INCLUDE)
FOPTS=-c -O2 -I$(MMAB_INC) -I$(NETCDF_INCLUDES)

#all -- makeall.mk suitable:  
#LIBS=/nwprod2/lib/mmab/v3.4.1/libombf.v3.4.1_4.a -lm

LIBS=$(MMAB_LIBF4)
CPP=g++
#CPPOPTS= -c -ansi -Wall -O2 -DLINUX -DCPLUS $(MMAB_INC)
CPPOPTS= -c -O2 -DLINUX -DCPLUS -I$(MMAB_INC) -I$(NETCDF_INCLUDES)
CPPLD=g++
CPPLDFLAGS=-lombf_4 -lombc_4

BINDIR= ../../../exec

CC=cc
#COPTS=-c -Wall -O2 -DLINUX $(MMAB_INC)
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

all : ssmis_tol2 

bayes : bayes.o
	g++ $< $(LIBS) -o $(@)

demo : demo.o
	g++ $< $(LIBS) -o $(@)

ssmis_tol2 : algorithm.o filter.o bufr.o skel.o ssmisu_decode.o
	$(FC) ssmisu_decode.o filter.o skel.o bufr.o algorithm.o -L$(NETCDF_LIBRARIES) -lnetcdf  $(NCEPLIB) $(LIBS) -o $(@)

install: 
	 -mv ssmis_tol2 ${BINDIR}/rtofs_ssmis_tol2

# ---------------------------------------------------------------------

clean :
	rm -f *.o ssmis_tol2

distclean : clean
	rm ssmis_tol2 bayes demo
