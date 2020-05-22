SHELL=/bin/sh

# These modules are used by several of the OFS source codes. They are 
# extracted to build a library, so that there won't be multiple copies 
# of the same files. 
 
LIB=../../librtofs_hycomiot_4.a
MODDIR=../../incmod/rtofs_hycomiot_mods_4

#--------------------------------------
# The following was used for XLF on AIX
FC=ifort
CPPFLAGS  = DIA32 -DREAL4 -DSERIAL_IO -DTIMER -DARCTIC
INC1   = ../../incmod/rtofs_serial_mods_4 
INC	= ${G2_INC4}
#LIBS  = -L/nwprod/lib -lw3nco_4 -lbacio_4  -L../../. -lrtofs_serial_mods_4
#
LIBS  = ${W3NCO_LIB4} ${BACIO_LIB4} ../../librtofs_serial_mods_4.a 
LIBS  = 


FFLAGS= -free -fp-model strict -assume byterecl -convert big_endian \
	-I$(INC1) -I$(INC) $(LIBS) 
CFLAGS= 
ARFLAGS= ruv
LDFLAGS= $(FFLAGS) 

RM            = /bin/rm -f
TESTD         = /usr/bin/test -d
TESTF         = /usr/bin/test -f
MV            = /bin/mv -f
CP            = /bin/cp -p
MKDIR         = /bin/mkdir -p
#
# rules.
#
.SUFFIXES: .a .f90

.c.o:
	$(CC) $(CPPFLAGS) $(CCFLAGS)  -c $*.c

.f.o:
	$(FC)  $(FCFFLAGS) -c $*.f

.F.o:
	$(RM) $<.f
	$(FC) $(CPPFLAGS) $(FFLAGS) -c $*.F

.f.a:	
	$(FC) -c $(FFLAGS) $<
	ar $(ARFLAGS) $@ $*.o

.f90.a:	
	$(FC) -c $(FFLAGS) $<
	ar $(ARFLAGS) $@ $*.o

$(LIB):	cleanall $(LIB)(mod_dump.o  mod_hytime.o mod_hycomio1.o mod_grib2io.o)
#-- $(LIB):	cleanall $(LIB)(flush.o mod_dump.o  mod_hytime.o mod_hycomio1.o mod_grib2io.o)

cleanall:
	-$(RM) *.F.f *.mod *.o $(LIB) core $(MODDIR)/*.mod

install: cleanall $(LIB)
	$(TESTD) $(MODDIR) || $(MKDIR) $(MODDIR) ; \
	$(MV) mod_dump.mod mod_grib2io.mod mod_hycomio1.mod mod_hytime.mod $(MODDIR)
	-$(RM) *.o

#-- flush.o: flush.f90
mod_dump.o: mod_dump..f90
mod_hytime.o: hytime.f90
mod_hycomio1.o: mod_hycomio1.f90 mod_hytime.o
mod_grib2io.o: mod_grib2io.f90
