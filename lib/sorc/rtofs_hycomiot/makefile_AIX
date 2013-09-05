SHELL=/bin/sh

# These modules are used by several of the OFS source codes. They are 
# extracted to build a library, so that there won't be multiple copies 
# of the same files. 
 
LIB=../../librtofs_hycomiot_4.a
MODDIR=../../incmod/rtofs_hycomiot_mods_4

#--------------------------------------
# The following was used for XLF on AIX
DEFS= -DAIX -DREAL4
FC=ncepxlf95
CPP=/usr/lib/cpp -P
CPPFLAGS  = DAIX -DREAL8 -DSERIAL_IO -DTIMER -DARCTIC
INC1   = ../../rtofs_serial_mods_4
LIBS  = -L/nwprod/lib -lw3_4 -lbacio_4  -L../../. -lrtofs_serial_mods_4
FFLAGS= -qfree=f90 -qstrict -qspillsize=32000 -qwarn64 \
        -qflttrap=overflow:zerodivide:invalid:enable:imprecise -qsigtrap \
	-I$(INC1) $(LIBS) 
CFLAGS= 
ARFLAGS= -X64 -q -v
LDFLAGS= $(FFLAGS) -b64

RM            = /usr/bin/rm -f
TESTD         = /usr/bin/test -d
TESTF         = /usr/bin/test -f
MV            = /usr/bin/mv -f
CP            = /usr/bin/cp -p
MKDIR         = /usr/bin/mkdir -p
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
	$(CPP) $(CPPFLAGS) $< | sed -e '/^ *$$/d' > $<.f
	$(FC) $(FFLAGS) -c $<.f

.f.a:	
	$(FC) -c $(FFLAGS) $<
	ar $(ARFLAGS) $@ $*.o

.f90.a:	
	$(FC) -c $(FFLAGS) $<
	ar $(ARFLAGS) $@ $*.o

$(LIB):	cleanall $(LIB)(flush.o mod_dump.o  mod_hytime.o mod_hycomio1.o mod_gribio.o)

cleanall:
	-$(RM) *.F.f *.mod *.o $(LIB) core $(MODDIR)/*.mod

install: cleanall $(LIB)
	$(TESTD) $(MODDIR) || $(MKDIR) $(MODDIR) ; \
	$(MV) mod_dump.mod mod_gribio.mod mod_hycomio1.mod mod_hytime.mod $(MODDIR)
	-$(RM) *.o

flush.o: flush.f90
mod_dump.o: mod_dump..f90
mod_hytime.o: hytime.f90
mod_hycomio1.o: mod_hycomio1.f90 mod_hytime.o
mod_gribio.o: mod_gribio.f90
