SHELL=/bin/sh

# These modules are used by several of the OFS source codes. They are 
# extracted to build a library, so that there won't be multiple copies 
# of the same files. 
 
LIBMPI=../../librtofs_mpi_mods_8.a
LIBSER=../../librtofs_serial_mods_4.a
MODMPI=../../incmod/rtofs_mpi_mods_8
MODSER=../../incmod/rtofs_serial_mods_4

#--------------------------------------
# The following was used for XLF on AIX
FC=ncepxlf95
CPP=/usr/lib/cpp -P
CPPFLAGS  =DAIX -DREAL8 -DMPI -DSERIAL_IO -DTIMER -DARCTIC
FFLAGSADD= 
FFLAGSSER= -qfixed -qstrict -qspillsize=32000 -qwarn64 \
        -qflttrap=overflow:zerodivide:invalid:enable:imprecise -qsigtrap
FFLAGS=$(FFLAGSSER) $(FFLAGSADD)
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
.SUFFIXES: .c .a .f .F 

.c.o:
	$(CC) $(CPPFLAGS) $(CCFLAGS)  -c $*.c

.f.o:
	$(FC)  $(FCFFLAGS) -c $*.f

.F.o:
	$(RM) $<.f
	$(CPP) $(CPPFLAGS) $< | sed -e '/^ *$$/d' > $<.f
	$(FC) $(FFLAGS) -c $<.f

.f.a:	$(FC) -c $(FFLAGS) $<
	ar $(ARFLAGS) $@ $*.o

mpi:  cleanmpi dimension_MPI $(LIBMPI)( mod_dimensions.o mod_xc.o mod_za.o wtime.o )
	$(TESTD) $(MODMPI) || $(MKDIR) $(MODMPI) ; \
	$(MV) mod_dimensions.mod mod_xc.mod mod_za.mod $(MODMPI)

ser:  cleanser dimension_SER $(LIBSER)( mod_dimensions.o mod_xc.o mod_za.o wtime.o )
	$(TESTD) $(MODSER) || $(MKDIR) $(MODSER) ; \
	$(MV) mod_dimensions.mod mod_xc.mod mod_za.mod $(MODSER)

dimension_MPI: dimensions_0.08_mpi.h dimensions_0.08_serial.h
	$(CP) dimensions_0.08_mpi.h dimensions.h

dimension_SER: dimensions_0.08_serial.h
	$(CP) dimensions_0.08_serial.h dimensions.h

cleanmpi:
	-$(RM) *.F.f *.mod *.o $(LIBMPI) core $(MODMPI)/*.mod  dimensions.h

cleanser:
	-$(RM) *.F.f *.mod *.o $(LIBSER) core $(MODSER)/*.mod  dimensions.h

cleanall:
	-$(RM) *.F.f *.mod *.o $(LIBSER) $(LIBMPI) core $(MODMPI)/*.mod $(MODSER)/*.mod dimensions.h

install: cleanall 
	make mpi FFLAGSADD="-qrealsize=8 -qintsize=4"
	make ser 
	$(RM) dimensions.h

mod_dimensions.o:   mod_dimensions.F dimensions.h
mod_xc.o: mod_xc.F  mod_dimensions.o mod_xc_sm.h mod_xc_mp.h unit_offset.h
mod_za.o: mod_za.F  mod_xc.o         mod_za_sm.h mod_za_mp.h mod_za_mp1.h
wtime.o:   wtime.F

