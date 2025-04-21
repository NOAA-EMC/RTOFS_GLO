#
# Makefile for archive processing
#
# --- usage: make -f Makefile
# To create specific executables in this file
#

#
# --- usage: make -f Makefile
# To create specific executables in this file
# --- usage: make -f Makefile programs=isubregion
#

# ------------------------------------------------------------------------
# common definitions for IBM SMP Power3, single processor, real*4, 64-bit.
# ------------------------------------------------------------------------
#
# MACROS      DESCRIPTIONS:
#
# FC:         Fortran 90 compiler.
# FCFFLAGS:   Fortran 90 compilation flags.
# CC:         C compiler.
# CCFLAGS:    C compilation flags.
# CPP:        cpp preprocessor (may be implied by FC).
# CPPFLAGS:   cpp -D macro flags.
# LD:         Loader.
# LDFLAGS:    Loader flags.
# EXTRALIBS:  Extra local libraries (if any).
#
FC            = ftn
FCFFLAGS      = -fixed -assume byterecl -convert big_endian -I$(NETCDF_INCLUDES)
DEBUG         = -g -O0 -fixed -assume byterecl -convert big_endian -check all -check noarg_temp_created -check nopointer -warn -warn noerrors -nogen-interfaces -fp-stack-check -fstack-protector-all -fpe0 -debug -traceback -ftrapuv -I$(NETCDF_INCLUDES)
CC            = cc
CCFLAGS       =
CPP           = /usr/bin/cpp 
CPPFLAGS      = -P -traditional-cpp -DREAL4
LD            = $(FC)
LDFLAGS       = 
BINDIR= ../../../exec
##LIBRARY       = -L/nwprod/lib
EXTRALIBS     = ${W3NCO_LIB4} ${BACIO_LIB4} -L$(NETCDF_LIBRARIES) -lnetcdff

#
# --- generic make definitions
#
SHELL         = /bin/sh
RM            = \rm -f

#
# rules.
#
.SUFFIXES:
.SUFFIXES: .c .F .f .o

.c.o:
	$(CC) $(CPPFLAGS) $(CCFLAGS)  -c $*.c

.f.o:
	$(FC)             $(FCFFLAGS) -c $*.f

.F.o:
	 $(RM) $<.f
	$(CPP) $(CPPFLAGS) $< | sed -e '/^ *$$/d' > $<.f
	$(FC) $(FCFFLAGS) -c $<.f
	-\mv $<.o $*.o
	$(RM) $<.f

#include ../../config/$(ARCH)_setup
CMD=	rtofs_archv2ncdf2d  rtofs_archv2ncdf3z rtofs_field2ncdf2d 
#
# ---------------------------------------------------------------------
# Standard part of the Makefile starts here
# ---------------------------------------------------------------------
#

all: ${CMD} 

install:
	-mv $(CMD) ${BINDIR}

debug:  FCFFLAGS = $(DEBUG)
debug:  all

test:
	$(CMD) < input.txt > output.txt
	diff output.txt valid_output.txt

clean:
	/bin/rm -f *.o *.a *.mod M*log *2archv *2d *2t *3z *sf *sfl *sig2a *restart *hv *ncombc $(CMD)

finish:
	-rm -f *.o *.a *.mod M*log *2archv *2d *2t *3z *sf *sfl *sig2a *restart *hv *ncombc

everything: clean $(CMD) install finish

MODS     =	mod_plot.o mod_xc.o mod_za.o               wtime.o
MODB     =	mod_plot.o mod_xc.o mod_za.o mod_zb.o      wtime.o
MODR     =	mod_plot.o mod_xc.o mod_za.o mod_restart.o wtime.o

DATAX_OBJ = 	bigrid.o  bigrd1.o blkin.o    \
		buoflx.o                      \
		extrct.o  extrot.o fordate.o  \
                getdat.o  getdtm.o getdepth.o \
		          indxi.o  indxj.o    \
                pakk.o    poisnd.o psmoo.o    \
		sbmerg.o  zebra.o  zh.o       \
                dum_gks.o

DATAH_OBJ = 	$(DATAX_OBJ) horout_nc.o
DATAR_OBJ = 	$(DATAX_OBJ) putdat.o
DATAZ_OBJ = 	$(DATAH_OBJ) layer2z.o mixlay.o

NCDFH_OBJ = 	$(DATAX_OBJ) horout_nc.o
NCDFZ_OBJ = 	$(NCDFH_OBJ) layer2z.o mixlay.o

ARCHM_OBJ = 	bigrid.o blkin.o  \
		extrct.o extrot.o \
                getdat.o getdtm.o getdepth.f putdat.o \
		indxi.o  indxj.o  \
		zebra.o  zh.o     dum_gks.o \
		pakk.o   
# 		pakk_micom.o   

ARCHV_OBJ = 	bigrid.o blkin.o  \
		extrct.o extrot.o fordate.o \
                getdat.o getdtm.o getdepth.o putdat.o \
		indxi.o  indxj.o  pakk.o     psmoo.o  \
		zebra.o   zh.o    dum_gks.o 

ARCHV_SRC = 	hycomarchv.f micomarchv.f \
                conv_archv.f trim_archv.f mrgl_archv.f cooperhaines_archv.f ncoda_archv.f ncoda_archv_vel.f remap_archv.f remapi_archv.f \
                archv2data2d.f archv2data2t.f archv2data3z.f archv2datasf.f archv2datasfl.f archv2restart.f putdat.f

LIBN     = ${LIBRARY} -L${NETCDF_LIBRARIES} -lnetcdff ${EXTRALIBS}
INCLUDE  = ${NETCDF_INCLUDES}
LN       =/bin/ln

#
# --- executables.
#

rtofs_archv2data2d:  	$(MODB) archv2data2d.o  $(DATAH_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2data2d  archv2data2d.o  $(MODB) $(DATAH_OBJ) 

rtofs_archm2data2d:       $(MODB) archm2data2d.o  $(DATAH_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archm2data2d  archm2data2d.o  $(MODB) $(DATAH_OBJ)

rtofs_archv2data2t:       $(MODB) archv2data2t.o  $(DATAH_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2data2t  archv2data2t.o  $(MODB) $(DATAH_OBJ)

rtofs_archm2data2t:       $(MODB) archm2data2t.o  $(DATAH_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archm2data2t  archm2data2t.o  $(MODB) $(DATAH_OBJ)

rtofs_archv2data3z:       $(MODB) archv2data3z.o  $(DATAZ_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2data3z  archv2data3z.o  $(MODB) $(DATAZ_OBJ)

rtofs_archm2data3z:       $(MODB) archm2data3z.o  $(DATAZ_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archm2data3z  archm2data3z.o  $(MODB) $(DATAZ_OBJ)

rtofs_archv2datasf:       $(MODB) archv2datasf.o  $(DATAZ_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2datasf  archv2datasf.o  $(MODB) $(DATAZ_OBJ)

rtofs_archv2datasfl:      $(MODB) archv2datasfl.o $(DATAZ_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2datasfl archv2datasfl.o $(MODB) $(DATAZ_OBJ)

rtofs_archv2ncdf2d:  $(NETCDF_STUFF)	$(MODB) archv2data2d.o  $(NCDFH_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2ncdf2d  archv2data2d.o  $(MODB) $(NCDFH_OBJ) $(LIBN)

rtofs_field2ncdf2d:  $(NETCDF_STUFF)	$(MODB) field2data.o  $(NCDFH_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_field2ncdf2d  field2data.o  $(MODB) $(NCDFH_OBJ) $(LIBN)

rtofs_archm2ncdf2d:       $(MODB) archm2data2d.o  $(NCDFH_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archm2ncdf2d  archm2data2d.o  $(MODB) $(NCDFH_OBJ) libnetcdf.a

rtofs_archv2ncdf2t:       $(MODB) archv2data2t.o  $(NCDFH_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2ncdf2t  archv2data2t.o  $(MODB) $(NCDFH_OBJ) libnetcdf.a

rtofs_archv2ncdf3z:       $(MODB) archv2data3z.o  $(NCDFZ_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2ncdf3z  archv2data3z.o  $(MODB) $(NCDFZ_OBJ) $(LIBN)

rtofs_archm2ncdf3z:       $(MODB) archm2data3z.o  $(NCDFZ_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archm2ncdf3z  archm2data3z.o  $(MODB) $(NCDFZ_OBJ) libnetcdf.a

rtofs_archv2ncdfsf:       $(MODB) archv2datasf.o  $(NCDFZ_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2ncdfsf  archv2datasf.o  $(MODB) $(NCDFZ_OBJ) libnetcdf.a

rtofs_archv2ncdfsfl:      $(MODB) archv2datasfl.o $(NCDFZ_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2ncdfsfl archv2datasfl.o $(MODB) $(NCDFZ_OBJ) libnetcdf.a

rtofs_archv2ncombc:       $(MODB) archv2ncombc.o  $(DATAH_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2ncombc  archv2ncombc.o  $(MODB) $(DATAH_OBJ)

rtofs_archv2restart:      $(MODR) archv2restart.o $(DATAX_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archv2restart archv2restart.o $(MODR) $(DATAX_OBJ)

rtofs_archm2restart:      $(MODR) archm2restart.o $(DATAX_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_archm2restart archm2restart.o $(MODR) $(DATAX_OBJ)

rtofs_cooperhaines_archv: $(MODS) cooperhaines_archv.o  $(ARCHV_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_cooperhaines_archv cooperhaines_archv.o $(MODS) $(ARCHV_OBJ)

rtofs_hycomarchv:         $(MODS) hycomarchv.o   $(ARCHV_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_hycomarchv  hycomarchv.o $(MODS) $(ARCHV_OBJ)

rtofs_micomarchv:         $(MODS) micomarchv.o   $(ARCHM_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_micomarchv  micomarchv.o $(MODS) $(ARCHM_OBJ)

rtofs_conv_archv:         $(MODS) conv_archv.o   $(ARCHV_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_conv_archv  conv_archv.o $(MODS) $(ARCHV_OBJ)

rtofs_mrgl_archv:         $(MODS) mrgl_archv.o   $(ARCHV_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_mrgl_archv  mrgl_archv.o $(MODS) $(ARCHV_OBJ)

rtofs_trim_archv:         $(MODS) trim_archv.o   $(ARCHV_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_trim_archv  trim_archv.o $(MODS) $(ARCHV_OBJ)

rtofs_ncoda_archv:        $(MODS) ncoda_archv.o  $(ARCHV_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_ncoda_archv ncoda_archv.o $(MODS) $(ARCHV_OBJ)

rtofs_ncoda_archv_vel:    $(MODS) ncoda_archv_vel.o  $(ARCHV_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_ncoda_archv_vel ncoda_archv_vel.o $(MODS) $(ARCHV_OBJ)

rtofs_remap_archv:        $(MODS) remap_archv.o  $(ARCHV_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_remap_archv remap_archv.o $(MODS) $(ARCHV_OBJ)

rtofs_remapi_archv:       $(MODS) remapi_archv.o  $(ARCHV_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_remapi_archv remapi_archv.o $(MODS) $(ARCHV_OBJ)

remap_ncombc:       $(MODS) remap_ncombc.o $(ARCHV_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_remap_ncombc remap_ncombc.o $(MODS) $(ARCHV_OBJ)

rtofs_restart2archv:      $(MODR) restart2archv.o $(DATAR_OBJ)
	$(LD) $(LDFLAGS) -o rtofs_restart2archv restart2archv.o $(MODR) $(DATAR_OBJ)

rtofs_restart2archv_ssh:  $(MODR) restart2archv_ssh.o   $(DATAR_OBJ)
	$(LD) $(LDFLAGS)  -o rtofs_restart2archv_ssh   restart2archv_ssh.o   $(MODR) $(DATAR_OBJ)

rtofs_restart2archv_sig2a: $(MODR) restart2archv_sig2a.o $(DATAR_OBJ)
	$(LD) $(LDFLAGS)  -o rtofs_restart2archv_sig2a restart2archv_sig2a.o $(MODR) $(DATAR_OBJ)

#
#from Carlos: this is not a robust way to collect files from other directories.
netcdf.inc:
	$(LN)  -s $(INCLUDE)/netcdf.inc netcdf.inc

netcdf.mod:
	$(LN)  -s $(INCLUDE)/netcdf.mod netcdf.mod


#
# --- explicit dependencies for each .[fF] file.
#

NETCDF_STUFF:  netcdf.inc    netcdf.mod

# --- explicit dependencies for each .[fF] file.
#

archv2ncdf3z.o:  archv2data3z.f  mod_plot.o mod_za.o getdat.o
archv2data2d.o:  archv2data2d.f  mod_plot.o mod_za.o 
field2data.o:  field2data.f  mod_plot.o mod_za.o 
archm2data2d.o:  archm2data2d.f  mod_plot.o mod_za.o
archv2data3z.o:  archv2data3z.f  mod_plot.o mod_za.o
archm2data3z.o:  archm2data3z.f  mod_plot.o mod_za.o
archv2datasf.o:  archv2datasf.f  mod_plot.o mod_za.o
archv2datasfl.o: archv2datasfl.f mod_plot.o mod_za.o
archv2ncombc.o:  archv2ncombc.f  mod_plot.o mod_za.o
archv2restart.o: archv2restart.f mod_plot.o mod_za.o mod_restart.o
archm2restart.o: archm2restart.f mod_plot.o mod_za.o mod_restart.o

hycomarchv.o:    hycomarchv.f    mod_plot.o mod_za.o
micomarchv.o:    micomarchv.f    mod_plot.o mod_za.o
mrgl_archv.o:    mrgl_archv.f    mod_plot.o mod_za.o
trim_archv.o:    trim_archv.f    mod_plot.o mod_za.o
remap_archv.o:   remap_archv.f   mod_plot.o mod_za.o

restart2archv.o: restart2archv.f mod_plot.o mod_za.o mod_restart.o

bigrd1.o:   bigrd1.f      mod_plot.o
bigrid.o:   bigrid.f      mod_plot.o
blkin.o:    blkin.f
buoflx.o:   buoflx.f
dum_gks.o:  dum_gks.f
extrct.o:   extrct.f
extrot.o:   extrot.f
fordate.o:  fordate.f
getdat.o:   getdat.f      mod_plot.o mod_za.o
getdepth.o: getdepth.f    mod_plot.o mod_za.o
getdtm.o:   getdtm.f      mod_plot.o mod_za.o
horout_nc.o: horout_nc.f  mod_plot.o mod_xc.o mod_zb.o
indxi.o:    indxi.f       mod_plot.o
indxj.o:    indxj.f       mod_plot.o
layer2z.o:  layer2z.f
mixlay.o:   mixlay.f
pakk.o:     pakk.f
poisnd.o:   poisnd.f      mod_plot.o
prtmsk.o:   prtmsk.f
psmoo.o:    psmoo.f       mod_plot.o
putdat.o:   putdat.f      mod_plot.o mod_za.o
sbmerg.o:   sbmerg.f      mod_plot.o
wtime.o:    wtime.F
zebra.o:    zebra.f

mod_plot.o: mod_plot.F
mod_xc.o:   mod_xc.F   
mod_za.o:   mod_za.F   mod_xc.o
mod_zb.o:   mod_zb.F

zh.o:       zh.F zh_sun.f zh_t3e.f zh_ymp.f
