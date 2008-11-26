################################################################################
#
#     Makefile for Ocean Forecast Model rtofs_forecast
#                  core: hycom version 2.0
#     Tunable parameters in ../config/$(ARCH)_$(TYPE)
#
#     Use:
#     make         -  build the executable
#     make clean   -  start with a clean slate
#
#     The following macros will be of interest:
#
#         TARGET   - name of the executable
#         FC       - name of Fortran compiler
#         CPP      - name of CPP
#         ARCH     - architecture
#         CPPFLAGS - CPP flags
#         OPTS     - compiler code optimizations
#         LIST     - source listing
#         SMP      - threading
#         TRAPS    - runtime traps for floating point exceptions
#         PROFILE  - source code profiling ( -pg )
#         DEBUG    - -g
#         MEM      - user data area and stack size
#         MAP      - load map
#         W3LIB    - w3lib
#         BACIO    - bacio lib
#         ESSL     - ESSL library
#         MASS     - MASS library
#         HPMLIB   - hpm lib
#         SEARCH   - library search location
#
#         This version for eta_post with more intelligent memory allocation
#         Jim Tuccillo   Feb 2001
#
#         This version for eta_post with asynchronous I/O server.
#         Jim Tuccillo   June 2001
#
#         A few minor changes
#         Jim Tuccillo   Feb 2002
#
#################################################################################
#
# Define the name of the executable
#
TARGET = rtofs_forecast
#
# CPP, Compiler, and Linker Options
#
FC       = ncepmpxlf95
CPP      = /usr/lib/cpp -P
RCH     =
CPPFLAGS = -DAIX -DREAL8 -DMPI -DSERIAL_IO -DTIMER -DARCTIC
OPTS     = -O -qfixed -qstrict -qtune=auto -qcache=auto
LIST     = -qspillsize=32000 -qrealsize=8 -qintsize=4 -qfullpath
SMP      =
TRAPS    =
PROFILE  =
DEBUG    =
MEM      =
MAP      =
W3LIB    =
BACIO    =
ESSL     =
MASS     =  -lmass
HPMLIB   =
SEARCH   =
#
# Assemble Options
#
FFLAGS   = $(OPTS) $(LIST) $(TRAPS) $(PROFILE) $(DEBUG)
CFLAGS   =
FFLAGST  = $(FFLAGS) $(SMP)
LDFLAGS  = $(FFLAGS) $(MEM) $(MAP) $(SMP) $(PROFILE)
LIBS     = $(W3LIB) $(BACIO) $(ESSL) $(MASS) $(HPMLIB) $(SEARCH)

#
# --- generic make definitions
#
SHELL         = /bin/sh

#
# rules.
#

.SUFFIXES:
.SUFFIXES: .c .F .f .o

#
# rules.
#
.c.o:
	$(CC) $(CPPFLAGS) $(CFLAGS)  -c $*.c

.f.o:
	$(FC)             $(FFLAGS) -c $*.f

.F.o:
	$(CPP) $(CPPFLAGS) $< | sed -e '/^ *$$/d' > $<.f
	$(FC) $(FFLAGS) -c $<.f
	-\mv $<.o $*.o
#############################################################
MODS =  mod_dimensions.o mod_xc.o mod_za.o mod_pipe.o mod_incupd.o mod_floats.o mod_tides.o mod_mean.o mod_hycom.o

OBJS =	archiv.o barotp.o  bigrid.o blkdat.o  cnuity.o convec.o \
	diapfl.o dpthuv.o  dpudpv.o forfun.o  geopar.o hybgen.o \
	icloan.o inicon.o inigiss.o inikpp.o   inimy.o latbdy.o \
	matinv.o momtum.o  mxkprf.o  mxkrt.o  mxkrtm.o  mxpwp.o \
	overtn.o poflat.o  prtmsk.o  psmoo.o restart.o \
	thermf.o trcupd.o  tsadvc.o  \
       machine.o  wtime.o machi_c.o  isnan.o

SRCS =	hycom.f archiv.f  barotp.f bigrid.f  blkdat.f cnuity.f convec.f \
	diapfl.f dpthuv.f  dpudpv.f forfun.f  geopar.f hybgen.f \
	icloan.f inicon.f inigiss.f inikpp.f  inimy.f  latbdy.f \
	matinv.f momtum.f  mxkprf.f  mxkrt.f  mxkrtm.f  mxpwp.f \
	overtn.f poflat.f  prtmsk.f  psmoo.f restart.f \
	thermf.f trcupd.f  tsadvc.f  inicon_mode_m1.f \
        machine.f  wtime.f machi_c.c \
        mod_pipe.F \
        mod_xc.F mod_xc_sm.F mod_xc_mp.F \
        mod_za.F mod_za_sm.F mod_za_mp.F mod_za_mp1.F

#
#hycom:	$(MODS) $(OBJS) hycom.o
#	$(LD)  $(LDFLAGS) -o hycom  hycom.o $(MODS) $(OBJS) $(EXTRALIBS)


# ---------------------------------------------------------------------
# Standard part of the Makefile starts here
# ---------------------------------------------------------------------
$(TARGET):	$(MODS) $(OBJS) hycom.o
	$(FC) $(LDFLAGS) -o $@ hycom.o $(MODS) $(OBJS) $(LIBS)

esmf:	$(MODS) $(OBJS)
	@echo "--- ESMF hycom component has been built ---"

hycom_cice:	$(MODS) $(OBJS) mod_OICPL.o hycom_cice.o
	$(LD)  $(LDFLAGS) -o hycom_cice \
                             hycom_cice.o mod_OICPL.o \
                             $(MODS) $(OBJS) \
                             ${CICE_DIR}/esmf/compile/*.o \
                             $(EXTRALIBS)
clean:
	/bin/rm *.o *.mod *.F.f
#####################################################################
hycom.o:        hycom.F       mod_hycom.o
hycom_cice.o:   hycom_cice.F  mod_hycom.o mod_OICPL.o

archiv.o:  archiv.f  mod_xc.o common_blocks.h            mod_za.o
barotp.o:  barotp.f  mod_xc.o common_blocks.h                     mod_pipe.o \
	                                                          mod_tides.o
bigrid.o:  bigrid.f  mod_xc.o 
blkdat.o:  blkdat.F  mod_xc.o common_blocks.h stmt_fns.h          mod_incupd.o \
	                                                          mod_floats.o \
	                                                          mod_tides.o
cnuity.o:  cnuity.f  mod_xc.o common_blocks.h                     mod_pipe.o
convec.o:  convec.f  mod_xc.o common_blocks.h stmt_fns.h
diapfl.o:  diapfl.f  mod_xc.o common_blocks.h stmt_fns.h
dpthuv.o:  dpthuv.f  mod_xc.o common_blocks.h
dpudpv.o:  dpudpv.f  mod_xc.o 
forfun.o:  forfun.f  mod_xc.o common_blocks.h            mod_za.o
geopar.o:  geopar.F  mod_xc.o common_blocks.h stmt_fns.h mod_za.o
hybgen.o:  hybgen.f  mod_xc.o common_blocks.h stmt_fns.h          mod_pipe.o
icloan.o:  icloan.f  mod_xc.o common_blocks.h stmt_fns.h
inicon.o:  inicon.f  mod_xc.o common_blocks.h stmt_fns.h          mod_pipe.o
inigiss.o: inigiss.f mod_xc.o common_blocks.h stmt_fns.h
inikpp.o:  inikpp.f  mod_xc.o common_blocks.h stmt_fns.h
inimy.o:   inimy.f   mod_xc.o common_blocks.h stmt_fns.h
isnan.o:   isnan.F
latbdy.o:  latbdy.f  mod_xc.o common_blocks.h                     mod_tides.o
machine.o: machine.F
machi_c.o: machi_c.c
matinv.o:  matinv.f  mod_xc.o common_blocks.h
momtum.o:  momtum.f  mod_xc.o common_blocks.h stmt_fns.h          mod_pipe.o \
	                                                          mod_tides.o
mxkprf.o:  mxkprf.f  mod_xc.o common_blocks.h stmt_fns.h          mod_pipe.o
mxkrt.o:   mxkrt.f   mod_xc.o common_blocks.h stmt_fns.h
mxkrtm.o:  mxkrtm.f  mod_xc.o common_blocks.h stmt_fns.h
mxpwp.o:   mxpwp.f   mod_xc.o common_blocks.h stmt_fns.h
overtn.o:  overtn.f  mod_xc.o common_blocks.h
poflat.o:  poflat.f
prtmsk.o:  prtmsk.f
psmoo.o:   psmoo.f   mod_xc.o 
restart.o: restart.f mod_xc.o common_blocks.h            mod_za.o
thermf.o:  thermf.f  mod_xc.o common_blocks.h stmt_fns.h
trcupd.o:  trcupd.f  mod_xc.o common_blocks.h                     mod_pipe.o
tsadvc.o:  tsadvc.f  mod_xc.o common_blocks.h stmt_fns.h          mod_pipe.o
wtime.o:   wtime.F
mod_hycom.o: \
        mod_hycom.F  mod_xc.o common_blocks.h            mod_za.o mod_pipe.o \
	                                                          mod_incupd.o \
	                                                          mod_mean.o \
	                                                          mod_floats.o
mod_incupd.o: \
        mod_incupd.F mod_xc.o common_blocks.h            mod_za.o
mod_floats.o: \
        mod_floats.F mod_xc.o common_blocks.h            mod_za.o mod_pipe.o
mod_pipe.o: \
        mod_pipe.F   mod_xc.o common_blocks.h
mod_tides.o: \
        mod_tides.F  mod_xc.o common_blocks.h            mod_za.o
mod_mean.o: \
        mod_mean.F   mod_xc.o common_blocks.h            mod_za.o

mod_dimensions.o:   mod_dimensions.F dimensions.h
mod_xc.o: mod_xc.F  mod_dimensions.o mod_xc_sm.h mod_xc_mp.h
mod_za.o: mod_za.F  mod_xc.o         mod_za_sm.h mod_za_mp.h mod_za_mp1.h

mod_OICPL.o: mod_OICPL.F
