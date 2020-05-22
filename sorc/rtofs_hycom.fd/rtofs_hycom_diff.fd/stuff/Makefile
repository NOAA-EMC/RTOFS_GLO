#
# Makefile for meanstd processing
#
# --- usage: make mnsq ARCH=intelGF >& Make_mnsq.log
#
# --- Tunable parameters in ../../config/$(ARCH)_setup
#

.SUFFIXES: 
.SUFFIXES: .c .F .f .o

.F:
	@echo "Must have an explicit rule for" $*
	@exit 1
.f:
	@echo "Must have an explicit rule for" $*
	@exit 1
.c:
	@echo "Must have an explicit rule for" $*
	@exit 1


include ./intelIF_setup
#
# ---------------------------------------------------------------------
# Standard part of the Makefile starts here
# ---------------------------------------------------------------------
#

all:
	/bin/csh Make_all.csh

clean:
	/bin/rm *.o *.a *.mod M*log *mnsq *std *diff *wsum

MODS     =	mod_mean.o mod_mean_esmf.o mod_xc.o mod_za.o wtime.o zh.o

MEAN_OBJ = 	bigrid.o blkin.o \
		extrct.o \
                getdat.o putdat.o

MEAN_SRC = 	putdat.f

#
# --- executables.
#

hycom_diff: $(MODS) hycom_diff.o $(MEAN_OBJ)
	$(LD) $(LDFLAGS)  -o hycom_diff  hycom_diff.o $(MODS) $(MEAN_OBJ)


#
# --- explicit dependencies for each .[fF] file.
#

hycom_diff.o: hycom_diff.f mod_mean.o                 mod_za.o

mod_mean.o: mod_mean.F
mod_mean_esmf.o: mod_mean_esmf.F
mod_xc.o:   mod_xc.F
mod_za.o:   mod_za.F	mod_xc.o

zh.o:       zh.F zh_sun.f zh_t3e.f zh_ymp.f
