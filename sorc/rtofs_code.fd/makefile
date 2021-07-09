SHELL=/bin/sh
#RTOFS_GLO init makefile

SUBDIRS = \
	rtofs_archv2netCDF.fd \
	rtofs_atmforcing_correct.fd \
	rtofs_atmforcing.fd \
	rtofs_hycom_extract.fd \
	rtofs_hycom2raw8.fd \
	rtofs_hycom_diff.fd \
	rtofs_hycom_expr.fd \
	rtofs_nc2grb2.fd \
	rtofs_ncoda_archv_inc.fd \
	rtofs_raw2hycom.fd \
	rtofs_getkpds.fd

# old rtofs_cice_stat.fd , , , rtofs_hycom_range.fd , rtofs_restart2archv.fd 
# no ice so don't need	rtofs_ssmis_tol2.cd

all: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
	done

check: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make -f makefile.check ); \
	done

clean: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	   make $@) ; \
	done

install: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	   make $@) ; \
	done

