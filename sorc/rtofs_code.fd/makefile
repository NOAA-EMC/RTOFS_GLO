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
	rtofs_getkpds.fd \
	rtofs_ssmis_tol2.cd

# old rtofs_cice_stat.fd , , , rtofs_hycom_range.fd , rtofs_restart2archv.fd 

all: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
	done

debug: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	   make $@) ; \
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

test: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	   make $@) ; \
	done

