SHELL=/bin/sh
#RTOFS_GLO init makefile

# RTOFS V2.5
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

# RTOFS V3.0
SUBDIRS = \
	rtofs_nc2grb2.fd \
	rtofs_ncodaz_inc2mom6nc_glb_lyr.fd \
	rtofs_ssmis_tol2.cd

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

