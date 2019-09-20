SHELL=/bin/sh
#RTOFS_GLO init makefile

SUBDIRS = \
	rtofs_archv2netCDF.fd \
	rtofs_atmforcing_correct.fd \
	rtofs_atmforcing.fd \
	rtofs_cice_stat.fd \
	rtofs_getkpds.fd \
	rtofs_hycom2raw8.fd \
	rtofs_hycom_expr.fd \
	rtofs_hycom_extract.fd \
	rtofs_hycom_range.fd \
	rtofs_nc2grb2.fd \
	rtofs_restart2archv.fd \

all: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make install ); \
	done

clean: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	   make $@) ; \
	done

