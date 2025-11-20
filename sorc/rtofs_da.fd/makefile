SHELL=/bin/sh

SUBDIRS = \
	proc_obs

all: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make "cray_intel" ); \
	done

debug: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make "cray_intel_debug" ); \
	done

clean: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	   make $@) ; \
	done

install: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   (cd $$dir; cp *.x ../../../exec); \
	done

