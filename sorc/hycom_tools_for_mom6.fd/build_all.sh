#!/bin/sh

BASE=`pwd`
dir_mods="$(dirname ${BASE})"
dir_mod0="$(dirname ${dir_mods})"

echo " "
echo "Load modules listed at: "
echo ${dir_mod0}"/versions/build.ver"
source ${BASE}/load_modules.sh ${dir_mod0}

make all

# move executables to exec/
if [ ! -d "${dir_mod0}/exec"  ]; then
  mkdir -p "${dir_mod0}/exec"
fi

mv rtofs*.x "${dir_mod0}/exec"

#clean: $(SUBDIRS)
#	for dir in $(SUBDIRS); do \
#	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
#	   make $@) ; \
#	done

#install: $(SUBDIRS)
#	for dir in $(SUBDIRS); do \
#	   (cd $$dir; cp -p bin/* ../../../exec); \
#	done

