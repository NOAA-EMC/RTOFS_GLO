#!/bin/sh

BASE=`pwd`
dir_mods="$(dirname ${BASE})"
dir_mod0="$(dirname ${dir_mods})"

echo " "
echo "Load modules listed at: "
echo ${dir_mod0}"/versions/build.ver"
source ${BASE}/load_modules.sh ${dir_mod0}

SUBDIRS=${BASE}/observations

for dir in ${SUBDIRS}; do \
  cd ${dir}; make all
  cd -
done

# move executables
mkdir -p ${BASE}/exec
for dir in ${SUBDIRS}; do \
  mv ${dir}/*.x ${BASE}/exec
done

#clean: $(SUBDIRS)
#	for dir in $(SUBDIRS); do \
#	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
#	   make $@) ; \
#	done

#install: $(SUBDIRS)
#	for dir in $(SUBDIRS); do \
#	   (cd $$dir; cp -p bin/* ../../../exec); \
#	done

