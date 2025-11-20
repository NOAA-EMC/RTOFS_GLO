#
# CONFIGURATION IDENTIFICATION:
#      $HeadURL$
#      @(#)$Id$
#
#   makefile to build program ocean obs post-processor
#
#   other dependencies

include ../config.os

include ../config.user

INCLUDE_DIR = ../include

LIBDIR   = ../lib

LIBS   = \
	$(LIBDIR)/lib_obs_post_proc.a

OBJS     = obs_post_proc.o

PROGRAM  = obs_post_proc.x

target : $(PROGRAM) 

#   loader directive

$(PROGRAM) : $(OBJS) $(LIBS)
	$(FC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS) $(EXTRALIBS)

.f.o:
	$(RM) $@
	$(FC) -c $(FFLAGS_QC) -I$(INCLUDE_DIR) $*.f

########################################################################
# common rules for all Makefiles - do not edit
########################################################################

emptyrule:

clean:
	(if [ -d rii_files ] ; then rm -rf rii_files ; fi) ; 
	$(RM_CMD) "#"*

