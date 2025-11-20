#
# CONFIGURATION IDENTIFICATION:
#      $HeadURL$
#      @(#)$Id$
#
# This config.os file is included in the toplevel and the intermediate 
# Makefiles.
#
# -------------------------------------------------------------------
#
# General Macros

SHELL=/bin/sh

RM = rm -f

MAKE = gmake

RM_CMD = $(RM) *.l *.BAK *.bak *.o *.i core errs ,* *~ *.a .emacs_* \
*.mod *.s tags TAGS make*.log MakeOut 

# ------------------------------------------------------------------
# Prompt the user for os type
#

default:
	@echo "To make observation processing executables and libraries"
	@echo "type one of the following:"
	@echo "   make cray_intel"
	@echo "   make cray_intel_debug"
	@echo "   make clean"

#-------------------------------------------------------------------

