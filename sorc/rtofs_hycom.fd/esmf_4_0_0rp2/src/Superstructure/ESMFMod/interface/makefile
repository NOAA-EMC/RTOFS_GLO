# $Id: makefile,v 1.10 2008/09/26 05:18:12 cdeluca Exp $

ALL: build_here 

SOURCEC	  = ESMCI_Init.C ESMC_Init.C
SOURCEF	  = ESMF_Init_C.F90
SOURCEH	  = 

# list all .h files which should be copied to common include dir
STOREH	  = ESMC_Init.h ESMC.h ESMCI_Init.h ESMCI.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/ESMFMod/interface

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

include ${ESMF_DIR}/makefile

DIRS	  = 

