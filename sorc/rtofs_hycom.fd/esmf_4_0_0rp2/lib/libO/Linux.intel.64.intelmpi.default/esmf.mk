# ESMF application makefile fragment
#
# Use the following ESMF_ variables to compile and link
# your ESMF application against this ESMF build.
#
# !!! VERY IMPORTANT: If the location of this ESMF build is   !!!
# !!! changed, e.g. libesmf.a is copied to another directory, !!!
# !!! this file - esmf.mk - must be edited to adjust to the   !!!
# !!! correct new path                                        !!!
#
# Please see end of file for options used on this ESMF build
#

ESMF_F90COMPILER=ftn
ESMF_F90LINKER=ftn

ESMF_F90COMPILEOPTS=-O -fPIC -m64 -mcmodel=small
ESMF_F90COMPILEPATHS=-I/lfs/h2/emc/eib/noscrub/Dan.Iredell/EMC_rtofs_glo_prod/sorc/rtofs_hycom.fd/esmf_4_0_0rp2/mod/modO/Linux.intel.64.intelmpi.default -I/lfs/h2/emc/eib/noscrub/Dan.Iredell/EMC_rtofs_glo_prod/sorc/rtofs_hycom.fd/esmf_4_0_0rp2/src/include
ESMF_F90COMPILECPPFLAGS=-DESMF_NO_PTHREADS -DESMF_NO_OPENMP -DSx86_64_small=1 -DESMF_OS_Linux=1
ESMF_F90COMPILEFREECPP=
ESMF_F90COMPILEFREENOCPP=
ESMF_F90COMPILEFIXCPP=
ESMF_F90COMPILEFIXNOCPP=

ESMF_F90LINKOPTS= -m64 -mcmodel=small
ESMF_F90LINKPATHS=-L/lfs/h2/emc/eib/noscrub/Dan.Iredell/EMC_rtofs_glo_prod/sorc/rtofs_hycom.fd/esmf_4_0_0rp2/lib/libO/Linux.intel.64.intelmpi.default -L/pe/intel/compilers_and_libraries_2020.4.304/linux/mpi/intel64/lib/release -L/pe/intel/compilers_and_libraries_2020.4.304/linux/mpi/intel64/lib -L/pe/intel/compilers_and_libraries_2020.4.304/linux/ipp/lib/intel64 -L/pe/intel/compilers_and_libraries_2020.4.304/linux/compiler/lib/intel64_lin -L/pe/intel/compilers_and_libraries_2020.4.304/linux/mkl/lib/intel64_lin -L/pe/intel/compilers_and_libraries_2020.4.304/linux/tbb/lib/intel64/gcc4.8 -L/pe/intel/compilers_and_libraries_2020.4.304/linux/compiler/lib/intel64_lin -L/usr/lib64/gcc/x86_64-suse-linux/7/ -L/usr/lib64/gcc/x86_64-suse-linux/7/../../../../lib64 -L/usr/lib64/gcc/x86_64-suse-linux/7/../../../../lib64/ -L/lib/../lib64 -L/lib/../lib64/ -L/usr/lib/../lib64 -L/usr/lib/../lib64/ -L/pe/intel/compilers_and_libraries_2020.4.304/linux/ipp/lib/intel64/ -L/pe/intel/compilers_and_libraries_2020.4.304/linux/compiler/lib/intel64_lin/ -L/pe/intel/compilers_and_libraries_2020.4.304/linux/mkl/lib/intel64_lin/ -L/pe/intel/compilers_and_libraries_2020.4.304/linux/tbb/lib/intel64/gcc4.8/ -L/usr/lib64/gcc/x86_64-suse-linux/7/../../../../x86_64-suse-linux/lib/ -L/usr/lib64/gcc/x86_64-suse-linux/7/../../../ -L/lib64 -L/lib/ -L/usr/lib64 -L/usr/lib
ESMF_F90LINKRPATHS=-Wl,-rpath,/lfs/h2/emc/eib/noscrub/Dan.Iredell/EMC_rtofs_glo_prod/sorc/rtofs_hycom.fd/esmf_4_0_0rp2/lib/libO/Linux.intel.64.intelmpi.default
ESMF_F90LINKLIBS= -ldl -lrt -lpthread -limf -lsvml -lirng -lstdc++ -lm -lipgo -ldecimal -lcilkrts -lstdc++ -lgcc -lgcc_s -lirc -lsvml -lgcc -lgcc_s -lirc_s -ldl -lrt -ldl
ESMF_F90ESMFLINKLIBS=-lesmf  -ldl -lrt -lpthread -limf -lsvml -lirng -lstdc++ -lm -lipgo -ldecimal -lcilkrts -lstdc++ -lgcc -lgcc_s -lirc -lsvml -lgcc -lgcc_s -lirc_s -ldl -lrt -ldl

ESMF_CXXCOMPILER=mpiicpc
ESMF_CXXLINKER=mpiicpc

ESMF_CXXCOMPILEOPTS=-O -DNDEBUG -fPIC -m64 -mcmodel=small
ESMF_CXXCOMPILEPATHS=-I/lfs/h2/emc/eib/noscrub/Dan.Iredell/EMC_rtofs_glo_prod/sorc/rtofs_hycom.fd/esmf_4_0_0rp2/src/include
ESMF_CXXCOMPILECPPFLAGS=-DESMF_NO_PTHREADS -DESMF_NO_OPENMP -DSx86_64_small=1 -DESMF_OS_Linux=1 -D__SDIR__=''

ESMF_CXXLINKOPTS= -m64 -mcmodel=small
ESMF_CXXLINKPATHS=-L/lfs/h2/emc/eib/noscrub/Dan.Iredell/EMC_rtofs_glo_prod/sorc/rtofs_hycom.fd/esmf_4_0_0rp2/lib/libO/Linux.intel.64.intelmpi.default -L/pe/intel/compilers_and_libraries_2020.4.304/linux/compiler/lib/intel64_lin/
ESMF_CXXLINKRPATHS=-Wl,-rpath,/lfs/h2/emc/eib/noscrub/Dan.Iredell/EMC_rtofs_glo_prod/sorc/rtofs_hycom.fd/esmf_4_0_0rp2/lib/libO/Linux.intel.64.intelmpi.default -Wl,-rpath,/pe/intel/compilers_and_libraries_2020.4.304/linux/compiler/lib/intel64_lin/
ESMF_CXXLINKLIBS= -limf -lm -lpthread -lifport -lifcoremt_pic -limf -lsvml -lm -lipgo -lirc -lpthread -lsvml -lgcc -lgcc_s -lirc_s -ldl -lrt -ldl
ESMF_CXXESMFLINKLIBS=-lesmf  -limf -lm -lpthread -lifport -lifcoremt_pic -limf -lsvml -lm -lipgo -lirc -lpthread -lsvml -lgcc -lgcc_s -lirc_s -ldl -lrt -ldl

ESMF_SO_F90COMPILEOPTS=-fPIC
ESMF_SO_F90LINKOPTS=-shared
ESMF_SO_F90LINKOPTSEXE=-Wl,-export-dynamic
ESMF_SO_CXXCOMPILEOPTS=-fPIC
ESMF_SO_CXXLINKOPTS=-shared
ESMF_SO_CXXLINKOPTSEXE=-Wl,-export-dynamic

ESMF_OPENMP_F90COMPILEOPTS= -qopenmp
ESMF_OPENMP_F90LINKOPTS= -qopenmp
ESMF_OPENMP_CXXCOMPILEOPTS= -qopenmp
ESMF_OPENMP_CXXLINKOPTS= -qopenmp

#
# !!! The following options were used on this ESMF build !!!
#
# ESMF_DIR: /lfs/h2/emc/eib/noscrub/Dan.Iredell/EMC_rtofs_glo_prod/sorc/rtofs_hycom.fd/esmf_4_0_0rp2
# ESMF_OS: Linux
# ESMF_MACHINE: x86_64
# ESMF_ABI: 64
# ESMF_COMPILER: intel
# ESMF_BOPT: O
# ESMF_COMM: intelmpi
# ESMF_SITE: default
# ESMF_PTHREADS: OFF
# ESMF_OPENMP: OFF
# ESMF_ARRAY_LITE: FALSE
# ESMF_NO_INTEGER_1_BYTE: FALSE
# ESMF_NO_INTEGER_2_BYTE: FALSE
# ESMF_FORTRANSYMBOLS: default
# ESMF_DEFER_LIB_BUILD:   ON
# ESMF_TESTEXHAUSTIVE: OFF
# ESMF_TESTWITHTHREADS: OFF
# ESMF_TESTMPMD: OFF
# ESMF_TESTSHAREDOBJ: OFF
# ESMF_TESTFORCEOPENMP: OFF
# ESMF_TESTHARNESS: NONEXHAUSTIVE
# 
# ESMF environment variables pointing to 3rd party software:
