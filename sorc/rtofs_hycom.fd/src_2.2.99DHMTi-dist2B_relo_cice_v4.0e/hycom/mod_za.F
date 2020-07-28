c #
c DISTRIBUTION STATEMENT B: Distribution authorized to U.S. Government
c agencies based upon the reasons of possible Premature Distribution
c and the possibility of containing Software Documentation as listed
c on Table 1 of DoD Instruction 5230.24, Distribution Statements on
c Technical Documents, of 23 August 2012. Other requests for this
c document shall be made to Dr. Ruth H. Preller, Superintendent,
c Oceanography Division, U.S. Naval Research Laboratory, DEPARTMENT
c OF THE NAVY, John C. Stennis Space Center, MS 39529-5004; (228)
c 688-4670 (voice); ruth.preller@nrlssc.navy.mil (e-mail).
c #
      module mod_za
      use mod_xc  ! HYCOM communication API
c
      implicit none
c
c --- HYCOM I/O interface.
c
c --- See README.src.mod_za for more details.
c
#if defined(MPI) && ! defined(SERIAL_IO)
      integer, save, private ::
     &  file_info_zaiord,file_count_zaiord,
     &  file_info_zaiowr,file_count_zaiowr
#else
      private zaiordd,zaiowrd
#endif
      private ztiowrd
c
      integer, private, parameter :: uaoff = 1000 + uoff  !array I/O unit offset
#if defined(RELO)
c
c --- initialized in zaiost.
      integer, private, save :: n2drec
      real*4,  private, save, allocatable, dimension(:) ::
     &           wminy,wmaxy,htmp
#else
c
      integer, parameter :: n2drec=((itdm*jtdm+4095)/4096)*4096
      real*4,  private, save, dimension(jtdm)    :: wminy,wmaxy
      real*4,  private, save, dimension(idm*jdm) :: htmp
#endif
c
      real*4,  private, save, allocatable, dimension(:) :: w
c
      contains
#if ! defined(MPI) && ! defined(SHMEM)
#       include "mod_za_sm.h"
#elif defined(SERIAL_IO)
#       include "mod_za_mp1.h"
#else
#       include "mod_za_mp.h"
#endif
#      include "mod_za_zt.h"
      end module mod_za

#if defined(ENDIAN_IO_F90)  /* see mach_i.c for new C version */
      subroutine zaio_endian(a,n)
      implicit none
c
      integer,         intent(in)    :: n
      integer(kind=4), intent(inout) :: a(n)  ! 4-bytes
c
c**********
c*
c 1)  swap the endian-ness of the array.
c
c 2)  assumes integer(kind=1) and integer(kind=4) ocupy one and four
c     bytes respectively.
c*
c**********
c
      integer k
c
      integer(kind=4) ii4,   io4     ! 4-bytes
      common/czioxe/  ii4,   io4     ! helps prevent unwanted optimization
      save  /czioxe/
c
      integer(kind=1) ii1(4),io1(4)  ! 1-byte
      equivalence    (ii4,ii1(1)), (io4,io1(1))  ! non-standard f90
c
      do k= 1,n
        ii4 = a(k)
        io1(1) = ii1(4)
        io1(2) = ii1(3)
        io1(3) = ii1(2)
        io1(4) = ii1(1)
        a(k) = io4
      enddo
      return
      end subroutine zaio_endian
#endif /* ENDIAN_IO_F90 */
