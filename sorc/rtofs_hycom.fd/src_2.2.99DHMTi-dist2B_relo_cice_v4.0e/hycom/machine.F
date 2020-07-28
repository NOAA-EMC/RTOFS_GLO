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
c
c --- machine-specific Fortran routines
c
      subroutine machine()
c
c --- always called once at the start of the program.
c
#if defined(SGI)
      call zunder()  ! C-wrapper to flush underflow to zero on R10000
#endif
      end
#if defined(AIX)
      subroutine flush(iunit)
      implicit none
      integer iunit
c
c --- wrapper for flush system call under AIX.
c
      integer*4 iunit4
c
      iunit4=iunit
      call flush_(iunit4)
      return
      end
#endif /* AIX */
#if defined(X1)
      subroutine x1flush(iunit)
      implicit none
      integer iunit
c
c --- wrapper for flush system call on the Cray X1.
c
      integer ierr
c
      call FLUSH(iunit,ierr)
      return
      end
#endif /* X1 */
#if defined(IFC)
      subroutine flush(iunit)
      implicit none
      integer iunit
c
c --- disable the flush system call under Intel's IFC compiler.
c
      return
      end
#endif /* IFC */
#if defined(SUN)
      subroutine ieee_retrospective()
c
c     dummy routine to turn off ieee warning messages on a Sun.
c
      end
#endif /* SUN */
#if defined(T3E) || defined(YMP) || defined(X1)
      subroutine getenv(cname, cvalue)
      implicit none
c
      character*(*) cname,cvalue
c
c     this subroutine provides getenv functionality
c     on the t3e, using pxfgetenv.
c
      integer iname,ivalue,ierr
c
      iname = 0
      ierr  = 0
      call pxfgetenv(cname,iname, cvalue,ivalue, ierr)
      if     (ierr.ne.0) then
        cvalue = ' '
      endif
      return
c     end of getenv.
      end
#endif /* T3E || YMP || X1 */
