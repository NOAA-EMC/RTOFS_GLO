! #
! DISTRIBUTION STATEMENT B: Distribution authorized to U.S. Government
! agencies based upon the reasons of possible Premature Distribution
! and the possibility of containing Software Documentation as listed
! on Table 1 of DoD Instruction 5230.24, Distribution Statements on
! Technical Documents, of 23 August 2012. Other requests for this
! document shall be made to Dr. Ruth H. Preller, Superintendent,
! Oceanography Division, U.S. Naval Research Laboratory, DEPARTMENT
! OF THE NAVY, John C. Stennis Space Center, MS 39529-5004; (228)
! 688-4670 (voice); ruth.preller@nrlssc.navy.mil (e-mail).
! #
!=======================================================================
!
!BOP
!
! !MODULE: ice_exit - exit the model
!
! !DESCRIPTION:
!
! Exit the model. 
!
! !REVISION HISTORY:
!  SVN:$Id: ice_exit.F90 82 2007-10-02 15:44:25Z eclare $
!
! authors William H. Lipscomb (LANL)
!         Elizabeth C. Hunke (LANL)
! 2006 ECH: separated serial and mpi functionality
!
! !INTERFACE:
!
      module ice_exit
!
! !USES:
!
      use ice_kinds_mod
!
!EOP
!
      implicit none

!=======================================================================

      contains

!=======================================================================
!BOP
!
! !ROUTINE: abort_ice - abort the model
!
! !INTERFACE:
!
      subroutine abort_ice(error_message)
!
! !DESCRIPTION:
!
!  This routine aborts the ice model and prints an error message.
!
! !REVISION HISTORY:
!
! same as module
!
! !USES:
!
      use ice_fileunits
      use ice_communicate
#if (defined CCSM) || (defined SEQ_MCT)
      use shr_sys_mod
#endif

      include 'mpif.h'   ! MPI Fortran include file
!
!
! !INPUT/OUTPUT PARAMETERS:
!
      character (len=*), intent(in) :: error_message
!
!EOP
!
      integer (int_kind) :: ierr ! MPI error flag

#if (defined CCSM) || (defined SEQ_MCT)
      call shr_sys_abort(error_message)
#else
      call flush_fileunit(nu_diag)

      write (ice_stderr,*) error_message
      call flush_fileunit(ice_stderr)

      call MPI_ABORT(MPI_COMM_WORLD, ierr)
      stop
#endif

      end subroutine abort_ice

!=======================================================================
!BOP
!
! !IROUTINE: end_run - ends run
!
! !INTERFACE:
!
      subroutine end_run
!
! !DESCRIPTION:
!
! Ends run by calling MPI_FINALIZE.
!
! !REVISION HISTORY:
!
! author: ?
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!

      integer (int_kind) :: ierr ! MPI error flag

!pgp
!      call MPI_FINALIZE(ierr)
!
!EOP
!
      end subroutine end_run

!=======================================================================

      end module ice_exit

!=======================================================================
