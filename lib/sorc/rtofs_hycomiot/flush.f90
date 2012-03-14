SUBROUTINE flush(iunit)
IMPLICIT NONE
INTEGER iunit
!
! --- wrapper for flush system call under AIX.
!
INTEGER*4 iunit4
!
iunit4=iunit
CALL flush_(iunit4)
RETURN
END SUBROUTINE flush
