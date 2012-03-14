MODULE mod_dump
 PRIVATE
 PUBLIC dumpr,dumpi
CONTAINS
!
!========================================================================
!
  SUBROUTINE dumpr(lu,a,nx,ny,fname)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: lu,nx,ny
    REAL, INTENT(in) :: a(nx,ny)
    INTEGER :: i,j
    CHARACTER*(*) fname
    WRITE(*,*)'dumpr: '//TRIM(fname)//': max,min=',MAXVAL(a),MINVAL(a)
    OPEN(unit=lu,file=TRIM(fname),form="formatted")
    WRITE(lu,'(4f15.6)') ((a(i,j),i=1,nx),j=1,ny)
    CLOSE (lu)
    RETURN
  END SUBROUTINE 
!
!========================================================================
!
  SUBROUTINE dumpi(lu,a,nx,ny,fname)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: lu,nx,ny
    INTEGER, INTENT(in) :: a(nx,ny)
    INTEGER :: i,j
    CHARACTER*(*) fname
    WRITE(*,*)'dumpi: '//TRIM(fname)//': max,min=',MAXVAL(a),MINVAL(a)
    OPEN(unit=lu,file=TRIM(fname),form="formatted")
    WRITE(lu,'(30i2)') ((a(i,j),i=1,nx),j=1,ny)
    CLOSE (lu)
  RETURN
  END SUBROUTINE 
!
!===========================================================================================
!
END MODULE mod_dump
