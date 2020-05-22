MODULE mod_hytime
 PRIVATE
 PUBLIC hytime,jdn
CONTAINS
!
!==========================================================================
!
   SUBROUTINE hytime(date,hdate)
    IMPLICIT NONE
    REAL,INTENT(out) :: hdate
    CHARACTER, INTENT(in):: date*(*)
    INTEGER :: iyear,imonth,iday,ihour,jdn,day1
    INTEGER,SAVE  :: day0=-1
    EXTERNAL jdn
! ---   model day is calendar days since 12/31/1900
    IF(day0<0) CALL julday(day0,1900,12,31)
    READ(date(1:10),'(i4,i2,i2,i2)') iyear,imonth,iday,ihour
    CALL julday(day1,iyear,imonth,iday)
    hdate=day1-day0+ihour/24.d0
    RETURN
  END SUBROUTINE hytime
!
!==========================================================================
!
   SUBROUTINE julday(jdn,iyear,month,iday)
      IMPLICIT NONE
      INTEGER,INTENT(in) :: iyear,month,iday
      INTEGER,INTENT(out) :: jdn
      jdn  =    iday - 32075 &
 &          + 1461 * (iyear + 4800 + (month - 14) / 12) / 4 &
 &          + 367 * (month - 2 - (month -14) / 12 * 12) / 12 &
 &          - 3 * ((iyear + 4900 + (month - 14) / 12) / 100) / 4
      RETURN
  END SUBROUTINE julday
!
!===========================================================================================
!
END MODULE mod_hytime
