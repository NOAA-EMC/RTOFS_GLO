      SUBROUTINE wmoout(BULHEAD, KW, yy, mm, dd, hh, 
     1                  lwork, linelen, nlines, grib, lgrib, wmounit)
C     Robert Grumbine 1998
   
      IMPLICIT none

      CHARACTER(6) BULHEAD
      CHARACTER(4) KW
      INTEGER yy, mm, dd, hh
      INTEGER linelen, nlines, lwork(linelen, nlines)
      INTEGER lgrib, wmounit
      CHARACTER(1) grib(lgrib) 

      INTEGER IDS(6)
      CHARACTER(1) HEADER(21)
      CHARACTER(1) QUEUE(80)
      INTEGER mlen, mfin

      IDS(1) = yy
      IDS(2) = mm
      IDS(3) = dd
      IDS(4) = hh
      IDS(5) = 0
      IDS(6) = 0
      mlen = lgrib

      CALL MAKWMO(BULHEAD, IDS, HEADER, KW)
      CALL QUEDES(QUEUE, BULHEAD, mlen, KW)
      CALL TRANST(wmounit, grib, HEADER, QUEUE, mlen, mfin,
     1            lwork, linelen, nlines)

      RETURN
      END
