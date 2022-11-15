      SUBROUTINE CFSREAD(fname, Z, ZZ, ALON, ALAT, H)
!Robert Grumbine
!
!~1998 developed
!Legacy as of ~2006 when COFS was de-implemented
!9 June 2018 -- f95 compliance

      IMPLICIT none

      INTEGER KB, IM, JM
      PARAMETER (KB = 19)
      PARAMETER (IM = 181)
      PARAMETER (JM = 101)

      CHARACTER(60) fname
      INTEGER KBB
      INTEGER IMM, JMM

      REAL Z(KB), ZZ(KB), DZ(KB), DZZ(KB)
      REAL ALON(IM, JM), ALAT(IM, JM), DX(IM, JM), DY(IM, JM), H(IM, JM)

      OPEN (14, FILE=fname, FORM="UNFORMATTED", STATUS="OLD")

      READ (14) KBB, Z, ZZ, DZ, DZZ, IMM, JMM, ALON, ALAT, DX, DY, H 
      
      CLOSE (14)

      RETURN
      END

