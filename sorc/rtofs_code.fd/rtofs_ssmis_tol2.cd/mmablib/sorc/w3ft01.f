       SUBROUTINE W3FT01(STI,STJ,FLD,HI,II,JJ,NCYCLK,LIN)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3FT01         INTERPOLATE VALUES IN A DATA FIELD
!   AUTHOR: MCDONELL, J.     ORG: W345       DATE: 84-06-27
!   UPDATE: JONES,R.E.       ORG: W342       DATE: 87-03-19
!
! ABSTRACT: FOR A GIVEN GRID COORDINATE IN A DATA ARRAY, ESTIMATES
!   A DATA VALUE FOR THAT POINT USING EITHER A LINEAR OR QUADRATIC
!   INTERPOLATION METHOD.
!
! PROGRAM HISTORY LOG:
!   84-06-27  J.MCDONELL
!   89-11-01  R.E.JONES   CHANGE TO CRAY CFT77 FORTRAN
!   Continued minor language maintenance: Robert Grumbine 8 Oct 2014
!
! USAGE:  CALL W3FT01 (STI, STJ, FLD, HI, II, JJ, NCYCLK, LIN)
!
!   INPUT VARIABLES:
!     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
!     ------ --------- -----------------------------------------------
!     STI    ARG LIST  REAL*4 I GRID COORDINATE OF THE POINT FOR WHICH
!                      AN INTERPOLATED VALUE IS DESIRED
!     STJ    ARG LIST  REAL*4 J GRID COORDINATE OF THE POINT FOR WHICH
!                      AN INTERPOLATED VALUE IS DESIRED
!     FLD    ARG LIST  REAL*4 SIZE(II,JJ) DATA FIELD
!     II     ARG LIST  INTEGER*4 NUMBER OF COLUMNS IN 'FLD'
!     JJ     ARG LIST  INTEGER*4 NUMBER OF ROWS IN 'FLD'
!     NCYCLK ARG LIST  INTEGER*4 CODE TO SPECIFY IF GRID IS CYCLIC OR
!                      NOT:
!                       = 0 NON-CYCLIC IN II, NON-CYCLIC IN JJ
!                       = 1 CYCLIC IN II, NON-CYCLIC IN JJ
!                       = 2 CYCLIC IN JJ, NON-CYCLIC IN II
!                       = 3 CYCLIC IN II, CYCLIC IN JJ
!     LIN    ARG LIST  INTEGER*4 CODE SPECIFYING INTERPOLATION METHOD:
!                       = 1 LINEAR INTERPOLATION
!                      .NE.1  QUADRATIC INTERPOLATION
!
!   OUTPUT VARIABLES:
!     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
!     ------ --------- -----------------------------------------------
!     HI     ARG LIST  REAL*4 DATA FIELD VALUE AT (STI,STJ) OBTAINED
!                      BY INTERPOLATION.
!
! ATTRIBUTES:
!   LANGUAGE: CRAY CFT77 FORTRAN
!   MACHINE:  CRAY Y-MP8/832
!
!$$$
!
      IMPLICIT none
      INTEGER I, J, II, JJ, NCYCLK, LIN
      INTEGER IP2, IM1, ICYCLK, JCYCLK, K, J1, IP1

      REAL    ERAS(4)
      REAL    FLD(II,JJ)
      REAL    JY(4)
      REAL STI, STJ, HI
      REAL FI, FJ, XDELI, XDELJ, XI2TM, XJ2TM

      I     = STI
      J     = STJ
      FI    = I
      FJ    = J
      XDELI = STI - FI
      XDELJ = STJ - FJ
      IP2   = I + 2
      IM1   = I - 1
      IP1   = I + 1
      JY(4) = J + 2
      JY(1) = J - 1
      JY(3) = J + 1
      JY(2) = J
      XI2TM = 0.0
      XJ2TM = 0.0
      IF (LIN.NE.1) THEN
        XI2TM = XDELI * (XDELI - 1.0) * 0.25
        XJ2TM = XDELJ * (XDELJ - 1.0) * 0.25
      ENDIF
      IF ((I.LT.2).OR.(J.LT.2))       GO TO 10
      IF ((I.GT.II-3).OR.(J.GT.JJ-3)) GO TO 10
!
!     QUADRATIC (LINEAR TOO) OK W/O FURTHER ADO SO GO TO 170
!
      GO TO 170

   10 CONTINUE
        ICYCLK = 0
        JCYCLK = 0
! Fortran 66 Arithmetic IF -- labels for if NCYCLK < 0, = 0, > 0, respectively
        IF (NCYCLK) 20,120,20

   20 CONTINUE
        IF (NCYCLK / 2 .NE. 0) JCYCLK = 1
        IF (NCYCLK .NE. 2)     ICYCLK = 1
        IF (ICYCLK .NE. 0) THEN

        IF (I.EQ.1)      GO TO 40
        IF (I.EQ.(II-1)) GO TO 50
        IP2 = I + 2
        IM1 = I - 1
        GO TO 60

   40 CONTINUE
        IP2 = 3
        IM1 = II - 1
        GO TO 60

   50 CONTINUE
        IP2 = 2
        IM1 = II - 2

   60 CONTINUE
        IP1 = I + 1
      ENDIF

      IF (JCYCLK .NE. 0) THEN
        IF (J.EQ.1)      GO TO 90
        IF (J.EQ.(JJ-1)) GO TO 100
        JY(4) = J + 2
        JY(1) = J - 1
        GO TO 110

   90 CONTINUE
        JY(4) = 3
        JY(1) = JJ - 1
        GO TO 110

  100 CONTINUE
        JY(4) = 2
        JY(1) = JJ - 2

  110 CONTINUE
        JY(3) = J + 1
        JY(2) = J
      ENDIF
!----------------- end NCYCLK > or < 0 work

  120 CONTINUE
        IF (LIN.EQ.1) GO TO 160

        IF (ICYCLK .EQ. 0) THEN
          IF ((I.LT.2).OR.(I.GE.(II-1)))  XI2TM = 0.0
        ENDIF

        IF (JCYCLK .EQ. 0) THEN 
          IF ((J.LT.2).OR.(J.GE.(JJ-1)))  XJ2TM = 0.0
        ENDIF
  160 CONTINUE
!
!.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
!
        IF (I.LT.1)   I   = 1
        IF (IP1.LT.1) IP1 = 1
        IF (IP2.LT.1) IP2 = 1
        IF (IM1.LT.1) IM1 = 1
!
!.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
!
        IF (I.GT.II)   I   = II
        IF (IP1.GT.II) IP1 = II
        IF (IP2.GT.II) IP2 = II
        IF (IM1.GT.II) IM1 = II
!
  170 CONTINUE
      DO K = 1,4
        J1 = JY(K)
!
!.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
!
        IF (J1.LT.1)  J1 = 1
        IF (J1.GT.JJ) J1 = JJ
        ERAS(K) = (FLD(IP1,J1) - FLD(I,J1)) * XDELI + FLD(I,J1) +
     &  (FLD(IM1,J1) - FLD(I,J1) - FLD(IP1,J1) + FLD(IP2,J1)) * XI2TM
      ENDDO
!
      HI = ERAS(2) + (ERAS(3) - ERAS(2)) * XDELJ + (ERAS(1) -
     &     ERAS(2) -  ERAS(3) + ERAS(4)) * XJ2TM
!
      RETURN
      END
