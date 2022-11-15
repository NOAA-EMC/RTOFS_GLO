      SUBROUTINE GAULAT(GAUL,K)
!     LAST MODIFIED 7 April 1994 Mark Iredell
!
!     15 April 2014 Implicit none Robert Grumbine

      IMPLICIT none
      DOUBLE PRECISION A(3200)
      INTEGER K, KK, IS, ITER, N
      REAL GAUL(1)

      DOUBLE PRECISION ESP, C, FK, XZ, PKM1, PKM2, FN, PK, PKMRK 
      DOUBLE PRECISION SP, AVSP, RADI
      INTEGER L

      ESP=1.D-14
      C=(1.D0-(2.D0/3.14159265358979D0)**2)*0.25D0
      FK=K
      KK=K/2

      CALL BSSLZ1(A,KK)

      DO IS=1,KK
      XZ=COS(A(IS)/SQRT((FK+0.5D0)**2+C))
      ITER=0
   10 PKM2=1.D0
      PKM1=XZ
      ITER=ITER+1
      IF(ITER.GT.10) RETURN

      DO N=2,K
        FN=N
        PK=((2.D0*FN-1.D0)*XZ*PKM1-(FN-1.D0)*PKM2)/FN
        PKM2=PKM1
        PKM1=PK
      ENDDO
      PKM1=PKM2
      PKMRK=(FK*(PKM1-XZ*PK))/(1.D0-XZ**2)
      SP=PK/PKMRK
      XZ=XZ-SP
      AVSP=ABS(SP)
      IF(AVSP.GT.ESP) GO TO 10
      A(IS)=XZ
      ENDDO

      IF(K.NE.KK*2) THEN
        A(KK+1)=0.D0
        PK=2.D0/FK**2
        DO N=2,K,2
          FN=N
          PK=PK*FN**2/(FN-1.D0)**2
        ENDDO
      ENDIF

      DO N=1,KK
        L=K+1-N
        A(L)=-A(N)
      ENDDO
C
      RADI=180./(4.*ATAN(1.))
      DO N=1,K
        !Drop to single precision A 14 December 2005 because of linux not
        !  finding proper library when compiling into C
        !GAUL(N)=ACOS(A(N))*RADI
        GAUL(N)=ACOS(SNGL(A(N)))*RADI
      ENDDO
C
      RETURN
      END
