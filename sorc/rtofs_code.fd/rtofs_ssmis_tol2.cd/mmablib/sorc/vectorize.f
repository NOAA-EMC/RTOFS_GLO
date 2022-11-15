      SUBROUTINE vectorize(mag, angle, x, y, npts)
C     Given magnitude and angle in degrees, compute x and y equivalents.
C     Robert Grumbine 5 April 1994.
C     LAST MODIFIED 8 April 1994.

      IMPLICIT none

      INTEGER npts
      REAL mag(npts), angle(npts), x(npts), y(npts)

      INTEGER i
      REAL pi, rpdg

      pi = ATAN(1.)*4.
      rpdg = pi / 180.
      DO i = 1, npts
        x(i) = mag(i)*COS(rpdg*angle(i))
        y(i) = mag(i)*SIN(rpdg*angle(i))
      ENDDO

      RETURN
      END
