      SUBROUTINE correl(x, y, k, r2, xbar, ybar, sig2x, sig2y)
C     Compute statistical parameters between two vectors:
C       mean for each, variance for each, and correlation between.
C     Uses unbiased estimator of variance.
C     Robert Grumbine 1/22/94.
C     LAST MODIFIED 8 April 1994.

      IMPLICIT none

      INTEGER k
      REAL x(k), y(k)
      REAL r2, xbar, ybar, sig2x, sig2y
      
      REAL sumx, sumx2, sumxy
C     The above are functions which compute sums of x, x**2, x*y,
C       respectively.
      REAL sx, sy, x2, y2, xy

      sx = sumx(x, k)
      sy = sumx(y, k)
      x2 = sumx2(x, k)
      y2 = sumx2(y, k)
      xy = sumxy(x, y, k)

      xbar = sx/FLOAT(k)
      ybar = sy/FLOAT(k)
      sig2x = (FLOAT(k)*x2-sx*sx)/FLOAT(k)/FLOAT(k-1)
      sig2y = (FLOAT(k)*y2-sy*sy)/FLOAT(k)/FLOAT(k-1)
      r2    = (FLOAT(k)*xy - sx*sy) / 
     1    SQRT( (FLOAT(k)*x2-sx*sx)*(FLOAT(k)*y2-sy*sy) )

      RETURN
      END 
