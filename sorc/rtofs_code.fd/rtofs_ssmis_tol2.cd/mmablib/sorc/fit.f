      SUBROUTINE fit(x, y, n, a, b, r)
C     Compute the best fit line (y = a + bx) and its correlation to obs
C     Robert Grumbine 15 December 1994

      IMPLICIT none

      INTEGER n
      REAL x(n), y(n), corxy
      REAL a, b, r
 
      INTEGER i
      REAL sumxy, sumx, sumy, sumxx, sumyy
      REAL xm, ym
      
      sumx  = 0.0
      sumy  = 0.0
      sumxy = 0.0
      sumxx = 0.0
      sumyy = 0.0

      DO i = 1, n
        sumx  = sumx + x(i)
        sumxx = sumxx + x(i)*x(i)
        sumxy = sumxy + x(i)*y(i)
        sumyy = sumyy + y(i)*y(i)
        sumy  = sumy  + y(i)
      ENDDO

      xm = sumx / FLOAT(n)
      ym = sumy / FLOAT(n)

      b = (sumxy-n*xm*ym)/(sumxx - n*xm*xm)
      a = ym - b*xm
      r = (sumxy-n*xm*ym)/SQRT(sumxx-n*xm*xm)/SQRT(sumyy-n*ym*ym)

      RETURN
      END
