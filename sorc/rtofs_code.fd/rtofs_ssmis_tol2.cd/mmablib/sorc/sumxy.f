      FUNCTION sumxy(x, y, n)
C     Function to compute the sum of the product of two vectors.
C       Use double precision summation, but return a single precision answer.  
C     Robert Grumbine 12/09/1993.

      IMPLICIT none

      INTEGER n
      REAL x(n), y(n), sumxy

      INTEGER i
      DOUBLE PRECISION sum

      sum = 0.0
      DO i = 1, n
        sum = sum + x(i)*y(i)
      ENDDO

      sumxy = SNGL(sum)
      RETURN
      END
