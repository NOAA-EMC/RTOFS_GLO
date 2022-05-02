      FUNCTION sumx2(x, n)
C     Function to compute the sum of the square of terms in a vector.
C       Use double precision summation, but return a single precision answer.  
C     Robert Grumbine 12/09/1993.

      IMPLICIT none

      INTEGER n
      REAL sumx2, x(n)

      DOUBLE PRECISION sum
      INTEGER i
      
      sum = 0.0
      DO i = 1, n
        sum = sum + DBLE(x(i)*x(i))
      ENDDO
      sumx2 = SNGL(sum)

      RETURN
      END
