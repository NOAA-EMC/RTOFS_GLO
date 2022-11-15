      COMPLEX FUNCTION ciagree(r, x, n)
C     Compute the index of agreement between two vectors.
C     Complex valued variant 11 April 1995
C     Robert Grumbine

      IMPLICIT none

      INTEGER n
      COMPLEX r(0:n), x(0:n)

      COMPLEX rbar, sr2, sx2, srx, spx
      INTEGER i

      sr2 = 0.0
      sx2 = 0.0
      srx = 0.0
      spx = 0.0
      rbar = 0.0

      DO i = 0, n
        rbar = rbar + r(i)
      ENDDO
      rbar = rbar / FLOAT (n)

      DO i = 0, n
          sr2 = sr2 + r(i)*CONJG(r(i))
          sx2 = sx2 + x(i)*CONJG(x(i))
          srx = srx + r(i)*x(i)
          spx = spx + (ABS(x(i)-rbar) + ABS(r(i)-rbar) )**2
      ENDDO

      IF (spx .NE. 0.0) THEN
        ciagree = 1.0 - (sx2 - 2.*srx + sr2) / spx
       ELSE 
        ciagree = 0.0
      ENDIF

      RETURN
      END 
