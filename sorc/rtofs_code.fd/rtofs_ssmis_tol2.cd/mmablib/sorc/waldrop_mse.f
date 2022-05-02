      SUBROUTINE waldrop(obsd, persist, fcst, climo, n)
!translated from John Waldrop writings Apr  1  1997
!Robert Grumbine 1 May 2014

      IMPLICIT none

      INTEGER n
      REAL obsd(n), persist(n), fcst(n), climo(n)
      REAL a(n), b(n), c(n)
      REAL r1, r2, r3
      REAL ss1, ss2
      REAL mse1, mse2, mse3
      REAL correl, sumx, sumx2, sumxy

      a = persist - obsd
      b = fcst    - obsd
      c = climo   - obsd
      
      mse3 = sumx2(c,n)
      mse2 = sumx2(b,n)
      mse1 = sumx2(a,n)
!Murphy Skill score -- mse vs. reference
      ss1 = 1 - mse1/mse3
      ss2 = 1 - mse2/mse3
      
      r1 = correl(obsd, persist, n)
      r2 = correl(obsd, fcst, n)
      r3 = correl(obsd, climo, n)

        
      WRITE (*,*) mse1, mse2, mse3, ss1, ss2, r1, r2, r3

      RETURN
      END
