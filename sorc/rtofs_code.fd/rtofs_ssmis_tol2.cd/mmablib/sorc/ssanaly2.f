      SUBROUTINE ssanaly(dist1, dir1, dist2, dir2, npts, ia, r2, vcor)
!     Conduct simple analyses of displacement fields
!     1- correlation between 1 and 2 displacements
!     2  index of agreement
!     3  vector correlation.
!     Robert Grumbine 21 April 1994.
!     Revised for generality in working with buoys as well as forecast models.
!     Robert Grumbine 10 April 1995.
!     Variant ssanaly derived from sanaly for use with a single vector.
!     Robert Grumbine 10 April 1995.

      IMPLICIT none

      INTEGER npts, scorefile

!     Parameters for reading data in
      INTEGER nmax
      PARAMETER (nmax = 400*1024)
      REAL dir1(npts), dist1(npts)
      REAL dir2(npts), dist2(npts)
      REAL x1(nmax), y1(nmax)
      REAL x2(nmax), y2(nmax)

      REAL ia, r2, vcor, iagree, rbar
    
      ia = iagree(dist1, dist2, npts)
   
      CALL correl(dist1, dist2, npts, r2, 
     1                      rbar, rbar, rbar, rbar)

      CALL vectorize(dist1, dir1, x1, y1, npts)
      CALL vectorize(dist2, dir2, x2, y2, npts)

      CALL vcc(x1, y1, x2, y2, npts, vcor)

   
      RETURN
      END
