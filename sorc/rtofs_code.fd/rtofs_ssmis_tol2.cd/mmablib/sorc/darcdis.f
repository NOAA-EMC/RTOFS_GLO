      DOUBLE PRECISION FUNCTION darcdis (long1, lat1, long2, lat2)
!     Function to compute the distance between two latitude, longitude
!       points in nautical miles.
!     Algorithm from Doug MacAyeal, The University of Chicago, 1985.
!     Bullet-proofing added by Robert Grumbine 6 April 1994.
!     Returns -1 in event of errors.  Diagnostic printout eliminated.
!       Robert Grumbine 15 September 2005

      IMPLICIT none

      DOUBLE PRECISION ab, ac, bc, arg
      DOUBLE PRECISION lat1, long1, lat2, long2

      DOUBLE PRECISION pi, rdpdg, rearth
      PARAMETER (pi = 3.141592654)
      PARAMETER (rdpdg = pi / 180.)
      PARAMETER (rearth = 6370.949e3)

!     Bullet-proofing: Check that lats are within +- 90, longs
!       longs are within +- 360.
      IF (ABS(lat1) .GT. 90. .OR. ABS(lat2) .GT. 90.) THEN
          darcdis = -1.
          RETURN
      ENDIF
      IF (ABS(long1) .GT. 360. .OR. ABS(long2) .GT. 360.) THEN
          darcdis = -1.
          RETURN
      ENDIF

!     Operational Code      
!     Special case included because trig round off can give identical
!      points a nonzero separating distance. BG 6/3/93.
      IF (long1 .EQ. long2 .AND. lat1 .EQ. lat2) THEN
        darcdis = 0.0
        RETURN
      ENDIF

      ab = (90.-lat1)*rdpdg
      ac = (90.-lat2)*rdpdg
      bc = ABS((long1-long2)*rdpdg)
!     arg introduced 6/3/93 to proof against the special case that
!       round-off errors in trig produce a value for the argument 
!       which is out of bounds for the arc-cosine function.  This
!       should be rare though not totally impossible.
      arg = DCOS(ab)*DCOS(ac)+DSIN(ab)*DSIN(ac)*DCOS(bc)
      IF (arg .GT.  1.0) THEN 
        arg =  1.0
      ENDIF
      IF (arg .LT. -1.0) THEN 
        arg = -1.0
      ENDIF
      darcdis = DACOS(arg)*rearth

!     Bullet-proofing: Verify that distance is less than circumference
!       of the earth.
      IF (darcdis .GT. 2.*pi*rearth) THEN
        darcdis = -1
        RETURN
      ENDIF

      RETURN
      END
