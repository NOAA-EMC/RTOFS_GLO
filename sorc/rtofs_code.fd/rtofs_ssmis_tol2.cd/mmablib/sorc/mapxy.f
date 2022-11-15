      SUBROUTINE mapxy (X,Y,ALAT,ALONG,SLAT,SLON,SGN,E,RE)
C$*****************************************************************************
C$                                                                            *
C$                                                                            *
C$    DESCRIPTION:                                                            *
C$                                                                            *
C$    This subroutine converts from Polar Stereographic (X,Y) coordinates     *
C$    to geodetic latitude and longitude for the polar regions. The equations *
C$    are from Snyder, J. P., 1982,  Map Projections Used by the U.S.         *
C$    Geological Survey, Geological Survey Bulletin 1532, U.S. Government     *
C$    Printing Office.  See JPL Technical Memorandum 3349-85-101 for further  *
C$    details.                                                                *
C$                                                                            *
C$                                                                            *
C$    ARGUMENTS:                                                              *
C$                                                                            *
C$    Variable   Type       I/O    Description                                *
C$                                                                            *
C$    X          REAL        I     Polar Stereographic X Coordinate (km)      *
C$    Y          REAL        I     Polar Stereographic Y Coordinate (km)      *
C$    ALAT       REAL        O     Geodetic Latitude (degrees, +90 to -90)    *
C$    ALONG      REAL        O     Geodetic Longitude (degrees, 0 to 360)     *
C$                                                                            *
C$                                                                            *
C$                  Written by C. S. Morris - April 29, 1985                  *
C$                  Revised by C. S. Morris - December 11, 1985               *
C$                                                                            *
C$                  Revised by V. J. Troisi - January 1990                    *
C$                  SGN - provide hemisphere dependency (+/- 1)               *
C$                                                                            *
C      LAST MODIFIED 6 April 1994 Robert Grumbine                             *
C$*****************************************************************************
      IMPLICIT none

      REAL X,Y,ALAT,ALONG,E,E2,CDR,PI, SLAT, SLON
      REAL CHI, CM, RE, RHO, SGN, SL, T
C$*****************************************************************************
C$                                                                            *
C$    DEFINITION OF CONSTANTS:                                                *
C$                                                                            *
C$    Conversion constant from degrees to radians = 57.29577951.              *
      CDR=57.29577951
      E2=E*E
      PI=3.141592654
C$                                                                            *
C$*****************************************************************************
      SL = SLAT*PI/180.
      RHO=SQRT(X**2+Y**2)

      IF (RHO .LE. 0.1) THEN
        ALAT=90.*SGN
        ALONG=0.0
      ELSE
        CM=COS(SL)/SQRT(1.0-E2*(SIN(SL)**2))
        T=TAN((PI/4.0)-(SL/(2.0)))/((1.0-E*SIN(SL))/
     1    (1.0+E*SIN(SL)))**(E/2.0)

        IF (ABS(SLAT-90.).LT.1.E-5) THEN
          T=RHO*SQRT((1.+E)**(1.+E)*(1.-E)**(1.-E))/2./RE
         ELSE
          T=RHO*T/(RE*CM)
        END IF
        CHI=(PI/2.0)-2.0*ATAN(T)
        ALAT=CHI + ((E2/2.0) + (5.0*E2**2.0/24.0) + 
     1   (E2**3.0/12.0))*SIN(2*CHI) + 
     2   ((7.0*E2**2.0/48.0) + (29.0*E2**3/240.0))*SIN(4.0*CHI) + 
     3   (7.0*E2**3.0/120.0)*SIN(6.0*CHI)

        ALAT  = SGN*ALAT
        ALONG = ATAN2(SGN*Y, SGN*X)
        ALONG = SGN*ALONG
        ALAT  = ALAT * CDR
        ALONG = ALONG * CDR - SLON 

      ENDIF

      RETURN
      END
