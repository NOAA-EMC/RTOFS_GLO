/* Robert Grumbine 1993-present */
/* #Icegrids.h dates back to mists of time, ca. 1993-4, when */
/* #  I started trying to work with ssmi-derived ice concentrations */
/* #  along with the sea ice model            */
/* #Oldest extant source is 18 Jul 1996       */
/* #No significant modifications through 2004 */
/* #Change, with notes, to define vs. const 2 April 2004 */

#ifndef ICEGRIDS
  #define ICEGRIDS

  /* Continue migration to proper c++, for now default to high resolution grids */
  /* #ifdef HIRES */
#define INFLATE 10
  /* #else */
  /*   #define INFLATE 5 */
  /* #endif */

  #define NX_NORTH    (77*INFLATE)
  #define NY_NORTH    (93*INFLATE)
  #define polei_NORTH (38.*INFLATE)
  #define polej_NORTH (46.*INFLATE)
  #define NX_SOUTH    (69*INFLATE)
  #define NY_SOUTH    (71*INFLATE)
  #define polei_SOUTH (30.*INFLATE)
  #define polej_SOUTH (36.*INFLATE)
  
  #define sgn_NORTH      1.0
  #define slat_NORTH    60.0
  #define slon_NORTH  (-10.0)
  #define xorig_NORTH (-polei_NORTH*dx)
  #define yorig_NORTH (-polej_NORTH*dy)

  #define sgn_SOUTH   (-1.0)
  #define slat_SOUTH    60.0
  #define slon_SOUTH   170.0
  #define xorig_SOUTH (-polei_SOUTH*dx)
  #define yorig_SOUTH (-polej_SOUTH*dy)

/* C and C++ are someone mutually hostile about define versus const
 * added 2 April 2004 */
#ifndef CPLUS
  const float dx = (127.0e3/INFLATE); 
  const float dy = (127.0e3/INFLATE); 
  /* error found 13 September 1995  #define rearth  6738.273e3 */
  const float rearth = 6378.160e3;  
  const float eccen2 = 0.006694604; 
#else
  #define dx  (127.0e3/INFLATE) 
  #define dy  (127.0e3/INFLATE)
  /* error found 13 September 1995  #define rearth  6738.273e3 */
  #define rearth  6378.160e3 
  #define eccen2     0.006694604 
#endif
  
#endif
