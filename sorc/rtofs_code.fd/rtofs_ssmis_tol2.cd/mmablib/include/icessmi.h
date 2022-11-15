#define OPERATIONS
/* Specifications for computing with the SSMI orbital data */
/* Robert Grumbine 10 Feb 1995 */

// Move to C++ default.  If C-only versions needed, it's now safer
//   to re-invent them than trust that the ancient legacies have been
//   maintained.  8 May 2013

#ifndef GRIDH
  #include "grid_math.h"
#endif
#ifndef SSMICLASS_INCLUDE
  #include "ssmiclass.h"
#endif
#ifndef SSMI_INCLUDE
  #include "ssmi.h"
#endif
/* Define special values for concentrations */
#ifndef PARAMETERS
  #include "params.h"
#endif

#ifndef ICESSMI

  #define ICESSMI

/* oldssmi is what was in use before approximately 18 November 1998 */
  typedef struct { unsigned int t19v : 16;
                   unsigned int t19h : 16;
                   unsigned int t22v : 16;
                   unsigned int t37v : 16;
                   unsigned int t37h : 16;
                   unsigned int t85v : 16;
                   unsigned int t85h : 16;
                   unsigned int conc_bar :  8;
                   unsigned int bar_conc :  8;
                 } oldssmi;

  typedef struct { unsigned int t19v : 16;
                   unsigned int t19h : 16;
                   unsigned int t22v : 16;
                   unsigned int t37v : 16;
                   unsigned int t37h : 16;
                   unsigned int t85v : 16;
                   unsigned int t85h : 16;
                   unsigned int conc_bar :  8;
                   unsigned int bar_conc :  8;
                   unsigned int count    :  8; /* Added 18 Nov 1998 */
                   unsigned int hires_conc : 8; /* Added 10 October 2001 */
                   unsigned int weather_count : 8; /* Added 30 April 2004 */
                   unsigned int old_conc      : 8; /* Added 23 Sep 2005 */
                 } ssmi;

  typedef struct { unsigned int t19v : 24;
                   unsigned int t19h : 24;
                   unsigned int t22v : 24;
                   unsigned int t37v : 24;
                   unsigned int t37h : 24;
                   unsigned int t85v : 24;
                   unsigned int t85h : 24;
                   unsigned int conc_bar  :  16;
                   unsigned int hires_bar :  16;
                   unsigned int count     :   8;
                   unsigned int weather_count : 8; /* Added 30 April 2004 */
                   unsigned int old_conc_bar  :  16; /* Added 23 Sep 2005 */
                 } ssmi_tmp;
 
// Elements for the TEAM2 algorithm 21 Oct 2005 Robert Grumbine
#define n_atm 12
#define n_tb   7

typedef struct {
  grid2<float> tbmfy;
  grid2<float> tbmow;
  grid2<float> tbmcc;
  grid2<float> tbmthin;
  grid2<float> LUT19[n_atm];
  grid2<float> LUT85[n_atm];
  grid2<float> LUT19thin[n_atm];
  grid2<float> LUT85thin[n_atm];
  grid2<float> LUTDGR[n_atm];
  grid2<float> LUTGR37[n_atm];
  double phi19, phi85;
  char pole;
} ssmi_team2_tables;

float nasa_team2(float v19, float h19, float v22, float v37, float h37,
          float v85, float h85, ssmi_team2_tables &tab, int satno );

/* define field numbers */
  #define SSMI_T19V 0
  #define SSMI_T19H 1
  #define SSMI_T22V 2
  #define SSMI_T37V 3
  #define SSMI_T37H 4
  #define SSMI_T85V 5
  #define SSMI_T85H 6
  #define SSMI_CONC_BAR 7
  #define SSMI_BAR_CONC 8
  #define SSMI_COUNT    9
  #define SSMI_HIRES_CONC 10
  #define SSMI_WEATHER_COUNT 11
  #define SSMI_OLD_CONC 12




/* General purpose prototypes */
  extern "C" int newfilt(ssmi *nmap, ssmi *smap);
  extern "C" int ssmi_pole_fill(unsigned char *map, int x);

/* Ice function prototypes*/

  extern "C" int getfld(ssmi *ice, int npts, unsigned char *fld, float 
                             *rfld, int sel);
  extern int ice_add_bufr(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp,
                 ssmi_bufr_line *a, ssmi_team2_tables &arctic, ssmi_team2_tables &antarctic);

  extern int ice_avg_data(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp,
                 ssmi *north, ssmi *south, 
                 const int north_pts, const int south_pts, ssmi_team2_tables &arctic, ssmi_team2_tables &antarctic);


extern int ice_zero(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp,
             const int north_pts, const int south_pts);

extern int ice_mask( ssmi *north, ssmi *south,
              const int north_pts, const int south_pts,
              unsigned char *nmap, unsigned char *smap ) ;

/* Items for the concentration algorithm: */
#define SSMI_ANTENNA 1
#define SSMI_REGRESS 1
#define SSMI_GR37LIM 0.05
#define SSMI_GR22LIM 0.045

/* Additional function: */
extern "C" void mapll(const float lat, const float lon, int *ilat, int *ilon, 
           const float xorig, const float yorig, const float eccen2, 
           const float slat, const float slon, const float rearth, 
           const float dx, const float dy, const float sgn);


extern float nasa_team(float t19v, float f19h, float t22v, float t37v, float t37h,
                float t85v, float t85h, const char pole, const int ant, 
                const int satno);


#endif
