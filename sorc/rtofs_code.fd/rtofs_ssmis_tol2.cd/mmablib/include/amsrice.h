/* Specifications for computing with the AMSR orbital data */
/* Robert Grumbine */
/* August 2005 */ 

#include "grid_math.h"

#ifndef AMSR_INCLUDE
  #include "amsr.h"
#endif
#ifndef PARAMETERS
  #include "params.h"
#endif


#ifndef ICEAMSR

  #define ICEAMSR

  typedef struct { unsigned int t19v : 16;
                   unsigned int t19h : 16;
                   unsigned int t24v : 16;
                   unsigned int t24h : 16;
                   unsigned int t37v : 16;
                   unsigned int t37h : 16;
                   unsigned int t89v : 16;
                   unsigned int t89h : 16;
                   unsigned int conc_bar :  8;
                   unsigned int bar_conc :  8;
                   unsigned int count    :  8; 
                   unsigned int hires_conc : 8; 
                   unsigned int weather_count : 8; 
                 } amsr;

  typedef struct { unsigned int t19v : 24;
                   unsigned int t19h : 24;
                   unsigned int t24v : 24;
                   unsigned int t24h : 24;
                   unsigned int t37v : 24;
                   unsigned int t37h : 24;
                   unsigned int t89v : 24;
                   unsigned int t89h : 24;
                   unsigned int conc_bar  :  16;
                   unsigned int hires_bar :  16;
                   unsigned int count     :   8;
                   unsigned int weather_count : 8; 
                 } amsr_tmp;

// Elements for the TEAM2 algorithm 21 Oct 2005 Robert Grumbine
#define n_atm 12
#define n_tb   7

typedef struct {
  grid2<float> tbmfy;
  grid2<float> tbmow;
  grid2<float> tbmcc;
  grid2<float> tbmthin;
  grid2<float> LUT19[n_atm];
  grid2<float> LUT89[n_atm];
  grid2<float> LUT19thin[n_atm];
  grid2<float> LUT89thin[n_atm];
  grid2<float> LUTDGR[n_atm];
  grid2<float> LUTGR37[n_atm];
  double phi19, phi89;
  char pole;
} amsr_team2_tables;

float nasa_team2(float v19, float h19, float v24, float v37, float h37,
          float v89, float h89, amsr_team2_tables &tab );

/* define field numbers */
/* already in amsr.h, which is included above 7 May 2013 */

/* Define special values for concentrations */

/* Define terms for handling transfer from AMSR file to ice grid */
/*    #define DATA_WINDOW 12
*/

/* General purpose prototypes */
extern "C" int amsr_pole_fill(unsigned char *map, int x);

extern "C" int amsr_getfld(amsr *ice, int npts, unsigned char *fld, float 
                             *rfld, int sel);

/* Ice function prototypes*/

extern int ice_add_bufr(amsr_tmp *north_tmp, amsr_tmp *south_tmp,
                 amsr_scan_points *a, int nread, 
                 amsr_team2_tables &arctic, amsr_team2_tables &antarctic);

extern int ice_avg_data(amsr_tmp *north_tmp, amsr_tmp *south_tmp,
                 amsr *north, amsr *south, 
                 const int north_pts, const int south_pts, 
                 amsr_team2_tables &arctic, amsr_team2_tables &antarctic);


extern int ice_zero(amsr_tmp *north_tmp, amsr_tmp *south_tmp,
             const int north_pts, const int south_pts);

extern int ice_mask( amsr *north, amsr *south,
              const int north_pts, const int south_pts,
              unsigned char *nmap, unsigned char *smap ) ;

/* Continue defining AMSR data type elements */
#define AMSR_T89V 14
#define AMSR_T89H 15
#define AMSR_CONC_BAR 16
#define AMSR_BAR_CONC 17
#define AMSR_COUNT 18
#define AMSR_WEATHER_COUNT 19
#define AMSR_HIRES_CONC 20 

/* Items for the concentration algorithm: */
#define AMSR_ANTENNA 1
#define AMSR_REGRESS 1
#define AMSR_GR37LIM 0.05
#define AMSR_GR24LIM 0.045

extern "C" void mapll(const float lat, const float lon, int *ilat, int *ilon, 
           const float xorig, const float yorig, const float eccen2, 
           const float slat, const float slon, const float rearth, 
           const float dx, const float dy, const float sgn);


/* Constants used in processing */
#define MAX_ICE 128
#define MIN_CONC 15

#endif
