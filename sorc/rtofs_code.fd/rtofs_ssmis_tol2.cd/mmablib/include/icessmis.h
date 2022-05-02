/* Specifications for computing with the SSMI-S orbital data */
/* Robert Grumbine 7 February 2012 */

/* Define special values for concentrations */
#ifndef PARAMETERS
  #include "params.h"
#endif
#ifndef GRIDH
  #include "grid_math.h"
#endif
#ifndef SSMIS_INCLUDE
  #include "ssmisclass.h"
#endif

#ifndef ICESSMIS

  #define ICESSMIS

// Elements for the TEAM2 algorithm 21 Oct 2005 Robert Grumbine
#define n_atm 12
#define n_tb   7

typedef struct {
  grid2<float> tbmfy;
  grid2<float> tbmow;
  grid2<float> tbmcc;
  grid2<float> tbmthin;
  grid2<float> LUT19[n_atm];
  grid2<float> LUT92[n_atm];
  grid2<float> LUT19thin[n_atm];
  grid2<float> LUT92thin[n_atm];
  grid2<float> LUTDGR[n_atm];
  grid2<float> LUTGR37[n_atm];
  double phi19, phi92;
  char pole;
} ssmis_team2_tables;

float nasa_team2(float v19, float h19, float v22, float v37, float h37,
          float v92, float h92, float h150, ssmis_team2_tables &tab, int satno );

/* Define rest of ssmis indices */
#define SSMIS_CONC_BAR 8
#define SSMIS_BAR_CONC 9
#define SSMIS_COUNT   10
#define SSMIS_WEATHER_COUNT 11
#define SSMIS_HIRES_CONC    12


/* General purpose prototypes */
extern "C" int ssmis_newfilt(ssmis *nmap, ssmis *smap);
extern "C" int ssmis_pole_fill(unsigned char *map, int x);

/* Ice function prototypes*/
extern "C" int ssmis_getfld(ssmis *ice, int npts, unsigned char *fld, float 
                             *rfld, int sel);
extern int ice_add_bufr(ssmis_tmp *north_tmp, ssmis_tmp *south_tmp,
                 ssmisupt *a, ssmis_team2_tables &arctic, ssmis_team2_tables &antarctic);

extern int ice_avg_data(ssmis_tmp *north_tmp, ssmis_tmp *south_tmp,
                 ssmis *north, ssmis *south, 
                 const int north_pts, const int south_pts, 
                 ssmis_team2_tables &arctic, ssmis_team2_tables &antarctic);

extern int ice_zero(ssmis_tmp *north_tmp, ssmis_tmp *south_tmp,
             const int north_pts, const int south_pts);

extern int ice_mask(ssmis *north, ssmis *south,
              const int north_pts, const int south_pts,
              unsigned char *nmap, unsigned char *smap ) ;

/* Items for the concentration algorithm: */
#define SSMIS_ANTENNA 0
#define SSMIS_GR37LIM 0.05
#define SSMIS_GR22LIM 0.045

/* Additional function: */
extern "C" void mapll(const float lat, const float lon, int *ilat, int *ilon, 
           const float xorig, const float yorig, const float eccen2, 
           const float slat, const float slon, const float rearth, 
           const float dx, const float dy, const float sgn);


extern float nasa_team(float t19v, float f19h, float t22v, float t37v, float t37h,
                float t92v, float t92h, float t150h, const char pole, const int ant, 
                const int satno);

#endif
