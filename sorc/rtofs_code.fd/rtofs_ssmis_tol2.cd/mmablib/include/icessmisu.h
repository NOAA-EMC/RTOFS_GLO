/* Specifications for computing with the SSMI-S orbital data */
/* Robert Grumbine 10 Feb 1995 */
/* Updated for TEAM2 21 Oct 2005 Robert Grumbine */

  #include "grid_math.h"
  #ifndef SSMISU_INCLUDE
    #include "ssmisuclass.h"
  #endif

#ifndef ICESSMISU

  #define ICESSMISU

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
} team2_tables;

float nasa_team2(float v19, float h19, float v22, float v37, float h37,
          float v85, float h85, team2_tables &tab, int satno );

extern float nasa_team1(float t19v, float f19h, float t22v, float t37v, float t37h,
                float t85v, float t85h, const char pole, const int ant, 
                const int satno);

/* Define special values for concentrations */
#ifndef PARAMETERS
  #include "params.h"
#endif

/* General purpose prototypes */
extern "C" int newfilt(ssmi *nmap, ssmi *smap);
extern "C" int pole_fill(unsigned char *map, int x);

/* Ice function prototypes*/
extern "C" int getfld(ssmi *ice, int npts, unsigned char *fld, float 
                           *rfld, int sel);
extern int ice_add_bufr(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp,
               bufr_line *a, team2_tables &arctic, team2_tables &antarctic);

extern int ice_avg_data(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp,
               ssmi *north, ssmi *south, 
               const int north_pts, const int south_pts, 
               team2_tables &arctic, team2_tables &antarctic);


extern int ice_zero(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp,
             const int north_pts, const int south_pts);

extern int ice_mask( ssmi *north, ssmi *south,
              const int north_pts, const int south_pts,
              unsigned char *nmap, unsigned char *smap ) ;

/* Additional function: */
extern "C" void mapll(const float lat, const float lon, int *ilat, int *ilon, 
           const float xorig, const float yorig, const float eccen2, 
           const float slat, const float slon, const float rearth, 
           const float dx, const float dy, const float sgn);




#endif
