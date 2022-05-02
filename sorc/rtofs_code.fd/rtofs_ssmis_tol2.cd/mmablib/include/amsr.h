#ifndef AMSR_INCLUDE
#define AMSR_INCLUDE

// Robert Grumbine 2009 -- present

// Set up data types/classes as appropriate for the AVHRR instruments on
//   NOAA-17, 18, and METOP-A

// Translate to AMSR-suitable 10 April 2009

// Frequencies are approx. 6.9, 10.65, 18.7, 23.8, 36.5, 89.0 GHz, 
//   horizontal, then vertical, polarization
// 18 Aug 2010 -- carry the two separate locations for the 89 GHz channels

typedef struct {
  unsigned char channel;
  float tmbr;          // 0.1 degree precision
} amsre_spots;

typedef struct {
  short int satid;

  short int year;
  unsigned char month, day, hour, minute, second;

  double clat[2], clon[2];

  amsre_spots obs[14]; // note that we get 2 89 GHz obs for each 1 of the others

} amsrept;

// Note that these indices refer to the amsrept, rather than the
//   ice spot values I'll be using later, in amsrice.h
#define AMSR_T7H 0
#define AMSR_T7V 1
#define AMSR_T11H 2
#define AMSR_T11V 3
#define AMSR_T19H 4
#define AMSR_T19V 5
#define AMSR_T24H 6
#define AMSR_T24V 7
#define AMSR_T37H 8
#define AMSR_T37V 9
#define AMSR_T89Ha 10
#define AMSR_T89Va 11
#define AMSR_T89Hb 12
#define AMSR_T89Vb 13


// RG: Look this up -- currently copying ssmi value
#define AMSR_MAX_LATITUDE 87.5
#define NSPOTS 1000
/* Structures which relate to bufr decoding */
  typedef struct {
      int kwrit;
      float latitude, longitude;
      int sftg, posn;
      float t89v, t89h;
  } amsr_short_bufr;

  typedef struct {
      amsrept full[NSPOTS];
  } amsr_scan_points;

extern int process_bufr(amsr_scan_points *b);
extern int check_bufr(amsr_scan_points *b);
extern void zero_bufr(amsr_scan_points *b, int i);

#endif
