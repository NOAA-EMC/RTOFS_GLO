#ifndef DATEH
  #include "date.h"
#endif

#ifndef AMSR2_INCLUDE
#define AMSR2_INCLUDE

/* Robert Grumbine 2016 -- present */
/* Translate to AMSR2-suitable 20 January 2016 */
/* Frequencies are approx. 6.9, 7.3, 10.65, 18.7, 23.8, 36.5, 89.0 GHz, */ 
/*   horizontal, then vertical, polarization */

typedef struct {
  float sccf, alfr, anpo, viirsq; /* alfr = 0 means land; anpo 0 = H polarization */
  float tmbr;          /* 0.01 degree precision */
} amsr2_spot;

typedef struct {
  short int satid;
/*  short int year; */
/*  unsigned char month, day, hour, minute, second; */
  bufr_date date;
  double clat, clon;  /* 0.00001 degree precision */

  unsigned char nspots;

} amsr2head;

typedef struct {
  amsr2head head;
  amsr2_spot obs[2];
} amsr2_hrpt;
typedef struct {
  amsr2head head;
  amsr2_spot obs[12];
} amsr2_lrpt;

#define AMSR2_T6p9H  0
#define AMSR2_T6p9V  1
#define AMSR2_T7p3H  2
#define AMSR2_T7p3V  3
#define AMSR2_T11H   4
#define AMSR2_T11V   5
#define AMSR2_T19H   6
#define AMSR2_T19V   7
#define AMSR2_T24H   8
#define AMSR2_T24V   9
#define AMSR2_T37H  10
#define AMSR2_T37V  11
#define AMSR2_T89H   0
#define AMSR2_T89V   1

#define AMSR2_MAX_LATITUDE 89.0

#endif
