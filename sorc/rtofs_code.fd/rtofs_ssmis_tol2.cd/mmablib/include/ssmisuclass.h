#include <cstdio>
// ssmiclass begun 5/5/2004  Robert Grumbine
//   Purpose is to transition to having a class for ssmi processing,
//   rather than requiring C-only codes 
// 4 February 2009: add member functions polarization, gradient, differences, qc, show
// Copied over to begin ssmi-s class


#ifndef SSMISUCLASS_INCLUDE
  #define SSMISUCLASS_INCLUDE
  
  #ifndef POINTSH
    #include "points.h"
  #endif
  #ifndef PARAMSH
    #include "params.h"
  #endif

/* Maximum observed latitude = */
  #define SSMISU_MAX_LATITUDE  87.5
/* Define the characteristics of the data file structure */
  #define NORBITS          1
  #define NSCANS           64
/* Structures which relate to bufr decoding */
  typedef struct {
      int kwrit;
      float latitude, longitude;
      int sftg, posn;
      float t91v, t91h;
  } short_bufr;

  typedef struct {
      int scan_counter;
      float latitude, longitude;
      int surface_type;
      int position_num;
      float t19v, t19h, t22v, t37v, t37h, t91v, t91h;
      short_bufr hires[3];
  } bufr_point;

  typedef struct {
      int satno, year, month, day, hour, mins, secs, scan_no;
      bufr_point full[NSCANS];
  } bufr_line;
/* Declare BUFR function prototypes */
extern int process_bufr(bufr_line *b);
extern int check_bufr(bufr_line *b);
extern int check_short_bufr(short_bufr *b);
extern void zero_bufr(bufr_line *b, int i);


/* define field numbers */
#define T19V 0
#define T19H 1
#define T22V 2
#define T37V 3
#define T37H 4
#define T91V 5
#define T91H 6
#define T150H 7
#define CONC_BAR 8
#define BAR_CONC 9
#define COUNT    10
#define HIRES_CONC 11
#define WEATHER_COUNT 12

// 4 Feb 2009 RG note: want to do something stable about the flag values
//    used in processing -- LAND, ...

  typedef struct { unsigned int t19v : 16;
                   unsigned int t19h : 16;
                   unsigned int t22v : 16;
                   unsigned int t37v : 16;
                   unsigned int t37h : 16;
                   unsigned int t91v : 16;
                   unsigned int t91h : 16;
                   unsigned int t150h : 16;
                   unsigned int conc_bar :  8;
                   unsigned int bar_conc :  8;
                   unsigned int count    :  8; 
                   unsigned int hires_conc : 8; 
                   unsigned int weather_count : 8; 
                   unsigned int old_conc      : 8; 
                 } ssmisu;

  typedef struct { unsigned int t19v : 24;
                   unsigned int t19h : 24;
                   unsigned int t22v : 24;
                   unsigned int t37v : 24;
                   unsigned int t37h : 24;
                   unsigned int t91v : 24;
                   unsigned int t91h : 24;
                   unsigned int t150h : 24;
                   unsigned int conc_bar  :  16;
                   unsigned int hires_bar :  16;
                   unsigned int count     :   8;
                   unsigned int weather_count : 8; 
                   unsigned int old_conc_bar  :  16; 
                 } ssmisu_tmp;



class ssmisupt {
  public:
    ssmisu obs;
    ssmisupt();
    operator double();
    ssmisupt(ssmisupt &);
    ssmisupt & operator=(ssmisupt &);
// Added 4 February 2009, after versions in 28 Jul 2000 experiment
    point3<float> polarization();
    point3<float> gradient();
    point3<float> differences();
    int qc();
    void show();
// Future: To add, after methods in ssmisuclass.operations.h 
//    ssmisupt & operator=(int );
};


ssmisupt::ssmisupt() {
  obs.t19v = 0;
  obs.t19h = 0;
  obs.t22v = 0;
  obs.t37v = 0;
  obs.t37h = 0;
  obs.t91v = 0;
  obs.t91h = 0;
  obs.conc_bar      = 0;
  obs.bar_conc      = 0;
  obs.count         = 0;
  obs.hires_conc    = 0;
  obs.weather_count = 0;
}
ssmisupt::operator double() {
  return (double) obs.conc_bar;
}
ssmisupt::ssmisupt(ssmisupt &x) {
  obs.t19v = x.obs.t19v;
  obs.t19h = x.obs.t19h;
  obs.t22v = x.obs.t22v;
  obs.t37v = x.obs.t37v;
  obs.t37h = x.obs.t37h;
  obs.t91v = x.obs.t91v;
  obs.t91h = x.obs.t91h;
  obs.conc_bar      = x.obs.conc_bar;
  obs.bar_conc      = x.obs.bar_conc;
  obs.count         = x.obs.count;
  obs.hires_conc    = x.obs.hires_conc;
  obs.weather_count = x.obs.weather_count;
}
ssmisupt & ssmisupt::operator=(ssmisupt &x) {
  obs.t19v = x.obs.t19v;
  obs.t19h = x.obs.t19h;
  obs.t22v = x.obs.t22v;
  obs.t37v = x.obs.t37v;
  obs.t37h = x.obs.t37h;
  obs.t91v = x.obs.t91v;
  obs.t91h = x.obs.t91h;
  obs.conc_bar      = x.obs.conc_bar;
  obs.bar_conc      = x.obs.bar_conc;
  obs.count         = x.obs.count;
  obs.hires_conc    = x.obs.hires_conc;
  obs.weather_count = x.obs.weather_count;
  return *this;
}
// Added 4 February 2009, after versions in 28 Jul 2000 experiment
void ssmisupt::show() {
  printf("ssmisupt %5d %5d %5d %5d %5d %5d %5d   %3d %3d\n",
    obs.t19v, obs.t19h,obs.t22v, obs.t37v, obs.t37h, obs.t91v, obs.t91h,
    obs.conc_bar, obs.bar_conc);
}
int ssmisupt::qc() {
  int ret=0;
// If not a data point, don't worry
  if (obs.conc_bar == LAND ||
      obs.conc_bar == WEATHER ||
      obs.conc_bar == COAST ||
      obs.conc_bar == NO_DATA ||
      obs.bar_conc == LAND ||
      obs.bar_conc == WEATHER ||
      obs.bar_conc == COAST ||
      obs.bar_conc == NO_DATA ) {
    return ret;
  }
  if (obs.t19v < 50*100 || obs.t19v > 300*100) {
    ret += 1;
  }
  if (obs.t19h < 50*100 || obs.t19h > 300*100) {
    ret += 1;
  }
  if (obs.t22v < 50*100 || obs.t22v > 300*100) {
    ret += 1;
  }
  if (obs.t37v < 50*100 || obs.t37v > 300*100) {
    ret += 1;
  }
  if (obs.t37h < 50*100 || obs.t37h > 300*100) {
    ret += 1;
  }
  if (obs.t91v < 50*100 || obs.t91v > 300*100) {
    ret += 1;
  }
  if (obs.t91h < 50*100 || obs.t91h > 300*100) {
    ret += 1;
  }
  
  return ret;
}

point3<float> ssmisupt::gradient() {
  point3<float> x;
  x.i = (float) obs.t22v / (float) obs.t19v;
  x.j = (float) obs.t37v / (float) obs.t19v;
  x.k = (float) obs.t91v / (float) obs.t19v;
  return x;
}
point3<float> ssmisupt::polarization() {
  point3<float> x;
  x.i = (float) obs.t19v / (float) obs.t19h;
  x.j = (float) obs.t37v / (float) obs.t37h;
  x.k = (float) obs.t91v / (float) obs.t91h;
  return x;
}
point3<float> ssmisupt::differences() {
  point3<float> x;
  x.i = (float) obs.t19v - (float) obs.t19h;
  x.j = (float) obs.t37v - (float) obs.t37h;
  x.k = (float) obs.t91v - (float) obs.t91h;
  return x;
}


#endif
