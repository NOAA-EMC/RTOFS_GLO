#include <cstdio>
// ssmiclass begun 5/5/2004  Robert Grumbine
//   Purpose is to transition to having a class for ssmi processing,
//   rather than requiring C-only codes 
// 4 February 2009: add member functions polarization, gradient, differences, qc, show
// 7 February 2012: spawn off version for ssmis

#ifndef SSMIS_INCLUDE
  #include "ssmis.h"
#endif

#ifndef SSMISCLASS_INCLUDE
  #define SSMISCLASS_INCLUDE
  
  #ifndef POINTSH
    #include "points.h"
  #endif
  #ifndef PARAMSH
    #include "params.h"
  #endif

/* Maximum observed latitude = */
  #ifndef MAX_LATITUDE
    #define MAX_LATITUDE  88.5
  #endif
/* Define the characteristics of the data file structure */
  #define NORBITS          1


/* define field numbers */
/* -- this is in the ssmis.h file, above */

// 4 Feb 2009 RG note: want to do something stable about the flag values
//    used in processing -- LAND, ...

  typedef struct { unsigned int t19v : 16;
                   unsigned int t19h : 16;
                   unsigned int t22v : 16;
                   unsigned int t37v : 16;
                   unsigned int t37h : 16;
                   unsigned int t92v : 16;
                   unsigned int t92h : 16;
                   unsigned int t150h : 16;
                   unsigned int conc_bar :  8;
                   unsigned int bar_conc :  8;
                   unsigned int count    :  8; /* Added 18 Nov 1998 */
                   unsigned int hires_conc : 8; /* Added 10 October 2001 */
                   unsigned int weather_count : 8; /* Added 30 April 2004 */
                   unsigned int old_conc      : 8; /* Added 23 Sep 2005 */
                 } ssmis;

  typedef struct { unsigned int t19v : 24;
                   unsigned int t19h : 24;
                   unsigned int t22v : 24;
                   unsigned int t37v : 24;
                   unsigned int t37h : 24;
                   unsigned int t92v : 24;
                   unsigned int t92h : 24;
                   unsigned int t150h : 24;
                   unsigned int conc_bar  :  16;
                   unsigned int hires_bar :  16;
                   unsigned int count     :   8;
                   unsigned int weather_count : 8; /* Added 30 April 2004 */
                   unsigned int old_conc_bar  :  16; /* Added 23 Sep 2005 */
                 } ssmis_tmp;



class ssmispt {
  public:
    ssmis obs;
    ssmispt();
    operator double();
    ssmispt(ssmispt &);
    ssmispt & operator=(ssmispt &);
// Added 4 February 2009, after versions in 28 Jul 2000 experiment
    point3<float> polarization();
    point3<float> gradient();
    point3<float> differences();
    int qc();
    void show();
// Future: To add, after methods in ssmisclass.operations.h 
//    ssmispt & operator=(int );
};


ssmispt::ssmispt() {
  obs.t19v = 0;
  obs.t19h = 0;
  obs.t22v = 0;
  obs.t37v = 0;
  obs.t37h = 0;
  obs.t92v = 0;
  obs.t92h = 0;
  obs.t150h = 0;
  obs.conc_bar      = 0;
  obs.bar_conc      = 0;
  obs.count         = 0;
  obs.hires_conc    = 0;
  obs.weather_count = 0;
}
ssmispt::operator double() {
  return (double) obs.conc_bar;
}
ssmispt::ssmispt(ssmispt &x) {
  obs.t19v = x.obs.t19v;
  obs.t19h = x.obs.t19h;
  obs.t22v = x.obs.t22v;
  obs.t37v = x.obs.t37v;
  obs.t37h = x.obs.t37h;
  obs.t92v = x.obs.t92v;
  obs.t92h = x.obs.t92h;
  obs.t150h = x.obs.t150h;
  obs.conc_bar      = x.obs.conc_bar;
  obs.bar_conc      = x.obs.bar_conc;
  obs.count         = x.obs.count;
  obs.hires_conc    = x.obs.hires_conc;
  obs.weather_count = x.obs.weather_count;
}
ssmispt & ssmispt::operator=(ssmispt &x) {
  obs.t19v = x.obs.t19v;
  obs.t19h = x.obs.t19h;
  obs.t22v = x.obs.t22v;
  obs.t37v = x.obs.t37v;
  obs.t37h = x.obs.t37h;
  obs.t92v = x.obs.t92v;
  obs.t92h = x.obs.t92h;
  obs.t150h = x.obs.t150h;
  obs.conc_bar      = x.obs.conc_bar;
  obs.bar_conc      = x.obs.bar_conc;
  obs.count         = x.obs.count;
  obs.hires_conc    = x.obs.hires_conc;
  obs.weather_count = x.obs.weather_count;
  return *this;
}
// Added 4 February 2009, after versions in 28 Jul 2000 experiment
void ssmispt::show() {
  printf("ssmispt %5d %5d %5d %5d %5d %5d %5d %5d  %3d %3d\n",
    obs.t19v, obs.t19h,obs.t22v, obs.t37v, obs.t37h, obs.t92v, obs.t92h, obs.t150h,
    obs.conc_bar, obs.bar_conc);
}
int ssmispt::qc() {
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
  if (obs.t92v < 50*100 || obs.t92v > 300*100) {
    ret += 1;
  }
  if (obs.t92h < 50*100 || obs.t92h > 300*100) {
    ret += 1;
  }
  if (obs.t150h <  1*100 || obs.t150h > 300*100) {
    ret += 1;
  }
  
  return ret;
}

point3<float> ssmispt::gradient() {
  point3<float> x;
  x.i = (float) obs.t22v / (float) obs.t19v;
  x.j = (float) obs.t37v / (float) obs.t19v;
  x.k = (float) obs.t92v / (float) obs.t19v;
  return x;
}
point3<float> ssmispt::polarization() {
  point3<float> x;
  x.i = (float) obs.t19v / (float) obs.t19h;
  x.j = (float) obs.t37v / (float) obs.t37h;
  x.k = (float) obs.t92v / (float) obs.t92h;
  return x;
}
point3<float> ssmispt::differences() {
  point3<float> x;
  x.i = (float) obs.t19v - (float) obs.t19h;
  x.j = (float) obs.t37v - (float) obs.t37h;
  x.k = (float) obs.t92v - (float) obs.t92h;
  return x;
}


#endif
