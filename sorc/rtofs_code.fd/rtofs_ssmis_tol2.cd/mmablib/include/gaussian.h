#include <cstdlib>
#include <cmath>
//#include <unistd.h>
#include <iostream>
using namespace std;

//Class to define gaussian grids.
//These are defined as descendants of metricgrids.
//Robert Grumbine 14 December 2000
//
//Modifications:
//
//   23 April 2001   generalized to work on other compilers (gaulat call)
//                   note that revinterpolate to be moved to mvector
//    3 December 2002 Change return of fijpt to use global_fijpt as
//                    return of local copy of class variable is verboten.
//    3 April 2003   Improve type-casting in estimator of nx, ny
//   25 May 2004     vector -> mvector, default changed to T254
//   14 December 2005 Default changed to T382 (required update of gaulat)
//    9 July     2007 'thisification', refs now this->; also add T170 
//   19 February 2010 Add t574


#ifndef POINTSH
  #include "points.h"
#endif
#ifndef MVECTORH
  #include "mvector.h"
#endif
#ifndef GRIBH
  #include "grib.h"
#endif
#ifndef METRICH
  #include "metric.h"
#endif

#ifndef GAUSSIANH
  #define GAUSSIANH

//Utility function to be migrated to mvector.h
template <class T>
float revinterpolate(mvector<T> &ref, T value) ;

#ifdef LINUX
  extern "C" void gaulat_(float *g, int *n);
#elif SGI
  extern "C" void gaulat_(float *g, int *n);
#elif IBM
  extern "C" void gaulat(float *g, int *n);
#else
  extern "C" void gaulat_(float *g, int *n);
#endif

//////////// Class Definition:
#define T1534 4718592
#define T574  1548800 
#define T382   663552
#define T254   294912
#define T190   165888
#define T170   131072
#define T126    72960
#define T62     18048

template<class T>
class gaussian : public metricgrid<T> {
  private:
    int waves;
    mvector<float> gaulats;
    float dlon;

  public:
    gaussian(void);
    gaussian(int);
    gaussian(int, int);

    fijpt& locate(const latpt &);
    latpt locate(const ijpt  &);
    latpt locate(const fijpt &);
};

/////// Class operations:

template <class T>
gaussian<T>::gaussian() {
  float *g; int i;
  //waves = 254;
  //nx = 768;
  //ny = 384;
  //waves = 382;
  //this->nx = 1152;
  //this->ny =  576;
  //waves = 574;
  //this->nx = 1760;
  //this->ny =  880;
  waves = 1534;
  this->nx = 3072;
  this->ny = 1536;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "Failed to new in gaussian(void)\n";
    cout.flush(); }
  dlon = 360. / (float) this->nx;
  g = new float[this->ny];
  #ifdef LINUX
    gaulat_(g, &this->ny);
    #ifdef VERBOSE
    cout << "linux gaulat\n"; cout.flush();
    #endif
  #elif SGI
    gaulat_(g, &this->ny);
    #ifdef VERBOSE
    cout << "sgi gaulat\n"; cout.flush();
    #endif
  #elif IBM
    gaulat(g, &this->ny);
    #ifdef VERBOSE
    cout << "ibm gaulat\n"; cout.flush();
    #endif
  #else
    gaulat_(g, &this->ny);
    #ifdef VERBOSE
    cout << "else gaulat\n"; cout.flush();
    #endif
  #endif
  gaulats.resize(this->ny);
  for (i = 0; i < this->ny; i++) {
    //gaulats[i] = g[i] - 90.0;
    gaulats[i] = 90. - g[i];
    #ifdef VERBOSE
      printf("i, gaulat %d %f\n",i,gaulats[i]);
    #endif
  }
  this->cyclicx = (fabs(this->nx * dlon) >= 360.0);
  #ifdef VERBOSE
    printf("this->cyclicx = %d\n", (int) this->cyclicx);
  #endif
}

template <class T>
gaussian<T>::gaussian(int nwave) {
  int x, nearest2, nearest3;
  float *g;
  int i;
// Note that the number of x points for a given gaussian grid is any 
// arbitrary figure greater than or equal to 3nwave+1.  The following
// algorithm produces estimates given the number of waves, though
// does not guarantee a match.  For guarantee, you should be using
// the gaussian(nx, ny) initializer:
    x = 3*nwave + 1;
    nearest2 = (int) (0.5 + log((double)x) / log(2.0) );
    nearest3 = (int) (0.5 + (log((double)nwave) / log(2.0) ) );
    if (pow(2.0, (double)nearest2) < x && 3*pow(2, (double)nearest3) < x) {
      printf("%d err in constructing gaussian grid -- double negative\n", nwave);
      exit(1);
    }
    else if (pow(2.0, (double)nearest2) >= x && 3*pow(2.0, (double)nearest3) < x) {
      this->nx =  (int) pow(2.0, (double)nearest2) ;
    }
    else if (pow(2.0, (double)nearest2) >= x && 3*pow(2.0, (double)nearest3) >= x &&
             pow(2.0, (double)nearest2) < 3*pow(2.0, (double)nearest3) ) {
      this->nx =  (int) pow(2.0, (double)nearest2) ;
    }
    else {
      this->nx = (int) pow(2.0, (double)nearest3) * 3;
    }

  waves = nwave;
  this->ny = (3*nwave + 2) / 2;
  // 31 January 2006 -- special case for T382 vs. 254, 170, 126, 62 grids.
  if (nwave == 382 || nwave == 254) this->ny += 2; 
  if (nwave == 382) this->nx = 1152;
  // 19 February 2010 T574 case
  if (nwave == 574) {
    this->nx = 1760;
    this->ny =  880;
  }
  // 19 February 2010 T190 case
  if (nwave == 190) {
    this->nx = 576;
    this->ny = 288;
  }

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "Failed to new in gaussian(void)\n";
    cout.flush(); }
  dlon = 360. / (float) this->nx;
  g = new float[this->ny];
  #ifdef LINUX
    gaulat_(g, &this->ny);
  #elif SGI
    gaulat_(g, &this->ny);
  #elif IBM
    gaulat(g, &this->ny);
  #else
    gaulat_(g, &this->ny);
  #endif
  gaulats.resize(this->ny);
  for (i = 0; i < this->ny; i++) {
    //gaulats[i] = g[i] - 90.0;
    gaulats[i] = 90. - g[i];
    #ifdef VERBOSE
      printf("nwave %d %f\n",i,gaulats[i]);
    #endif
  }
  #ifdef VERBOSE
    printf("this->cyclicx = %d\n", (int) this->cyclicx);
  #endif
}

template<class T>
gaussian<T>::gaussian(int x, int y) {
  float *g;
  int i;

  this->nx = x;
  this->ny = y;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "Failed to new in gaussian(void)\n";
    cout.flush(); }
  waves = (2*this->ny - 2)/3;
  dlon = 360. / (float) this->nx;
  g = new float[this->ny];
  #ifdef LINUX
    gaulat_(g, &this->ny);
  #elif SGI
    gaulat_(g, &this->ny);
  #elif IBM
    gaulat(g, &this->ny);
  #else
    gaulat_(g, &this->ny);
  #endif
  gaulats.resize(this->ny);
  for (i = 0; i < this->ny; i++) {
    gaulats[i] = g[i] - 90.0;
  }
  #ifdef VERBOSE
    printf("cyclicx = %d\n", (int) this->cyclicx);
  #endif
}
template<class T>
fijpt& gaussian<T>::locate(const latpt &x) {
  fijpt fij;
  fij.i = (x.lon / dlon);
  if (fij.i < 0) fij.i += (float)this->nx;
  fij.j = revinterpolate(gaulats, x.lat); 
  global_fijpt = fij;
  return global_fijpt;
}
template<class T>
latpt gaussian<T>::locate(const ijpt &x) {
  latpt lloc;
  lloc.lat = gaulats[x.j];
  lloc.lon = x.i * dlon;
  return lloc;
}
template<class T>
latpt gaussian<T>::locate(const fijpt &x) {
  latpt lloc;
  lloc.lat = gaulats[(int)(0.5+x.j)];
  lloc.lon = x.i * dlon;
  return lloc;
}

//Given a monotonic mvector of values (ref), and a value (value), find
//  the index which is the best interpolated estimate of the points in
//  ref which would correspond to the value 
//Function is used by the 'locate' functions
template <class T>
float revinterpolate(mvector<T> &ref, T value) {
  float index, val1, val2;
  int i, i1, i2;
  int nmax = ref.xpoints() - 1, nstart = 0;

  if (ref[0] < ref[nmax] ) {
    //increasing values 
    if (value >= ref[nmax] ) return (nmax);
    if (value <= ref[0] ) return 0;
//This stretch is to accelerate the loop some, picking a better
//  starting point
    if (value > ref[nmax - nmax/8] ) {
      nstart = nmax - nmax/8;
    }
    else if (value > ref[nmax - nmax/4] ) {
      nstart = nmax - nmax/4;
    }
    else if (value > ref[nmax/2] ) {
      nstart = nmax/2;
    }
    else if (value > ref[nmax/4] ) {
      nstart = nmax / 4;
    }
    else if (value > ref[nmax/8 ] ) {
      nstart = nmax / 8;
    }
    for (i = nstart; i <= nmax; i++) {
      if ( value <= ref[i] ) {
        i1 = i - 1;
        i2 = i;
        val2 = ref[i2];
        val1 = ref[i1];
        index = i1 + (value - val1) / (val2 - val1) ;
        return index;
      }
    }
    return (float) i;
  }
  else {
    //decreasing values 
    if (value <= ref[nmax] ) {
       return (nmax);
    }
    if (value >= ref[0] ) return 0;
//This stretch is to accelerate the loop some, picking a better
//  starting point
    if (value < ref[nmax - nmax/8] ) {
      nstart = nmax - nmax/8;
    }
    else if (value < ref[nmax - nmax/4] ) {
      nstart = nmax - nmax/4;
    }
    else if (value < ref[nmax/2] ) {
      nstart = nmax/2;
    }
    else if (value < ref[nmax/4] ) {
      nstart = nmax / 4;
    }
    else if (value < ref[nmax/8 ] ) {
      nstart = nmax / 8;
    }
    for (i = nstart; i <= nmax; i++) {
      if ( value >= ref[i] ) {
        i1 = i - 1;
        i2 = i ;
        val1 = ref[i1];
        val2 = ref[i2];
        index = i2 - (value - val2) / (val1 - val2) ;
        return index;
      }
    }
    return (float) i;
  }
  return 0.0;
}

#endif
