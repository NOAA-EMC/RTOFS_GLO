#include <cstdio>
#include <cmath>
#include <iostream>
using namespace std;

// Begin building up the metric grids -- those for which there is 
//   a mapping between i, j, and a specific point in some space.
// Robert Grumbine (Oldest immediately at hand 29 Oct 1997)
//
// Modifications:
//   Dec 12 1997: Add mercator class
//   Aug  3 1998: metricgrid operator=, elaborate on psgrid
//   Aug  4 1998: expand and #VERBOSE gribbing
//   17 Jun 1999: Add comments, make cfs ftn calls system-dependant, start
//                moving to cout, new vs. malloc, subsetting of psgrids,
//                nx*ny vs. nx, ny
//    4 Oct 1999: Add cellarea(psgrid), cyclic(xy)
//   14 Dec 2000: Add fromall, lambert class include, lin
//   20 Jun 2001: Add gaussian class include
//   24 Sep 2004: Add eta class include
//   30 Jan 2006: Add laplacean, gradsq for llgrids, former lapl.C
//    2 Nov 2007: Thisification, this-> refs
//   22 Dec 2011: Add gradients (return separately d/dx, d/dy, and |grad|)


#ifndef METRICH

#define METRICH

#ifndef GRIDH
  #include "grid_math.h"
#endif
#ifndef GRIBH
  #include "grib.h"
#endif
#ifndef PARAMETERS
  #include "params.h"
#endif

#define rpdg parameters::radians_per_degree

// The metric grids will inherit from the grid2 types, hence the
//   above include.
// Metrics include:
//   Polar stereographic (ncep standards applied)
//   Lat-long grid
//   Rotated lat-long grid
//   Mercator
//   Gaussian
//   Lambert Conformal
//   Eta
  
// Note that arcdis returns km.  Should be changed.  RG 20050412

// section out arcdis to avoid conflict with buoy.h 7 May 2013
#ifndef ARCDIS_READY
  #define ARCDIS_READY

  #ifdef ABSOFT
    extern "C" float arcdis(float &long1, float &lat1, float &long2,
                            float &lat2 );
    #define ARCDIS arcdis
  #endif
  
  #ifdef LINUX
    extern "C" float arcdis_(float &long1, float &lat1, float &long2,
                             float &lat2 );
    #define ARCDIS arcdis_
  #elif IBM
    extern "C" float arcdis(float &long1, float &lat1, float &long2,
                            float &lat2 );
    #define ARCDIS arcdis
  #endif

#endif

#ifdef LINUX
  extern "C" int w3ft01_(float *x, float *y, float *fld, float *dum, 
                         int *nx, int *ny, int *ncyc, int *interp);
  extern "C" void w3fi72_(int *i1, float *fr, int *i2, int *nbit, int *i3,
                      int *pds, char *pdsc, int *i4, int *i5, int *gds,
                      int *i6, int *i7, int *bitmask, int *npts, int *bds,
                      int *i8, char *grib, int *lgrib, int *ierr);
#elif IBM
  extern "C" int w3ft01(float *x, float *y, float *fld, float *dum, 
                         int *nx, int *ny, int *ncyc, int *interp);
  extern "C" void w3fi72(int *i1, float *fr, int *i2, int *nbit, int *i3,
                      int *pds, char *pdsc, int *i4, int *i5, int *gds,
                      int *i6, int *i7, int *bitmask, int *npts, int *bds,
                      int *i8, char *grib, int *lgrib, int *ierr);
#elif SGI
  extern "C" int w3ft01_(float *x, float *y, float *fld, float *dum,
                         int *nx, int *ny, int *ncyc, int *interp);
  extern "C" void w3fi72_(int *i1, float *fr, int *i2, int *nbit, int *i3,
                      int *pds, char *pdsc, int *i4, int *i5, int *gds,
                      int *i6, int *i7, int *bitmask, int *npts, int *bds,
                      int *i8, char *grib, int *lgrib, int *ierr);
#else
  extern "C" void W3FT01(float *x, float *y, float *fld, float *dum, 
                         int *nx, int *ny, int *ncyc, int *interp);
  extern "C" void W3FI72(int *i1, float *fr, int *i2, int *nbit, int *i3,
                      int *pds, char *pdsc, int *i4, int *i5, int *gds,
                      int *i6, int *i7, int *bitmask, int *npts, int *bds,
                      int *i8, char *grib, int *lgrib, int *ierr);
#endif


extern "C" void fmapll(const float lat, const float lon, float *i, float *j,
           const float xorig, const float yorig, const float eccen2, 
           const float slat, const float slon, const float rearth, 
           const float dx, const float dy, const float sgn);
extern "C" void mapxy(float *lat, float *lon, const int i, const int j,
           const float xorig, const float yorig, const float dx,
           const float dy, 
           const float slat, const float slon, const float sgn, 
           const float rearth, const float eccen2);


// Begin building up the metric grids -- those for which there is 
//   a mapping between i, j, and a specific point in some space.
// The metric grids will inherit from the grid2 types, hence the
//   above include.

//Define some auxiliary types:
#ifndef LATPT
#define LATPT
typedef struct {
  float lat, lon;
}  latpt;
#endif

#ifndef DLATPT
#define DLATPT
typedef struct {
  double lat, lon;
} dlatpt;
#endif

float lonpos(float lon) ;

///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
//Declare the Abstract Base Class - Metric Grids
//  It will be impossible to instantiate a metricgrid, and
//  this declaration will enforce that all metricgrids come
//  with the right locate functions.
template<class T>
class metricgrid :  public grid2<T> {
public:
//Metric grids are capable of GRIB write out
   grib_pds pds;
   grib_gds gds;
//Metricgrid creation/destruction
   metricgrid(grid2<T> &);
   metricgrid(metricgrid<T> &);
   metricgrid(void);
   virtual ~metricgrid(void);
//Required operators, pure virtual
   virtual latpt locate(const ijpt &) = 0;
   virtual latpt locate(const fijpt &) = 0;
   virtual fijpt& locate(const latpt &) = 0;

//Metric grid operations
//   metricgrid<T>& operator=(metricgrid<T> &);
   bool llin(latpt& );
   bool lin(latpt ) ;
   T llget(latpt& );
   void fromall(metricgrid<float> &, float , float);
   void fromall(metricgrid<float> &, metricgrid<float> &, float , float);
   void fromall(metricgrid<double> &, metricgrid<double> &, double , double);
   virtual void set_gds();
   int gribit(int, int, int, char *, int&, int);

   virtual double integrate() { return (double) 0; }; 
            //Note that this gives a default
            //integral of zero, but which will be overridden by a class-
            //specific integral where one is defined.  This is not a pure
            //virtual (see the locate functions above) as it isn't requiring
            //integration to be defined in every class.
};
#ifndef FROMALLH
#include "fromall.h"
#endif

template<class T>
metricgrid<T>::metricgrid(void) {
  #ifdef VERBOSE
    cout <<"Entered ::metricgrid(void), nx = " << this->nx << "\n";
    cout.flush();
  #endif
}
template<class T>
metricgrid<T>::~metricgrid(void) {
  #ifdef VERBOSE
    cout << "Entered ::~metricgrid(void)\n";
    cout.flush();
  #endif
}

template<class T>
T metricgrid<T>::llget(latpt& x) {
  fijpt y;
  T val;
  ijpt yint;
  y = locate(x);
  if (this->in(y)) {
    yint.i = (int) (0.5 + y.i);
    yint.j = (int) (0.5 + y.j);
    val = this->grid[yint.i + yint.j*this->nx];
  }
  else {
   val = (T) -INT_MAX;
  }
  return val;

}

template<class T>
bool metricgrid<T>::lin(latpt x) {
  fijpt loc;
  loc = this->locate(x);
  return this->in(loc);
}

template<class T>
bool metricgrid<T>::llin(latpt &x) {
  bool yes=true, no=false;
  ijpt y(0, 0);
  latpt corner; 
  float minlat, maxlat, minlon, maxlon;

  corner = locate(y);
  if (corner.lon < 0.) corner.lon += 360. ;
  minlat = min(FLT_MAX, corner.lat);
  maxlat = max(-FLT_MAX, corner.lat);
  minlon = min(FLT_MAX, corner.lon);
  maxlon = max(-FLT_MAX, corner.lon);
  y.i = this->nx - 1;
  corner = locate(y);
  if (corner.lon < 0.) corner.lon += 360. ;
  minlat = min(minlat, corner.lat);
  maxlat = max(maxlat, corner.lat);
  minlon = min(minlon, corner.lon);
  maxlon = max(maxlon, corner.lon);
  y.j = this->ny - 1;
  corner = locate(y);
  if (corner.lon < 0.) corner.lon += 360. ;
  minlat = min(minlat, corner.lat);
  maxlat = max(maxlat, corner.lat);
  minlon = min(minlon, corner.lon);
  maxlon = max(maxlon, corner.lon);
  y.i = 0;
  corner = locate(y);
  if (corner.lon < 0.) corner.lon += 360. ;
  minlat = min(minlat, corner.lat);
  maxlat = max(maxlat, corner.lat);
  minlon = min(minlon, corner.lon);
  maxlon = max(maxlon, corner.lon);

  if (x.lat <= maxlat && x.lat >= minlat && 
      x.lon <= maxlon && x.lon >= minlon ) {
     return yes;
  }
  else {
     return no;
  }
 
  return no;
  
}

template<class T>
metricgrid<T>::metricgrid(grid2<T> &x) {
  int j;
  #ifdef VERBOSE
    cout <<"Entered ::metricgrid(grid2)\n";
    cout.flush();
  #endif
  if (this->nx == 0 || this->ny == 0) {
    this->resize(x.xpoints(), x.ypoints() );
  }
  if (this->nx != x.xpoints() || this->ny != x.ypoints() ) {
    cout <<"unequal grid sizes in metricgrid=\n";
    cout.flush();
    return; 
  }
  
  for (j = 0; j < this->ny*this->nx; j++) {
    this->grid[j] = x[j];
  }
}

template<class T>
metricgrid<T>::metricgrid(metricgrid<T> &x) {
  int j;
  #ifdef VERBOSE
    cout <<"Entered ::metricgrid(metricgrid)\n";
    cout.flush();
  #endif
  if (this->nx == 0 || this->ny == 0) {
    this->resize(x.xpoints(), x.ypoints() );
  }
  if (this->nx != x.xpoints() || this->ny != x.ypoints() ) {
    cout <<"unequal grid sizes in metricgrid=\n";
    cout.flush();
    return; 
  }
  
  for (j = 0; j < this->ny*this->nx; j++) {
    this->grid[j] = x.grid[j];
  }
  
}


template <class T>
void metricgrid<T>::set_gds() {
  latpt first;
  ijpt  firstij;

  gds.gds[2] = 255; //  IDRT = 0 for regular lat-long grids.
  gds.gds[3] = this->nx; //  NX
  gds.gds[4] = this->ny; //  NY

  firstij.i = 0;
  firstij.j = 0;
  first = this->locate(firstij);
  firstij.i = (int) (first.lon * 1000. + 0.5);
  firstij.j = (int) (first.lat * 1000. + 0.5);
  gds.gds[5] = firstij.j; //  LAT1
  gds.gds[6] = firstij.i; //  LON1

}
template <class T>
int metricgrid<T>::gribit(int parmno, int depth, int fcst_lead, 
              char *grib, int &lgrib, int mxbit) {
  int *ibm;
  grid2<float> round(this->nx, this->ny);  // note that this is always float
  float *fgrid;
  int nbit, npts = this->nx*this->ny, ierr;
  int i;

  if (grib == (char *) NULL) {
    grib = new char[(100 + 28 + this->nx*this->ny*(mxbit + 1)) / 8];
    cout << "New'ed a new grib in gribit\n";
    cout.flush();
  }
  fgrid = new float[this->nx*this->ny];
  ibm = new int[this->nx*this->ny];
  if (ibm == (int *) NULL) {
    cout << "Failed to new an ibm in gribit\n";
    cout.flush();
  }
  for (i = 0; i < npts; i++) {
    ibm[i] = 0;
  }

// Set up the Grid Description Section
  set_gds();
  printf("About to call grib_scale, pred = %d, nbit = %d\n",pds.get_precision(),
          nbit); fflush(stdout);
  this->grib_scale(pds.get_precision(), nbit, round);
  #ifdef VERBOSE
    printf("scaling max min of scaling %d %f %f %d\n", pds.get_precision(),
                     round.gridmax(), round.gridmin(), nbit );
    fflush(stdout);
  #endif
  pds.set_parmno(parmno);
  pds.set_depth(depth);
  pds.set_fcst_lead(fcst_lead);
 
  #ifdef VERBOSE
    printf("nbit, mxbit = %d %d\n", nbit, mxbit);
    fflush(stdout);
  #endif
  if (mxbit != 0) {
    nbit = min(nbit, mxbit);
  }
  else {
    //do nothing, take value from grib_scale. 
  }
  #ifdef VERBOSE
  printf("nbit = %d\n",nbit);
  #endif

  ierr = 0;
  #ifdef W3LIB
    i1 = 0; //itype, 0 -> floating point , 1-> integer (ifld must be filled)
    i2 = 0; //ifld -> if itype=1, integer data array
    i3 = 0; //ipflag = 0 -> make pds from user supplied integer array id 
    i4 = 1; //igflag 0 -> make gds based on igrid value,
            //igflag 1 -> use user-supplied grid def
    i5 = 255; //igrid -> grid number, if 255, user must supply 
              // Future: check pds for grid number
    i6 = 0; //icomp -> 0 means earth-oriented winds, 1 -> grid-oriented
    i7 = 0; //ibflag -> 0 make bit map from user supplied data
    i8 = 0; //carries returned number of grid points in field
    i8 = npts;
    #ifdef VERBOSE
    {
      int jjj;
      int i1=0, i2=0, i3=0, i4=1, i5=255, i6=0, i7=0, i8=0; // dummies for w3fi72
      cout <<"Calling w3fi72, pds = \n";
      for (jjj = 0; jjj < 25; jjj++) {
         printf("%d pds %d\n",jjj, pds.pds[jjj]);
      }
      cout <<"Calling w3fi72, gds = \n";
      for (jjj = 0; jjj < 18; jjj++) {
         printf("%d gds %d\n",jjj, gds.gds[jjj]);
      }
    }
    #endif
    round.strip(fgrid);
    int bds[9] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
    char pdsc[28];
    #ifdef SGI
    w3fi72_(&i1, fgrid, &i2, &nbit, &i3, pds.pds, pdsc, 
           &i4, &i5, gds.gds, &i6, &i7, ibm, &npts, bds,  
           &i8, grib, &lgrib, &ierr);
    #elif LINUX
    w3fi72_(&i1, fgrid, &i2, &nbit, &i3, pds.pds, pdsc,
           &i4, &i5, gds.gds, &i6, &i7, ibm, &npts, bds,
           &i8, grib, &lgrib, &ierr);
    #elif IBM
    w3fi72(&i1, fgrid, &i2, &nbit, &i3, pds.pds, pdsc,
           &i4, &i5, gds.gds, &i6, &i7, ibm, &npts, bds,
           &i8, grib, &lgrib, &ierr);
    #elif CRAY
    W3FI72(&i1, fgrid, &i2, &nbit, &i3, pds.pds, pdsc, 
           &i4, &i5, gds.gds, &i6, &i7, ibm, &npts, bds,  
           &i8, grib, &lgrib, &ierr);
    #endif
  #endif
  
  #ifdef VERBOSE
    printf("GRIB? %c %c %c %c\n",grib[0], grib[1], grib[2], grib[3]);
  #endif
  return ierr;
}
///////////////////////////////////////////////////////////////////////
//Include the lambert conformal class
#ifndef LAMBERTH
#include "lambert.h"
#endif

///////////////////////////////////////////////////////////////////////
//Include the eta native grids class
#ifndef ETAH
#include "eta.h"
#endif

///////////////////////////////////////////////////////////////////////
//Include the gaussian grid class
#ifndef GAUSSIANH
#include "gaussian.h"
#endif

///////////////////////////////////////////////////////////////////////
//Include the psgrids grid class
#ifndef PSGRIDH
#include "psgrid.h"
#endif

///////////////////////////////////////////////////////////////////////
//Include the latlon grid class
#ifndef LLGRIDH
#include "llgrid.h"
#endif

///////////////////////////////////////////////////////////////////////
//Include the mercator grid class
#ifndef MERCATORH
#include "mercator.h"
#endif

///////////////////////////////////////////////////////////////////////
//////////////////Utility function:
float lonpos(float lon) {
  while (lon < 0.) {
    lon += 360.;
  }
  return lon;
}


#endif
