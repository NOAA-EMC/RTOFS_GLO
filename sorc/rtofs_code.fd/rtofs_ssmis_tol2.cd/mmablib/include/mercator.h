#include <cstdio>
#include <cmath>
#include <iostream>
using namespace std;

// Extract the Mercator grid type to its own file 7 May 2013

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
  #include "metric.h"
#endif

#ifndef MERCATORH
#define MERCATORH

#ifndef GRIDH
  #include "grid_math.h"
#endif
#ifndef GRIBH
  #include "grib.h"
#endif
#ifndef PARAMETERS
  #include "params.h"
#endif

///////////////////////////////////////////////////////////////////////
//Declare a class for Mercator grids
template <class T>
class mercator : public metricgrid<T> {
  private:
   double r0_base, cos_slat;
  public:
// Most of these data elements can probably be private
   float xorig, yorig, slat, slon, dx, dy;
   double f, eccen2, rearth;
   double r0, eccen;
   mercator(void); /* Construction creator */
   fijpt& locate(const latpt &);
   latpt locate(const ijpt &);
   latpt locate(const fijpt &);
};
template <class T>
mercator<T>::mercator(void) {
//  rearth = 6378.160e3;
//  eccen2 = 0.006694604; //earth definition is from GRIB standard
// NIC Uses Clarke 1866 ellipsoid.  Principle difference is in flattening,
//  which is much different than the 1/298.25 used by WGS 84 and any
//  other earth since the early 1900's.
  f      = 1. / 294.9786982 ;
  rearth = 6378.2064e3   ;
  eccen2 = f*(2.-f);
  eccen  = sqrt(eccen2);
  this->grid = (T *) NULL;
  dx = 2550.;
  dy = 2550.;
  dy = dy - 12.;
  this->nx = 516;
  this->ny = 510;
  this->grid = new T[this->nx*this->ny];
  slat =  (45.+2./60.+24./3600.)*rpdg;  // NIC said 45
  slon = -(84.+8./60.+24./3600.)*rpdg;  // NIC said -84
  xorig = -649446.25;
  yorig =  3306260.;
  r0 = rearth;
  r0_base = rearth *
           pow(  (1.-eccen*sin(slat)) / (1+eccen*sin(slat)) , eccen/2. );
  cos_slat = cos(slat);
}
template <class T>
fijpt& mercator<T>::locate(const latpt &x) {
  fijpt y;
  float xlon = x.lon;
  r0 = rearth * pow( (1.-eccen*sin(x.lat*rpdg)) / (1+eccen*sin(x.lat*rpdg)) , 
                    eccen/2. );
  if (slon < 0) {
    if (xlon > 0.) xlon -= 360.;
  }
  y.i = -xorig + (xlon*rpdg - slon)*rearth*cos_slat;
  y.j = -yorig + r0*cos_slat*log(tan( M_PI_4 + x.lat*rpdg/2. ) );
  y.i = y.i / dx;
  y.j = y.j / dy;
  global_fijpt = y;
  return global_fijpt;
}
template <class T>
latpt mercator<T>::locate(const ijpt &x) {
  latpt y;
  int i;
  double yloc_km = x.j*dy + yorig;
  y.lon = 0.0;

  y.lat = -M_PI_2 + 2. * atan( exp ( yloc_km / r0_base / cos_slat ) );

  for (i = 0; i < 2; i++) {
    r0 = rearth * pow(  (1.-eccen*sin(y.lat)) / (1.+eccen*sin(y.lat)) ,
                               eccen/2. );
    y.lat = -M_PI_2 + 2. * atan( exp ( yloc_km / r0 / cos_slat ) );
  }

  y.lon = slon + (x.i*dx + xorig)/rearth/cos_slat ;

  y.lat /= rpdg;
  y.lon /= rpdg;
  return y;
}
template <class T>
latpt mercator<T>::locate(const fijpt &fx) {
  latpt y;
  ijpt x; 
  int i;
  x.i = lrint( fx.i);
  x.j = lrint( fx.j);
  y.lon = 0.0;

  r0 = rearth * pow(  (1.-eccen*sin(slat)) / (1+eccen*sin(slat)) , eccen/2. );
  y.lat = -(M_PI_2) + 2. * atan( exp ( (x.j*dy + yorig) / r0 / cos_slat ) );

  for (i = 0; i < 2; i++) {
    r0 = rearth * pow(  (1.-eccen*sin(y.lat)) / (1.+eccen*sin(y.lat)) 
                                 , eccen/2. );
    y.lat = -M_PI_2 + 2. * atan( exp ( (x.j*dy + yorig) / r0 / cos_slat ) );
  }

  y.lon = slon + (x.i*dx + xorig)/rearth/cos_slat ;

  y.lat = y.lat / rpdg;
  y.lon = y.lon / rpdg; 
  return y;
}

///////////////////////////////////////////////////////////////////////

#endif
