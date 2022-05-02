// Include which declares the lambert conformal class.
// This is a descendant of the metricgrid class.
// The default grid is the ETA model's grid #221
// Robert Grumbine 5 Dec 2000
//
//  Modifications:
//   20 June 2001     Expand/generalize to general lamberts
//    3 December 2002 Change return of fijpt to use global_fijpt as
//                    return of local copy of class variable is verboten.
//    5 May 2004      Move to using params.h
//   24 September 2004 Add a Great Lakes Lambert grid (1 km) for experiments
//                    and further development of ice products
//    5 April     2007 thisification
//   15 May       2008 explicit declaration of all needed includes

#ifndef LAMBERTH
  #define LAMBERTH
  #include <cmath>
  
  #ifndef PARAMETERS
    #include "params.h"
  #endif
  #ifndef GRIBH
    #include "grib.h"
  #endif
  #ifndef COLORH
    #include "color.h"
  #endif
  #ifndef POINTH
    #include "points.h"
  #endif
  #ifndef MVECTORH
    #include "mvector.h"
  #endif

  #ifndef GRID2_BASE
    #include "grid_base.h"
  #endif
  #ifndef GRIDH
    #include "grid_math.h"
  #endif
  #ifndef METRICH
    #include "metric.h"
  #endif


//Class:
template <class T>  
class lambert : public metricgrid<T> { 

  protected : 
    //Grid-specific parameters
    float lat1, lon1, reflat, dx, dy;

    //Fundamental or derived parameters:
    float a;
    double an, de, de2, antr, h, dlon1, dr, orient, x0, y0;

  public : 
    lambert(void);                // default parameters for this projection
    lambert(lambert<T> &); // how to copy over one of these grids to another
    lambert(int, int, float, float, float, float, float, float, float);

    //Mandatory functions (grid defaults to current ETA-32 lambert):
    inline fijpt& locate(const latpt &); // translate lat-lon to grid ij
    inline latpt locate(const ijpt &);  // translate integer ij to lat-lon
    inline latpt locate(const fijpt &); // translate float ij to lat-lon
    float firstlon(void);
  
};

template <class T>
lambert<T>::lambert(void) {

  a = parameters::a; // value from IPLIB, spherical earth

// Elementary parameters:
  this->nx = 349;
  this->ny = 277;
  lat1   =    1.0;
  lon1   = -145.5;
  orient = -107.0;
  reflat = 50.0;
  dx = 32.463e3 ;
  dy = 32.463e3 ;
  h  = 1.; // -1 for south

  this->grid = new T[this->nx*this->ny]; 

  this->pds.set_gridid(221);

//Derived parameters:
  double dpr = parameters::degrees_per_radian;
  an = sin(h*reflat/dpr);
  // If two reference latitudes:
  // an = ln(cos(reflat1/dpr)/cos(reflat2/dpr))/
  //      ln(tan((90. - reflat1)/2./dpr)/tan((90.-reflat2)/2./dpr) )
  de = a * cos(reflat/dpr)*
            pow(tan( (h*reflat + 90.)/2. /dpr), an) / an;
  dr = de / pow(tan( (h*lat1 + 90.)/2. /dpr), an);
  dlon1 = (lon1 - orient);
  // The following two are for Fortran-indexed grids
  //x0 = 1. - h*sin(an*dlon1 /dpr) * dr / dx;
  //y0 = 1. + cos(an*dlon1 /dpr ) * dr / dy;
  x0 =  - h*sin(an*dlon1 /dpr) * dr / dx;
  y0 =  + cos(an*dlon1 /dpr ) * dr / dy;
  antr = 1./2./an;
  de2 = de*de;

  #ifdef VERBOSE
    printf("dx, dy, h dlon1 %f %f %f %f\n", dx, dy, h, dlon1);
    printf("dr, de, xp, yp, an %f %f %f %f %f\n",dr, de, x0, y0, an);
  #endif

}
template <class T>
lambert<T>::lambert(int inx, int iny, float ilat1, float ilon1, float iorient, 
                    float ireflat, float idx, float idy, float isgn) {
//isgn = +1 for northern hemisphere, -1 for southern.

  a = parameters::a; // value from IPLIB, spherical earth

  this->nx = inx;
  this->ny = iny;
  lat1   = ilat1;
  lon1   = ilon1;
  orient = iorient;
  reflat = ireflat;
  dx = idx;
  dy = idy;
  h  = isgn / fabs(isgn);  // ensure the magnitude is 1

  this->grid = new T[this->nx*this->ny];

//Derived parameters:
  double dpr = parameters::degrees_per_radian;
  an = sin(h*reflat/dpr);
  de = a * cos(reflat/dpr)*
            pow(tan( (h*reflat + 90.)/2. /dpr), an) / an;
  dr = de / pow(tan( (h*lat1 + 90.)/2. /dpr), an);
  dlon1 = (lon1 - orient);
  x0 =  - h*sin(an*dlon1 /dpr) * dr / dx;
  y0 =  + cos(an*dlon1 /dpr ) * dr / dy;
  antr = 1./2./an;
  de2 = de*de;

  #ifdef VERBOSE
    printf("dx, dy, h dlon1 %f %f %f %f\n", dx, dy, h, dlon1);
    printf("dr, de, xp, yp, an %f %f %f %f %f\n",dr, de, x0, y0, an);
  #endif

}

template <class T>
lambert<T>::lambert(lambert<T> &x) {
  ijpt loc;
  lat1   = x.lat1;
  lon1   = x.lon1;
  reflat = x.reflat;
  dx = x.dx;
  dy = x.dy;
  this->nx = x.nx;
  this->ny = x.ny;
  if (this->grid != (T *) NULL) {
    delete this->grid;
  }
  this->grid = new T[this->nx*this->ny];
  for (loc.j = 0; loc.j < this->ny; loc.j++) {
  for (loc.i = 0; loc.i < this->nx; loc.i++) {
    this->grid[loc.i + loc.j*this->nx] = x[loc];
  }
  }
}
 
template <class T>
fijpt& lambert<T>::locate(const latpt &ll) {
  fijpt loc;
  float dpr = parameters::degrees_per_radian, dlon;

  dr = de*pow(tan((90.-h*ll.lat)/2./dpr),an);
  dlon = ll.lon - orient + 180. + 3600.;
  while (dlon > 360.) {dlon -= 360.;}
  dlon -= 180.;
  loc.i = x0 + h*sin(an*dlon/dpr)*dr/dx;
  loc.j = y0 - cos(an*dlon/dpr)*dr/dy;
  global_fijpt = loc;
  return global_fijpt;
}
template <class T>
latpt lambert<T>::locate(const ijpt &ij) {
  latpt ll;
  double dpr = parameters::degrees_per_radian;
  float x, y, dr2;

  x = ((float)ij.i-x0) * dx;
  y = ((float)ij.j-y0) * dy;
  dr2 = x*x + y*y;
  if (dr2 < 1.e-6) {
    ll.lon = 0.;
    ll.lat = h*90.;
    return ll;
  }

  ll.lat = h* (2.*dpr * atan( pow(de2/dr2, antr) ) - 90.);

  ll.lon = (orient + h/an*dpr * atan2(x, -y));
  while (ll.lon < -180.) {
    ll.lon += 360.;
  }
  while (ll.lon > 360.) {
    ll.lon -= 360.;
  }

  return ll; 
}
template <class T>
latpt lambert<T>::locate(const fijpt &ij) {
  latpt ll;
  float x, y;
  float dr2;
  double dpr = parameters::degrees_per_radian;

  x = ((float)ij.i-x0) * dx;
  y = ((float)ij.j-y0) * dy;
  dr2 = x*x + y*y;
  if (dr2 < 1.e-6) {
    ll.lon = 0.;
    ll.lat = h*90.;
    return ll;
  }

  ll.lat = h* (2.*dpr * atan( pow(de2/dr2, antr) ) - 90.);

  ll.lon = (orient + h/an*dpr * atan2(x, -y));
  while (ll.lon < -180.) {
    ll.lon += 360.;
  }
  while (ll.lon > 360.) {
    ll.lon -= 360.;
  }

  return ll;
}
template <class T>
float lambert<T>::firstlon(void) {
  ijpt loc;
  latpt ll;
  loc.i = 0; loc.j = 0;
  ll = this->locate(loc);
  return ll.lon;
}
template <class T>
class gllamb : public lambert<T> {
  public :
    gllamb(void);
};
template <class T>
gllamb<T>::gllamb(void) {
  this->a = parameters::a; // value from IPLIB, spherical earth
  
  this->nx = 1500;
  this->ny = 1000;
  this->lat1   = 40.5;
  this->lon1   = -92.5;
  this->orient = -90; 
  this->reflat = 45.0; 
  this->dx = 1.e3;
  this->dy = 1.e3;
  this->h  = 1;

  this->grid = new T[this->nx*this->ny];

//Derived parameters:
  double dpr = parameters::degrees_per_radian;
  this->an = sin(this->h*this->reflat/dpr);
  this->de = this->a * cos(this->reflat/dpr)*
            pow(tan( (this->h*this->reflat + 90.)/2. /dpr), this->an) / this->an;
  this->dr = this->de / pow(tan( (this->h*this->lat1 + 90.)/2. /dpr), this->an);
  this->dlon1 = (this->lon1 - this->orient);
  this->x0 =  - this->h*sin(this->an*this->dlon1 /dpr) * this->dr / this->dx;
  this->y0 =  + cos(this->an*this->dlon1 /dpr ) * this->dr / this->dy;
  this->antr = 1./2./this->an;
  this->de2 = this->de*this->de;
}

template <class T>
class ndfd : public lambert<T> {
  public :
    ndfd(void);
};
template <class T>
ndfd<T>::ndfd(void) {
  this->a = parameters::a; // value from IPLIB, spherical earth
  
  this->nx = 2345;
  this->ny = 1597;
  this->lat1   = 19.229;
  this->lon1   = 233.723448 - 360.;
  this->orient =  -95; 
  this->reflat = 25.0; 
  this->dx = 2539.703;
  this->dy = 2539.703;
  this->h  = 1;

  this->grid = new T[this->nx*this->ny];

//Derived parameters:
  double dpr = parameters::degrees_per_radian;
  this->an = sin(this->h*this->reflat/dpr);
  this->de = this->a * cos(this->reflat/dpr)*
            pow(tan( (this->h*this->reflat + 90.)/2. /dpr), this->an) / this->an;
  this->dr = this->de / pow(tan( (this->h*this->lat1 + 90.)/2. /dpr), this->an);
  this->dlon1 = (this->lon1 - this->orient);
  this->x0 =  - this->h*sin(this->an*this->dlon1 /dpr) * this->dr / this->dx;
  this->y0 =  + cos(this->an*this->dlon1 /dpr ) * this->dr / this->dy;
  this->antr = 1./2./this->an;
  this->de2 = this->de*this->de;
}

#endif
