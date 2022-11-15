#include <cstdio>
#include <cmath>
#include <iostream>
using namespace std;

// Extract psgrids to solo file 7 May 2013

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

#ifndef PSGRIDH
  #define PSGRIDH

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

///////////////////////////////////////////////////////////////////////
//Declare the base class for polar stereographic grids
template<class T> 
class psgrid : public metricgrid<T> {
  private:
// Most of these data elements can probably be private
  protected:
   double cm, first_longitude;
   float dx, dy, xorig, yorig;
   //double sgn, slat, slon, eccen2, rearth;
   double sgn, slon, eccen2, eccen, ps_ll, rearth;
  public:
   double slat, tnaught, sl;
// Required functions in all classes
   psgrid(void); /* Construction creator */
   ~psgrid(void); /* Destructor */
   psgrid(psgrid<T> &);
   inline fijpt& locate(const latpt& ); 
   inline latpt locate(const ijpt &);
   latpt locate(const fijpt &);
   psgrid(int, int, float, float, float, float, float, float, float );
   void set_gds();
// Optional, but good:
   void subset(psgrid<T> &, ijpt &, ijpt &);
   void subset(psgrid<T> &, double &, double &, double &, double &); 
   void subset(psgrid<T> &, int &, int &, int &, int &);
   //lat-lon-based subsetting and location 'in' testing added 10 Jul 2007
   bool in(double &, double &);
   bool in(ijpt &);
   bool in(fijpt &);
   double integrate();
   double integrate(T );
   double cellarea(ijpt &);
   float firstlon() {ijpt loc; latpt ll; loc.i = 0; loc.j = 0; ll = this->locate(loc); return ll.lon;}
   float firstlat() {ijpt loc; latpt ll; loc.i = 0; loc.j = 0; ll = this->locate(loc); return ll.lat;}
// Added 20 August 2007 -- find an approximate latitude spacing for psgrids.
   float dlat() {return dx/111.1e3; }
// Permit interrogation of protected data:
   float deltax() {return dx;}
   float ipole() {return -this->xorig / dx;}
   float jpole() {return -this->yorig / dy;}
   
//// Note that in constructors, it is necessary to include the following
////   when slat != 60.0  !!!
//  double rearth = parameters::rearth;
//  double eccen2 = parameters::eccen2;
//  double eccen  = sqrt(eccen2);
//  this->sl = this->slat / parameters::degrees_per_radian;
//  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
//  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
//           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);

};


template<class T>
double psgrid<T>::cellarea(ijpt &loc) {
// Return the area of the grid cell
  double sum = 0.0, radpdg, tmp;
  double polei, polej, dxdeg = parameters::m_per_degree;
  double sinphi;

  polei = -this->xorig / dx;
  polej = -this->yorig / dy;
  radpdg = parameters::radians_per_degree;

  tmp = sqrt(
    (dx*(loc.i-polei))*(dx*(loc.i-polei))
   +(dy*(loc.j-polej))*(dy*(loc.j-polej)) ) / dxdeg - 90.  ;
  sinphi = fabs(sin(radpdg * tmp) ) / fabs(sin(radpdg*slat));
  sum = (1. + sinphi)*(1.+sinphi) / 4. * dx * dy;

  return sum;
}



template<class T>
double psgrid<T>::integrate(T flag) {
// Integrate the calling grid over the domain
  int j;
  double sum = 0.0,  radpdg; 
  float polei, polej, dxdeg, tmp;
  ijpt loc;
  grid2<double> sinphi(this->xpoints(), this->ypoints() );

  polei = -this->xorig / dx;
  polej = -this->yorig / dy;
  dxdeg = 111.1e3;
  radpdg = parameters::radians_per_degree;

  for (loc.j = 0; loc.j < this->ny; loc.j++) {
    for (loc.i = 0; loc.i < this->nx; loc.i++) {
      tmp = sqrt(
        (dx*(loc.i-polei))*(dx*(loc.i-polei))
       +(dy*(loc.j-polej))*(dy*(loc.j-polej)) ) / dxdeg - 90.  ;
      sinphi[loc] = fabs(sin(radpdg * tmp) ) / fabs(sin(radpdg*slat));
    }
  }
  for (j = 0; j < this->ny*this->nx; j++) {
    if (this->grid[j] != flag) {
      sum += ((double) this->grid[j]) * (1. + sinphi[j])*(1.+sinphi[j]) / 4.;
    }
  }
  sum *= dx*dy;

  return sum;

}
template<class T>
double psgrid<T>::integrate() {
// Integrate the calling grid over the domain
  int j;
  double sum = 0.0,  radpdg; 
  float polei, polej, dxdeg, tmp;
  ijpt loc;
  grid2<double> sinphi(this->xpoints(), this->ypoints() );

  polei = -this->xorig / dx;
  polej = -this->yorig / dy;
  dxdeg = 111.1e3;
  radpdg = parameters::radians_per_degree;

  for (loc.j = 0; loc.j < this->ny; loc.j++) {
    for (loc.i = 0; loc.i < this->nx; loc.i++) {
      tmp = sqrt(
        (dx*(loc.i-polei))*(dx*(loc.i-polei))
       +(dy*(loc.j-polej))*(dy*(loc.j-polej)) ) / dxdeg - 90.  ;
      sinphi[loc] = fabs(sin(radpdg * tmp) ) / fabs(sin(radpdg*slat));
    }
  }
  for (j = 0; j < this->ny*this->nx; j++) {
    sum += ((double) this->grid[j]) * (1. + sinphi[j])*(1.+sinphi[j]) / 4.;
  }
  sum *= dx*dy;

  return sum;

}


template<class T>
psgrid<T>::psgrid(psgrid<T> &x) {
  parameters xxxx;
  #ifdef VERBOSE
    cout <<"Entered ::psgrid(psgrid)\n";
    cout.flush();
  #endif
  this->nx = x.xpoints();
  this->ny = x.ypoints();
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout <<"Failed to new in psgrid(met)\n";
    cout.flush();}

  rearth = parameters::rearth;
  eccen2 = parameters::eccen2;
  eccen  = sqrt(eccen2);
  ps_ll  = pow( pow(1.+eccen,1.+eccen) * pow(1.-eccen,1.-eccen) , 1./2.);
  this->xorig = x.xorig;
  this->yorig = x.yorig;
  sgn = x.sgn;
  slat = x.slat;
  slon = x.slon;
  dx = x.dx;
  dy = x.dy; 
  sl = x.sl;
  cm = x.cm;
  tnaught = x.tnaught; 
  first_longitude = x.first_longitude; 

  return ;
}
//Polar stereographic grids:
template <class T>
void psgrid<T>::set_gds() {
  latpt first;
  ijpt  firstij;

  this->gds.gds[2] = 5; //  IDRT = 5 for polar stereographic
  this->gds.gds[3] = this->nx; //  NX
  this->gds.gds[4] = this->ny; //  NY

  firstij.i = 0;
  firstij.j = 0;
  first = this->locate(firstij);
  firstij.i = (int) (first.lon * 1000. + 0.5);
  firstij.j = (int) (first.lat * 1000. + 0.5);
  this->gds.gds[5] = firstij.j; //  LAT1
  this->gds.gds[6] = firstij.i; //  LON1

  this->gds.gds[7] = 128; //  IRESFL - see grib table 7

  if (-90. - slon < 0.) {
    this->gds.gds[8] = (int) (1000.*(-90. - slon + 360.) );
             // this is supposed to be east longitude.
  }
  else {
    this->gds.gds[8] = (int) (1000.*(-90. - slon) );
  }

  this->gds.gds[9] = (int) (0.5 + dx);
  this->gds.gds[10] = (int) (0.5 + dy);


  if (slat > 0.) {
    this->gds.gds[11] = 0;
  }
  else {
    this->gds.gds[11] = 128;
  }

  // Set scanning mode:
  this->gds.gds[12] = 0;
  if ( dy > 0.) {
    this->gds.gds[12] += 64;
  }
  if ( dx < 0.) {
    this->gds.gds[12] += 128;
  }

}

template<class T>
bool psgrid<T>::in(fijpt &loc) {
  if (loc.i > -0.5 && loc.j > -0.5 && 
      loc.i < this->xpoints() - 0.5 && 
      loc.j < this->ypoints() - 0.5) {
    return true;
  }
  return false;
}
template<class T>
bool psgrid<T>::in(ijpt &loc) {
  if (loc.i > -0.5 && loc.j > -0.5 && 
      loc.i < this->xpoints() - 0.5 && 
      loc.j < this->ypoints() - 0.5) {
    return true;
  }
  return false;
}

template<class T>
bool psgrid<T>::in(double &lat, double &lon) {
  fijpt floc;
  latpt ll;
  ll.lat = lat; ll.lon = lon;
  floc = this->locate(ll);
  if (floc.i > -0.5 && floc.j > -0.5 && 
      floc.i < this->xpoints() - 0.5 && 
      floc.j < this->ypoints() - 0.5) {
    return true;
  }
  return false;
}
// Use given integers to subset grid with ij corner subsetter
// 30 Jul 2009
template<class T>
void psgrid<T>::subset(psgrid<T> &x, int &north, int &south, int &east, int &west) {
  ijpt ll, ur;
  ll.j = south; ll.i = west;
  ur.j = north; ur.i = east;
  x.subset(*this, ll, ur);
  return;
}
template<class T>
void psgrid<T>::subset(psgrid<T> &x, double &north, double &south, double &east, double &west) {
// Find extremal corner points and then invoke the corner point subsetter
// 10 Jul 2007
  latpt tlat;
  fijpt floc;
  ijpt ll, ur;
  int mini, minj, maxi, maxj;

 if (x.in(north, west) || x.in(north,east) || x.in(south, west) || x.in(south,east)) {
  tlat.lat = north;
  tlat.lon = west;
  floc = x.locate(tlat);
  mini = (int) floc.i;
  minj = (int) floc.j;
  maxi = (int) (0.5 + floc.i);
  maxj = (int) (0.5 + floc.j);

  tlat.lon = east;
  floc = x.locate(tlat);
  mini = min(mini, (int) floc.i);
  minj = min(minj, (int) floc.j);
  maxi = max(maxi, (int) (0.5 + floc.i) );
  maxj = max(maxj, (int) (0.5 + floc.j) );
  
  tlat.lat = south;
  floc = x.locate(tlat);
  mini = min(mini, floc.i);
  minj = min(minj, floc.j);
  maxi = max(maxi, (int) (0.5 + floc.i) );
  maxj = max(maxj, (int) (0.5 + floc.j) );
  
  tlat.lon = west;
  floc = x.locate(tlat);
  mini = min(mini, floc.i);
  minj = min(minj, floc.j);
  maxi = max(maxi, (int) (0.5 + floc.i) );
  maxj = max(maxj, (int) (0.5 + floc.j) );
 }
 else {
   mini = 0; 
   minj = 0;
   maxi = 0;
   maxj = 0;
 }

  if (mini < 0) mini = 0;
  if (minj < 0) minj = 0;
  if (maxi > x.xpoints() - 1) maxi =  x.xpoints() - 1;
  if (maxj > x.ypoints() - 1) maxj =  x.ypoints() - 1;

  if (mini > x.xpoints() - 1 || minj > x.ypoints() - 1 ||
      maxi < 0 || maxj < 0 ) {
    mini = 0; minj = 0; maxi = 0; maxj = 0;
    // this branch because if minimum point is greater than domain,
    //   then all four points must lie off the grid
    // conversely, if maximum values are negative, all four are off
    // shouldn't be reached:
    cout << "reached last ditch branch\n"; cout.flush();
  }
  printf("subsetter min max %d %d %d %d  %d %d\n",mini, minj, maxi, maxj, maxi-mini, maxj-minj); fflush(stdout);

  ll.i = mini; ll.j = minj;
  ur.i = maxi; ur.j = maxj;

  this->subset(x, ll, ur);

}

template<class T>
void psgrid<T>::subset(psgrid<T> &x, ijpt &ll, ijpt &ur) {
  ijpt local, tloc;

  #ifdef VERBOSE
    cout <<"Entered ::subset(psgrid, ll, ur)\n";
    cout.flush();
  #endif
  this->nx = ur.i - ll.i + 1;
  this->ny = ur.j - ll.j + 1;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout <<"Failed to new in psgrid(ll, ur)\n";
    cout.flush();}

  for (local.j = 0; local.j < this->ny; local.j++) {
  for (local.i = 0; local.i < this->nx; local.i++) {
    tloc = local ; tloc += ll;
    this->grid[local.i + local.j*this->nx] = x[tloc];
  }
  }

  this->xorig = x.xorig + ll.i*x.dx;
  this->yorig = x.yorig + ll.j*x.dy;

  sgn = x.sgn;
  slat = x.slat;
  slon = x.slon;
  dx = x.dx;
  dy = x.dy; 

  rearth = parameters::rearth;
  eccen2 = parameters::eccen2;
  eccen  = sqrt(eccen2);
  ps_ll  = pow( pow(1.+eccen,1.+eccen) * pow(1.-eccen,1.-eccen) , 1./2.);

  return ; 
}
template<class T>
psgrid<T>::psgrid(int xpt, int ypt, float polei, float polej, 
              float islat, float islon, float isgn, float idx, float idy) {
  parameters xxxx;
  #ifdef VERBOSE
    cout << "Entered ::psgrid(arglist)\n";
    cout.flush();
  #endif
  double eccen;
  this->nx = xpt;
  this->ny = ypt;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in psgrid(args)\n";
    cout.flush();}
  sgn = isgn;
  slat = islat;
  slon = islon;
  dx = idx;
  dy = idy;
  this->xorig = -polei*dx;
  this->yorig = -polej*dy;
  rearth = parameters::rearth;
  eccen2 = parameters::eccen2;
  eccen  = sqrt(eccen2);
  ps_ll  = pow( pow(1.+eccen,1.+eccen) * pow(1.-eccen,1.-eccen) , 1./2.);

  sl = slat / parameters::degrees_per_radian;
  cm = cos(sl)/ sqrt(1.0-eccen2*sin(sl)*sin(sl) );
  tnaught  = tan(M_PI_4 - sl/2.) /
           pow(  ((1.0 - eccen*sin(sl))/(1.0+eccen*sin(sl))), eccen/2.);
  ijpt f;
  f.i = 0; f.j = 0;
  first_longitude = (this->locate(f)).lon;

} 
  

template<class T> 
psgrid<T>::~psgrid(void) {
  #ifdef VERBOSE
    cout << "Entered psgrid destructor\n";
    cout.flush();
  #endif
}

template<class T> 
psgrid<T>::psgrid(void) {
  parameters xxxx;
  #ifdef VERBOSE
    cout << "Entered ::psgrid(void)\n";
    cout.flush();
  #endif
  rearth = parameters::rearth;
  eccen2 = parameters::eccen2;
  eccen  = sqrt(eccen2);
  ps_ll  = pow( pow(1.+eccen,1.+eccen) * pow(1.-eccen,1.-eccen) , 1./2.);
  sgn = 1.;
  slat = 60.;
  slon = -10.; //values from NCEP standard
  this->nx = 0;
  this->ny = 0;
  dx = 0;
  dy = 0; 
  this->xorig = 0;
  this->yorig = 0;
  #ifdef VERBOSE
    cout << "before the grid = null assign in ::psgrid(void) \n";
    cout.flush();
  #endif
  this->grid = (T *) NULL;
  #ifdef VERBOSE
    cout << "past the grid = null assign in ::psgrid(void) \n";
    cout.flush();
  #endif
  sl = slat / parameters::degrees_per_radian;
  cm = cos(sl)/ sqrt(1.0-eccen2*sin(sl)*sin(sl) );
  tnaught  = tan(M_PI_4 - sl/2.) /
           pow(  ((1.0 - eccen*sin(sl))/
                  (1.0+eccen*sin(sl))), eccen/2.);
  ijpt f;
  f.i = 0; f.j = 0;
  first_longitude = (this->locate(f)).lon;

  return ; 
}

template<class T> 
latpt psgrid<T>::locate(const fijpt &fx) {
  latpt y;
  fijpt x;
// Work on efficiency, replacing variables with class constants:
/* After V. Troisi program.  Given grid coordinates, compute latitude
  and longitude on a polar stereographic grid */
/* Robert Grumbine */
/* Last Modified 3 June 1994. */
  double eccen = sqrt(parameters::eccen2);
  double xp, yp, rho, t, chi;

  xp = fx.i * dx + this->xorig;
  yp = fx.j * dy + this->yorig;
  rho = sqrt(xp*xp+yp*yp);
  if (rho <= 0.1) {
      y.lat  = 90. * sgn;
      y.lon  = 0.0;
      return y;
  }

  if ( fabs(slat - 90) < 1.e-5 ) {
        t = rho*sqrt( pow(1.+eccen, 1.+eccen) * pow(1.-eccen, 1.-eccen) ) 
                        / 2. / rearth;
  }
  else {
        t = tnaught * rho / (rearth * cm);
  }

  chi = (M_PI_2) - 2.*atan(t);
  y.lat = chi +
              parameters::ps_chi1 * sin(2.*chi) +
              parameters::ps_chi2 * sin(4.*chi) +
              parameters::ps_chi3 * sin(6.*chi) ;

  y.lat *= sgn*parameters::degrees_per_radian;
  y.lon = sgn*atan2(sgn*yp, sgn*xp)*parameters::degrees_per_radian - slon;

  return y;
}
template<class T> 
inline latpt psgrid<T>::locate(const ijpt &x) {
  latpt y;
  double eccen = sqrt(parameters::eccen2);
  double xp, yp, rho, t, chi;

  xp = x.i * dx + this->xorig;
  yp = x.j * dy + this->yorig;
  rho = sqrt(xp*xp+yp*yp);
  if (rho <= 0.1) {
      y.lat  = 90. * sgn;
      y.lon  = 0.0;
      return y;
  }

  if ( fabs(slat - 90) < 1.e-5 ) {
        t = rho*sqrt( pow(1.+eccen, 1.+eccen) * pow(1.-eccen, 1.-eccen) )
                        / 2. / rearth;
  }
  else {
        t = tnaught * rho / (rearth * cm);
  }

  chi = (M_PI_2) - 2.*atan(t);
  y.lat = chi +
              parameters::ps_chi1 * sin(2.*chi) +
              parameters::ps_chi2 * sin(4.*chi) +
              parameters::ps_chi3 * sin(6.*chi) ;

  y.lat *= sgn*parameters::degrees_per_radian;
  y.lon = sgn*atan2(sgn*yp, sgn*xp)*parameters::degrees_per_radian - slon;

  return y;
}
template<class T> 
inline fijpt& psgrid<T>::locate(const latpt &x) {
   double alat, along;
   double t, xp, yp, rho;
/* Test is for a sign difference, so can do both + vs - and -vs+ in this */
   global_fijpt.i = -1.;
   global_fijpt.j = -1.;
   if (x.lat*sgn <= 0. || fabs(x.lat) > 90.0 ) {
     return global_fijpt;
   }

   alat  = fabs(x.lat) / parameters::degrees_per_radian;
   along = x.lon       / parameters::degrees_per_radian;

   t = tan(M_PI_4 - alat/2.) /
     pow( (1.-sqrt(parameters::eccen2)*sin(alat))/(1.+sqrt(parameters::eccen2)*sin(alat)), 
            sqrt(parameters::eccen2) / 2.);

   if ( (90. - fabs(slat) ) < 1.E-3) {
     rho = 2.*rearth*t/ ps_ll;
   }
   else { 
     rho = rearth * cm*t/tnaught;
   }

   xp = rho*sgn*cos(sgn*(along+slon/parameters::degrees_per_radian));
   yp = rho*sgn*sin(sgn*(along+slon/parameters::degrees_per_radian));

   global_fijpt.i = ((xp - this->xorig)/dx);
   global_fijpt.j = ((yp - this->yorig)/dy);
   

// Additional checking -- if points are extrapolated off the grid,
//   again, return -1, -1
// 11 Jul 2007
//   if (global_fijpt.i > this->xpoints() - 0.5 ||
//       global_fijpt.j > this->ypoints() - 0.5 ||
//       global_fijpt.i < -0.5 || 
//       global_fijpt.j < -0.5    ) {
//     global_fijpt.i = -1.;
//     global_fijpt.j = -1.;
//   } 

  return global_fijpt;
}
///////////////////////////////////////////////////////////////////////




#endif
