#include <cstdio>
#include <cmath>
#include <iostream>
using namespace std;

// Extract the LLGRIDS to their own file 7 May 2013

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

#ifndef LLGRIDH
#define LLGRIDH

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
//Declare things relevant to latitude-longitude grids
template<class T> 
class llgrid : public metricgrid<T> {
  protected:
  public:
   float dlat, dlon, firstlat, firstlon;
  public:
// Required functions
   llgrid(void); /* Construction creator */
   ~llgrid(void); /* Destructor */
   llgrid(int, int, float, float, float, float); /* construction creator */
   llgrid(llgrid<T> &);
   void set_gds();
   fijpt& locate(const latpt &);
   latpt locate(const ijpt &x) { latpt y;
      y.lat = firstlat + dlat * x.j; y.lon = firstlon + dlon * x.i;
      //while (y.lon < 0) { y.lon += 360.; }
      while (y.lon < firstlon) { y.lon += 360.; }
      return y; }
   latpt locate(const fijpt &);
// Optional, but good functions:
   llgrid(metricgrid<T> &);
   llgrid<T>& operator=(metricgrid<T> &);
   llgrid<T>& operator=(llgrid<T> &);
   //lat-lon-based subsetting and location 'in' testing added 23 Jul 2007
   bool in(double &, double &);
   bool in(ijpt &);
   bool in(fijpt &);

   dlatpt dlocate(const ijpt &x) { dlatpt y;
      y.lat = (double)firstlat + (double)dlat * (double)x.j; 
      y.lon = (double)firstlon + (double)dlon * (double)x.i;
      while (y.lon < firstlon) { y.lon += 360.; }
      return y; }

// Utility to migrate up to base class:
   double integrate();
   double integrate(T );
   double cellarea(ijpt &);
   void subset(llgrid<T> &, double, double, double, double); 
// Test
   bool iscyclicx() { this->cyclicx=(fabs(this->nx * dlon) >= 360.0); return this->cyclicx;}
};

template<class T>
bool llgrid<T>::in(fijpt &loc) {
  if (loc.i > -0.5 && loc.j > -0.5 &&
      loc.i < this->xpoints() - 0.5 &&
      loc.j < this->ypoints() - 0.5) {
    return true;
  }
  return false;
}
template<class T>
bool llgrid<T>::in(ijpt &loc) {
  if (loc.i > -0.5 && loc.j > -0.5 &&
      loc.i < this->xpoints() - 0.5 &&
      loc.j < this->ypoints() - 0.5) {
    return true;
  }
  return false;
}

template<class T>
bool llgrid<T>::in(double &lat, double &lon) {
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

template<class T>
void llgrid<T>::subset(llgrid<T> &field, double north, double south, 
                                         double east, double west) {
//Robert Grumbine 21 November 2003
  ijpt ill, iul, iur, ilr, loc, dloc;
  latpt ll, ul, ur, lr;

  field.dlat = dlat;
  field.dlon = dlon;

  ll.lat = south; ll.lon = west;
  lr.lat = south; lr.lon = east;
  ul.lat = north; ul.lon = west;
  ur.lat = north; ur.lon = east;
  ill = locate(ll);
  ilr = locate(lr);
  iul = locate(ul);
  iur = locate(ur);

// Since we're an llgrid, the i,j of bounding points is determined by
//   just the ll and ur, but ensure that we have the right sense for
//   the grids.
  if (ill.j > iur.j) {
    #ifdef VERBOSE
      cout << "flipping j\n"; cout.flush();
    #endif
    loc.j = iur.j;
    iur.j = ill.j;
    ill.j = loc.j;
  }
  if (ill.i > iur.i) {
    #ifdef VERBOSE
      cout << "flipping i\n"; cout.flush();
    #endif
    loc.i = iur.i;
    iur.i = ill.i;
    ill.i = loc.i;
  }
  if (ill.i < 0 || ill.j < 0) {
    fprintf(stderr, "negative points -- %d %d\n",ill.i, ill.j);
    fprintf(stderr, "field ll %d %d  ur %d %d\n", ill.i, ill.j, iur.i, iur.j);
    fflush(stderr);
    field.resize(0,0);
    return;
  }

  field.resize(iur.i - ill.i + 1, iur.j - ill.j + 1);
  field.firstlon = (locate(ill)).lon;
  field.firstlat = (locate(ill)).lat;
#ifdef VERBOSE
  printf("field %f %f  %f %f\n",field.dlat, field.dlon, field.firstlon, field.firstlat);
  fflush(stdout);
#endif
  field.cyclicx = (fabs(field.nx * field.dlon) >= 360.0);
  field.cyclicy = false;
 
  for (loc.j = ill.j; loc.j <= iur.j; loc.j++) {
  dloc.j = loc.j - ill.j;
  for (loc.i = ill.i; loc.i <= iur.i; loc.i++) {
     dloc.i = loc.i - ill.i;
     field[dloc] = this->grid[loc.i + loc.j*this->nx];
  }
  }

  return;
}
template<class T>
double llgrid<T>::cellarea(ijpt &loc) {
  latpt ll;
  double rad = parameters::radians_per_degree;
  double rearth = parameters::a;
// RG Note 21 November 2003 -- migrate to using params.h for rearth  
  ll = this->locate(loc);
  return cos(rad*ll.lat)*rearth*rearth*rad*rad*fabs(dlat*dlon);
}
  
template<class T>
double llgrid<T>::integrate() {
  double sum = 0.0;
  // This value corresponds to a spheroid double rearth = 6378.160e3;
  // The following is for a spherical:
  // fthetaa introduced 1/18/2012 in hopes of some speedup
  double theta, fthetaa;
  double rearth = parameters::a;
  double rad = parameters::radians_per_degree;
  ijpt x;
//  area = integral (grid * r^2*cos(lat) dtheta dlon)
  #ifdef VERBOSE
    cout <<"In integrate, nx, ny = " << this->nx << this->ny << "\n";
    cout.flush();
  #endif
  for (x.j = 0; x.j < this->ny; x.j++) {
    theta = (firstlat + x.j * dlat) * rad;
    fthetaa = fabs(cos(theta));
    for (x.i = 0 ; x.i < this->nx; x.i++) {
       sum += ((double) this->grid[x.i + x.j*this->nx ]) * fthetaa;
    }
  }
  #ifdef VERBOSE
    printf("In integrate, %f\n", (float) sum );
  #endif
  sum *= rearth * rearth * rad * rad * (fabs(dlat)*fabs(dlon) ) ;
  return  sum;
}
// Added 16 August 2007 -- skip flag values
template<class T>
double llgrid<T>::integrate(T flag) {
  double sum = 0.0;
  // This value corresponds to a spheroid double rearth = 6378.160e3;
  // The following is for a spherical:
  double theta;
  double rearth = parameters::a;
  double rad = parameters::radians_per_degree;
  ijpt x;
//  area = integral (grid * r^2*cos(lat) dtheta dlon)
  #ifdef VERBOSE
    cout <<"In integrate, nx, ny = " << this->nx << this->ny << "\n";
    cout.flush();
  #endif
  for (x.j = 0; x.j < this->ny; x.j++) {
    theta = (firstlat + x.j * dlat) * rad;
    for (x.i = 0 ; x.i < this->nx; x.i++) {
     if (this->grid[x.i + x.j*this->nx ] != flag) {
       sum += ((double) this->grid[x.i + x.j*this->nx ]) * fabs(cos(theta)) ;
     }
    }
  }
  #ifdef VERBOSE
    printf("In integrate, %f\n", (float) sum );
  #endif
  sum *= rearth * rearth * rad * rad * (fabs(dlat)*fabs(dlon) ) ;
  return  sum;
}


template<class T>
llgrid<T> & llgrid<T>::operator=(llgrid<T> &x) {
  int j;

// Note that the following tests ignore the possible response of
//  doing a llgrid::from(llgrid), for now
  if (this->nx != x.xpoints() || this->ny != x.ypoints() ) { 
    cout <<"Cannot equate the two grids \n";
    cout << " --  different sizes in llgrid=llgrid\n";
    cout.flush();
  }
  if (dlat != x.dlat || dlon != x.dlon ) {
    cout << "Cannot equate the two grids \n";
    cout << " --  different deltas in llgrid=llgrid\n";
  }
  if (firstlat != x.firstlat || firstlon != x.firstlon ) {
    cout << "Cannot equate the two grids \n";
    cout << " --  different corner points in llgrid=llgrid\n";
    cout.flush();
  }

  if (this->grid == (T *) NULL) { 
     cout << "Cannot equate grids in llgrid::=llgrid -- current is null\n";
    cout.flush();
  }

  #ifdef VERBOSE
    cout << "in llgrid::operator=(llgrid) \n";
    cout.flush();
  #endif
  for (j = 0; j < this->ny*this->nx; j++) {
      this->grid[j] = x.grid[j];
  }
  this->cyclicx = (fabs(this->nx * dlon) >= 360.0);
  this->cyclicy = false;
  return *this;
}

template<class T>
llgrid<T> & llgrid<T>::operator=(metricgrid<T> &x) {
  int j;
  if (this->nx != x.xpoints() || this->ny != x.ypoints() ) { 
    cout << "Cannot equate the two grids \n";
    cout << " --  different sizes in llgrid=metricgrid\n";
    cout.flush();
  }
  if (this->grid == (T *) NULL) { 
     cout << "Cannot equate grids in llgrid::=metricgrid\n";
    cout.flush();  
  }

  #ifdef VERBOSE
    cout << "in llgrid::operator=(metric) \n";
    cout.flush();
  #endif
  for (j = 0; j < this->ny*this->nx; j++) {
      this->grid[j] = x[j];
  }
  this->cyclicx = (fabs(this->nx * dlon) >= 360.0);
  this->cyclicy = false;
  return *this;

}
template<class T>
llgrid<T>::llgrid(llgrid<T> &x) {
  int j;
  this->nx = x.xpoints();
  this->ny = x.ypoints();
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in llgrid(llgrid)\n";
    cout.flush();}

  dlat = x.dlat;
  dlon = x.dlon;
  firstlat = x.firstlat;
  firstlon = x.firstlon;
  for (j = 0; j < this->ny*this->nx; j++) {
      this->grid[j] = x.grid[j];
  }
  this->cyclicx = (fabs(this->nx * dlon) >= 360.0);
  this->cyclicy = false;
}
template<class T>
llgrid<T>::llgrid(metricgrid<T> &x) {
  this->nx = x.xpoints();
  this->ny = x.ypoints();
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in llgrid(met)\n";
    cout.flush();}

  dlat = -1.0;
  dlon =  1.0;
  firstlat = 89.5;
  firstlon = 0.5;
  this->cyclicx = (fabs(this->nx * dlon) >= 360.0);
  this->cyclicy = false;
}
  
template<class T>
llgrid<T>::~llgrid(void) {
  #ifdef VERBOSE
    cout << "entered the llgrid destructor\n";
    cout.flush();
  #endif
  this->nx = 0;
  this->ny = 0;
  dlat = 0.0;
  dlon = 0.0;
  firstlat = 0.0;
  firstlon = 0.0;
}

template<class T>
fijpt& llgrid<T>::locate(const latpt &x) {
  float tlon, inlon;
  fijpt y;

//Longitude is the more difficult:

  if (firstlon < 0. ) {
    y.i = (x.lon - firstlon)/ dlon;
  }
  else {
    inlon = x.lon;
    tlon  = firstlon;
    while (inlon < 0.) { inlon += 360.; }
    while (tlon < 0.) { tlon += 360.; } 
    y.i = (inlon - tlon) / dlon;
  }

  y.j = (x.lat - firstlat) / dlat;
  global_fijpt = y;
  return global_fijpt;

}

template<class T>
latpt llgrid<T>::locate(const fijpt &fx) {
  latpt y;
  //ijpt x;
  //x.i = (int) (0.5 + fx.i);
  //x.j = (int) (0.5 + fx.j); 
  //y.lat = firstlat + dlat * x.j;
  //y.lon = firstlon + dlon * x.i;
// Change over to a true floating ij location.  Robert Grumbine 21 November 2003
  y.lat = firstlat + dlat * fx.j;
  y.lon = firstlon + dlon * fx.i;
  
  while (y.lon < firstlon) { y.lon += 360.; }
  return y;
}

template<class T> 
llgrid<T>::llgrid(void) {
  if (this->nx == 0 && this->ny == 0 ) {
    #ifdef VERBOSE
      cout << "Entered llgrid:llgrid(void), nx = ";
      cout << this->nx << "\n" ;
    cout.flush();
    #endif
    dlat = -1.0;
    dlon =  1.0;
    firstlat = 89.5;
    firstlon = 0.5;
  }
  this->cyclicx = (fabs(this->nx * dlon) >= 360.0);
  this->cyclicy = false;

}

template<class T> 
llgrid<T>::llgrid(int n1, int n2, float dllat, float dllon, 
                                  float dfirstlat, float dfirstlon) {
  this->nx = n1;
  this->ny = n2;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in llgrid(args)\n";
    cout.flush();}

  dlat = dllat;
  dlon = dllon;
  firstlat = dfirstlat; 
  firstlon = dfirstlon; 
  this->cyclicx = (fabs(this->nx * dlon) >= 360.0);
  this->cyclicy = false;
}

// The following is incorporated from lapl.C, formerly independant file
//   Also to be generalized to all metric grids.  future.
// Robert Grumbine 30 January 2006

template <class T>
void laplacean(llgrid<T> &x, llgrid<T> &y, T flag) ;

template <class T>
void gradsq(llgrid<T> &x, llgrid<T> &y, T flag);

template <class T>
void gradients(llgrid<T> &x, llgrid<T> &dx, llgrid<T> &dy, llgrid<T> &mag, T flag);
/////////////////////////////////////////////////////
//Compute divergence on a spherical earth
template<class T>
void divergence(llgrid<T> &dx, llgrid<T> &dy, llgrid<T> &div) {
  ijpt loc, ip, jp, im, jm;
  float cosphi, cosplus, cosminus, deltax, deltay;
  latpt ll;

  div.set((float) 0.0);
  deltax = div.dlon*parameters::a*parameters::radians_per_degree;
  deltay = abs(div.dlat)*parameters::a*parameters::radians_per_degree;

// d/dx terms:
  for (loc.j = 0; loc.j < div.ypoints(); loc.j++) {
    ip.j = loc.j;
    im.j = loc.j;
  for (loc.i = 1; loc.i < div.xpoints()-1; loc.i++) {
    ip.i = loc.i+1;
    im.i = loc.i-1;
    ll = div.locate(loc);
    cosphi = cos(parameters::radians_per_degree * ll.lat);
    div[loc] = (dx[ip]-dx[im])/cosphi/deltax/cosphi/2.; 
  }
  // edge cases:
  loc.i = 0;
  ip.i = 1;
  im.i = div.xpoints()-1;
    ll = div.locate(loc);
    cosphi = cos(parameters::radians_per_degree * ll.lat);
    div[loc] = (dx[ip]-dx[im])/cosphi/deltax/cosphi/2.; 
  loc.i = div.xpoints()-1;
  ip.i = 0;
  im.i = div.xpoints()-2;
    ll = div.locate(loc);
    cosphi = cos(parameters::radians_per_degree * ll.lat);
    div[loc] = (dx[ip]-dx[im])/cosphi/deltax/cosphi/2.; 

  } // end d/dx

// d/dy terms:
  for (loc.i = 0; loc.i < div.xpoints(); loc.i++) {
    jp.i = loc.i;
    jm.i = loc.i;
  for (loc.j = 1; loc.j < div.ypoints()-1; loc.j++) {
    jp.j = loc.j + 1;
    jm.j = loc.j - 1;
    ll = div.locate(loc);
    cosphi   = cos(parameters::radians_per_degree * ll.lat);
    cosplus  = cos(parameters::radians_per_degree * (ll.lat+div.dlat) );
    cosminus = cos(parameters::radians_per_degree * (ll.lat-div.dlat) );
    div[loc] += (dy[jp]*cosplus-dy[jm]*cosminus)/cosphi/deltay/2.;
  }
  // edge cases:
  loc.j = 0;
  loc.j = div.ypoints() - 1;
  }

  div /= parameters::a;
    
  return;
}


//Compute the gradients on a spherical earth 
template <class T>
void gradients(llgrid<T> &x, llgrid<T> &dx, llgrid<T> &dy, T flag) {
  llgrid<T> glat(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
  llgrid<T> glon(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
  parameters parms;
  ijpt loc, ip, jp, im, jm;

  glat.set(0.);
  glon.set(0.);

  for (loc.j = 1; loc.j < x.ypoints() - 1; loc.j++) {
  for (loc.i = 1; loc.i < x.xpoints() - 1; loc.i++) {
    jp = loc; jp.j += 1;
    jm = loc; jm.j -= 1;
    ip = loc; ip.i += 1;
    im = loc; im.i -= 1;

    if (x[jp] != flag && x[jm] != flag) {
      glat[loc] = x[jp] - x[jm];
    }
    else {
      glat[loc] = 0;
    }

    if (x[ip] != flag && x[im] != flag) {
      glon[loc] = (x[ip] - x[im]) / cos((x.firstlat+loc.j*x.dlat)*parameters::radians_per_degree);
    }
    else {
      glon[loc] = 0;
    }
  }
  loc.i = 0;
    jp = loc; jp.j += 1;
    jm = loc; jm.j -= 1;
    ip = loc; ip.i += 1;
    im = loc; im.i = x.xpoints() -1;

    if (x[jp] != flag && x[jm] != flag) {
      glat[loc] = x[jp] - x[jm];
    }
    else {
      glat[loc] = 0;
    }

    if (x[ip] != flag && x[im] != flag) {
      glon[loc] = (x[ip] - x[im]) / cos((x.firstlat+loc.j*x.dlat)*parameters::radians_per_degree);
    }
    else {
      glon[loc] = 0;
    }

  loc.i = x.xpoints() - 1;
    jp = loc; jp.j += 1;
    jm = loc; jm.j -= 1;
    ip = loc; ip.i = 0;
    im = loc; im.i -= 1;

    if (x[jp] != flag && x[jm] != flag) {
      glat[loc] = x[jp] - x[jm];
    }
    else {
      glat[loc] = 0;
    }

    if (x[ip] != flag && x[im] != flag) {
      glon[loc] = (x[ip] - x[im]) / cos((x.firstlat+loc.j*x.dlat)*parameters::radians_per_degree);
    }
    else {
      glon[loc] = 0;
    }

  }

  glat /= (parms.a*2.*fabs(x.dlat)*parameters::radians_per_degree);
  glon /= (parms.a*2.*fabs(x.dlon)*parameters::radians_per_degree);
  dx = glon;
  dy = glat;

  return;
}

//Compute the gradients and magnitude on a spherical earth 
template <class T>
void gradients(llgrid<T> &x, llgrid<T> &dx, llgrid<T> &dy, llgrid<T> &mag, T flag) {
  llgrid<T> glat(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
  llgrid<T> glon(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
  parameters parms;
  ijpt loc, ip, jp, im, jm;

  glat.set(0.);
  glon.set(0.);

  for (loc.j = 1; loc.j < x.ypoints() - 1; loc.j++) {
  for (loc.i = 1; loc.i < x.xpoints() - 1; loc.i++) {
    jp = loc; jp.j += 1;
    jm = loc; jm.j -= 1;
    ip = loc; ip.i += 1;
    im = loc; im.i -= 1;

    if (x[jp] != flag && x[jm] != flag) {
      glat[loc] = x[jp] - x[jm];
    }
    else {
      glat[loc] = 0;
    }

    if (x[ip] != flag && x[im] != flag) {
      glon[loc] = (x[ip] - x[im]) / cos((x.firstlat+loc.j*x.dlat)*parameters::radians_per_degree);
    }
    else {
      glon[loc] = 0;
    }
  }
  }
  glat /= (parms.a*2.*x.dlat*parameters::radians_per_degree);
  glon /= (parms.a*2.*x.dlon*parameters::radians_per_degree);

  dx = glon;
  dy = glat;
  for (loc.j = 1; loc.j < x.ypoints() - 1; loc.j++) {
  for (loc.i = 1; loc.i < x.xpoints() - 1; loc.i++) {
     mag[loc] = sqrt(glat[loc]*glat[loc] + glon[loc]*glon[loc]) ;
  }
  }

  return;
}


//Compute the gradient squared on a spherical earth 
template <class T>
void gradsq(llgrid<T> &x, llgrid<T> &y, T flag) {
  llgrid<T> glat(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
  llgrid<T> glon(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
  parameters parms;
  ijpt loc, ip, jp, im, jm;
  double fred;

  fred = M_PI/180.0;

  glat.set(0.);
  glon.set(0.);
     y.set(0.);

//future: Handle cyclicity in x, and pole-crossing in y
  for (loc.j = 1; loc.j < x.ypoints() - 1; loc.j++) {
  for (loc.i = 1; loc.i < x.xpoints() - 1; loc.i++) {
    jp = loc; jp.j += 1;
    jm = loc; jm.j -= 1;
    ip = loc; ip.i += 1;
    im = loc; im.i -= 1;

    if (x[jp] != flag && x[jm] != flag) {
      glat[loc] = x[jp] - x[jm];
    }
    else {
      glat[loc] = 0;
    }

    if (x[ip] != flag && x[im] != flag) {
      glon[loc] = (x[ip] - x[im]) / cos((x.firstlat+loc.j*x.dlat)*fred);
    }
    else {
      glon[loc] = 0;
    }
  }
  }
  glat /= (parms.a*2.*x.dlat*fred);
  glon /= (parms.a*2.*x.dlon*fred);

  for (loc.j = 1; loc.j < x.ypoints() - 1; loc.j++) {
  for (loc.i = 1; loc.i < x.xpoints() - 1; loc.i++) {
     y[loc] = glat[loc]*glat[loc] + glon[loc]*glon[loc] ;
  }
  }

  return;
}



//Compute the laplacean on a spherical earth 
template <class T>
void laplacean(llgrid<T> &x, llgrid<T> &y, T flag) {
  llgrid<T> glat(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
  llgrid<T> glon(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
  parameters parms;
  ijpt loc, ip, jp, im, jm;
  double fred, first, second;

  fred = M_PI/180.0;

  glat.set(0.);
  glon.set(0.);

  for (loc.j = 1; loc.j < x.ypoints() - 1; loc.j++) {
  for (loc.i = 1; loc.i < x.xpoints() - 1; loc.i++) {
    jp = loc; jp.j += 1;
    jm = loc; jm.j -= 1;
    ip = loc; ip.i += 1;
    im = loc; im.i -= 1;

    if (x[jp] != flag && x[jm] != flag && x[loc] != flag) {
      first = (x[jp] - x[jm])/(fred*x.dlat*2.);
      second = (x[jp] - 2.*x[loc] + x[jm])/(fred*fred*x.dlat*x.dlat);
      glat[loc] = second - tan((x.firstlat+loc.j*x.dlat)*fred) * first;
    }
    else {
      glat[loc] = 0;
    }

    if (x[ip] != flag && x[im] != flag && x[loc] != flag) {
      glon[loc] = (x[ip] - 2.*x[loc] + x[im])
          / cos((x.firstlat+loc.j*x.dlat)*fred)
          / cos((x.firstlat+loc.j*x.dlat)*fred)  ;  
    } 
    else {
      glon[loc] = 0;
    }
  }
  }
  glat /= (parms.a*parms.a);
  glon /= (parms.a*parms.a*(x.dlon*fred)*(x.dlon*fred) );

  y = glat;
  y += glon;

  return;
}



////////////////////////////////////////////////////////////////////
//Create a class for a rotated lat-long grid
template<class T>
class rotllgrid : public llgrid<T> {
  public:
   float rotation;
   rotllgrid(float ); /* Construction creator */
   fijpt& locate(const latpt &); /* Must override the usual llgrid locates due to */
   latpt locate(const ijpt &);  /*   rotation */
   latpt locate(const fijpt &);
};
template<class T>
rotllgrid<T>::rotllgrid(float rot) {
  rotation = rot*rpdg;
  this->nx = 362;
  this->ny = 153;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new in rotllgrid()\n";
    cout.flush();}

  this->dlat =  1.0;
  this->dlon =  1.0;
  this->firstlat = -80.5;
  this->firstlon =  -0.5;
  return;
}
template<class T>
inline fijpt& rotllgrid<T>::locate(const latpt &ptin) {
// The latpt turned in is assumed referenced to the standard fixed lat-long
//   coordinates.  The firstlat, firstlon in the object are in local coords.
  fijpt ptout;
  latpt radpt;
  float xprime, yprime, zprime;
  float lonprime, latprime;

  radpt.lat = ptin.lat*rpdg;
  radpt.lon = ptin.lon*rpdg;
  xprime = cos(radpt.lat)*sin(radpt.lon);
  yprime = cos(rotation)*cos(radpt.lat)*cos(radpt.lon) + 
           sin(rotation)*sin(radpt.lat);
  zprime = cos(rotation)*sin(radpt.lat) 
         - sin(rotation)*cos(radpt.lat)*cos(radpt.lon);
 
  lonprime = atan2(xprime, yprime);
  latprime = atan2(zprime, sqrt(xprime*xprime+yprime*yprime) ); 
  
  ptout.i = (lonprime/rpdg - this->firstlon) / this->dlon;
  ptout.j = (latprime/rpdg - this->firstlat) / this->dlat;
  if (ptout.i < 0) ptout.i += this->nx;
  global_fijpt = ptout;
  return global_fijpt;

}

template<class T>
inline latpt rotllgrid<T>::locate(const fijpt &ptin) {
// The latpoint returned is in the coordinates of the standard fixed 
//   coordinate.  
   latpt y;
   return y;
}
template<class T>
inline latpt rotllgrid<T>::locate(const ijpt &ptin) {
// The latpoint returned is in the coordinates of the standard fixed 
//   coordinate.  
   latpt y;
   return y;
}


//GRIB
// Set up the grid-specific GDS elements, start with llgrids:
template <class T>
void llgrid<T>::set_gds() {
  latpt first;
  ijpt  firstij;

  this->gds.gds[2] = 0; //  IDRT = 0 for regular lat-long grids.
  this->gds.gds[3] = this->nx; //  NX
  this->gds.gds[4] = this->ny; //  NY

  firstij.i = 0;
  firstij.j = 0;
  first = this->locate(firstij);
  firstij.i = (int) (first.lon * 1000. + 0.5);
  firstij.j = (int) (first.lat * 1000. + 0.5);
  this->gds.gds[5] = firstij.j; //  LAT1
  this->gds.gds[6] = firstij.i; //  LON1

  firstij.i = this->nx - 1;
  firstij.j = this->ny - 1;
  first = this->locate(firstij);
  firstij.i = (int) (first.lon * 1000. + 0.5);
  firstij.j = (int) (first.lat * 1000. + 0.5);
  this->gds.gds[8] = firstij.j; //  Last point - lat, lon
  this->gds.gds[9] = firstij.i; //  

  this->gds.gds[7] = 128; //  IRESFL - see grib table 7

  this->gds.gds[12] = 0;
  if (dlat < 0.) {
    this->gds.gds[10] = -(int) (dlat * 1000. - 0.5); //  IGDS11; // ""
    this->gds.gds[12] += 64;
  }
  else {
    this->gds.gds[10] = (int) (dlat * 1000. + 0.5); //  IGDS11; // ""
  }

  if (dlon < 0.) {
    this->gds.gds[11] = -(int) (dlon * 1000. - 0.5); //  IGDS12; // ""
    this->gds.gds[12] += 128;
  }
  else {
    this->gds.gds[11] = (int) (dlon * 1000. + 0.5); //  IGDS12; // ""
  }
}

///////////////////////////////////////////////////////////////////////
  
#endif
