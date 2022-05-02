// Class for managing eta grids -- as members of the metricgrid class
// Current members are 80, 32, 22, and 12 km eta grids.  The default
//   (class etagrid) is 12 km.
// Robert Grumbine 18 April 2002
// Modifications:
//
//  18 April 2003: Add 22, 12 km eta grids, make 12 km default
//  25 May 2004: Use parms.h for rads per degree
//   5 Apr 2007: thisification (this-> references)
//  15 May 2008: List all files which need to be included

#ifndef ETAH
  #define ETAH
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


template<class T> 
class etagrid : public metricgrid<T> {
  protected:
    double tlm0d, tph0d, wbd, sbd, dlmd, dphd; 
    double tph0, wb, sb, dlm, dph, tdlm, tdph, stph0, ctph0;
  
  public:
// Required operations
    etagrid(void); /* Construction */
    ~etagrid(void); /* Destruction */
    etagrid(etagrid<T> &); /* Copy */
    fijpt& locate(const latpt &);
    latpt locate(const fijpt &);
    latpt locate(const ijpt &);
// Optional:
    etagrid(int, int, double, double, double, double, double, double); 
};
template <class T>
etagrid<T>::etagrid() {
// Let the default be eta12
  this->nx = 606;
  this->ny = 1067;
  tlm0d = -111.0;
  tph0d =   50.0;
  wbd   =  -53.0;
  sbd   =  -40.0;
  dlmd  = 0.087603305;
  dphd  = 0.075046904;

  this->grid  = new T[this->nx*this->ny];
  #ifdef VERBOSE
    cout << "constructing etagrid\n"; cout.flush();
  #endif
  double rdpdg = parameters::degrees_per_radian;
  tph0 = tph0d * rdpdg;
  wb   = wbd * rdpdg;
  sb   = sbd * rdpdg;
  dlm  = dlmd * rdpdg;
  dph  = dphd * rdpdg;
  tdlm = dlm + dlm;
  tdph = dph + dph;
  stph0 = sin(tph0);
  ctph0 = cos(tph0);
}  

template<class T>
etagrid<T>::~etagrid(void) {
  #ifdef VERBOSE
    cout << "Entered etagrid destructor\n";
    cout.flush();
  #endif
}  

template<class T>
etagrid<T>::etagrid(int i1, int i2, double d1, double d2, double d3, double d4, double d5, double d6) {
  double rdpdg = parameters::degrees_per_radian;
  this->nx = i1;
  this->ny = i2;
  this->grid  = new T[this->nx*this->ny];
  tlm0d = d1;
  tph0d = d2;
  wbd   = d3;
  sbd   = d4;
  dlmd  = d5;
  dphd  = d6;
  tph0 = tph0d * rdpdg;
  wb   = wbd * rdpdg;
  sb   = sbd * rdpdg;
  dlm  = dlmd * rdpdg;
  dph  = dphd * rdpdg;
  tdlm = dlm + dlm;
  tdph = dph + dph;
  stph0 = sin(tph0);
  ctph0 = cos(tph0);
}

template<class T>
etagrid<T>::etagrid(etagrid<T> &x) {
  double rdpdg = parameters::degrees_per_radian;
  this->nx = x.xpoints();
  this->ny = x.ypoints();
  this->grid  = new T[this->nx*this->ny];
  tlm0d = x.tlm0d;
  tph0d = x.tph0d;
  wbd   = x.wbd;
  sbd   = x.sbd;
  dlmd  = x.dlmd;
  dphd  = x.dphd;
  tph0 = tph0d * rdpdg;
  wb   = wbd * rdpdg;
  sb   = sbd * rdpdg;
  dlm  = dlmd * rdpdg;
  dph  = dphd * rdpdg;
  tdlm = dlm + dlm;
  tdph = dph + dph;
  stph0 = sin(tph0);
  ctph0 = cos(tph0);
}

template<class T>
latpt etagrid<T>::locate(const fijpt &fx) {
  ijpt x;
  x.i = lrint(fx.i);
  x.j = lrint(fx.j);
  return this->locate(x);
}
template<class T>
latpt etagrid<T>::locate(const ijpt &x) {
  latpt y;
  double rdpdg = parameters::degrees_per_radian;
  double tphh, tlmh;
  double stph, ctph; 
  double sphh, clmh, facth;
  double glonh, glath;
  double hlon, hlat;
// Note that the j+1 is used since C++ is 0-based in arrays and Fortran,
//   the original, is 1-based
  tphh = sb - dph;
// Inside loop in original eta code
  tlmh = wb - tdlm + ( (x.j + 1 + 1) % 2 ) *dlm;
  tphh = tphh + (x.j+1)*dph;
  stph = sin(tphh);
  ctph = cos(tphh);

  tlmh = tlmh + tdlm*(x.i + 1);
  sphh = ctph0*stph + stph0*ctph*cos(tlmh);
  glath = asin(sphh);
  clmh  = ctph*cos(tlmh)/(cos(glath)*ctph0) - tan(glath)*tan(tph0);
  clmh = min(1.0, clmh);
  clmh = max(-1.0, clmh);
  facth = 1.; if (tlmh > 0.) facth = -1.;
  glonh = -tlm0d*rdpdg + facth*acos(clmh);
  hlat = glath/rdpdg;
  hlon = 360. - glonh/rdpdg;
  if (hlon >= 360.0) hlon -= 360.;

  y.lat = hlat;
  y.lon = hlon;
  
  return y;
}
template<class T>
fijpt& etagrid<T>::locate(const latpt &x) {
  cout << "trying to use locate(latpt), failure\n"; exit(1);
  return global_fijpt;
}

///////////////////////////////////////////////////////////////////
// Specialized examples:
template<class T>
class eta12 : public etagrid<T> {
  public: 
    eta12();
};
template<class T>
eta12<T>::eta12() {
  this->nx = 606;
  this->ny = 1067;
  this->tlm0d = -111.0;
  this->tph0d =   50.0;
  this->wbd   =  -53.0;
  this->sbd   =  -40.0;
  this->dlmd  = 0.087603305;
  this->dphd  = 0.075046904;

  this->grid  = new T[this->nx*this->ny];
  #ifdef VERBOSE
    cout << "constructing eta12\n"; cout.flush();
  #endif
  double rdpdg = parameters::degrees_per_radian;
  this->tph0 = this->tph0d * rdpdg;
  this->wb   = this->wbd * rdpdg;
  this->sb   = this->sbd * rdpdg;
  this->dlm  = this->dlmd * rdpdg;
  this->dph  = this->dphd * rdpdg;
  this->tdlm = this->dlm + this->dlm;
  this->tdph = this->dph + this->dph;
  this->stph0 = sin(this->tph0);
  this->ctph0 = cos(this->tph0);
}

template<class T>
class eta22 : public etagrid<T> {
  public: 
    eta22();
};
template<class T>
eta22<T>::eta22() {
  this->nx = 345;
  this->ny = 569;
  this->tlm0d = -111.0;
  this->tph0d =   50.0;
  this->wbd   =  -53.0;
  this->sbd   =  -40.0;
  this->dlmd  = 0.154069767;
  this->dphd  = 0.14084507;

  this->grid  = new T[this->nx*this->ny];
  #ifdef VERBOSE
    cout << "constructing eta22\n"; cout.flush();
  #endif
  double rdpdg = parameters::degrees_per_radian;
  this->tph0 = this->tph0d * rdpdg;
  this->wb   = this->wbd * rdpdg;
  this->sb   = this->sbd * rdpdg;
  this->dlm  = this->dlmd * rdpdg;
  this->dph  = this->dphd * rdpdg;
  this->tdlm = this->dlm + this->dlm;
  this->tdph = this->dph + this->dph;
  this->stph0 = sin(this->tph0);
  this->ctph0 = cos(this->tph0);
}

template<class T>
class eta32 : public etagrid<T> {
  public: 
    eta32();
};
template<class T>
eta32<T>::eta32() {
  this->nx = 237;
  this->ny = 387;
  this->tlm0d = -111.0;
  this->tph0d =   50.0;
  this->wbd   =  -53.0;
  this->sbd   =  -40.0;
  this->dlmd  = 0.224576271;
  this->dphd  = 0.207253886;

  double rdpdg = parameters::degrees_per_radian;
  this->grid  = new T[this->nx*this->ny];
  #ifdef VERBOSE
    cout << "constructing eta32\n"; cout.flush();
  #endif
  this->tph0 = this->tph0d * rdpdg;
  this->wb   = this->wbd * rdpdg;
  this->sb   = this->sbd * rdpdg;
  this->dlm  = this->dlmd * rdpdg;
  this->dph  = this->dphd * rdpdg;
  this->tdlm = this->dlm + this->dlm;
  this->tdph = this->dph + this->dph;
  this->stph0 = sin(this->tph0);
  this->ctph0 = cos(this->tph0);
}
template<class T>
class eta80 : public etagrid<T> {
  public: 
    eta80();
};
template<class T>
eta80<T>::eta80() {
  this->nx    = 92;
  this->ny    = 141;
  this->tlm0d = -111.0;
  this->tph0d =   50.0;
  this->wbd   =  -52.5;
  this->sbd   =  -37.692308;
  this->dlmd  = 0.576923076;
  this->dphd  = 0.538461538;

  this->grid  = new T[this->nx*this->ny];
  #ifdef VERBOSE
    cout << "constructing eta80\n"; cout.flush();
  #endif
  double rdpdg = parameters::degrees_per_radian;
  this->tph0 = this->tph0d * rdpdg;
  this->wb   = this->wbd * rdpdg;
  this->sb   = this->sbd * rdpdg;
  this->dlm  = this->dlmd * rdpdg;
  this->dph  = this->dphd * rdpdg;
  this->tdlm = this->dlm + this->dlm;
  this->tdph = this->dph + this->dph;
  this->stph0 = sin(this->tph0);
  this->ctph0 = cos(this->tph0);
}

  
#endif
