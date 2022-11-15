//NCEP grids -- these require that the metric class already
//  be defined and any required projection work is already done
//  over there.  'NCEP grid' is a loose concept, meaning merely
//  that the grid is in use somewhere within NCEP.  It may not
//  be official in any of the senses that term has, and it may
//  not have originated here.
//Robert Grumbine 11 August 1997.
//  Modifications:
//    3 December 2002 Change return of fijpt to use global_fijpt as
//                    return of local copy of class variable is verboten.
//   30 July     2009 'thisification' (this-> references)
//
//Include file for a class to manage the peculiar cofs grid
//Begun in dark past, oldest extant file 24 Feb 2000
//
//Add SGI friend declarations 14 Apr 2000
// 20 November 2000  Robert Grumbine - move acfsread into the include itself
//April 2001   -- rotate, derotate winds to/from native grid and geographic
//Apr 26  2002 -- Remove unused variables, go to bool for logical function(s)
//May 2004     -- move to mvector from user-vector
//ca. 2004     -- move to iostream from iostream.h, add copy ctor
//3 June 2005  -- Adapt for IBM and SGI (again) -- separate IBM and SGI

// This is a legacy file, COFS was retired in ~2006.

#include <cstdio>
#include <cmath>

#include <iostream>
using namespace std;

#ifndef POINTSH
  #include "points.h"
#endif
#ifndef PARAMETERS
  #include "params.h"
#endif
#ifndef MVECTORH
  #include "mvector.h"
#endif
#ifndef GRIBH
  #include "grib.h"
#endif
#ifndef GRID3H
  #include "grid3.h"
#endif
#ifndef METRICH
  #include "metric.h"
#endif

#ifndef COFSGRIDS
#define COFSGRIDS

///////////////////////////////////

// Function translated to C and incorporated to the include library
// 20 November 2000  Robert Grumbine
void acfsread(char *fname, float *z, float *zz, float *alon, float *alat, float *h);

void acfsread(char *fname, float *z, float *zz, float *alon, float *alat, float *h) {
  FILE *fin;
  const int kb =19, jm =101, im =181;
  float f1, f2, f3;
  int i, j, k;

  #ifdef VERBOSE
    cout << "Entered acfsread\n"; cout.flush();
  #endif

  fin = fopen(fname, "r");
  for (i = 0; i < kb; i++) {
    fscanf(fin, "%d %f\n",&k, &f1);
    z[k-1] = f1;
  }
  for (i = 0; i < kb; i++) {
    fscanf(fin, "%d %f\n",&k, &f1);
    zz[k-1] = f1;
  }

  for (k = 0; k < jm*im; k++) {
    fscanf(fin, "%d %d %f %f %f\n",&i, &j, &f1, &f2, &f3);
    #ifdef VERBOSE2
      printf("%d %d %f %f %f\n",i, j, f1, f2, f3); fflush(stdout);
    #endif
    i -= 1; j -= 1;
    alon[j*im+i] = f1;
    alat[j*im+i] = f2;
    h   [j*im+i] = f3;
  }

  fclose(fin);

  #ifdef VERBOSE
    cout << "Leaving acfsread\n"; cout.flush();
  #endif
  return;
}

////////// Define the regular cfs grid here, even though it is a
//////////   llgrid
template<class T>
class cfsreg : public llgrid<T> {
   public:
     cfsreg(void) ;
     ~cfsreg(void) {};
     cfsreg(cfsreg<T> &);
     cfsreg(const cfsreg<T> &);            // Added copy ctor
     cfsreg<T> & operator=(cfsreg<T> &);
};
template<class T>
cfsreg<T>::cfsreg(void) {
  this->nx   = 332;
  this->ny   = 210;
  this->dlat = 0.1;
  this->dlon = 0.1;
  this->firstlon =  -83.05;
  this->firstlat =   26.35;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {
    cout << "failed to new a cfsreg\n"; 
    cout.flush();
  }

  this->pds.set_gridid(251); // ON 388 grid identifier

  return;
}
template<class T>
cfsreg<T>::cfsreg(const cfsreg<T> &y) {                 // Added copy ctor
  int i;
  this->nx = y.nx; this->ny = y.ny;
  if (this == &y) {
    cout <<  "this and y are the same, returning\n";
    cout.flush();
    return;
  }
  this->grid = new T[y.nx*y.ny] ;
  for (i = 0; i < y.nx*y.ny; i++) {
    this->grid[i] = y.grid[i];
  }
}
template<class T>
cfsreg<T>::cfsreg(cfsreg<T> &x) {
  int i;
  this->nx = x.nx; this->ny = x.ny;
  this->grid = new T[this->nx*this->ny];
  for (i = 0; i < this->ny*this->nx; i++) {
     this->grid[i] = x[i];
  }
}
template<class T>
cfsreg<T> &cfsreg<T>::operator=(cfsreg<T> &x) {
  int i;
  this->dlat = x.dlat;
  this->dlon = x.dlon;
  this->firstlat = x.firstlat;
  this->firstlon = x.firstlon;
  this->cyclicx  = x.cyclicx;
  this->cyclicy  = x.cyclicy;
  for (i = 0; i < this->ny*this->nx; i++) {
     this->grid[i] = x[i];
  }
  return *this;
}

//////////////////////////////////////////////////////////////////
//       CFS Native definitions
// Note that a cfsgrid cannot be a metricgrid since the metricgrids
//  require a locate function, which can't be inlined at this point.
// 23 February 2000: Not true.  See cfsnative -- use of static for the
//   metric components
template<class T>
class cfsgrid : public grid2<T> {
  public:
   cfsgrid(void);   /* Construction creator */
   cfsgrid(const cfsgrid<T> &);                 // Added copy ctor
   ~cfsgrid(void) ; /* Destructor */
};
template<class T>
cfsgrid<T>::~cfsgrid(void) {
  this->nx = 0;
  this->ny = 0;
}

template<class T>
cfsgrid<T>::cfsgrid(void) {
  this->nx = 181;
  this->ny = 101;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "failed to new in cfsgrid()\n";
    cout.flush();
  }
  return;
}
template<class T>
cfsgrid<T>::cfsgrid(const cfsgrid<T> &y) {                 // Added copy ctor
  int i;
  this->nx = y.nx; this->ny = y.ny;
  if (this == &y) {
    cout <<  "this and y are the same, returning\n";
    cout.flush(); 
    return;
  }
  this->grid = new T[y.nx*y.ny] ;
  for (i = 0; i < y.nx*y.ny; i++) {
    this->grid[i] = y.grid[i];
  }

}
  

///////////////////////////////////
////// cfslevel is deprecated.  Do not use in new coding, use cfsnative
////// instead.  Robert Grumbine 23 February 2000
template<class T>
class cfslevel : public metricgrid<float> {
  public:
    cfsgrid<float> alat, alon, depth, t;
    cfsreg<float> fi, fj;
    mvector<float> z, zz, dz, dzz;
    int nz;
// Constructor
    cfslevel(void);
// Public utility:
    bool land(ijpt &t) { return ( depth[t] <= 10.0 ); } ;
    bool land(ijpt &t,float toler) { return ( depth[t] <= toler ); } ;
    int xpoints() { return depth.xpoints(); };
    int ypoints() { return depth.ypoints(); };
    void operator=(cfsgrid<T> &);

    fijpt& locate(const latpt &);
    latpt locate(const ijpt &);
    latpt locate(const fijpt &);

    void from(cfslevel<T> &, cfsgrid<T> &, T, T, metricgrid<T>  &);
    void from(metricgrid<T> &, metricgrid<T> &, T, T, cfsgrid<T> & );
};

template<class T>
void cfslevel<T>::operator=(cfsgrid<T> &x ){
  int j;
  for (j = 0; j < ny*nx; j++) {
    grid[j] = x[j];
  }
}

template<class T>
cfslevel<T>::cfslevel(void) {
// Get hold of the metric fields
    FILE *fout;
    int nz = 19;
    float *ztmp, *zztmp, *alontmp, *alattmp, *deptmp;

    ztmp = new float[nz];
    zztmp = new float[nz];
    alontmp = new float[alat.xpoints()*alat.ypoints()];
    alattmp = new float[alat.xpoints()*alat.ypoints()];
    deptmp  = new float[alat.xpoints()*alat.ypoints()];

    z.resize(nz);
    zz.resize(nz);
    dz.resize(nz);
    dzz.resize(nz);
    #ifdef ASCII
      acfsread("inhts.cfs", ztmp, zztmp, alontmp, alattmp, deptmp);
      fout = fopen("cfs.bin.full","w");
      if (fout == (FILE *) NULL ) { cout << "Failed to open cfs.bin.full\n"; 
    cout.flush();
  }
      for (j = 0; j < nz; j++) {
        z[j] = ztmp[j];
        zz[j] = zztmp[j];
      }
      for (xij.j = 0; xij.j < alat.ypoints() ; xij.j++) {
      for (xij.i = 0; xij.i < alat.xpoints() ; xij.i++) {
        alon[xij] = alontmp[xij.i + xij.j*alat.xpoints()];
        alat[xij] = alattmp[xij.i + xij.j*alat.xpoints()];
        depth[xij] = deptmp[xij.i + xij.j*alat.xpoints()];
      }
      }

      z.binout(fout);
      zz.binout(fout);
      alon.binout(fout);
      alat.binout(fout);
      depth.binout(fout);
      fclose(fout);
    #else
      fout = fopen("cfs.bin.full","r");
      if (fout == (FILE *) NULL ) { cout << "Failed to open cfs.bin.full\n"; 
    cout.flush();
  }
      z.binin(fout);
      zz.binin(fout);
      alon.binin(fout);
      alat.binin(fout);
      depth.binin(fout);
      fclose(fout);
    #endif

    this->nx = alon.xpoints();
    this->ny = alon.ypoints() ;
    if (this->grid != (T *) NULL) { delete []grid; }
    this->grid = new T[nx*ny];
    if (this->grid == (T *) NULL) { cout << "Failed to new cfslevel.grid\n";
                              cout.flush();
                      }
    fout = fopen("cfs.binj","r");
    if (fout == (FILE *) NULL ) { 
           cout << "Failed to open cfs.binj, setting fj to -99\n"; 
           cout.flush();
           fj.set(-99.0); }
    else {
      fj.binin(fout);
      fclose(fout);
    }
    fout = fopen("cfs.bini","r");
    if (fout == (FILE *) NULL ) { cout << "Failed to open cfs.bini, setting fi to -99\n";
           cout.flush();
           fi.set(-99.0); }
    else {
      fi.binin(fout);
      fclose(fout);
    }

    return;
}

template<class T>
fijpt& cfslevel<T>::locate(const latpt &x) {
// Given a lat-long pt, locate the corresponding i, j in the
//  CFS model.  Currently a dummy function.
  fijpt y, fx;
  ijpt xij;

  fx = fi.locate(x); //ij in the cfs(reg) grid corresponding to lat-lon
  if (fi.in(fx)) {
    // we found a point in the domain of the CFS Reg grid, therefore
    // have a chance of being in the CFS native grid
    xij.i = (int) (fx.i + 0.5);
    xij.j = (int) (fx.j + 0.5);
    y.i = fi[xij];
    y.j = fj[xij];
    if (this->in(y) ) {
     // We've landed inside the CFS native grid, good location, return it
    }
    else {
     // bad location, guarantee return of -1, -1
     y.i = -2.0;
     y.j = -2.0;
    }
  }
  else {
    // not even in the CFS regular grid
    y.i = -2.0;
    y.j = -2.0;
  }
  global_fijpt = y;
  return global_fijpt;

}

template<class T>
latpt cfslevel<T>::locate(const ijpt &x) {
// Given an i,j pt, locate the corresponding latitude/longitude in CFS
  latpt y;
  y.lat = alat[x];
  y.lon = alon[x];
  return y;
}
template<class T>
latpt cfslevel<T>::locate(const fijpt &x) {
// Given floating i,j pt, locate the corresponding latitude/longitude in CFS
  ijpt xij;
  fijpt del;
  latpt res;
  float v1, v2, v3, v4;
  latpt yll, ylr, yur, yul;

  if (x.i < 0 || x.i > this->nx - 1 || x.j < 0 || x.j > ny - 1) {
    res.lat = -99.9;
    res.lon = -999.9;
    return res;
  }
  xij.i = (int) x.i;
  xij.j = (int) x.j;
  yll.lat = alat[xij];
  yll.lon = alon[xij];
  ylr.lat = alat[xij.j*nx+xij.i + 1];
  ylr.lon = alon[xij.j*nx+xij.i + 1];
  yur.lat = alat[(xij.j+1)*nx+xij.i + 1];
  yur.lon = alon[(xij.j+1)*nx+xij.i + 1];
  yul.lat = alat[(xij.j+1)*nx+xij.i ];
  yul.lon = alon[(xij.j+1)*nx+xij.i ];
  del.i = 1. - (x.i - xij.i);
  del.j = 1. - (x.j - xij.j);
//Apply Chalikov weighting interpolation, RG notes of 5/21/97.
  v1 = del.i * del.j ;
  v2 = del.i * (1. - del.j);
  v3 = (1. - del.i) * (1. - del.j);
  v4 = (1. - del.i) * del.j;

  res.lat = yll.lat * v1 + yul.lat*v2 + yur.lat * v3 + ylr.lat * v4;
  res.lon = yll.lon * v1 + yul.lon*v2 + yur.lon * v3 + ylr.lon * v4;

  return res;
}
template<class T>
void cfslevel<T>::from(cfslevel<T> &llt, cfsgrid<T> &mask, T flagval, T nonval,
                     metricgrid<T> &x ) {
  #include "fromcfs.h"
}
template<class T>
void cfslevel<T>::from(metricgrid<T> &llt, metricgrid<T> &mask, T flagval,
                         T nonval, cfsgrid<T> & x) {
#include "from.h"
}


/////////// Class for cfs native grids.  Use this rather than
/////////// cfslevel.
template<class T>
class cfsnative : public metricgrid<T> {
  public:
    // These data are shared by all cfsnative grids, used for doing the
    // interpolation.  We absolutely to not want people messing with these.
    static cfsgrid<float> alon, alat, depth;
    static cfsreg<float> fi, fj;
    static mvector<float> z, zz, dz, dzz;
    static int nz;
    #if ( SGI )
      friend int cfsinit(T);
    #elif IBM
      friend int cfsinit(float );
    #elif LINUX
      friend int cfsinit(float );
    #else
      friend int cfsinit<T>();
    #endif
  public:
    cfsnative(void);
    ~cfsnative(void);
    latpt locate(const ijpt &);
    latpt locate(const fijpt &);
    fijpt& locate(const latpt &);
    bool land(ijpt &t) { return ( depth[t] <= 10.0 ); } ;
    bool land(ijpt &t, float toler) { 
                    return ( depth[t] <= toler ); } ;
};

#if ( SGI )
  template <class T>
  int cfsinit(T dummy) {
#elif ( IBM )
  template <class T>
  int cfsinit(T dummy) {
#else
  template <class T>
  int cfsinit() {
#endif
// Get hold of the metric fields
    FILE *fout;
    int nz = 19, nx = 181, ny = 101;
    float *ztmp, *zztmp, *alontmp, *alattmp, *deptmp;

    ztmp = new float[nz];
    zztmp = new float[nz];
    alontmp = new float[nx*ny];
    alattmp = new float[nx*ny];
    deptmp  = new float[nx*ny];

    cfsnative<T>::z.resize(nz);
    cfsnative<T>::zz.resize(nz);
    cfsnative<T>::dz.resize(nz);
    cfsnative<T>::dzz.resize(nz);

    #ifdef ASCII
      acfsread("inhts.cfs", ztmp, zztmp, alontmp, alattmp, deptmp);

      fout = fopen("cfs.bin.full","w");
      if (fout == (FILE *) NULL ) { cout << "Failed to open cfs.bin.full\n";
                                    cout.flush(); }

      for (j = 0; j < nz; j++) {
        cfsnative<T>::z[j] = ztmp[j];
        cfsnative<T>::zz[j] = zztmp[j];
      }
      for (xij.j = 0; xij.j < ny ; xij.j++) {
      for (xij.i = 0; xij.i < nx ; xij.i++) {
        cfsnative<T>::alon[xij] = alontmp[xij.i + xij.j*nx ];
        cfsnative<T>::alat[xij] = alattmp[xij.i + xij.j*nx];
        cfsnative<T>::depth[xij] = deptmp[xij.i + xij.j*nx];
      }
      }

      cfsnative<T>::z.binout(fout);
      cfsnative<T>::zz.binout(fout);
      cfsnative<T>::alon.binout(fout);
      cfsnative<T>::alat.binout(fout);
      cfsnative<T>::depth.binout(fout);
      fclose(fout);
    #else
      fout = fopen("cfs.bin.full","r");
      if (fout == (FILE *) NULL ) { cout << "Failed to open cfs.bin.full\n";
                                    cout.flush(); }

      cfsnative<T>::z.binin(fout);
      cfsnative<T>::zz.binin(fout);
      cfsnative<T>::alon.binin(fout);
      cfsnative<T>::alat.binin(fout);
      cfsnative<T>::depth.binin(fout);
      fclose(fout);
    #endif

    // Note that we have to do a lot of specifying for the cfsreg grids,
    //  but didn't have that problem for cfsgrids -- cfsreg is an llgrid,
    //  while cfsgrid is a grid2
      cfsnative<T>::fi.resize(332, 210);
      cfsnative<T>::fi.dlat = 0.1;
      cfsnative<T>::fi.dlon = 0.1;
      cfsnative<T>::fi.firstlon = -83.05;
      cfsnative<T>::fi.firstlat = 26.35;
      cfsnative<T>::fj.resize(332, 210);
      cfsnative<T>::fj.dlat = 0.1;
      cfsnative<T>::fj.dlon = 0.1;
      cfsnative<T>::fj.firstlon = -83.05;
      cfsnative<T>::fj.firstlat = 26.35;

    fout = fopen("cfs.binj","r");
    if (fout == (FILE *) NULL ) { 
           cout << "Failed to open cfs.binj, setting fj to -99\n";
           cout.flush();
           cfsnative<T>::fj.set(-99.0); }
    else {
      cfsnative<T>::fj.binin(fout);
      fclose(fout);
    }
    fout = fopen("cfs.bini","r");
    if (fout == (FILE *) NULL ) { cout << "Failed to open cfs.bini, setting fi to -99\n";
           cfsnative<T>::fi.set(-99.0); }
    else {
      cfsnative<T>::fi.binin(fout);
      fclose(fout);
    }


    return nz;
}

cfsreg<float> nullcfsreg() {
  cfsreg<float> x;
  return x;
}
cfsgrid<float> nullcfsgrid() {
  cfsgrid<float> x;
  return x;
}
template<> mvector<float> cfsnative<float>::z       = nullvec();
template<> cfsgrid<float> cfsnative<float>::alon   = nullcfsgrid();
template<> cfsreg<float> cfsnative<float>::fi      = nullcfsreg();
template<> cfsreg<float> cfsnative<float>::fj      = cfsnative<float>::fi;
template<> cfsgrid<float> cfsnative<float>::alat   = cfsnative<float>::alon;
template<> cfsgrid<float> cfsnative<float>::depth  = cfsnative<float>::alon;
template<> mvector<float> cfsnative<float>::zz      = cfsnative<float>::z;
template<> mvector<float> cfsnative<float>::dz      = cfsnative<float>::z;
template<> mvector<float> cfsnative<float>::dzz     = cfsnative<float>::z;

// The following does the real initialization
#if ( SGI )
  template<> int cfsnative<float>::nz = cfsinit((float)5.0);
#elif ( IBM )
  template<> int cfsnative<float>::nz = cfsinit((float)5.0);
#else
  template<> int cfsnative<float>::nz = cfsinit<float>();
#endif


template<class T>
latpt cfsnative<T>::locate(const ijpt &x) {
// Given an i,j pt, locate the corresponding latitude/longitude in CFS
  latpt y;
  y.lat = alat[x];
  y.lon = alon[x];
  return y;
}
template<class T>
latpt cfsnative<T>::locate(const fijpt &x) {
// Given floating i,j pt, locate the corresponding latitude/longitude in CFS
  ijpt xij;
  fijpt del;
  latpt res;
  float v1, v2, v3, v4;
  latpt yll, ylr, yur, yul;

  if (x.i < 0 || x.i > this->nx - 1 || x.j < 0 || x.j > this->ny - 1) {
    res.lat = -99.9;
    res.lon = -999.9;
    return res;
  }
  xij.i = (int) x.i;
  xij.j = (int) x.j;
  yll.lat = alat[xij];
  yll.lon = alon[xij];
  ylr.lat = alat[xij.j*this->nx+xij.i + 1];
  ylr.lon = alon[xij.j*this->nx+xij.i + 1];
  yur.lat = alat[(xij.j+1)*this->nx+xij.i + 1];
  yur.lon = alon[(xij.j+1)*this->nx+xij.i + 1];
  yul.lat = alat[(xij.j+1)*this->nx+xij.i ];
  yul.lon = alon[(xij.j+1)*this->nx+xij.i ];
  del.i = 1. - (x.i - xij.i);
  del.j = 1. - (x.j - xij.j);
//Apply Chalikov weighting interpolation, RG notes of 5/21/97.
  v1 = del.i * del.j ;
  v2 = del.i * (1. - del.j);
  v3 = (1. - del.i) * (1. - del.j);
  v4 = (1. - del.i) * del.j;

  res.lat = yll.lat * v1 + yul.lat*v2 + yur.lat * v3 + ylr.lat * v4;
  res.lon = yll.lon * v1 + yul.lon*v2 + yur.lon * v3 + ylr.lon * v4;

  return res;
}

template<class T>
fijpt& cfsnative<T>::locate(const latpt &x) {
// Given a lat-long pt, locate the corresponding i, j in the
//  CFS model.
  fijpt y, fx;
  ijpt xij;

  fx = fi.locate(x); //ij in the cfs(reg) grid corresponding to lat-lon

  if (fi.in(fx)) {
    // we found a point in the domain of the CFS Reg grid, therefore
    // have a chance of being in the CFS native grid
    xij.i = (int) (fx.i + 0.5);
    xij.j = (int) (fx.j + 0.5);
    y.i = fi[xij];
    y.j = fj[xij];
    if (this->in(y) ) {
     // We've landed inside the CFS native grid, good location, return it
    }
    else {
     // bad location, guarantee return of -1, -1
     y.i = -2.0;
     y.j = -2.0;
    }
  }
  else {
    // not even in the CFS regular grid
    y.i = -2.0;
    y.j = -2.0;
  }
  global_fijpt = y;
  return global_fijpt;

}


template<class T>
cfsnative<T>::cfsnative(void) {
  this->nx = 181;
  this->ny = 101;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "failed to new in cfsgrid()\n";
      cout.flush();
  }
  return;
}
template<class T>
cfsnative<T>::~cfsnative(void) {
  this->nx = 0;
  this->ny = 0;
}

////////////// END OF CFSNATIVE

///////////////////////////////////

////////////////////////////////////////////////
//Now declare the 3d version of cofs grid
template<class T>
class cfsgrid3 : public grid3<T> {
  public :
    cfsgrid3(void);
    cfsgrid3(int );
    cfsgrid3(cfsgrid3<T> &);
    cfsgrid3(grid3<T> &);
    cfsgrid3<T> & operator=(grid3<T> &);
    cfsgrid3<T> & operator=(cfsgrid3<T> &);
};

template <class T>
cfsgrid3<T> & cfsgrid3<T>::operator=(cfsgrid3<T> &x) {
  cfsgrid<T> xcfs;
  int k;
  this->nz = x.nz;
  this->nx = xcfs.xpoints() ;
  this->ny = xcfs.ypoints() ;
  this->grid = new T[this->nx*this->ny*this->nz];
  if (this->grid == (T *) NULL) { cout << "failed to new in cfsgrid3()\n";
      cout.flush();
  }
  for (k = 0; k < this->nz*this->ny*this->nx; k++) {
     this->grid[k] = (T) x[k] ;
  }
  return *this;
}
template <class T>
cfsgrid3<T> & cfsgrid3<T>::operator=(grid3<T> &x) {
  cfsgrid<T> xcfs;
  int i, j, k;
  this->nz = x.nz;
  this->nx = xcfs.xpoints() ;
  this->ny = xcfs.ypoints() ;
  this->grid = new T[this->nx*this->ny*this->nz];
  if (this->grid == (T *) NULL) { cout << "failed to new in cfsgrid3()\n";
      cout.flush();
  }
  if (x.xpoints() != xcfs.xpoints() || x.ypoints() != xcfs.ypoints() ) {
    cout << "Cannot copy over grid3 to cfsgrid3.  Will fill with zeroes\n";
      cout.flush();
    for (k = 0; k < this->nz*this->ny*this->nx; k++) {
       this->grid[k] = (T) 0;
    }
  }
  else {
    for (k = 0; k < this->nz*this->ny*this->nx; k++) {
       this->grid[k] = (T) x[k] ;
    }
  }
  return *this;
}
template <class T>
cfsgrid3<T>::cfsgrid3(grid3<T> &x) {
  cfsgrid<T> xcfs;
  int i, j, k;
  this->nz = x.nz;
  this->nx = xcfs.xpoints() ;
  this->ny = xcfs.ypoints() ;
  this->grid = new T[this->nx*this->ny*this->nz];
  if (this->grid == (T *) NULL) { cout << "failed to new in cfsgrid3()\n";
    cout.flush();
  }
  if (x.xpoints() != xcfs.xpoints() || x.ypoints() != xcfs.ypoints() ) {
    cout << "Cannot copy over grid3 to cfsgrid3.  Will fill with zeroes\n";
    cout.flush();
    for (k = 0; k < this->nz*this->ny*this->nx; k++) {
       this->grid[k] = (T) 0;
    }
  }
  else {
    for (k = 0; k < this->nz*this->ny*this->nx; k++) {
       this->grid[k] = (T) x[k] ;
    }
  }

}
template <class T>
cfsgrid3<T>::cfsgrid3(cfsgrid3<T> &x) {
  cfsgrid<T> xcfs;
  int i, j, k;
  this->nz = x.nz;
  this->nx = xcfs.xpoints() ;
  this->ny = xcfs.ypoints() ;
  this->grid = new T[this->nx*this->ny*this->nz];
  if (this->grid == (T *) NULL) { cout<< "failed to new in cfsgrid3()\n";
    cout.flush();}
  for (k = 0; k < this->nz*this->ny*this->nx; k++) {
     this->grid[k] = (T) x[k] ;
  }

}

template <class T>
cfsgrid3<T>::cfsgrid3(void) {
  cfsgrid<T> x;
  this->nz = 1;
  this->nx = x.xpoints() ;
  this->ny = x.ypoints() ;
  this->grid = new T[this->nx*this->ny*this->nz];
  if (this->grid == (T *) NULL) { cout << "failed to new in cfsgrid3()\n";
    cout.flush();}
}
template <class T>
cfsgrid3<T>::cfsgrid3(int n) {
  cfsgrid<T> x;
  this->nz = n;
  this->nx = x.xpoints();
  this->ny = x.ypoints() ;
  this->grid = new T[this->nx*this->ny*this->nz];
  if (this->grid == (T *) NULL) { cout << "failed to new in cfsgrid3(int)\n";
    cout.flush();}
}

/////////////////////////////////////////////////////////////////
// These are utility functions for working with cofs grids, needed
// for rotating from geographic grids to COFS native (rotate) and
// back (derotate):
void grid_derotate(cfsgrid<float> &uvel_in, cfsgrid<float> &vvel_in,
                   cfsgrid<float> &uvel_out, cfsgrid<float> &vvel_out,
                   cfslevel<float> &x) ;
void grid_rotate(cfsgrid<float> &uvel_in, cfsgrid<float> &vvel_in,
                   cfsgrid<float> &uvel_out, cfsgrid<float> &vvel_out,
                   cfslevel<float> &x) ;


void grid_derotate(cfsgrid<float> &uvel_in, cfsgrid<float> &vvel_in,
                   cfsgrid<float> &uvel_out, cfsgrid<float> &vvel_out,
                   cfslevel<float> &x) {
  cfsgrid<float> angle;
  float rad = parameters::radians_per_degree;
  float dlon, dlat, dlnt;
  ijpt loc, locp;

  cout << "Entered grid_derotate\n"; cout.flush();
// Set up the rotation matrix
  for (loc.j = 0; loc.j < angle.ypoints() ; loc.j++) {
     locp.j = loc.j;
  for (loc.i = 0; loc.i < angle.xpoints() - 1; loc.i++) {
     locp.i = loc.i + 1;
     if ( ! x.alon.in(locp) || ! x.alon.in(loc) ||
          ! x.alat.in(locp) || ! x.alat.in(loc)  ) {
        cout << "point not legal on lat/long grid "; 
        cout << loc.i; cout << " "; cout << loc.j; cout << "\n";
    cout.flush();
     }
     dlon = (x.alon[locp] - x.alon[loc])*cos(x.alat[loc]*rad);
     dlat =  x.alat[locp] - x.alat[loc];
     dlnt = sqrt(dlon*dlon + dlat*dlat);
     angle[loc] = asin(dlat/dlnt);
  }
  angle[locp] = angle[loc];
  }

//  Rotate back to the geographic grid from the COFS native grid
  for (loc.j = 0; loc.j < angle.ypoints() - 1; loc.j++ ) {
  for (loc.i = 0; loc.i < angle.xpoints() - 1; loc.i++ ) {
     uvel_out[loc] = uvel_in[loc]*cos(angle[loc]) -
                     vvel_in[loc]*sin(angle[loc]);
     vvel_out[loc] = uvel_in[loc]*sin(angle[loc]) +
                     vvel_in[loc]*cos(angle[loc]);
  }
  }

  return;
}

void grid_rotate(cfsgrid<float> &uvel_in, cfsgrid<float> &vvel_in,
                   cfsgrid<float> &uvel_out, cfsgrid<float> &vvel_out,
                   cfslevel<float> &x) {
  cfsgrid<float> angle;
  float rad = parameters::radians_per_degree;
  float dlon, dlat, dlnt;
  ijpt loc, locp;

  cout << "Entered grid_rotate\n"; cout.flush();
// Set up the rotation matrix
  for (loc.j = 0; loc.j < angle.ypoints() ; loc.j++) {
     locp.j = loc.j;
  for (loc.i = 0; loc.i < angle.xpoints() - 1; loc.i++) {
     locp.i = loc.i + 1;
     if ( ! x.alon.in(locp) || ! x.alon.in(loc) ||
          ! x.alat.in(locp) || ! x.alat.in(loc)  ) {
        cout << "point not legal on lat/long grid "; 
        cout << loc.i; cout << " "; cout << loc.j; cout << "\n";
    cout.flush();
     }
     dlon = (x.alon[locp] - x.alon[loc])*cos(x.alat[loc]*rad);
     dlat =  x.alat[locp] - x.alat[loc];
     dlnt = sqrt(dlon*dlon + dlat*dlat);
     angle[loc] = asin(dlat/dlnt);
  }
  angle[locp] = angle[loc];
  }

// Now rotate from the geographic grid to the cofs native grid
  for (loc.j = 0; loc.j < angle.ypoints() - 1; loc.j++ ) {
  for (loc.i = 0; loc.i < angle.xpoints() - 1; loc.i++ ) {
     uvel_out[loc] =   uvel_in[loc]*cos(angle[loc]) +
                       vvel_in[loc]*sin(angle[loc]);
     vvel_out[loc] = - uvel_in[loc]*sin(angle[loc]) +
                       vvel_in[loc]*cos(angle[loc]);
  }
  }

  return;
}

#endif
