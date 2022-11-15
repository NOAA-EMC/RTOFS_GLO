//NCEP grids -- these require that the metric class already
//  be defined and any required projection work is already done
//  over there.  'NCEP grid' is a loose concept, meaning merely
//  that the grid is in use somewhere within NCEP.  It may not
//  be official in any of the senses that term has, and it may
//  not have originated here.
//Robert Grumbine 11 August 1997.
//
// Extracted to here grids which used to be used, but are no more.
// sources: ncepgrid.h, resops.h (cofstype)
// Legacies as of 7 May 2013.
//

#include <cstdio>
#include <cmath>
#include <iostream>
using namespace std;

#ifndef LEGACY_GRIDS

#define LEGACY_GRIDS

#ifndef POINTSH
  #include "points.h"
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
#ifndef RESOPSH
  #include "resops.h"
#endif
//////////////////////////////////////
// Modifications:
// Robert Grumbine  
//   3 Dec 1997: xpoints vs. nx, add 'land' function
//  11 Dec 1997: drop including icegrids.h in favor of declaring values
//               here, declare chalikov grid, expand cfslevel
//  29 Jun 1998: add nsidc grid, many lat-long grids, areal integration on
//               psgrid, cfsgrid3
//   3 Aug 1998: add mrf1deg
//  
//  1999: new from malloc, multiple system capable ftn calls, southgrid copy
//        constructor, nesdis 1/8th grid, northwalsh, southwalsh, nx*ny from
//        xpoints, cfs-related,
//        Add northhigh, southhigh, nh_hazard
//  
//  14 Dec 2000: update wave model grids
//  20 Jun 2001: Add global_high (1/8th degree grid)
//               Remove cofs to its own include file
//  
//  25 May 2004: add global_12th, move to mvector, replace INFLATE with 
//               HIRES_RATIO
//               add global_nth, replace global_high with global_15th,
//               add nomura reanalysis grid,
//  24 Sep 2004: add great lakes 10 and 1 km psgrids
//  to 7 Dec 2005: add global_hazard (1/3rd), bedient_north(psgrid),
//                 laurentia(llgrid), ims_north,
//  18 May 2006: Add 200m and 1 km (ramp_high, ramp_low) south polar stereo grids
//  29 July 2007: Add this-> prefix for members of the current object class, rigor and compiler complaints
//  15 May  2008: pass grid2<T> &) in operator=; verbose option diagnostics added for construction
//  11 Sep  2009: add global_20th, ostia grid classes


//////////////////////////////////////
//Declare the first of the specialized metric classes: Polar stereographic grids
//  Examples:
//    northgrid, southgrid

//Declare grids for the sea ice model
#define HIRES_RATIO 2
template<class T> 
class northmodel : public psgrid<T> {
  public:
   northmodel(void); /* Construction creator */
};
template<class T> 
northmodel<T>::northmodel(void) {
  #ifdef VERBOSE
    cout << "in ::northmodel(void)\n";
    cout.flush();
  #endif
  this->nx   = 385/HIRES_RATIO;
  this->ny   = 465/HIRES_RATIO;
  this->dx   = 25.4e3*HIRES_RATIO;
  this->dy   = 25.4e3*HIRES_RATIO; 
  this->xorig = (-190. * this->dx  / HIRES_RATIO);
  this->yorig = (-230. * this->dy  / HIRES_RATIO);
//  sgn  = 1.0;
//  slat = 60.0;
//  slon = (-10.0);
// rearth and eccen2 are taken from psgrid base class

  this->grid = new T[this->nx*this->ny] ; 
  if (this->grid == (T *) NULL) { cout << "failed to new in northmodel()\n";
    cout.flush();}

  return;
} 

template<class T> 
class southmodel : public psgrid<T> {
  public:
   southmodel(void); /* Construction creator */
};
template<class T> 
southmodel<T>::southmodel(void) {
  #ifdef VERBOSE
    cout << "in ::southmodel(void)\n";
    cout.flush();
  #endif
  this->nx = 345/HIRES_RATIO;
  this->ny = 355/HIRES_RATIO;
  this->dx = 25.4e3*HIRES_RATIO;
  this->dy = 25.4e3*HIRES_RATIO; 
  this->sgn = -1.0;
  this->slat = 60.0;
  this->slon = 170.0;
  this->xorig = (-30.*5. * this->dx /HIRES_RATIO );
  this->yorig = (-36.*5. * this->dy /HIRES_RATIO );
// rearth and eccen2 are taken from psgrid base class

  this->grid = new T[this->nx*this->ny] ; 
  if (this->grid == (T *) NULL) { cout << "failed to new in southmodel()\n";
    cout.flush();}

  return;
} 

//Declare NESDIS Mastermap grids, 1/8th Bedient
template<class T>
class north_nesdis_eighth : public psgrid<T> {
  public:
    north_nesdis_eighth(void); /*Construction */
};
template<class T> 
north_nesdis_eighth<T>::north_nesdis_eighth(void) {
  #ifdef VERBOSE
    cout << "in ::north_nesdis_eighth(void)\n";
    cout.flush();
  #endif
  this->nx = 513;
  this->ny = 511;
  this->dx = 381.e3 / 8; /*This defines it as a 1/8th Bedient grid */
  this->dy = 381.e3 / 8;
  this->slat = 60.0;
  this->slon = -10.0;
  this->xorig = -257.*this->dx;
  this->yorig = -256.*this->dy;
  this->sgn   = 1.0;

  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) { cout << "Failed to new in north_nesdis_eighth(void)\n";
    cout.flush();}
 
  return ; 
}

template<class T>
class great_lakes_10km : public psgrid<T> {
  public:
    great_lakes_10km(void); /* Construction */
};
template<class T>
great_lakes_10km<T>::great_lakes_10km(void) {
  #ifdef VERBOSE
    cout << "in ::great_lakes_10km(void)\n";
    cout.flush();
  #endif
  this->nx = 150;
  this->ny = 100;
  this->dx = 10e3;
  this->dy = 10e3;
  this->slat =  45.0;
  this->slon = -10.0;
  this->xorig = -100*this->dx;
  this->yorig = -500*this->dy;
  this->sgn   = 1.0;

  this->rearth = parameters::rearth;
  this->eccen2 = parameters::eccen2;
  double eccen  = sqrt(this->eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-this->eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) / 
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in great_lakes_10km(void)\n";
    cout.flush();}

  return ;
} 
template<class T>
class great_lakes_1km : public psgrid<T> {
  public:
    great_lakes_1km(void); /* Construction */
};
template<class T>
great_lakes_1km<T>::great_lakes_1km(void) {
  #ifdef VERBOSE
    cout << "in ::great_lakes_1km(void)\n";
    cout.flush();
  #endif
  this->nx = 1500;
  this->ny = 1000;
  this->dx = 1e3;
  this->dy = 1e3;
  this->slat =  45.0;
  this->slon = -10.0;
  this->xorig = -1000*this->dx;  // _negative_ polei*dx
  this->yorig = -5000*this->dy;  // _negative_ polej*dy
  this->sgn   = 1.0;

  this->rearth = parameters::rearth;
  this->eccen2 = parameters::eccen2;
  double eccen  = sqrt(this->eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-this->eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);
  ijpt f;
  f.i = 0; f.j = 0;
  this->first_longitude = (this->locate(f)).lon;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in great_lakes_1km(void)\n";
    cout.flush();}

  return ;
} 

/////////////////////////////////////////////
// Latitude-longitude grids:
//  mrf1deg, global_ice, global_sst, 
template <class T>
class AKW_wave : public llgrid<T> {
  public:
    AKW_wave(void);
};
template <class T>
AKW_wave<T>::AKW_wave(void) {
  this->nx = 133;
  this->ny = 121;
  this->dlat = +0.25;
  this->dlon =  0.25;
  this->firstlon = -98.0;
  this->firstlat =  15.0;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) {cout << "failed to new a AKW_wave\n";
    cout.flush(); }

  return;
}

template <class T>
class ENP_wave : public llgrid<T> {
  public:
    ENP_wave(void);
};
template <class T>
ENP_wave<T>::ENP_wave(void) {
  this->nx = 361;
  this->ny = 153;
  this->dlat = +0.25;
  this->dlon =  0.25;
  this->firstlon = 187.25;
  this->firstlat =   7.25;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) {cout << "failed to new a ENP_wave\n";
    cout.flush(); }

  return;
}

template <class T>
class WNA_wave : public llgrid<T> {
  public:
    WNA_wave(void);
};
template <class T>
WNA_wave<T>::WNA_wave(void) {
  this->nx = 133;
  this->ny = 121;
  this->dlat = +0.25;
  this->dlon =  0.25;
  this->firstlon = -98.0;
  this->firstlat =  15.0;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) {cout << "failed to new a WNA_wave\n";
    cout.flush(); }

  return;
}

template <class T>
class laurentia : public llgrid<T> {
  public:
    laurentia();
};
template <class T>
laurentia<T>::laurentia() {
  //dlat = 1./12.;
  //dlon = 1./12.;
  this->dlat = 1./2.;
  this->dlon = 1./2.;
  this->nx = (int) (0.5 + 120 / this->dlon); 
  this->ny = (int) (0.5 +  60 / this->dlat);
  this->firstlon = +180.0 - this->dlon/2.;
  this->firstlat =   20.0 - this->dlat/2.;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) {cout << "failed to new a laurentia\n";
    cout.flush(); }

}


//Nomura reanalysis grid class.  
template <class T>
class nomura : public llgrid<T> {
  public:
    nomura();
};
template <class T>
nomura<T>::nomura() {
  this->nx = 360;
  this->ny = 180;
  this->dlat = 1.0;
  this->dlon = 1.0;
  this->firstlon =  -179.5;
  this->firstlat =  -89.5;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a nomura\n";
    cout.flush(); }

}
//Levitus atlas grid class.  Depths are specified in vector.h::levitus_depths
template <class T>
class levitus_atlas : public llgrid<T> {
  public:
    levitus_atlas();
};
template <class T>
levitus_atlas<T>::levitus_atlas() {
  this->nx = 360;
  this->ny = 180;
  this->dlat = 1.0;
  this->dlon = 1.0;
  this->firstlon =  0.5;
  this->firstlat =  -89.5;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny];
  if ( this->grid == (T *) NULL) {cout << "failed to new a levitus_atlas\n";
    cout.flush(); }

}

// CFS regular grid -- include cofs.h manually, separately -- requires input files to run
//#ifndef COFSH
//  #include "cofs.h"
//#endif

// CFS Resops-type grid:
/////////////////////////////////////////////////////////
// Instantiate a particular sort of resops -- the cofs grid.
// This will need further work before replacing the cofs native
// and cofs level grids, but this is a neater, more general version.
/////////////////////////////////////////////////////////
template <class T>
class cofstype : public resops<T> {
  protected:
    static int count;
    static grid2<float> lat, lon;
    static grid2<float> depth;
    static grid2<float> dlatdi, dlondi, dlatdj, dlondj, jacob, dx, dy;
  public:
    cofstype(void);
    latpt locate(const ijpt &);
    latpt locate(const fijpt &);
    fijpt& locate(const latpt& x) { return resops<T>::locate(x, lat, lon,
                          dlatdi, dlondi, dlatdj, dlondj, jacob) ; }
    bool land(ijpt &t) { return ( depth[t] <= 10.0 ); } ;
    bool land(ijpt &t,float toler) { return ( depth[t] <= toler ); } ;
// Not generally available in metric.h, yet, but we'll put here for now:
// Invoking grid returns ddx, ddy, respectively, and is aware of flag values
   void grad(cofstype<T> &, cofstype<T> &, float);
// Divergence is returned in the invoking grid:
   void div(cofstype<T> &, cofstype<T> &);
   T accurate(fijpt &);
   fijpt& advect(fijpt &, fijpt &); //14 June 2005 given starting point 
                 // and grid-referenced dx,dy, find new fijpt location
};
template<class T> int cofstype<T>::count = 0;
template<class T> grid2<float> cofstype<T>::lat    = nullgrid();
template<class T> grid2<float> cofstype<T>::lon    = nullgrid();
template<class T> grid2<float> cofstype<T>::depth  = nullgrid();
template<class T> grid2<float> cofstype<T>::dlatdi = nullgrid();
template<class T> grid2<float> cofstype<T>::dlondi = nullgrid();
template<class T> grid2<float> cofstype<T>::dlatdj = nullgrid();
template<class T> grid2<float> cofstype<T>::dlondj = nullgrid();
template<class T> grid2<float> cofstype<T>::jacob  = nullgrid();
template<class T> grid2<float> cofstype<T>::dx     = nullgrid();
template<class T> grid2<float> cofstype<T>::dy     = nullgrid();

template<class T>
latpt cofstype<T>::locate(const ijpt &x) {
  latpt y;
  y.lat = lat[x];
  y.lon = lon[x];
  return y;
}
template<class T>
latpt cofstype<T>::locate(const fijpt &fx) {
  ijpt x;
  x.i = (int) (0.5 + fx.i);
  x.j = (int) (0.5 + fx.j);
  return this->locate(x);
}
template<class T>
T cofstype<T>::accurate(fijpt &x) {
  ijpt ix;
  ix = x;
  T tmp = this->operator[](ix);
  tmp += (x.i - ix.i)*(this->operator[](ix.i + 1 + ix.j*this->nx) -
                       this->operator[](ix.i + ix.j*this->nx)       );
  tmp += (x.j - ix.j)*(this->operator[](ix.i + this->nx + ix.j*this->nx) -
                       this->operator[](ix.i + ix.j*this->nx)       );
  return tmp;
}
//14 June 2005 given grid-referenced dx,dy, find new fijpt location
template<class T>
fijpt& cofstype<T>::advect(fijpt &x, fijpt &delx) {
  fijpt di;

  di.i = delx.i / dx[x] ;
  di.j = delx.j / dy[x] ;

  global_fijpt = x;
  global_fijpt += di;

  return global_fijpt;
}


template <class T>
//debug 4/21/2010 cofstype<T>::cofstype<T>(void) {
cofstype<T>::cofstype(void) {

  this->nx = 181;
  this->ny = 101;
  this->grid = new T[this->nx*this->ny];
  
  if (count == 0) {
    FILE *fin;
    int nz = 19;
    mvector<float> z(nz), zz(nz);
    // Only initialize the statics if this is the first pass
    lat.resize(this->nx, this->ny);
    lon.resize(this->nx, this->ny);
    depth.resize(this->nx, this->ny);
    dlatdi.resize(this->nx, this->ny);
    dlondi.resize(this->nx, this->ny);
    dlatdj.resize(this->nx, this->ny);
    dlondj.resize(this->nx, this->ny);
    jacob.resize(this->nx, this->ny);
    dx.resize(this->nx, this->ny);
    dy.resize(this->nx, this->ny);

    fin = fopen("cfs.bin.full","r");
    if (fin == (FILE *) NULL) {
      cout << "Failed to open cfs.bin.full, aborting\n" << flush;
      exit(2);
    }
    z.binin(fin);
    zz.binin(fin);
    lon.binin(fin);
    lat.binin(fin);
    depth.binin(fin);
    fclose(fin);

    geometry(lat, lon, dlatdi, dlondi, dlatdj, dlondj, jacob, dx, dy) ;
    this->maxlat = lat.gridmax();
    this->maxlon = lon.gridmax();
    this->minlat = lat.gridmin();
    this->minlon = lon.gridmin();
  }
  count += 1;

}
template<class T>
void cofstype<T>::grad(cofstype<T> &ddx, cofstype<T> &ddy, float flagval) {
  ijpt loc;
  int indexb;

  #ifdef VERBOSE
  printf("in gradient, dx max, min, average = %f %f %f\n",this->dx.gridmax(),
                                this->dx.gridmin(), this->dx.average() );
  printf("in gradient, dy max, min, average = %f %f %f\n",this->dy.gridmax(),
                                this->dy.gridmin(), this->dy.average() );
  printf("in gradient, jacob max, min, average = %f %f %f\n",this->jacob.gridmax(),        
                                this->jacob.gridmin(), this->jacob.average() );
  printf("in gradient, dlondi max, min, average = %f %f %f\n",this->dlondi.gridmax(),
                                this->dlondi.gridmin(), this->dlondi.average() );
  fflush(stdout);
  #endif
  ddx.set((T) 0.);
  ddy.set((T) 0.);
  for (loc.j = 0; loc.j < this->ny; loc.j++) {
    indexb = loc.j*this->nx;
  for (loc.i = 0; loc.i < this->nx - 1; loc.i++) {
    if (this->grid[indexb+loc.i+1] != flagval &&  this->grid[indexb+loc.i] != flagval) {
      ddx[loc] = (this->grid[indexb+loc.i+1] - this->grid[indexb+loc.i])/dx[indexb+loc.i];
    }
    else {
      ddx[loc] = 0;
    }
  }
  }

  for (loc.j = 0; loc.j < this->ny - 1; loc.j++) {
    indexb = loc.j*this->nx;
  for (loc.i = 0; loc.i < this->nx ; loc.i++) {
    if (this->grid[indexb+loc.i+this->nx] != flagval && this->grid[indexb+loc.i] != flagval) {
      ddy[loc] = (this->grid[indexb+loc.i+this->nx] - this->grid[indexb+loc.i])/dy[indexb+loc.i];
    }
    else {
      ddy[loc] = 0;
    }
  }
  }

  // Should add the ny, nx strips
  ijpt locim, locjm;

  loc.i = this->nx - 1;
  locim.i = this->nx - 2;
  for (loc.j = 0; loc.j < this->ny; loc.j++) {
    ddx[loc] = ddx[locim];
  }
  loc.j = this->ny - 1;
  locjm.j = this->ny - 2;
  for (loc.i = 0; loc.i < this->nx; loc.i++) {
    ddy[loc] = ddy[locjm];
  }
                 
  return;
                  
} 
  
template<class T>
void cofstype<T>::div(cofstype<T> &ddx, cofstype<T> &ddy) {
  ijpt loc, locim, locjm;
  grid2<float> tmp(this->nx, this->ny);
     
  tmp.set((float) 0.);
  for (loc.j = 1; loc.j < this->ny; loc.j++) {
    locim.j = loc.j;
    locjm.j = loc.j - 1;
  for (loc.i = 1; loc.i < this->nx; loc.i++) {
    locim.i = loc.i - 1;
    locjm.i = loc.i;
    tmp[loc]  = (ddx[loc] - ddx[locim])/dx[locim];
    tmp[loc] += (ddy[loc] - ddy[locjm])/dy[locjm];
  } 
  }
  #ifdef VERBOSE2
  printf("ddx max, min, average inside loop %f %f %f\n",ddx.gridmax(), ddx.gridmin(), ddx.average() );
  printf("ddy max, min, average inside loop %f %f %f\n",ddy.gridmax(), ddy.gridmin(), ddy.average() );
  printf("dx max, min, average inside loop %f %f %f\n",dx.gridmax(), dx.gridmin(), dx.average() );
  printf("dy max, min, average inside loop %f %f %f\n",dy.gridmax(), dy.gridmin(), dy.average() );
  printf("tmp max, min, average inside loop %e %e %e\n",tmp.gridmax(), tmp.gridmin(), tmp.average() );
  fflush(stdout);
  #endif
  for (int index = 0; index < this->nx*this->ny; index++) {
    //grid[index] = tmp[index];
    this->operator[](index) = tmp[index];
  }

  return;
}

#endif
