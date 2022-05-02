//NCEP grids -- these require that the metric class already
//  be defined and any required projection work is already done
//  over there.  'NCEP grid' is a loose concept, meaning merely
//  that the grid is in use somewhere within NCEP.  It may not
//  be official in any of the senses that term has, and it may
//  not have originated here.
//Robert Grumbine 11 August 1997.
#include <cstdio>
#include <cmath>
#include <iostream>
using namespace std;

#ifndef NCEPGRIDS

#define NCEPGRIDS

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
//   7 May  2013: move legacy grid classed (i.e., not currently used) to legacy.h
//  25 Jul  2013: add gfs_quarter class
//  17 Jul  2015: add grids drewry_grid, nsidcnorth12, nsidcsouth12, 
//                    rips, global_reduced15, acnfs
//   2 Aug  2017: add northhigh2,3, southhigh2, 3


//////////////////////////////////////
//Declare the first of the specialized metric classes: Polar stereographic grids
//  Examples:
//    northgrid, southgrid
template<class T> 
class northgrid : public psgrid<T> {
  public:
   northgrid(void); /* Construction creator */
   northgrid(northgrid<T> &);
   void operator=(grid2<T> &);
};
template<class T> 
northgrid<T>::northgrid(northgrid<T> &x) {
  #ifdef VERBOSE
    cout << "Constructing a northgrid\n"; cout.flush();
  #endif
  if (this->grid != (T *) NULL ) {
    cout << "In northgrid constructor, needing to delete the grid *\n"; 
    cout.flush();
    delete []this->grid;
  }
  this->nx = x.xpoints();
  this->ny = x.ypoints();
  this->dx = x.dx;
  this->dy = x.dy;
  this->xorig = x.xorig;
  this->yorig = x.yorig;
  this->sgn = x.sgn;
  this->slat = x.slat;
  this->slon = x.slon;
// rearth and eccen2 are taken from psgrid base class

  this->grid = new T[this->nx*this->ny] ; 
  if (this->grid == (T *) NULL) { cout << "Failed to new in northgrid(void)\n";
    cout.flush();
                  }
  this->pds.set_gridid(219);
 
  return ; 
}
template<class T> 
northgrid<T>::northgrid(void) {
  #ifdef VERBOSE
    cout << "in ::northgrid(void)\n";
    cout.flush();
  #endif
  this->nx = 385;
  this->ny = 465;
  this->dx = 25.4e3;
  this->dy = 25.4e3; 
  this->xorig = (-38.*5* this->dx );
  this->yorig = (-46.*5* this->dy );
  this->sgn = 1.0;
  this->slat = 60.0;
  this->slon = (-10.0);
// rearth and eccen2 are taken from psgrid base class

  this->grid = new T[this->nx*this->ny] ; 
  if (this->grid == (T *) NULL) { cout << "Failed to new in northgrid(void)\n";
    cout.flush(); }
  this->pds.set_gridid(219);
 
  return ; 
}
template<class T> 
void northgrid<T>::operator=(grid2<T> &x) {
  int j;
  if (x.xpoints() != this->nx || x.ypoints() != this->ny) {
    cout << "size mismatch\n";
    cout.flush();
  }
  for (j = 0; j < this->ny*this->nx; j++) {
      this->grid[j] = x[j];
  }
  this->pds.set_gridid(219);
}
// High resolution (12.7 km) north hemisphere grid
template<class T>
class northhigh : public northgrid<T> {
  public:
    northhigh();
};
template<class T>
northhigh<T>::northhigh() {
  #ifdef VERBOSE
    cout << "in ::northhigh(void)\n";
    cout.flush();
  #endif
  this->pds.set_gridid(171);
  this->nx *= 2;
  this->ny *= 2;
  this->dx /= 2;
  this->dy /= 2;
  delete [] this->grid;
  this->grid = new T[this->nx*this->ny];
}
// High resolution-2 (6.35 km) north hemisphere grid
template<class T>
class northhigh2 : public northgrid<T> {
  public:
    northhigh2();
};
template<class T>
northhigh2<T>::northhigh2() {
  #ifdef VERBOSE
    cout << "in ::northhigh(void)\n";
    cout.flush();
  #endif
  this->pds.set_gridid(255);
  this->nx *= 4;
  this->ny *= 4;
  this->dx /= 4;
  this->dy /= 4;
  delete [] this->grid;
  this->grid = new T[this->nx*this->ny];
}
// High resolution-2 (4.2 km) north hemisphere grid
template<class T>
class northhigh3 : public northgrid<T> {
  public:
    northhigh3();
};
template<class T>
northhigh3<T>::northhigh3() {
  #ifdef VERBOSE
    cout << "in ::northhigh3(void)\n";
    cout.flush();
  #endif
  this->pds.set_gridid(255);
  this->nx *= 6;
  this->ny *= 6;
  this->dx /= 6;
  this->dy /= 6;
  delete [] this->grid;
  this->grid = new T[this->nx*this->ny];
}


//Now declare the southern hemisphere
template<class T> 
class southgrid : public psgrid<T> {
  public:
   southgrid(void);           /* Construction creator */
   southgrid(southgrid<T> &); /* Copy creator */
};
template<class T>
southgrid<T>::southgrid(southgrid<T> &x) {
  int gridid = 220;
  #ifdef VERBOSE
    cout << "in ::southhigh(void)\n";
    cout.flush();
  #endif
  this->nx = x.xpoints();
  this->ny = x.ypoints();
  this->dx = x.dx;
  this->dy = x.dy;
  this->xorig = x.xorig;
  this->yorig = x.yorig;
  this->sgn = x.sgn;
  this->slat = x.slat;
  this->slon = x.slon;
// rearth and eccen2 are taken from psgrid base class

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in northgrid(void)\n";
    cout.flush(); }
  this->pds.set_gridid(gridid);

  return ;
}

template<class T> 
southgrid<T>::southgrid(void) {
  #ifdef VERBOSE
    cout << "in ::southgrid(void)\n";
    cout.flush();
  #endif
  int gridid = 220;
  this->nx    = 345;
  this->ny    = 355;
  this->sgn   = -1.0;
  this->slat  = 60.0;
  this->slon  = 170.0;
  this->dx    = 25.4e3;
  this->dy    = 25.4e3; 
  this->xorig = (-150. * this->dx );
  this->yorig = (-180. * this->dy );
  this->pds.set_gridid(gridid);
// rearth and eccen2 are taken from psgrid base class

  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) { cout << "failed to new in southgrid::void\n";
    cout.flush();}
 
  return ; 
}
// High resolution (12.7 km) south hemisphere grid
template<class T>
class southhigh : public southgrid<T> {
  public:
    southhigh();
};
template<class T>
southhigh<T>::southhigh() {
  int gridid = 171;
  #ifdef VERBOSE
    cout << "in ::southhigh()\n";
    cout.flush();
  #endif
  this->pds.set_gridid(gridid);
  this->nx *= 2;
  this->ny *= 2;
  this->dx /= 2;
  this->dy /= 2;
  delete [] this->grid;
  this->grid = new T[this->nx*this->ny];
}
// High resolution-2 (6.35 km) south hemisphere grid
template<class T>
class southhigh2 : public southgrid<T> {
  public:
    southhigh2();
};
template<class T>
southhigh2<T>::southhigh2() {
  int gridid = 255;
  #ifdef VERBOSE
    cout << "in ::southhigh2()\n";
    cout.flush();
  #endif
  this->pds.set_gridid(gridid);
  this->nx *= 4;
  this->ny *= 4;
  this->dx /= 4;
  this->dy /= 4;
  delete [] this->grid;
  this->grid = new T[this->nx*this->ny];
}

// High resolution-3 (4.233 km) south hemisphere grid
template<class T>
class southhigh3 : public southgrid<T> {
  public:
    southhigh3();
};
template<class T>
southhigh3<T>::southhigh3() {
  int gridid = 255;
  #ifdef VERBOSE
    cout << "in ::southhigh3()\n";
    cout.flush();
  #endif
  this->pds.set_gridid(gridid);
  this->nx *= 6;
  this->ny *= 6;
  this->dx /= 6;
  this->dy /= 6;
  delete [] this->grid;
  this->grid = new T[this->nx*this->ny];
}



template<class T>
class ramp_high : public psgrid<T> {
  public:
    ramp_high(void);
};
template<class T>
ramp_high<T>::ramp_high(void) {
  #ifdef VERBOSE
    cout << "in ::ramphigh()\n";
    cout.flush();
  #endif
  this->nx = 5736*5;
  this->ny = 4916*5;
  this->dx = 1.0e3/5.;
  this->dy = 1.0e3/5.;
  this->xorig = (-2868000.);
  this->yorig = (-2458000.);
  this->sgn  = -1.0;
  this->slat = 71.0;
  this->slon = (-90.0+180);
  //slon = (-90.0);
// rearth and eccen2 are taken from psgrid base class
// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);

  ijpt f;
  f.i = 0; f.j = 0;
  this->first_longitude = (this->locate(f)).lon;

  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) { cout << "Failed to new in ramp_high(void)\n";
    cout.flush(); }
  return ;
}

template<class T>
class ramp_low : public psgrid<T> {
  public:
    ramp_low(void);
};
template<class T>
ramp_low<T>::ramp_low(void) {
  #ifdef VERBOSE
    cout << "in ::ramplow(void)\n";
    cout.flush();
  #endif
  this->nx = 5736;
  this->ny = 4916;
  this->dx = 1.0e3;
  this->dy = 1.0e3;
  this->xorig = (-2868000.);
  this->yorig = (-2458000.);
  this->sgn  = -1.0;
  this->slat = 71.0;
  this->slon = (-90.0+180);
  //slon = (-90.0);
  //slon = (0.0);
// rearth and eccen2 are taken from psgrid base class
// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);

  ijpt f;
  f.i = 0; f.j = 0;
  this->first_longitude = (this->locate(f)).lon;


  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) { cout << "Failed to new in ramp_low(void)\n";
    cout.flush(); }
// For debugging only:
  parameters xxx;

  return ;
}
template<class T>
class drewry_grid : public psgrid<T> {
  public:
   drewry_grid(void); /* Construction creator */
   drewry_grid(drewry_grid<T> &);
   void show() {printf("drewry slat = %f\n",this->slat); fflush(stdout); }
};
template<class T>
drewry_grid<T>::drewry_grid(void) {
  this->nx = 281;
  this->ny = 281;
  this->dx = 20.0e3;
  this->dy = 20.0e3;
  this->xorig = (-140.*this->dx);
  this->yorig = (-140.*this->dy);
  this->sgn  = -1.0;
  this->slon = (-90.0+180);

  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) { cout << "Failed to new in drewry_grid(void)\n";
                            fflush(stdout); }

  this->slat = 60.0;
//or:
  this->slat = 71.0;
// rearth and eccen2 are taken from psgrid base class
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-this->eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - parameters::eccen*sin(this->sl))/
                  (1.0 + parameters::eccen*sin(this->sl))), parameters::eccen/2.);

  return ;
}




//Declare NSIDC (NASA) Grids:
template<class T>
class nsidcnorth : public psgrid<T> {
  public:
   nsidcnorth(void); /* Construction creator */
};
template<class T>
nsidcnorth<T>::nsidcnorth(void) {
  #ifdef VERBOSE
    cout << "in ::nsidcnorth(void)\n";
    cout.flush();
  #endif
  this->nx = 304;
  this->ny = 448;
  this->xorig = -3850.e3 + 12.5e3;
  this->yorig = -5350.e3 + 12.5e3;
  this->sgn = 1.0;
  this->slon = -45.; // -35 in old nsidc/nasa
  this->slat = 70.0;
  this->dx = 25.0e3;
  this->dy = 25.0e3;
// // rearth and eccen2 are _not_ taken from psgrid base class, use Hughes
//   this->rearth = 6378.273e3;
//   this->eccen2 = 0.081816153*0.081816153;
// // copy over to ensure initialization
// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);


  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in northgrid(void)\n";
    cout.flush();}

  return ;
}
template<class T>
class nsidcnorth12 : public psgrid<T> {
  public:
   nsidcnorth12(void); /* Construction creator */
};
template<class T>
nsidcnorth12<T>::nsidcnorth12(void) {
  #ifdef VERBOSE
    cout << "in ::nsidcnorth12(void)\n";
    cout.flush();
  #endif
  this->nx = 304*2;
  this->ny = 448*2;
  this->xorig = -3850.e3 + 12.5e3/2;
  this->yorig = -5350.e3 + 12.5e3/2;
  this->sgn = 1.0;
  this->slon = -45.; // -35 in old nsidc/nasa
  this->slat = 70.0;
  this->dx = 25.0e3/2.;
  this->dy = 25.0e3/2.;
// // rearth and eccen2 are _not_ taken from psgrid base class, use Hughes
//   this->rearth = 6378.273e3;
//   this->eccen2 = 0.081816153*0.081816153;
// // copy over to ensure initialization
// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);


  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in north12grid(void)\n";
    cout.flush();}

  return ;
}
template<class T> 
class nsidcsouth : public psgrid<T> {
  public:
   nsidcsouth(void); /* Construction creator */
};
template<class T> 
nsidcsouth<T>::nsidcsouth(void) {
  #ifdef VERBOSE
    cout << "in ::nsidcsouth(void)\n";
    cout.flush();
  #endif
  this->nx = 316;
  this->ny = 332;
  this->dx = 25.0e3;
  this->dy = 25.0e3; 
  this->xorig = -3950.e3 + 12.5e3; // need shift because mine are cell 
  //this->yorig = -4350.e3 ;  // old
  this->yorig = -3950.e3 + 12.5e3; //  centers, rather than NSIDC edges
  this->sgn = -1.0;
  this->slon = 90. ; // -80 in old version
  this->slat = 70.0;
// rearth and eccen2 are taken from psgrid base class
// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);


  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) { cout << "Failed to new in northgrid(void)\n";
    cout.flush();}
 
  return ; 
}
template<class T> 
class nsidcsouth12 : public psgrid<T> {
  public:
   nsidcsouth12(void); /* Construction creator */
};
template<class T> 
nsidcsouth12<T>::nsidcsouth12(void) {
  #ifdef VERBOSE
    cout << "in ::nsidcsouth12(void)\n";
    cout.flush();
  #endif
  this->nx = 316*2;
  this->ny = 332*2;
  this->dx = 25.0e3/2.;
  this->dy = 25.0e3/2.; 
  this->xorig = -3950.e3 + 12.5e3/2.; // need shift because mine are cell 
  //this->yorig = -4350.e3 ;  // old
  this->yorig = -3950.e3 + 12.5e3/2.; //  centers, rather than NSIDC edges
  this->sgn = -1.0;
  this->slon = 90. ; // -80 in old version
  this->slat = 70.0;
// rearth and eccen2 are taken from psgrid base class
// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);


  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) { cout << "Failed to new in nsidcsouth12(void)\n";
    cout.flush();}
 
  return ; 
}

//Declare 'Bedient' as a general grid type, defaulting to 1/15th (res of northgrid)
template<class T>
class bedient_north : public psgrid<T> {
  public:
    bedient_north(int );
    bedient_north(void);
};
template<class T>
bedient_north<T>::bedient_north(void) {
  int n = 15;
  #ifdef VERBOSE
    cout << "in ::bedient_north(void)\n";
    cout.flush();
  #endif
  this->nx = 64*n;
  this->ny = 64*n;
  this->dx = 381.e3 / n;
  this->dy = 381.e3 / n;
  this->slat = 60.0;
  this->slon = -10.0;
  this->xorig = -(this->nx/2 )*this->dx ;
  this->yorig = -(this->ny/2 )*this->dy ;
  this->sgn   = 1.0;

// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);
  
  ijpt f;
  f.i = 0; f.j = 0;
  this->first_longitude = (this->locate(f)).lon;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in bedient_north(void)\n";
    cout.flush();}

  return ;
}
template<class T>
bedient_north<T>::bedient_north(int n) {
  #ifdef VERBOSE
    cout << "in ::bedient_north(int)\n";
    cout.flush();
  #endif
  this->nx = 64*n;
  this->ny = 64*n;
  this->dx = 381.e3 / n;
  this->dy = 381.e3 / n;
  this->slat = 60.0;
  //this->slat = 61.2;
  this->slon = -10.0;
  this->xorig = -(this->nx/2 - 0.0)*this->dx ;
  this->yorig = -(this->ny/2      )*this->dy ;
  this->sgn   = 1.0;

// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);
  
  ijpt f;
  f.i = 0; f.j = 0;
  this->first_longitude = (this->locate(f)).lon;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in bedient_north(void)\n";
    cout.flush();}

  return ;
}

//Declare IMS as a general grid type, defaulting to 1/16th 
template<class T>
class ims_north : public psgrid<T> {
  public:
    ims_north(int = 16);
};
template<class T>
ims_north<T>::ims_north(int n) {
  #ifdef VERBOSE
    cout << "in ::ims_north(void)\n";
    cout.flush();
  #endif
  this->nx = 64*n;
  this->ny = 64*n;
  this->dx = 381.e3 / n;
  this->dy = 381.e3 / n;
  this->slat = 58.5;  // Note that the grid isn't quite 60.0
  this->slon = -10.0;
  this->xorig = -(this->nx/2)*this->dx ;
  this->yorig = -(this->ny/2)*this->dy ;
  this->sgn   = 1.0;

// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);

  ijpt f;
  f.i = 0; f.j = 0;
  this->first_longitude = (this->locate(f)).lon;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in ims_north(void)\n";
    cout.flush();}

  return ;
}

// RIPS (Environment Canada Regional Ice Prediction System
template<class T>
class rips : public psgrid<T> {
  public:
    rips();
};
template<class T>
rips<T>::rips() {
  #ifdef VERBOSE
    cout << "in ::rips(void)\n";
    cout.flush();
  #endif
  this->nx = 1770;
  this->ny = 1610;
  this->dx = 5.e3 ;
  this->dy = 5.e3 ;
  this->slat = 60.0;  
  this->slon = -10.0;
  this->xorig = -(1161.5)*this->dx ;
  this->yorig = -(705.5)*this->dy ;
  this->sgn   = 1.0;

// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);

  ijpt f;
  f.i = 0; f.j = 0;
  this->first_longitude = (this->locate(f)).lon;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in rips(void)\n";
    cout.flush();}

  return ;
}

//Declare John Walsh Grids:
template<class T> 
class northwalsh : public metricgrid<T> {
  private:
    float dx, dy, rearth, xorig, yorig;
  public:
   northwalsh(void); /* Construction creator */
   fijpt& locate(const latpt &);
   latpt locate(const ijpt &);
   latpt locate(const fijpt &);
   void walshread_c(FILE *);  
   void walshread_i(FILE *);  
   void walshread_f(FILE *);  
};
template<class T> 
northwalsh<T>::northwalsh(void) {
  #ifdef VERBOSE
    cout << "in ::northwalsh(void)\n";
    cout.flush();
  #endif

  if (this->grid != (T *) NULL ) {
    cout << "nwalsh grid not null in ::nw, deleting\n"; cout.flush();
    delete []this->grid;
  }

  this->nx = 80;
  this->ny = 58;
  this->yorig = (float) (24.0 - 1.);
  this->xorig = (float) (35.0 - 1.);

  #ifdef VERBOSE
    cout << "Constructing an nwalsh\n"; cout.flush();
  #endif
  this->grid = new T[this->nx*this->ny] ; 
  if (this->grid == (T *) NULL) { cout << "Failed to new in northwalsh(void)\n";
    cout.flush();}
 
  return ; 
}
template<class T> 
fijpt& northwalsh<T>::locate(const latpt &ll) {
  fijpt retloc;
  retloc.i = this->xorig + (90.-ll.lat)*sin(2.*M_PI*(ll.lon+110.)/360.);
  retloc.j = this->yorig + (90.-ll.lat)*cos(2.*M_PI*(ll.lon+110.)/360.);
  global_fijpt = retloc;
  return global_fijpt;
}
template<class T>
latpt northwalsh<T>::locate(const ijpt &loc) {
  ijpt tloc;
  latpt ll;
  float psi;
  tloc.i = (int)  (loc.i - this->xorig);
  tloc.j = (int)  (loc.j - this->yorig);
  ll.lat = 90. - sqrt((double)(tloc.i*tloc.i + tloc.j*tloc.j));
  psi = atan2((double) tloc.i, (double) tloc.j);
  ll.lon = 180.*psi / M_PI - 110. ;
  return ll;
}
template<class T>
latpt northwalsh<T>::locate(const fijpt &loc) {
  fijpt tloc;
  latpt ll;
  float psi;
  tloc.i = loc.i - this->xorig;
  tloc.j = loc.j - this->yorig;
  ll.lat = 90. - sqrt((double) (tloc.i*tloc.i + tloc.j*tloc.j));
  psi = atan2((double) tloc.i, (double) tloc.j);
  ll.lon = 180.*psi / M_PI - 110.;
  return ll;
}
template<class T>
void northwalsh<T>::walshread_i(FILE *fin) {
  int month, year, i;
  ijpt x;
  char s;

  fscanf(fin, "%d %d", &month, &year);
  fscanf(fin, "%1c",&s); // dummy to skip cr

  for (x.j = 0; x.j < this->ny; x.j++) {
    for (x.i = 0; x.i < this->nx; x.i++) {
      fscanf(fin, "%d", &i) ;
      this->operator[](x)  = i;
    }
    fscanf(fin, "%1c",&s); // dummy to skip cr
  }
}
template<class T>
void northwalsh<T>::walshread_c(FILE *fin) {
  int month, year;
  ijpt x;
  char s;

  fscanf(fin, "%d %d", &month, &year);
  fscanf(fin, "%1c",&s); // dummy to skip cr

  for (x.j = 0; x.j < this->ny; x.j++) {
    for (x.i = 0; x.i < this->nx; x.i++) {
      fscanf(fin, "%1c", &s) ;
      this->operator[](x)  = s;
    }
    fscanf(fin, "%1c",&s); // dummy to skip cr
  }
}
template<class T>
void northwalsh<T>::walshread_f(FILE *fin) {
  ijpt x;
  char s;
  float f;

  for (x.j = 0; x.j < this->ny; x.j++) {
    for (x.i = 0; x.i < this->nx; x.i++) {
      fscanf(fin, "%8f", &f) ;
      this->operator[](x)  = f;
    }
    fscanf(fin, "%1c",&s); // dummy to skip cr
  }
}

template<class T> 
class southwalsh : public metricgrid<T> {
  private:
    float dx, dy, rearth, xorig, yorig;
  public:
   southwalsh(void); /* Construction creator */
   fijpt& locate(const latpt &);
   latpt locate(const ijpt &);
   latpt locate(const fijpt &);
   void walshread_c(FILE *);
   void walshread_i(FILE *);
   void walshread_f(FILE *);
};
template<class T> 
southwalsh<T>::southwalsh(void) {
  #ifdef VERBOSE
    cout << "in ::southwalsh(void)\n";
    cout.flush();
  #endif
  this->nx = 80;
  this->ny = 80;
  this->xorig = (40-0);
  this->yorig = (40-1);

  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) { cout << "Failed to new in northwalsh(void)\n";
    cout.flush();}

  return ;
}
template<class T>
fijpt& southwalsh<T>::locate(const latpt &ll) {
  fijpt retloc;
  retloc.i = this->xorig + (90.+ll.lat)*sin(2.*M_PI*(90.-ll.lon)/360.);
  retloc.j = this->yorig + (90.+ll.lat)*cos(2.*M_PI*(90.-ll.lon)/360.);
  global_fijpt = retloc;
  return global_fijpt;
}
template<class T>
latpt southwalsh<T>::locate(const ijpt &loc) {
  ijpt tloc;
  latpt ll;
  float psi;
  tloc.i = (int)  (loc.i - this->xorig);
  tloc.j = (int)  (loc.j - this->yorig);
  ll.lat = 90. - sqrt((double) (tloc.i*tloc.i + tloc.j*tloc.j));
  ll.lat = -ll.lat;
  psi = atan2((double) tloc.i, (double) tloc.j);
  //ll.lon = 180.*psi / M_PI - 110. ;
  ll.lon = 90 - 180.*psi / M_PI ;
  return ll;
}
template<class T>
latpt southwalsh<T>::locate(const fijpt &loc) {
  fijpt tloc;
  latpt ll;
  float psi;
  tloc.i = (loc.i - this->xorig);
  tloc.j = (loc.j - this->yorig);
  ll.lat = 90. - sqrt((double) (tloc.i*tloc.i + tloc.j*tloc.j) );
  ll.lat = -ll.lat;
  psi = atan2((double) tloc.i, (double) tloc.j);
  //ll.lon = 180.*psi / M_PI - 110. ;
  ll.lon = 90 - 180.*psi / M_PI ;
  return ll;
}
template<class T>
void southwalsh<T>::walshread_i(FILE *fin) {
  int month, year, i;
  ijpt x;
  char s;

  fscanf(fin, "%d %d", &month, &year);
  printf("%d %d\n",month, year);
  fscanf(fin, "%1c",&s); // dummy to skip cr
  for (x.j = 0; x.j < this->ny; x.j++) {
    for (x.i = 0; x.i < this->nx; x.i++) {
      fscanf(fin, "%d", &i) ;
      this->operator[](x)  = i;
    }
    fscanf(fin, "%1c",&s); // dummy to skip cr
  }
}
template<class T>
void southwalsh<T>::walshread_c(FILE *fin) {
  int month, year;
  ijpt x;
  char s;

  fscanf(fin, "%d %d", &month, &year);
  fscanf(fin, "%1c",&s); // dummy to skip cr
  for (x.j = 0; x.j < this->ny; x.j++) {
    for (x.i = 0; x.i < this->nx; x.i++) {
      fscanf(fin, "%1c", &s) ;
      this->operator[](x)  = s;
    }
    fscanf(fin, "%1c",&s); // dummy to skip cr
  }
}
template<class T>
void southwalsh<T>::walshread_f(FILE *fin) {
  ijpt x;
  char s;
  float f;

  for (x.j = 0; x.j < this->ny; x.j++) {
    for (x.i = 0; x.i < this->nx; x.i++) {
      fscanf(fin, "%8f", &f) ;
      this->operator[](x)  = f;
    }
    fscanf(fin, "%1c",&s); // dummy to skip cr
  }
}


/////////////////////////////////////////////
// Latitude-longitude grids:
/////////////////////////////////////////////
template <class T>
class global_reduced15 : public llgrid<T> {
  public:
    global_reduced15(void);
};
template <class T>
global_reduced15<T>::global_reduced15(void) {
  this->nx = 288;
  this->ny = 144;
  this->dlat = -1.25;
  this->dlon =  1.25;
  this->firstlon =  this->dlon/2.;
  this->firstlat =  90.0 + this->dlat/2.;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;


  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a global_reduced15\n";
    cout.flush(); }

  return;
}

template<class T>
class mrf1deg : public llgrid<T> {
  public:
   mrf1deg(void); /* Construction creator */
   ~mrf1deg(void) { };
   mrf1deg(mrf1deg<T>&);
   mrf1deg<T> & operator=(mrf1deg<T> &);
};
template<class T>
mrf1deg<T> & mrf1deg<T>::operator=(mrf1deg<T> &x) {
  int loc;
  this->nx = 360;
  this->ny = 181;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in mrf1deg()\n";
    cout.flush(); }
  this->dlat = -1.0;
  this->dlon = 1.0;
  this->firstlat = 90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(3);
//Future: copy the pds
  for (loc = 0; loc < this->ny*this->nx; loc++) {
     this->grid[loc] = x[loc];
  }
  return *this;
}
template<class T>
mrf1deg<T>::mrf1deg(mrf1deg<T> &x) {
  int loc;
  this->nx = 360;
  this->ny = 181;
  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) { cout << "Failed to new in mrf1deg()\n";
    cout.flush(); }
  this->dlat = -1.0;
  this->dlon = 1.0;
  this->firstlat = 90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(3);
//Future: copy the pds
  for (loc = 0; loc < this->ny*this->nx; loc++) {
     this->grid[loc] = x[loc];
  }
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

}
template<class T>
mrf1deg<T>::mrf1deg(void) {
  this->nx = 360 ;
  this->ny = 181;
  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) { cout << "Failed to new in mrf1deg()\n";
    cout.flush(); }
  this->dlat = -1.0;
  this->dlon = 1.0;
  this->firstlat = 90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(3);
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
}

template<class T>
class gfs_half : public llgrid<T> {
  public:
   gfs_half(void); /* Construction creator */
   ~gfs_half(void) { };
   gfs_half(gfs_half<T>&);
   gfs_half<T> & operator=(gfs_half<T> &);
};
template<class T>
gfs_half<T> & gfs_half<T>::operator=(gfs_half<T> &x) {
  int loc;
  this->nx = 360*2;
  this->ny = 180*2+1;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in gfs_half()\n"; }
  this->dlat = -1.0/2;
  this->dlon = 1.0/2;
  this->firstlat = 90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(255);
//Future: copy the pds
  for (loc = 0; loc < this->ny*this->nx; loc++) {
     this->grid[loc] = x[loc];
  }
  return *this;
}
template<class T>
gfs_half<T>::gfs_half(gfs_half<T> &x) {
  int loc;
  this->nx = 360*2;
  this->ny = 180*2+1;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in gfs_half()\n"; }
  this->dlat = -1.0/2;
  this->dlon = 1.0/2;
  this->firstlat = 90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(255);
//Future: copy the pds
  for (loc = 0; loc < this->ny*this->nx; loc++) {
     this->grid[loc] = x[loc];
  }
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
}
template<class T>
gfs_half<T>::gfs_half(void) {
  this->nx = 360*2;
  this->ny = 180*2+1;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in gfs_half()\n"; }
  this->dlat = -1.0/2;
  this->dlon = 1.0/2;
  this->firstlat = 90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(255);
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
}

template<class T>
class gfs_quarter : public llgrid<T> {
  public:
   gfs_quarter(void); /* Construction creator */
   ~gfs_quarter(void) { };
   gfs_quarter(gfs_quarter<T>&);
   gfs_quarter<T> & operator=(gfs_quarter<T> &);
};
template<class T>
gfs_quarter<T> & gfs_quarter<T>::operator=(gfs_quarter<T> &x) {
  int loc;
  this->nx = 360*4;
  this->ny = 180*4+1;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in gfs_quarter()\n"; }
  this->dlat = -1.0/4;
  this->dlon = 1.0/4;
  this->firstlat = 90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(255);
//Future: copy the pds
  for (loc = 0; loc < this->ny*this->nx; loc++) {
     this->grid[loc] = x[loc];
  }
  return *this;
}
template<class T>
gfs_quarter<T>::gfs_quarter(gfs_quarter<T> &x) {
  int loc;
  this->nx = 360*4;
  this->ny = 180*4+1;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in gfs_quarter()\n"; }
  this->dlat = -1.0/4;
  this->dlon = 1.0/4;
  this->firstlat = 90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(255);
//Future: copy the pds
  for (loc = 0; loc < this->ny*this->nx; loc++) {
     this->grid[loc] = x[loc];
  }
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
}
template<class T>
gfs_quarter<T>::gfs_quarter(void) {
  this->nx = 360*4;
  this->ny = 180*4+1;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in gfs_quarter()\n"; }
  this->dlat = -1.0/4;
  this->dlon = 1.0/4;
  this->firstlat = 90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(255);
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
}


template <class T>
class global_nesdis_half : public llgrid<T> {
  public:
    global_nesdis_half(void);
};
template <class T>
global_nesdis_half<T>::global_nesdis_half(void) {
  this->nx = 720;
  this->ny = 361;
  this->dlat = 0.5;
  this->dlon = 0.5;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->firstlat = -90.0;
  this->firstlon =   0.0;

  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) {cout << "failed to new a global_nesdis_half\n";
    cout.flush(); }

  return;
}

template <class T>
class global_wave : public llgrid<T> {
  public:
    global_wave(void);
};
template <class T>
global_wave<T>::global_wave(void) {
  this->nx = 288;
  this->ny = 157;
  this->dlat = -1.0;
  this->dlon =  1.25;
  this->firstlon =   0.0;
  this->firstlat =  78.0;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;


  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) {cout << "failed to new a global_wave\n";
    cout.flush(); }

  return;
}

template <class T>
class great_lakes_wave : public llgrid<T> {
  public:
    great_lakes_wave(void);
};
template <class T>
great_lakes_wave<T>::great_lakes_wave(void) {
  this->nx = 327;
  this->ny = 235;
  this->dlat = -0.035;
  this->dlon =  0.05;
  this->firstlon = -92.2;
  this->firstlat =  49.1;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) {cout << "failed to new a great_lakes_wave\n";
    cout.flush(); }

  return;
}


template <class T>
class nh_ocean_weather : public llgrid<T> {
  public:
    nh_ocean_weather();
};

template <class T>
nh_ocean_weather<T>::nh_ocean_weather() {
  this->nx = 361;
  this->ny =  91;
  this->dlat = 1.0;
  this->dlon = 1.0;
  this->firstlon =  0.0;
  this->firstlat =  0.0;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) {cout << "failed to new a nh_ocean_weather\n";
    cout.flush(); }

}
template <class T>
class nh_hazard : public llgrid<T> {
  public:
    nh_hazard();
};
template <class T>
nh_hazard<T>::nh_hazard() {
  this->nx = 721;
  this->dlat = 0.25;
  this->dlon = 0.5;
  this->ny = 421;
  this->firstlat = -20.0;
  this->firstlon = -270.0;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) {cout << "failed to new a nh_hazard\n";
    cout.flush(); }
}

//added 7 Dec 2005 for the hazard products
template <class T>
class global_hazard : public llgrid<T> {
  public:
    global_hazard(void);
};

template <class T>
global_hazard<T>::global_hazard(void) {
  int res = 20; // minutes resolution
  this->nx = (360*60) / res;
  this->ny = (180*60) / res;
  this->dlat = - (float) res / 60.0;
  this->dlon =   (float) res / 60.0;
  this->firstlon = 20.0 + this->dlon / 2.;
  this->firstlat = 90.0 + this->dlat / 2.;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a global_hazard\n";
    cout.flush(); }

}


template <class T>
class global_nth : public llgrid<T> {
  public:
    global_nth(float = 4.);
};
template <class T>
global_nth<T>::global_nth(float degreefrac) {
  this->nx = (int) (0.5 + 360.*degreefrac);
  this->ny = (int) (0.5 + 180.*degreefrac);
  this->dlat = -1./degreefrac;
  this->dlon =  1./degreefrac ;
  this->firstlon =  this->dlon / 2.;
  this->firstlat =  90. + this->dlat / 2.;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a global_nth\n";
    cout.flush(); }
}

template <class T>
class global_quarter : public llgrid<T> {
  public:
    global_quarter();
};
template <class T>
global_quarter<T>::global_quarter() {
  float  degreefrac = 4.;
  this->nx = (int) (0.5 + 360.*degreefrac);
  this->ny = (int) (0.5 + 180.*degreefrac);
  this->dlat = -1./degreefrac;
  this->dlon =  1./degreefrac;
  this->firstlon =  this->dlon / 2.;
  this->firstlat =  90. + this->dlat / 2.;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a global_quarter\n";
    cout.flush(); }

}

template <class T>
class global_eighth : public llgrid<T> {
  public:
    global_eighth();
};
template <class T>
global_eighth<T>::global_eighth() {
  float  degreefrac = 8.;
  this->nx = (int) (0.5 + 360.*degreefrac);
  this->ny = (int) (0.5 + 180.*degreefrac);
  this->dlat = -1./degreefrac;
  this->dlon =  1./degreefrac;
  this->firstlon =  this->dlon / 2.; 
  this->firstlat =  90. + this->dlat / 2.;  
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);
  
  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) {cout << "failed to new a global_eighth\n";
    cout.flush(); }

}

template <class T>
class global_12th : public llgrid<T> {
  public:
    global_12th();
};
template <class T>
global_12th<T>::global_12th() {
  float  degreefrac = 12.;
  this->nx = (int) (0.5 + 360.*degreefrac);
  this->ny = (int) (0.5 + 180.*degreefrac);
  this->dlat = -1./degreefrac;
  this->dlon =  1./degreefrac;
  this->firstlon =  this->dlon / 2.;
  this->firstlat =  90. + this->dlat / 2.;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a global_12th\n";
    cout.flush(); }

}

template <class T>
class global_15th : public llgrid<T> {
  public:
    global_15th();
};
template <class T>
global_15th<T>::global_15th() {
  float  degreefrac = 15.;
  this->nx = (int) (0.5 + 360.*degreefrac);
  this->ny = (int) (0.5 + 180.*degreefrac);
  this->dlat = -1./degreefrac;
  this->dlon =  1./degreefrac ;
  this->firstlon =  this->dlon / 2.;
  this->firstlat =  90. + this->dlat / 2.;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a global_15th\n";
    cout.flush(); }
}

// arctic cap nowcast/forecast model ----
template<class T>
class acnfs : public llgrid<T> {
  public :
    acnfs(void);
    acnfs(acnfs<T> &);

};
template <class T>
acnfs<T>::acnfs(void) {
  this->nx = 4500;
  this->ny = 1251;
  this->dlat = 0.04;
  this->dlon = 0.08;
  this->firstlat =  40.0;
  this->firstlon = -180.0;
  this->grid = new T [this->nx * this->ny];

  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

}
template <class T>
acnfs<T>::acnfs(acnfs<T> &x) {
  this->nx = x.nx;
  this->ny = x.ny;
  this->dlat = x.dlat;
  this->dlon = x.dlon;
  this->firstlat = x.firstlat;
  this->firstlon = x.firstlon;

  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  if (this->grid != (T *) NULL) {
    delete this->grid;
  }
  this->grid = new T[this->nx*this->ny];
  for (int i = 0; i < this->ny*this->nx; i++) {
    this->grid[i] = x[i];
  }
}






// NAM grid for/from flake climo
template <class T>
class nam_flake : public llgrid<T> {
  public:
    nam_flake();
};
template <class T>
nam_flake<T>::nam_flake() {
  this->nx = 3191;
  this->ny = 1501;
  this->dlat =  -0.0333333;
  this->dlon =  0.0333333;
  //this->firstlon =  (-161.363 + 360.);
  this->firstlon =  (-161.363 );
  this->firstlat =    75.000;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a nam_flake\n";
    cout.flush(); }

}


template <class T>
class global_20th : public llgrid<T> {
  public:
    global_20th();
};
template <class T>
global_20th<T>::global_20th() {
  float  degreefrac = 20.;
  this->nx = (int) (0.5 + 360.*degreefrac);
  this->ny = (int) (0.5 + 180.*degreefrac);
  this->dlat = -1./degreefrac;
  this->dlon =  1./degreefrac;
  this->firstlon =  this->dlon / 2.;
  this->firstlat =  90. + this->dlat / 2.;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a global_20th\n";
    cout.flush(); }

}

template <class T>
class ostia : public llgrid<T> {
  public:
    ostia();
};
template <class T>
ostia<T>::ostia() {
  float  degreefrac = 20.;
  this->nx = (int) (0.5 + 360.*degreefrac);
  this->ny = (int) (0.5 + 180.*degreefrac);
  this->dlat =  1./degreefrac;
  this->dlon =  1./degreefrac;
  this->firstlon =  this->dlon / 2.;
  this->firstlat =  - 90. + this->dlat / 2.; 
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) {cout << "failed to new an ostia\n";
    cout.flush(); }

}

template <class T>
class global_ice : public llgrid<T> {
  public:
    global_ice();
};
template <class T>
global_ice<T>::global_ice() {
  this->nx = 720;
  this->ny = 360;
  this->dlat = -0.5;
  this->dlon = 0.5;
  this->firstlon =  0.25;
  this->firstlat =  89.75;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(235);

  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) {cout << "failed to new a global_ice\n";
    cout.flush(); }

}

template <class T>
class global_sst : public llgrid<T> {
  public:
    global_sst();
};
template <class T>
global_sst<T>::global_sst() {
  this->nx = 360;
  this->ny = 180;
  this->dlat = -1.0;
  this->dlon = 1.0;
  this->firstlon =  0.5;
  this->firstlat =  89.5;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) {cout << "failed to new a global_sst\n";
    cout.flush(); }

}

/////////////////////////////////////////////////////////////////
// The following are standard functions which, however, are not internal 
//  to the class library.

//Use this function by declaring
//  unsigned char (*x)(unsigned char, unsigned char, int, int) ;
// x = &std_ice_coloring
// y.colorproc(yland, 7, 65, x);
// or just saying y.colorproc(yland, 7, 65, std_ice_coloring);
// business of rounding is for event that T is float
template <class T>
T std_ice_coloring(T conc, T sland, int cres, int cbase) {
   if ((int) (0.5 + sland) == (int) LAND) {
     return 0;
   }
   else if ((int) (0.5 + sland) == (int) COAST) {
     return 1;
   }
   else if ((int) (0.5 + conc) == NO_DATA || conc == BAD_DATA ||
            (int) (0.5 + sland) == NO_DATA ) {
     return 2;
   }
   else if ((int) (0.5 + conc) == WEATHER) {
     return 3;
   }
   else if ((int) (0.5 + conc) <= MIN_CONC) {
     return 4;
   }
   else {
     return ( 4 + (min((T)104, max((T)0, (T)conc) ) - MIN_CONC)/cres ) ;
   }

   return 0;
}


///////////////////////////
// testing:
#include "walcc.h"


template <class T>
class stlawrence : public llgrid<T> {
  public:
    stlawrence();
};
template <class T>
stlawrence<T>::stlawrence() {
  this->dlat =  1./12.;
  this->dlon =  1./12.;
  this->firstlon = 360.-70.- this->dlon/2.;
  this->firstlat = 44.0 -  this->dlat/2.; //52.0 + dlat/2.;
  this->nx = (int) (0.5 + (-52+360 - this->firstlon) /  this->dlon);
  this->ny = (int) (0.5 + (52.0+ this->dlat/2.    - this->firstlat) /  this->dlat);
//  printf("nx, ny = %d %d\n",nx, ny);
  this->cyclicx = (fabs( this->nx *  this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a stlawrence\n"; }

}

template <class T>
class okhotsk : public llgrid<T> {
  public:
    okhotsk();
};
template <class T>
okhotsk<T>::okhotsk() {
  this->dlat =  1./12.;
  this->dlon =  1./12.;
  this->firstlon = 135.0 - this->dlon/2.;
  this->firstlat =  42.0 - this->dlat/2.; 
  this->nx = (int) (0.5 + (165.0 + this->dlon/2. - this->firstlon) / this->dlon);
  this->ny = (int) (0.5 + ( 63.0 + this->dlat/2. - this->firstlat) / this->dlat);
  //printf("nx, ny = %d %d\n",nx, ny);
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a okhotsk\n"; }

}

#endif
