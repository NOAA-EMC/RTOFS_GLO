// Class suitable for manipulating grids which are specified
//  by the coordinates of their points, rather than by functions.
// Robert Grumbine 27 March 2003
// Structure is:
//    Resops:  Base class; includes the bulk of the work
//       -> hycom       Specialized for hycom .a, .b format
//       -> cofstype    COFS grid version
// 5 April 2005
//       -> readin      Read in lats, lons separately
// 9 July 2007: 'thisification', internal refs now are this->

// Managing the resops class:
//
//Resops itself
//  Declare a base class resops (inheriting from metricgrid)
//  Provide the required locate functions returning nulls, as there
//    cannot be any direct resop class, but metricgrid will insist.
//  Virtual function locate(ijpt, lat, lon), etc. -- require the
//    descendants to have these classes.
//
//In any descendant class:
//  Provide lat, lon, dlatdi, dlondi, dlatdj, dlondj, jacob;
//  include a protected static int count
//  Make these static within the descendant (problematic within families,
//    but at least distinguishes between families)
//  Inline calls from the mandatory metricgrid locates to the resops
//     versions (with appropriate args)

#ifndef RESOPSH
#define RESOPSH
#include <cstdlib>
#include "metric.h"

// Note that arcdis returns km.  Should be changed.  RG 20050412
#ifndef ARCDIS_READY
  #ifdef ABSOFT
    extern "C" float arcdis(float &long1, float &lat1, float &long2,
                            float &lat2 );
    #define ARCDIS arcdis
  #elif IBM
    extern "C" float arcdis(float &long1, float &lat1, float &long2,
                            float &lat2 );
    #define ARCDIS arcdis
  #else
    extern "C" float arcdis_(float &long1, float &lat1, float &long2,
                             float &lat2 );
    #define ARCDIS arcdis_
  #endif
#endif

/////////////////////////////////////////////////////////
// Geometry is a friend function to compute the local derivatives of
//  lat and lon w.r.t. i,j, and the jacobian of the transformation 
template <class T>
void geometry(grid2<T> &lat, grid2<T> &lon, 
         grid2<T> &dlatdi, grid2<T> &dlondi, 
         grid2<T> &dlatdj, grid2<T> &dlondj, 
         grid2<T> &jacob, grid2<T> &dx, grid2<T> &dy) {
  ijpt loc, locip, locjp, locim, locjm, ip, jp, im, jm;

  grid2<T> temporary(dx.xpoints(), dx.ypoints() );

  #ifdef VERBOSE
    cout << "Entered geometry\n"; cout.flush();
  #endif

  ip.i = 1; ip.j = 0;
  jp.i = 0; jp.j = 1;
  im.i = -1; im.j =  0;
  jm.i =  0; jm.j = -1;

// d/di terms:
  for (loc.j = 0; loc.j < lat.ypoints()  ; loc.j++) {
  for (loc.i = 0; loc.i < lat.xpoints()-1; loc.i++) {
    locip = loc;  locip += ip;
    dlatdi[loc] = lat[locip] - lat[loc];
    dlondi[loc] = lon[locip] - lon[loc];
    dx[loc] = 1000.*ARCDIS(lon[loc],lat[loc],lon[locip],lat[locip]);
  }
  }
  loc.i = lat.xpoints() - 1;
  for (loc.j = 0; loc.j < lat.ypoints()  ; loc.j++) {
    locim = loc;  locim += im;
    dlatdi[loc] = lat[loc] - lat[locim];
    dlondi[loc] = lon[loc] - lon[locim];
    dx[loc] = 1000.*ARCDIS(lon[loc],lat[loc],lon[locim],lat[locim]);
  }

// d/dj terms
  for (loc.j = 0; loc.j < lat.ypoints()-1; loc.j++) {
  for (loc.i = 0; loc.i < lat.xpoints()  ; loc.i++) {
    locjp = loc;  locjp += jp;
    dlatdj[loc] = lat[locjp] - lat[loc];
    dlondj[loc] = lon[locjp] - lon[loc];
    dy[loc] = 1000.*ARCDIS(lon[loc],lat[loc],lon[locjp],lat[locjp]);
  }
  }
  loc.j = lat.ypoints() - 1;
  for (loc.i = 0; loc.i < lat.xpoints()  ; loc.i++) {
    locjm = loc;  locjm += jm;
    dy[loc] = 1000.*ARCDIS(lon[loc],lat[loc],lon[locjm],lat[locjm]);
    dlatdj[loc] = lat[loc] - lat[locjm];
    dlondj[loc] = lon[loc] - lon[locjm];
  }

  #ifdef VERBOSE2
    palette<unsigned char> gg(19, 65);
    char fname[9];
    temporary = dx;
    temporary.scale();
    sprintf(fname,"dx.xpm");
    temporary.xpm(fname, 7,gg);
    temporary = dy;
    temporary.scale();
    sprintf(fname,"dy.xpm");
    temporary.xpm(fname,7,gg);
  #endif

  

// Compute Jacobian of the transformation and inspect for zero values
  for (loc.j = 0; loc.j < lat.ypoints()  ; loc.j++) {
  for (loc.i = 0; loc.i < lat.xpoints()  ; loc.i++) {
    jacob[loc] = dlondi[loc] * dlatdj[loc] - dlatdi[loc]*dlondj[loc];
    if (jacob[loc] == 0.0) {
      fprintf(stderr, "Found an identically zero jacobian at %d %d\n",loc.i,loc.j);
      //cout << "Aborting\n"; cout.flush();
      //exit(1);
    }
    #ifdef VERBOSE2
      if (jacob[loc] < 7.5e-4) {
        printf("%d %d  %e jacob\n", loc.i, loc.j, jacob[loc] );
      }
    #endif
  }
  }
    
  #ifdef VERBOSE
  printf("Jacobian min, max, average, rms %f %f %f %f\n",jacob.gridmin(),
          jacob.gridmax(), jacob.average(), jacob.rms() );
  #endif

} // end of 'geometry'

///////////////////////////////////////////////////////////////////
// Now define the resops class itself.  The only thing it really
//    does is define how to execute the locate(latpt ) with the 
//    required geometric information 
template <class T>
class resops : public metricgrid<T> {
protected:
    bool cyclicx;
// routine to handle the computation of basic geometric quantities
   friend void geometry<T>(grid2<T> &lat, grid2<T> &lon, 
                   grid2<T> &dlatdi, grid2<T> &dlondi, 
                   grid2<T> &dlatdj, grid2<T> &dlondj, 
                   grid2<T> &jacob, grid2<T> &dx, grid2<T> &dy) ;
   fijpt& locate(const latpt &, grid2<float> &, grid2<float> &, 
                          grid2<float> &, grid2<float> &, 
                          grid2<float> &, grid2<float> &, 
                          grid2<float> & ) ;
   // Note that the resops fields here are _not_ static.  This is so that
   // we can do things like subset without trampling on the originals
   grid2<float> lat, lon;
   grid2<float> dlatdi, dlondi, dlatdj, dlondj, jacob, dx, dy;
   float maxlat, minlat, maxlon, minlon;
public:
   resops(void) ;
   resops(const resops<T>&);
   void subset(resops<T> &, float, float, float, float); //subset in ll-space
   latpt locate(const ijpt &);
   latpt locate(const fijpt &);
   fijpt& locate(const latpt &x) { return resops<T>::locate(x,
                          lat, lon, dlatdi, dlondi, dlatdj, dlondj, jacob) ; }
   bool iscyclicx() { return cyclicx;}
   float firstlon() { return minlon;}

};
// Initialize the statics
grid2<float> nullgrid() {
  grid2<float> x;
  return x;
}

template<class T>
resops<T>::resops() {
}
template<class T>
resops<T>::resops(const resops<T> &x) {
  int i;
  if (x.xpoints() != this->nx || x.ypoints() != this->ny ) {
    printf("resops grids are different sizes, %d vs %d and %d vs %d\n",
              x.xpoints(), this->nx, x.ypoints(), this->ny);
    exit(1);
  }
  lat = x.lat;
  lon = x.lon;
  dlatdi = x.dlatdi;
  dlatdj = x.dlatdj;
  dlondi = x.dlondi;
  dlondj = x.dlondj;
  for (i = 0; i < this->nx*this->ny; i++) {
    this->grid[i] = x[i];
  }
}
template <class T>
void resops<T>::subset(resops<T> &x, 
                       float north, float south, float east, float west) {
// Subset in ll-space
  latpt ll, ur, lr, ul;
  ijpt ill, iur, loc, tloc, sloc;
  fijpt fll, fur, flr, ful;
  int xnx, xny, itmp;
  //int mini, maxi, minj, maxj;
  float fmini, fmaxi, fminj, fmaxj;
  bool flopped = false;

  if (west * east < 0) {
    west += 360.;
    east += 360.;
  }
  ll.lat = max(minlat,south);
  ll.lon = west;
  lr.lat = max(minlat,south);
  lr.lon = east;
  ur.lat = min(maxlat,north);
  ur.lon = east;
  ul.lat = min(maxlat,north);
  ul.lon = west;

  flr = this->locate(lr, lat, lon, dlatdi, dlondi, dlatdj, dlondj, jacob);
  fur = this->locate(ur, lat, lon, dlatdi, dlondi, dlatdj, dlondj, jacob);
  fll = this->locate(ll, lat, lon, dlatdi, dlondi, dlatdj, dlondj, jacob);
  ful = this->locate(ul, lat, lon, dlatdi, dlondi, dlatdj, dlondj, jacob);

  fmini = ( min(min(min(fll.i, flr.i), fur.i),ful.i));
  fmaxi = ( max(max(max(fll.i, flr.i), fur.i),ful.i));
  fminj = ( min(min(min(fll.j, flr.j), fur.j),ful.j));
  fmaxj = ( max(max(max(fll.j, flr.j), fur.j),ful.j));

  ill.i = (int) (0.5 + fmini);
  ill.j = (int) (0.5 + fminj);
  iur.i = (int) (0.5 + fmaxi);
  iur.j = (int) (0.5 + fmaxj);
  // Unless all the corner points are inside the grid, use the
  //   whole grid.  This avoids various pathologies with arbitrary
  //   grids.
  if (ill.i != 0 && ill.j != 0 && iur.i != 0 && iur.j != 0) {
    // run with subset already defined
    cout << "running with subset grid\n";
    cout.flush();
  }
  else {
    // run with entire grid
    ill.i = 0;  ill.j = 0;
    iur.i = this->nx -1, iur.j = this->ny-1;
    cout << "subset running with full grid\n";
    cout.flush();
  }

  if (iur.i < ill.i) {
    if (this->iscyclicx() ) {
      printf("Flopping i %d for %d\n",ill.i, iur.i);
      //branch for negative ll.i
      ill.i -= this->nx;//note that this is nx of the main grid 
      flopped = true;
    }
    else {
      itmp = ill.i;
      ill.i = iur.i;
      iur.i = itmp;
    }
  }
  if (iur.j < ill.j) {
    printf("Flopping j %d for %d\n",ill.j, iur.j);
    itmp = ill.j;
    ill.j = iur.j;
    iur.j = itmp;
  }
  if (iur.i == ill.i || iur.j == ill.j) {
    cout << "Cannot subset, merely equating the two\n";
    cout.flush();
    x = *this;
    return ;
  } 
  xnx = iur.i - ill.i + 1;
  xny = iur.j - ill.j + 1;
  
  #ifdef VERBOSE
  printf("subsetting %d by %d  corners %d %d  %d %d\n",xnx, xny, 
             ill.i, ill.j, iur.i, iur.j);
  fflush(stdout);
  #endif
  x.resize(xnx, xny);
  x.lat.resize(xnx, xny);
  x.lon.resize(xnx, xny);
  x.dlatdi.resize(xnx, xny);
  x.dlatdj.resize(xnx, xny);
  x.dlondi.resize(xnx, xny);
  x.dlondj.resize(xnx, xny);
  x.jacob.resize(xnx, xny);
  x.dx.resize(xnx, xny);
  x.dy.resize(xnx, xny);
  // Now copy over the information for the appropriate points:
  for (sloc.j = ill.j; sloc.j <= iur.j; sloc.j++) {
    tloc.j = sloc.j - ill.j;
    for (loc.i = ill.i; loc.i <= iur.i; loc.i++) {
      tloc.i = loc.i - ill.i;
      if (loc.i < 0) {
        sloc.i = loc.i + this->nx; 
      }
      else { 
        sloc.i = loc.i; 
      }
      x[tloc]        = this->operator[](sloc);
      x.lat[tloc]    = this->lat[sloc];
      x.dlatdi[tloc] = this->dlatdi[sloc];
      x.dlatdj[tloc] = this->dlatdj[sloc];
      x.dlondi[tloc] = this->dlondi[sloc];
      x.dlondj[tloc] = this->dlondj[sloc];
      x.jacob[tloc]  = this->jacob[sloc];
      x.dx[tloc]  = this->dx[sloc];
      x.dy[tloc]  = this->dy[sloc];
      if (west > this->minlon) {
        x.lon[tloc]    = this->lon[sloc];
      }
      else {
// Handle a case where the subset grid is west of the main, but still
//  truly a subset -- translate the far east to the far west.
        if (this->lon[sloc] > east) {
          x.lon[tloc] = this->lon[sloc] - 360.;
        }
        else {
          x.lon[tloc]    = this->lon[sloc];
        }
      }

    }
  }
  x.maxlat = x.lat.gridmax();
  x.maxlon = x.lon.gridmax();
  x.minlat = x.lat.gridmin();
  x.minlon = x.lon.gridmin(); 
  
  float del;
  ijpt locl, locr;
  locr.j = 0;
  locl.j = 0;
  locl.i = 0;
  locr.i = xnx - 1;
  del = x.lon[locr]; if (del >= 360.) del -= 360.;
  del -= x.lon[locl];
  del -=  360.*( (int)(del/360.)) ;
  if (fabs(del) <= fabs(x.dlondi[locl]) || fabs(del) <= fabs(x.dlondi[locr]) ) {
    x.cyclicx = true;
  }
  else {
    x.cyclicx = false;
  }

  #ifdef VERBOSE
    printf("cyclicity del %f vs %f %f and %f %f  %d\n", del, 
                x.lon[locl], x.lon[locr],
                x.dlondi[locl], x.dlondi[locr], (int) x.cyclicx); 
    printf("jacobian max, min, average %f %f %f from %f %f %f\n",
      x.jacob.gridmax(), x.jacob.gridmin(), x.jacob.average(),
      this->jacob.gridmax(), this->jacob.gridmin(), this->jacob.average() );
    printf("subset max min lat lon %f %f  %f %f\n",x.maxlat, x.minlat, 
                               x.maxlon, x.minlon);
    fflush(stdout);
    cout << "Leaving subsetter\n" << flush; 
  #endif

}

template<class T>
latpt resops<T>::locate(const ijpt &x) {
  latpt y;
  y.lat = lat[x];
  y.lon = lon[x];
  return y;
}
template<class T>
latpt resops<T>::locate(const fijpt &fx) {
  ijpt x;
  x.i = (int) (0.5 + fx.i);
  x.j = (int) (0.5 + fx.j);
  return this->locate(x);
}
template <class T>
fijpt& resops<T>::locate(const latpt &x, grid2<float> &lat, grid2<float> &lon,
                          grid2<float> &dlatdi, grid2<float> &dlondi, 
                          grid2<float> &dlatdj, grid2<float> &dlondj, 
                          grid2<float> &jacob) {
  fijpt retloc;
  latpt fll, refll;
  int iter, itmax = 12;
  float delta, dlat, dlon, toler = 5.e-4;
  ijpt loc;

  #ifdef VERBOSE3
    cout << "Entered resops locate of llpt\n" << flush;
  #endif
  #ifdef VERBOSE3
    printf("max min lat lon %e %e  %e %e\n",this->maxlat, this->minlat,
                   this->maxlon, this->minlon);
  #endif

// Pre-emptive check for the point being on the grid.  Might be excessive
//   to insist on not at all out of range, as oppose to more than 1 delta
//   23 April 2003 Robert Grumbine
  refll = x;
  //if (maxlat != lat.gridmax() ) {
  if (fabs(maxlat) < 1.e-9 || fabs(minlat) < 1.e-9 || 
      fabs(maxlon) < 1.e-9 || fabs(minlon) < 1.e-9) {
    //cout << "maxlat got reset\n" << flush;
    this->maxlat = lat.gridmax();
    this->maxlon = lon.gridmax();
    this->minlat = lat.gridmin();
    this->minlon = lon.gridmin();
  }  
  
  if (refll.lat > this->maxlat || refll.lat < this->minlat) {
    #ifdef VERBOSE
    cout << "Pre-emptive from resops.locate(latpt) maxlat violation  ";
    cout.flush();
    printf("lat lon %f %f vs. %f %f\n",refll.lat, refll.lon, 
             this->minlon, this->maxlon);
    #endif
    retloc.i = -1;
    retloc.j = -1;
    global_fijpt = retloc;
    return global_fijpt;
  }

  while (refll.lon < this->minlon && refll.lon < this->maxlon) {
    refll.lon += 360.;
  }
  while (refll.lon > this->minlon && refll.lon > this->maxlon) {
    refll.lon -= 360.;
  }

  if (!this->iscyclicx() && 
       (refll.lon < this->minlon || refll.lon > this->maxlon) ) {
    if (refll.lon - 360. > this->minlon && refll.lon - 360. < this->maxlon) {
      printf("continuing with an adjusted longitude, %f vs. %f\n",
         refll.lon-360., refll.lon);
      refll.lon -= 360.;
    }
    else {
      #ifdef VERBOSE
      cout << "Pre-emptive from resops.locate(latpt)  ";
    cout.flush();
      printf("lat lon %f %f vs. %f %f\n",refll.lat, refll.lon, 
               this->minlon, this->maxlon);
      #endif
      retloc.i = -1;
      retloc.j = -1;
      global_fijpt = retloc;
      return global_fijpt;
    }
  }

  if (refll.lon < this->minlon ) refll.lon += 360.;
  if (refll.lon > this->maxlon ) refll.lon -= 360.;
  retloc.i = this->nx/2.;
  retloc.j = this->ny/2.;
  loc = retloc;
  fll.lat  = lat[loc];
  fll.lon  = lon[loc];
  fll.lon += dlondi[loc]*(retloc.i - loc.i);
  fll.lon += dlondj[loc]*(retloc.j - loc.j);
  fll.lat += dlatdi[loc]*(retloc.i - loc.i);
  fll.lat += dlatdj[loc]*(retloc.j - loc.j);
  dlat = refll.lat - fll.lat;
  dlon = refll.lon - fll.lon;

//Must handle case of point being off grid.
  //dlat = lat[retloc] - refll.lat;
  //dlon = lon[retloc] - refll.lon;
  if (dlon < -180.) dlon += 360.;
  if (dlon >  180.) dlon -= 360.;

  iter = 0;
  while (iter < itmax && (fabs(dlat) > toler || fabs(dlon) > toler) &&
         jacob[loc] != 0) {
    // The use of delta as an intermediate and limiting the magnitude of
    // changes instituted 26 September 2003 by Robert Grumbine for
    // the purpose of preventing iterations from leaping outside the
    // grid domain.
    // Test on jacobian != 0 added 3 Jan 2017
    delta = -(-dlat*dlondi[loc] + dlon*dlatdi[loc]) / jacob[loc];
    
    if (delta < 0 && (-delta > 0.9*retloc.j) )              delta /= 1.4;
    if (delta > 0 && ( delta > 0.9*(this->ny - retloc.j)) ) delta /= 1.4;
    retloc.j += delta;
    retloc.j = min(retloc.j, this->ny-1 + 0.4999);
    retloc.j = max(retloc.j, -0.4999);

    delta = -(-dlon*dlatdj[loc] + dlat*dlondj[loc]) / jacob[loc];
    if ( (delta < 0 && (-delta > 0.9*retloc.i)     )  || 
         (delta > 0 && (delta > 0.9*(this->nx - retloc.i)) ) ) delta /= 1.4;
    retloc.i += delta ;
    #ifdef VERBOSE
    if (iter > itmax/2) {
      printf("retloc.i %f dlon dlat %f %f dlatdj dlondj %f %f\n",
        retloc.i, dlon, dlat, dlatdj[loc], dlondj[loc] );
    }
    #endif
    // See about permitting wrap around for when the extrapolation is
    // over a bounding point 
    if (retloc.i >= this->nx - 1 + 0.5 || retloc.i < -0.5 ) {
      if (this->iscyclicx() ) { 
        if (retloc.i  >= this->nx - 1 + 0.5) {
          retloc.i -= (this->nx - 0.51);
        }
        else {
          retloc.i += (this->nx-0.51);
        }
      }
      else {
        if (retloc.i  > this->nx - 1 + 0.4999) {
          float tlon;
          tlon = minlon + dlondi[loc]*retloc.i - 360.;
          if (tlon < maxlon && tlon > minlon) {
            retloc.i = (tlon - minlon)/dlondi[loc];
          }
          else {
            retloc.i = min(retloc.i, this->nx-1 + 0.4999);
          }
        }
        else {
          float tlon;
          tlon = minlon + dlondi[loc]*retloc.i + 360.;
          if (tlon < maxlon && tlon > minlon) {
            retloc.i = (tlon - minlon)/dlondi[loc];
          }
          else {
            retloc.i = max(retloc.i, -0.4999);
          }
        }
      }
    } // testing retloc.i out of bounds
    /////////////////////
    #ifdef VERBOSE
    if (iter > itmax/2) {
      printf("b retloc.i %f dlon dlat %f %f dlatdj dlondj %f %f\n",
        retloc.i, dlon, dlat, dlatdj[loc], dlondj[loc] );
    }
    #endif

    if (retloc.i >= this->nx - 0.5) {
      retloc.i = this->nx - 0.5;
    }
    loc.i = (int) (0.5 + retloc.i);
    if (this->iscyclicx() && loc.i >= this->nx) loc.i -= this->nx;
    loc.j = (int) (0.5 + retloc.j);
    if (loc.i >= this->nx) {
      fprintf(stderr, "Insert bad language here, aborting ");
      fprintf(stderr, "loc = %d %d %f %f\n", loc.i, loc.j, retloc.i, retloc.j);
      fprintf(stderr, "lat-lon = %f %f\n",refll.lat, refll.lon);
      fprintf(stderr, "iterative scheme for finding point from resops(latpt) has failed\n");
      fflush(stderr);
      //exit(1);
    } // test on nx
      

    #ifdef VERBOSE2
      printf("%f %f  %d %d\n",retloc.i, retloc.j, loc.i, loc.j); fflush(stdout);
    #endif
    fll.lat  = lat[loc];
    fll.lon  = lon[loc];

    fll.lon += dlondi[loc]*(retloc.i - loc.i);
    fll.lon += dlondj[loc]*(retloc.j - loc.j);
    fll.lat += dlatdi[loc]*(retloc.i - loc.i);
    fll.lat += dlatdj[loc]*(retloc.j - loc.j);

    dlat = refll.lat - fll.lat;
    dlon = refll.lon - fll.lon;
    if (dlon < -180.) dlon += 360.;
    if (dlon >  180.) dlon -= 360.;
    #ifdef VERBOSE2
      printf("iter %2d %7.3f %7.3f  %8.3f %8.3f  %8.3f %8.3f\n",iter, 
              dlat, dlon, refll.lat, refll.lon, fll.lat, fll.lon); fflush(stdout);
    #endif

    iter += 1;
    #ifdef VERBOSE
    if (iter >= itmax/2) { 
    //if (iter >= 0) { 
      printf("%2d %d %d %7.3f %7.3f  %10.5f %10.5f %10.5f  %f %f\n",
            iter, loc.i, loc.j, retloc.i, retloc.j, 
            dlon, dlat, sqrt(dlat*dlat+dlon*dlon),dlondi[loc],dlatdj[loc] ); 
      fflush(stdout);
    }
    #endif
  }

  if (iter == itmax) {
    #ifdef VERBOSE
    printf("Failed: %f %f %f %f %f jacobian = %e\n",refll.lon, refll.lat, 
                          retloc.i, retloc.j, sqrt(dlat*dlat+dlon*dlon),
                          jacob[retloc]);
    #endif
    // Try (22 April 2003) going with best estimate anyhow
    if (sqrt(dlat*dlat+dlon*dlon) > 0.01*fabs(jacob[retloc]) ) {
      //printf("still too large, returning -1,-1 %f %f %f\n", dlat, dlon,
      //      sqrt(dlat*dlat+dlon*dlon) );
      retloc.i = -1;
      retloc.j = -1;
    }
    global_fijpt = retloc;
    return global_fijpt;
  }

  #ifdef VERBOSE2
    printf("retloc %3d %8.3f %8.3f  %12.5e %12.5e %12.5e\n",iter, retloc.i, retloc.j, 
                     dlat, dlon, sqrt(dlat*dlat+dlon*dlon) ); fflush(stdout);
  #endif
  global_fijpt = retloc;
  return global_fijpt;
}
/////////////////////////////////////////////////////////
// Instantiate a particular sort of resops -- the hycom grid
/////////////////////////////////////////////////////////
template <class T>
class hycom : public resops<T> {
  protected: 
    // Note that this declaration of lat, lon, etc. shadows those
    //  in the resops base class.  That's intentional so that we
    //  don't trample the resops in subsetting
    static int count;
    static grid2<float> lat, lon;
    static grid2<float> dlatdi, dlondi, dlatdj, dlondj, jacob, dx, dy;
    static int pad_size;
    static float pad_value;
    static mvector<float> padding;
  public:
    hycom(void);
    void outa(FILE *);
    void outb(FILE *);
    void outb(FILE *, char *message);
    void ina(FILE *fin);
    void inb(FILE *fin);

    latpt locate(const ijpt &);
    latpt locate(const fijpt &);
    fijpt& locate(const latpt &x) { return resops<T>::locate(x, 
                          lat, lon, dlatdi, dlondi, dlatdj, dlondj, jacob) ; } 
// Not generally available in metric.h, yet, but we'll put here for now:
// Invoking grid returns ddx, ddy, respectively, and is aware of flag values
   void grad(hycom<T> &, hycom<T> &, float);
// Divergence is returned in the invoking grid:
   void div(hycom<T> &, hycom<T> &);
   fijpt& advect(fijpt &, fijpt &); //14 June 2005 given starting point 
                 // and grid-referenced dx,dy, find new fijpt location
// Added to get more accurate values at floating locations:
   T accurate(fijpt &);
};
template<class T> int hycom<T>::count = 0;
template<class T> int hycom<T>::pad_size = 0;
template<class T> float hycom<T>::pad_value = 0;
template<class T> mvector<float> hycom<T>::padding = nullvec();
template<class T> grid2<float> hycom<T>::lat = nullgrid();
template<class T> grid2<float> hycom<T>::lon = nullgrid();
template<class T> grid2<float> hycom<T>::dlatdi = nullgrid();
template<class T> grid2<float> hycom<T>::dlondi = nullgrid();
template<class T> grid2<float> hycom<T>::dlatdj = nullgrid();
template<class T> grid2<float> hycom<T>::dlondj = nullgrid();
template<class T> grid2<float> hycom<T>::jacob = nullgrid();
template<class T> grid2<float> hycom<T>::dx = nullgrid();
template<class T> grid2<float> hycom<T>::dy = nullgrid();

 
template<class T>
T hycom<T>::accurate(fijpt &x) {
  ijpt ix;
  ix = x;
  T tmp = this->operator[](ix);
  tmp += (x.i - ix.i)*(this->operator[](ix.i + 1 + ix.j*this->nx) - 
                       this->operator[](ix.i + ix.j*this->nx)       );
  tmp += (x.j - ix.j)*(this->operator[](ix.i + this->nx + ix.j*this->nx) - 
                       this->operator[](ix.i + ix.j*this->nx)       );
  return tmp; 
}

template<class T>
latpt hycom<T>::locate(const ijpt &x) {
  latpt y;
  y.lat = lat[x];
  y.lon = lon[x];
  return y;
}
template<class T>
latpt hycom<T>::locate(const fijpt &fx) {
  ijpt x; fijpt dx;
  latpt tll;
  x.i = (int) (0.5 + fx.i);
  x.j = (int) (0.5 + fx.j);
  tll = this->locate(x);
  dx.i = fx.i;
  dx.j = fx.j;
  dx.i -= x.i;
  dx.j -= x.j;
  tll.lat += dlatdi[x]*dx.i + dlatdj[x]*dx.j;
  tll.lon += dlondi[x]*dx.i + dlondj[x]*dx.j;  
  //return this->locate(x);
  return tll;
}

//14 June 2005 given grid-referenced dx,dy, find new fijpt location
template<class T>
fijpt& hycom<T>::advect(fijpt &x, fijpt &delx) {
  fijpt di;

  di.i = delx.i / dx[x] ;
  di.j = delx.j / dy[x] ;

  global_fijpt = x;
  global_fijpt += di;
  
  return global_fijpt;
}

#define HYCOMLINELIM 900
template<class T>
void hycom<T>::outb(FILE *foutb) {
  fprintf(foutb, "%5d   'idm   ' = longitudinal array size\n", this->nx);
  fprintf(foutb, "%5d   'jdm   ' = latitudinal  array size\n", this->ny);
  fprintf(foutb, "    0   'mapflg' = map flag (-1=unknown,0=mercator,2=uniform,4=f-plane)\n");
  fprintf(foutb, "mask:  min,max = %f %f\n",this->gridmin(), this->gridmax() );
}
template<class T>
void hycom<T>::outb(FILE *foutb, char *message) {
  fprintf(foutb, "%5d   'idm   ' = longitudinal array size\n", this->nx);
  fprintf(foutb, "%5d   'jdm   ' = latitudinal  array size\n", this->ny);
  fprintf(foutb, "    5   'mapflg' = map flag (-1=unknown,0=mercator,2=uniform,4=f-plane)\n");
  fprintf(foutb, "%s:  min,max = %f %f\n",message,this->gridmin(), this->gridmax() );
}
template<class T>
void hycom<T>::outa(FILE *fouta) {
  this->binout(fouta);
  padding.binout(fouta);
}
template<class T>
void hycom<T>::ina(FILE *fina) {
  this->binin(fina);
  padding.binin(fina);
}
template<class T>
void hycom<T>::inb(FILE *finb) {
  char line[HYCOMLINELIM];
  while (!feof(finb) ) {
    fgets(line, HYCOMLINELIM, finb);
    printf("%s",line);
    fflush(stdout);
  }
}


template <class T>
//debug 21 apr 2010 hycom<T>::hycom<T>(void) {
hycom<T>::hycom(void) {

  if (count == 0) {
    FILE *fina, *finb;
    char line[HYCOMLINELIM];
    #ifdef VERBOSE
      cout << "Entered the count = 0 branch\n" << flush;
    #endif
    // Here is where the reading of the .a and .b files of lat-long locations
    // should be
//NOTE Extract for read/write
    fina = fopen("fort.061a","r");
    finb = fopen("fort.61","r");
    if (fina == (FILE *) NULL) {
      cout << "Failed to open the fort.061a file\n" << flush;
      exit(1);
    }
    if (finb == (FILE *) NULL) {
      cout << "Failed to open the fort.61 file\n" << flush;
      exit(1);
    }

  // Get nx, ny
    fgets(line, HYCOMLINELIM, finb);
    sscanf(line," %d ",&(this->nx) );
    fgets(line, HYCOMLINELIM, finb);
    sscanf(line," %d ",&(this->ny) );
    fclose (finb);

  // Compute the padding-related terms
    pad_size = ((this->nx*this->ny + 4095)/4096)*4096 - this->nx*this->ny;
    padding.resize(pad_size);
    pad_value = pow(2.0, 100.0);

  // Size and read in the lat, lon grids
    lat.resize(this->nx, this->ny);
    lon.resize(this->nx, this->ny);
  
    lon.binin(fina);
    padding.binin(fina);
    lat.binin(fina);
    padding.binin(fina);
    fclose (fina);

    #ifdef VERBOSE2
      lat.printer(stdout);
    #endif

  // Size and derive the geometric grids
    dlatdi.resize(this->nx, this->ny);
    dlondi.resize(this->nx, this->ny);
    dlatdj.resize(this->nx, this->ny);
    dlondj.resize(this->nx, this->ny);
    jacob.resize(this->nx, this->ny);
    dx.resize(this->nx, this->ny);
    dy.resize(this->nx, this->ny);
    geometry(lat, lon, dlatdi, dlondi, dlatdj, dlondj, jacob, dx, dy) ;
    resops<T>::lat = lat;
    resops<T>::lon = lon;
    resops<T>::dlatdi = dlatdi;
    resops<T>::dlatdj = dlatdj;
    resops<T>::dlondi = dlondi;
    resops<T>::dlondj = dlondj;
    resops<T>::jacob = jacob;
    resops<T>::dx    = dx;
    resops<T>::dy    = dy;

  }

  this->maxlat = lat.gridmax();
  this->maxlon = lon.gridmax();
  this->minlat = lat.gridmin();
  this->minlon = lon.gridmin(); 
  if (this->minlon * this->maxlon < 0.) {
    this->minlon += 360.;
    this->maxlon += 360.;
    lon += 360.;
  }
  #ifdef VERBOSE
  printf("bounding values %f %f and %f %f\n",this->maxlat, this->minlat, this->maxlon, this->minlon);
  #endif

  // note that this must be down here because we don't know nx, ny until after
  // reading format files.
  this->nx = lat.xpoints();
  this->ny = lat.ypoints();
  this->grid = new T[this->nx*this->ny];
  count += 1;

  float del;
  ijpt locl, locr;
  locr.j = 0;
  locl.j = 0;
  locl.i = 0;
  locr.i = this->nx - 1;
  del = lon[locr]; if (del >= 360.) del -= 360.;
  del -= lon[locl];
  del -=  360.*( (int)(del/360.)) ;
  if (fabs(del) <= fabs(dlondi[locl]) || fabs(del) <= fabs(dlondi[locr]) ) {
    this->cyclicx = true;
  }
  else {
    this->cyclicx = false;
  }

  #ifdef VERBOSE
  printf("cyclicity del %f vs %f %f and %f %f  %d\n", del, lon[locl], lon[locr],
              dlondi[locl], dlondi[locr], (int) this->cyclicx); fflush(stdout);
  #endif
    

}
template<class T>
void hycom<T>::grad(hycom<T> &ddx, hycom<T> &ddy, float flagval) { 
  ijpt loc;
  int indexb;

  #ifdef VERBOSE
  printf("in gradient, dx max, min, average = %f %f %f\n",this->dx.gridmax(), 
                                this->dx.gridmin(), this->dx.average() );
  printf("in gradient, dy max, min, average = %f %f %f\n",this->dy.gridmax(), 
                                this->dy.gridmin(), this->dy.average() );
  printf("in gradient, jacob max, min, average = %e %e %e\n",this->jacob.gridmax(), 
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
void hycom<T>::div(hycom<T> &ddx, hycom<T> &ddy) {
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

//////////////////////////////////////
template <class T>
class readin : public resops<T> {
  protected:
    static int count;
    static grid2<float> lat, lon;
    static grid2<float> dlatdi, dlondi, dlatdj, dlondj, jacob, dx, dy;
  public:
    readin(void);
    readin(grid2<float>&, grid2<float>&);
    T jacobian(const ijpt &loc) { return this->jacob[loc]; }
    T cellarea(const ijpt &loc) { return this->dx[loc]*this->dy[loc]; }
    latpt locate(const ijpt &);
    latpt locate(const fijpt &);
   fijpt& locate(const latpt &x) { 
         return resops<T>::locate(x,
                   lat, lon, dlatdi, dlondi, dlatdj, dlondj, jacob) ; 
    }
// Not generally available in metric.h, yet, but we'll put here for now:
// Invoking grid returns ddx, ddy, respectively, and is aware of flag values
   void grad(readin<T> &, readin<T> &, float);
// Divergence is returned in the invoking grid:
   void div(readin<T> &, readin<T> &);
//   void laplacean(resops<T> &);
};
template<class T> int readin<T>::count = 0;
template<class T> grid2<float> readin<T>::lat = nullgrid();
template<class T> grid2<float> readin<T>::lon = nullgrid();
template<class T> grid2<float> readin<T>::dlatdi = nullgrid();
template<class T> grid2<float> readin<T>::dlondi = nullgrid();
template<class T> grid2<float> readin<T>::dlatdj = nullgrid();
template<class T> grid2<float> readin<T>::dlondj = nullgrid();
template<class T> grid2<float> readin<T>::jacob = nullgrid();
template<class T> grid2<float> readin<T>::dx     = nullgrid();
template<class T> grid2<float> readin<T>::dy     = nullgrid();

template <class T>
//debug 4/21/2010 readin<T>::readin<T>(void) {
readin<T>::readin(void) {
  cout << "should not be here -- readin type resops grids should be created by two arguments\n" << flush;
  return;
}   
template <class T>
//debug 4/21/2010 readin<T>::readin<T>(grid2<float> &tlat, grid2<float> &tlon) {
readin<T>::readin(grid2<float> &tlat, grid2<float> &tlon) {
  this->nx = tlat.xpoints();
  this->ny = tlat.ypoints();
    
  this->grid = new T[this->nx*this->ny];

  if (count == 0) {
    // Only initialize the statics if this is the first pass
    lat.resize(this->nx, this->ny);
    lon.resize(this->nx, this->ny);
    dlatdi.resize(this->nx, this->ny);
    dlondi.resize(this->nx, this->ny);
    dlatdj.resize(this->nx, this->ny);
    dlondj.resize(this->nx, this->ny);
    jacob.resize(this->nx, this->ny);
    dx.resize(this->nx, this->ny);
    dy.resize(this->nx, this->ny);

    lat = tlat;
    lon = tlon;
    geometry(lat, lon, dlatdi, dlondi, dlatdj, dlondj, jacob, dx, dy);

    this->maxlat = lat.gridmax();
    this->maxlon = lon.gridmax();
    this->minlat = lat.gridmin();
    this->minlon = lon.gridmin();
  } 
  count += 1;
  return;
    
}  
template<class T>
void readin<T>::grad(readin<T> &ddx, readin<T> &ddy, float flagval) { 
  ijpt loc;
  int indexb;

  #ifdef VERBOSE
  printf("in gradient, dx max, min, average = %f %f %f\n",this->dx.gridmax(), 
                                this->dx.gridmin(), this->dx.average() );
  printf("in gradient, dy max, min, average = %f %f %f\n",this->dy.gridmax(), 
                                this->dy.gridmin(), this->dy.average() );
  printf("in gradient, jacob max, min, average = %e %e %e\n",this->jacob.gridmax(), 
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
void readin<T>::div(readin<T> &ddx, readin<T> &ddy) {
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

template<class T>
latpt readin<T>::locate(const ijpt &x) {
  latpt y;
  y.lat = lat[x];
  y.lon = lon[x];
  return y;
}
template<class T>
latpt readin<T>::locate(const fijpt &fx) {
  ijpt x;
  x.i = (int) (0.5 + fx.i);
  x.j = (int) (0.5 + fx.j);
  return this->locate(x);
}


#endif
