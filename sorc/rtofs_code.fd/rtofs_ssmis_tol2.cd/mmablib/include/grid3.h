#include <cstdio>
#include <cmath>

// Grid class for working with 3d grids
// Robert Grumbine Oldest Extant 24 Oct 1997
//
// Modifications:
//   03 Dec 1997: use .xpoints instead of .nx
//   17 Jun 1998: inline some functions, add operator=grid3, -=, *=
//   29 Mar 1999: inline more functions, collapse indices (k=1,nx*ny*nz vs
//                   1,nx; 1,ny; 1,nz)  The latter, especially, makes for a
//                   big increase in speed,
//                go to 'new' from malloc
//   24 Jun 1999: test for successful binout, rename printout to printer for
//                agreement with grid_base etc.
//   25 Apr 2002: Add some testing for conformability in doing grid3 assignment
//   13 Feb 2003: Collapse loop in grid assignment
//   02 Apr 2004: go to mvector from vector
//   12 Jun 2007: Thisification
//   15 May 2008: Member initializations, full declaration of includes needed

#ifndef GRID3H

#define GRID3H
  #ifndef MVECTORH
    #include "mvector.h"
  #endif
  #ifndef POINTH
    #include "points.h"
  #endif
  #ifndef COLORH
    #include "color.h"
  #endif

  #ifndef GRID2_BASE
    #include "grid_base.h"
  #endif

  #ifndef GRIDH
    #include "grid_math.h"
  #endif

// Declare the class grid3, which is an array of grid2s
template<class T>
class grid3 : public grid2<T> {
  public:
    int nz;
    grid3();
    grid3(grid3<T> &);
    grid3(int, int = 1, int = 1);
    int zpoints() { return this->nz; }
    T &operator[](ijpt &x) { return this->grid[ x.i + x.j*this->nx + x.k*this->nx*this->ny] ; }
    T &operator[](int x) { return this->grid[x]  ; }

    void set(T );
    void binout(FILE *fout) { int j; 
          j = fwrite(this->grid, sizeof(T), this->nx*this->ny*this->nz, fout); 
          if (j != this->nx*this->ny*this->nz) {
            printf("Error %d of %d written %ld bytes each\n",
                 j, this->nx*this->ny*this->nz, sizeof(T) ); 
          }
        }
   
    void binin(FILE *fin) { fread(this->grid, sizeof(T), this->nx*this->ny*nz, fin); }
    void printer(FILE *);

    inline mvector<T> get_sounding(ijpt &);
    inline void get_sounding(ijpt &, mvector<T> &);
    inline void put_sounding(ijpt &, mvector<T> &);    
    // Future: args being floating points (lateral interpolation to be done)

    grid2<T> get_layer(int );
    void get_layer(int , grid2<T> &);
    void put_layer(int , grid2<T> &);

    void get_transect(ijpt, ijpt, grid2<T> &);
    // Future: put_transect, 
    // Future: arg as fijpt
    grid3<T> & operator=(grid3<T> &) ;
    void operator+=(grid3<T> &) ;
    void operator-=(grid3<T> &) ;
    void operator/=(T );
    void operator*=(T );

};

template<class T>
void grid3<T>::get_transect(ijpt x1, ijpt x2, grid2<T> &transect) {
  //particular interpretation is that x1 and x2 are x,y
  //  coordinates of start/stop points for the transect.
  //The transect object itself is assumed to have dimensions,
  //  and the nx dimension is taken to be number of observing
  //  points along the transect.  ny is verified to be equal
  //  to nz of the grid3.
  //Does not assume any relation between x1, x2 (i.e, x1 may
  //  not be the lower left corner).
  int i;
  float di, dj;
  ijpt y;
  mvector<T> soundvec(nz);

  if (transect.ypoints() != this->nz) { 
    cout << "Transect and grid3 mismatch "; cout << transect.ypoints(); 
    cout << " vs "; cout << this->nz; cout << "\n";
    cout.flush();
    return;
  }
  di = (x2.i - x1.i)/(transect.xpoints()-1);
  dj = (x2.j - x1.j)/(transect.xpoints()-1);
  for (i = 0; i < transect.xpoints(); i++) { 
    y.i = x1.i + lrint(di * i );
    y.j = x1.j + lrint(dj * i );
    get_sounding(y , soundvec);
    transect.put_mvector(soundvec, i);
  }

  return;
}

template<class T>
inline void grid3<T>::get_sounding(ijpt &x, mvector<T> &sound) {
  int k;
  for (k = 0; k < this->nz; k++) {
    sound[k] = this->grid[k*this->nx*this->ny+x.j*this->nx + x.i];
  }
}

template<class T>
inline mvector<T> grid3<T>::get_sounding(ijpt &x) {
  int k;
  mvector<T> sound(nz);
  for (k = 0; k < this->nz; k++) {
    sound[k] = this->grid[k*this->nx*this->ny+x.j*this->nx + x.i];
  }
  return sound;
}

template<class T>
inline void grid3<T>::put_sounding(ijpt &x, mvector<T> &sound) {
  int k;
  for (k = 0; k < this->nz; k++) {
    this->grid[k*this->nx*this->ny+x.j*this->nx + x.i] = sound[k] ;
  }
  return ;
}

template<class T>
void grid3<T>::set(T val) {
  int k;
  for (k = 0; k < this->nz*this->ny*this->nx; k++) {
         this->grid[k] = (T) val;
  }
  return;
}
template<class T>
grid3<T>::grid3(grid3<T> &x): nz(x.nz) {
//grid3<T>::grid3(grid3<T> &x) {
  //nz = x.nz;
  this->nx = x.nx;
  this->ny = x.ny;
  this->grid = new T[this->nx*this->ny*nz]; 
  for (int k = 0; k < this->nz*this->ny*this->nx; k++) {
    this->grid[k] = x[k];
  }
  return;
}
template<class T>
grid3<T>::grid3(void):nz(0) {
//grid3<T>::grid3(void) {
  //nz = 0;
  this->nx = 0;
  this->ny = 0;
  this->grid = (T *) NULL;
  return;
}
template<class T>
grid3<T>::grid3(int x, int y, int z):nz(z) {
//grid3<T>::grid3(int x, int y, int z) {
  //nz = z;
  this->nx = x;
  this->ny = y;
  this->grid = new T[this->nx*this->ny*nz];
  if (this->grid == NULL) { cout << "Failed to new a grid3\n";
    cout.flush(); return;}
  return;
}
template<class T>
void grid3<T>::put_layer(int k, grid2<T> &x) {
  int j;
  if ( (this->nx != x.xpoints() ) || (this->ny != x.ypoints() ) ) {
    cout << "mismatched grid3 mismatch "; cout << this->nx; cout << " "; 
             cout << x.xpoints() ;
    cout << "  and "; cout << this->ny; cout << " "; cout << x.ypoints(); 
             cout << "\n";
    cout.flush();
  }
  if (k > this->nz) {
    cout << "layer number out of range in grid3::put_layer\n";
    cout.flush();
  }

  for (j = 0; j < this->ny*this->nx; j++) {
    this->grid[k*this->nx*this->ny + j ] = x[j] ;
  }

}
template<class T>
grid2<T> grid3<T>::get_layer(int k) {
  grid2<T> x(this->nx, this->ny);
  int j;
  for (j = 0; j < this->ny*this->nx; j++) {
     x[j] = this->grid[ k*this->nx*this->ny + j ];
  }
  return x;
}
template<class T>
void grid3<T>::get_layer(int k, grid2<T> &x) {
  int j;
  for (j = 0; j < this->ny*this->nx; j++) {
       x[j] = this->grid[ k*this->nx*this->ny + j ];
  }
}

template<class T>
void grid3<T>::printer(FILE *fout) {
  int k;
  grid2<T> tempor(this->nx, this->ny);
  for (k = 0; k < this->nz; k++) {
    get_layer(k, tempor);
    tempor.printer(fout);
  }
}

template<class T>
grid3<T> & grid3<T>::operator=(grid3<T> &y) {
  int k;
  #ifdef VERBOSE
    cout << "In grid3 = \n";
    cout.flush(); 
  #endif
  for (k = 0; k < this->nz*this->ny*this->nx; k++ ) {
    this->grid[k] = y[k];
  }
  return *this;
}
  
template<class T>
void grid3<T>::operator-=(grid3<T> &y) {
  int k;
  #ifdef VERBOSE
    cout << "In grid3 -= \n";
    cout.flush(); 
  #endif
  for (k = 0; k < this->nz*this->ny*this->nx; k++ ) {
    this->grid[k] -= y[k];
  }

  return ;
}
template<class T>
void grid3<T>::operator+=(grid3<T> &y) {
  int k;
  #ifdef VERBOSE
    cout << "In grid3 += \n";
    cout.flush(); 
  #endif
  for (k = 0; k < this->nz*this->ny*this->nx; k++ ) {
    this->grid[k] += y[k];
  }

  return ;
}

template<class T>
void grid3<T>::operator*=(T y) {
  int k;
  for (k = 0; k < this->nz*this->ny*this->nx; k++ ) {
    this->grid[k] *= y;
  }
  return ;
}
template<class T>
void grid3<T>::operator/=(T y) {
  int k;
  for (k = 0; k < this->nz*this->ny*this->nx; k++ ) {
    this->grid[k] /= y;
  }
  return ;
}

#endif
