#include <cstdio>
#include <iostream>
using namespace std;

// Robert Grumbine 4 May 1998 -- Original Construction
//   Code maintenance, documentation, and minor additions to class
//   Robert Grumbine 13 March 2001  
// OMB C++ Library Classes: grid2_base
// A grid2_base is a 2d array of things of type T.
//   T may be essentially any type of object, including other classes
//   of object.  Certainly numeric types are included (float, int, ...).
// Operations in the class are:
//   construction/destruction
//   NO mathematics are included.  That is performed in grid_math.h
//     (object class grid2, rather than grid2_base).
//   Input and output of grid2_base's
//   Referencing elements of the array (multiple methods)
//   Logical operations for determining if:
//     the element in question is in the grid, (in)
//     the grid point is masked (mask)
//   Shifting the grid by i,j points
//   Testing for equality of two grid2_base's.
//   Getting a 2d subset of the grid
//   Magnifying the grid by a factor of N (by repetition).
//   Entering data from a 2d grid into a desired segment of the grid2_base.
// Additional notes:
//   On the whole, we'll want to use grid2's (grid_math.h) rather than
//     grid2_base's (grid_base).  The prime utility of the grid2_base
//     is to do some data management of things for which you are
//     declaring a new type (i.e. you have, say, a grid of multi-
//     parameter observations, and want to deal with the elements of
//     that grid but don't intend to do arithmetic on them).
// Efficiency notes:
//   internal to the library certain hand-optimizations are being applied
//   which make the codes slightly less readable, but aremuch more efficient.
//   1) a.xpoints() and the like are not used for loop control, rather a 
//     local variable is used.  This avoids calling .xpoints nx times
//
// Modifications
//
//  3 Dec 1998 add operator[](fijpt)
//  1999: Inline operator[], in, mask, xpoints, ypoints
//        move to nx*ny vs. nx, ny for loops
//        start move to cout from printf
//  3 Feb 2000 Some revisions for efficiency -- dropping calls to .xpoints in
//                loop control
//  20 Nov 2000
//  14 Apr 2003: add 'set' operation
//  25 May 2004: mvector transition
//  10 May 2005: iostream from iostream.h, fix endian for ftnout
//   5 Oct 2007: cerr used instead of cout for error output
//  22 May 2008: add conv(T,U) -- utility rather than member
//  10 May 2010: add magnify(mag, grid)
//   7 May 2013: add flip (j) and flop (i)

#ifndef GRID2_BASE

#define GRID2_BASE

#ifndef MVECTORH
  #include "mvector.h"
#endif
#ifndef POINTH
  #include "points.h"
#endif

//Declare the basic class for ice grids
template<class T>
class grid2_base {
//Data Section
  protected:
   int nx, ny;
   bool cyclicx, cyclicy;
  public:
   T *grid;
  private :
   grid2_base<T> *local;

  public:
//Function Section

//Constructor/destructor, initialization
   grid2_base ();
   grid2_base (const int, const int=30 );
   grid2_base (const grid2_base<T> & );
   virtual ~grid2_base ( );
   void resize(const int, const int);

   int xpoints() { return nx; }
   int ypoints() { return ny; }

   T & operator[](const fijpt &);
   T & operator[](const int &x)  { return (grid[x]) ; }
   T & operator[](const ijpt &x) { return (grid[x.i+x.j*nx]); }
   bool iscyclicx() {return cyclicx; }
   bool iscyclicy() {return cyclicy; }

   void set(const T );
   void set(const grid2_base<T> &);
   void set(const int, const int, T*);
   void strip(T *); // Convert from grid2 to a bare array

// IO on grid2_base's:
   void binout(FILE *);
   int binin(FILE *);
   int ftnin(FILE *); // change to bool from void 2 Nov 2004
   int ftnout(FILE *); // added 10 March 2001
   int read(grid2_base<T> & , FILE *);
   int read(FILE *);
   int read(char *);

// Logical grid/point operations
   int anyof(T, int, ijpt &);
   bool mask(const T flagval, const ijpt &center) { 
                    return (grid[center.i+center.j*nx] == flagval); }
   inline bool mask(const T, const fijpt &);
   inline bool in(const fijpt & );
   inline bool in(const ijpt & );
   T nonmask(grid2_base<T> &, const T , const T, const fijpt & );

// Grid2 Operators
   grid2_base<T> & operator=(const T );
   grid2_base<T> & operator=(const grid2_base<T> &);
   bool operator==(const grid2_base<T> &);

//Vector/grid operations
   void put_mvector(mvector<T> & , int );
   void get_transect(const ijpt &, const ijpt &, mvector<T> &);
   void put_transect(const ijpt &, const ijpt &, mvector<T> &);

// Logical grid operations
   grid2_base<T> & shift(const ijpt &);
   grid2_base<T> & subset(const int, const int, const int, const int);
   grid2_base<T> & subset(const ijpt &, const ijpt &);
   grid2_base<T> & magnify(const int);
   void magnify(const int, grid2_base<T> &);        // Added 10 May 2010
   int subset(grid2_base<T> &, int, int, int, int); // Added 10 March 2001
   void enter(grid2_base<T> &, int, int, int, int);  // Added 10 March 2001
   void flop(); // Added 7 May 2013
   void flip(); // Added 7 May 2013

};


//Constructor/destructor, initialization++++++++++++++++++++++++++++++++++++++++
template<class T>
T & grid2_base<T>::operator[](const fijpt &y) {
  ijpt x;
  x.i = (int) (0.5 + y.i);
  x.j = (int) (0.5 + y.j);
 
  if (in(x)) {
    return (grid[x.i+x.j*this->nx]);
  }
  else {
    #ifdef VERBOSE
      cerr << "In grid2_base::operator[], trying to use a nonexistent ";
      cerr.flush(); 
      fprintf(stderr, "grid member.  ijpt %d %d vs. %d %d\n",x.i, x.j, this->nx, this->ny);
      fflush(stderr);
    #endif
    return grid[0];
  }
}


// Copy constructor
template<class T> 
grid2_base<T>::grid2_base(const grid2_base<T> &x) { 
  int j;
  cyclicx=false; cyclicy=false;
  this->nx = x.nx;
  this->ny = x.ny;
  grid = new T[this->nx*this->ny];
  for (j = 0; j < this->ny*this->nx; j++) {
    grid[j] = x[j];
  }
  #ifdef VERBOSE
    cout << "grid2_base copy operator\n";
    cout.flush();
  #endif
  return;
}

template<class T> 
void grid2_base<T>::resize(const int n1, const int n2) {
// Resize a grid2, throw out old one
  if (grid != (T *) NULL ) {
    #ifdef VERBOSE
      cout << "Need to destroy a this->grid\n";
      cout.flush();
    #endif
    delete []grid;
  }
  #ifdef VERBOSE
    cout << "Test for need to destroy a local->grid\n";
    cout.flush();
  #endif
  if (local != (grid2_base<T> *) NULL ) {
  #ifdef VERBOSE
    cout << "Local is non-null\n";
    cout.flush();
  #endif
    delete local;
  }
  this->nx = n1;
  this->ny = n2;
  grid = new T [this->nx*this->ny];
  if (grid == (T*) NULL) {
    cout << "failed to new a grid in grid2_base::resize\n";
    cout.flush();
  }
  local = (grid2_base<T> *) NULL;
}

template<class T> 
grid2_base<T>::~grid2_base() {
  #ifdef VERBOSE
    cout << "About to destroy a grid2_base\n"; cout.flush();
  #endif
  this->nx = 0;
  this->ny = 0;
  if (grid != (T *) NULL ) delete []grid;
  #ifdef VERBOSE
    cout <<"Test for need to destroy a local->grid\n";
    cout.flush();
  #endif
  if (local != (grid2_base<T> *) NULL ) {
  #ifdef VERBOSE
    cout << "Local is non-null\n";
    cout.flush();
  #endif
    delete local;
  }
  #ifdef VERBOSE
    cout << "Destroyed a grid2_base\n";
    cout.flush();
  #endif
  return;
}

//Member initialization
template<class T> 
grid2_base<T>::grid2_base (void): nx(0),ny(0),cyclicx(0),cyclicy(0),
                grid((T *) NULL),local((grid2_base<T> *) NULL)  {
  #ifdef VERBOSE
    cout << "In grid2_base::grid2_base(void) \n";
    cout.flush();
  #endif
}
template<class T> 
grid2_base<T>::grid2_base (const int x, const int y): nx(x),ny(y),cyclicx(0),cyclicy(0),
                grid(new T[nx*ny] ),local((grid2_base<T> *) NULL)  {
  #ifdef VERBOSE
    printf("Constructed a grid2_base, nx, ny = %d %d\n", this->nx, this->ny); 
    fflush(stdout);
  #endif
}

template<class T>
grid2_base<T> & grid2_base<T>::operator=(const T val) {
  int j;
  for (j = 0; j < this->ny*this->nx; j++) {
       grid[j] =  val;
  }
  return *this;
}

template<class T>
void grid2_base<T>::set(const T val) {
  int j;
  for (j = 0; j < this->ny*this->nx; j++) {
       grid[j] =  val;
  }
  return;
}
template<class T>
void grid2_base<T>::set(const int n1, const int n2, T *x) {
  int j;
  if (! ((n1 == this->nx) && (n2 == this->ny) ) ) {
    cout << "Error, grid2 not same size as passed array!\n";
    cout.flush();
  }
  else {
    for (j = 0; j < n1*n2; j++) {
      grid[j] = x[j];
    }
  }
  return;
}

template<class T>
void grid2_base<T>::set(const grid2_base<T> &y) {
  int j;
  for (j = 0; j < this->ny*this->nx; j++) {
       grid[j] =  y.grid[j];
  }
  local = (grid2_base<T> *) NULL;
  return;
}
template<class T>
void grid2_base<T>::strip(T *other) {
  int i;
  for (i = 0; i < this->nx*this->ny; i++) {
    other[i] = this->grid[i];
  }
  return;
}


// IO on grid2_base's:++++++++++++++++++++++++++++++++++++++++++++++++
template<class T> 
int grid2_base<T>::read(char *fname) {
  int i, j;
  FILE *fin;

  fin = fopen(fname, "r");
  if (fin == (FILE *) NULL) { 
     cout << "Failed to open file ";  cout << fname; 
             cout << " grid2_base::read(char*)\n"; 
     cout.flush();
     for (j = 0; j < this->ny*this->nx; j++) {
       grid[j] =  0;
     }
     return -1; 
  }
  i =  fread(grid, sizeof(T), this->nx * this->ny, fin);
  if (i != this->nx*this->ny) {
    printf("grid2_base.read fname failed to read in full data set.\
              %d of %d\n", i, this->nx*this->ny);
    fclose(fin);
    for (j = 0; j < this->ny*this->nx; j++) {
      grid[j] =  0;
    }
    return -1;
  }
  else {
    fclose(fin);
    return i;
  }
}
template<class T> 
int grid2_base<T>::read(FILE *fin) {
  int i, j;
  
  if (fin == (FILE *) NULL) { 
    cout << "Null file pointer in grid2_base::read\n";
    cout.flush();
    for (j = 0; j < this->ny*this->nx; j++) {
      grid[j] =  0;
    }
    return -1;
  }
  if (grid == (T *) NULL) {
    cout << "Null grid pointer in grid2_base::read\n";
    cout.flush();
    grid = new T[this->nx*this->ny];
    for (j = 0; j < this->ny*this->nx; j++) {
      grid[j] =  0;
    }
  }
 
  fflush(stdout);
  i =  fread(grid, sizeof(T), this->nx * this->ny, fin);
  if (i != this->nx*this->ny) {
    fprintf(stderr, "grid2_base.read file failed to read in full data set.\
              %d of %d\n", i, this->nx*this->ny);
    fflush(stderr);
    for (j = 0; j < this->ny*this->nx; j++) {
      grid[j] =  0;
    }
    return -1;
  }
  else {
    return i;
  }
}

template<class T> 
int grid2_base<T>::read(grid2_base<T> &x, FILE *fin) {
  int i, j;

  i =  fread(x.grid, sizeof(T), x.nx * x.ny, fin);
  if (i != x.nx*x.ny) {
    printf("grid2_base.read grid, file failed to read in full data set.\
              %d of %d\n", i, this->nx*this->ny);
    for (j = 0; j < this->ny*this->nx; j++) {
      x.grid[j] =  0;
    }
    return -1;
  }
  else {
    return i;
  }
}
template <class T>
int grid2_base<T>::ftnin(FILE *fin) {
  int dummy, nr;

  if (fin == (FILE *) NULL) {
    cout << "Failed to open the input file \n";
    cout.flush();
    return 0;
  }
  fread(&dummy, sizeof(int), 1, fin); 
  nr = fread(this->grid, sizeof(T), this->nx*this->ny, fin);
  if (nr != this->nx*this->ny) {
    if (nr != 0) {
      cout << "Failed to read in the right amount of data, got " ;
      cout << nr << " instead of " << this->nx*this->ny << " items \n";
      cout.flush();
    }
    return 0;
  }
  fread(&dummy, sizeof(int), 1, fin); 
  return nr;
}
// Binary output with Fortran IEEE buffering words:
// Fortran programs can then read in an array, one array at a
//   time, with the usual unformatted read.  READ (10) x
template <class T>
int grid2_base<T>::ftnout(FILE *fout) {
  int i, nwrite = 0;
  unsigned long int l;
  unsigned char h4[4];
  if (fout == (FILE *) NULL) {
    cout << "Invalid output file \n";
    cout.flush();
    return nwrite;
  }
//Construct a header word, following wgrib
  l = sizeof (T) * this->nx *this->ny;
  for (i = 0; i < 4; i++) {
     h4[i] = l & 255;
     l     >>= 8;
  }
  #ifdef LINUX
    putc(h4[0],fout);
    putc(h4[1],fout);
    putc(h4[2],fout);
    putc(h4[3],fout);
  #else
    putc(h4[3],fout);
    putc(h4[2],fout);
    putc(h4[1],fout);
    putc(h4[0],fout);
  #endif
// Write out the array:
  nwrite = fwrite(this->grid, sizeof(T), this->nx*this->ny, fout);
  if (nwrite != this->nx*this->ny) {
    cout << "Failed to read in the right amount of data, got " ;
    cout << nwrite << " instead of " << this->nx*this->ny << " items \n";
    cout.flush();
  }
//Repeat the closing header:
  #ifdef LINUX
    putc(h4[0],fout);
    putc(h4[1],fout);
    putc(h4[2],fout);
    putc(h4[3],fout);
  #else
    putc(h4[3],fout);
    putc(h4[2],fout);
    putc(h4[1],fout);
    putc(h4[0],fout);
  #endif

  return nwrite;
}



template<class T> 
int grid2_base<T>::binin(FILE *fout) {
  int i;
// 7 April 2004: Remove setting non-existent points to zero.  Programmer
//  needs to pay attention to the return count (anyhow).
  if (this->grid == (T*)NULL) {cerr << "Null grid pointer, cannot read in\n";
                     cerr.flush(); 
                     return 0;}
  if (fout == (FILE*)NULL) {cerr << "Null file pointer, cannot read in\n";
                     cerr.flush();
      return 0;}

  i = fread(this->grid, sizeof(T), this->nx*this->ny, fout);
  if (i != this->nx*this->ny) {
     cerr << "failed to read in the full array in grid2_base::binin\n";
     cerr.flush();
     fprintf(stderr, "Read %d of %d, this->nx ny = %d %d\n", i, this->nx*this->ny, this->nx, this->ny);
     fprintf(stderr, "size of T is %d\n",(int)sizeof(T) );
     fflush(stderr);
  }
  return i;
}
template<class T> 
void grid2_base<T>::binout(FILE *fout) {
  int i;
  if (this->grid == (T*)NULL) {cerr << "Null grid pointer, cannot write out\n";
                     cerr.flush(); return;}
  if (fout == (FILE*)NULL) {cerr << "Null file pointer, cannot write out\n"; 
                     cerr.flush(); return;}
  i = fwrite(this->grid, sizeof(T), this->nx*this->ny, fout);
  if (i != this->nx*this->ny) {
     cerr << "failed to write out the full array in grid2_base::binout\n";
     cerr.flush();
    #ifdef VERBOSE
      fprintf(stderr, "grid2_base::binout failure i, nx, ny  %d %d %d\n",i, this->nx, this->ny);
      fflush(stderr);
  #endif
  }
  return;
}


// Logical grid/point operations++++++++++++++++++++++++++++++++++++++++++

template<class T>
grid2_base<T>& grid2_base<T>::shift(const ijpt &d) {
  int i, j;

  if (d.i >= 0 && d.j >= 0) {
    // case - shift up and right
    for (j = this->ny - 1 ; j > d.j ; j-- ) {
       for ( i = this->nx - 1 ; i > d.i ; i-- ) {
          grid[i+j*this->nx] = grid[ (i - d.i) + (j-d.j)*this->nx ] ;
       }
    }
  }
  else if (d.i >= 0 && d.j < 0) {
    // Future: Finished cases for grid2_base.shift
    // case - shift down and right
    //for (j = d.j ; j < ny - 1 - d.j ; j++ ) {
    //   for ( i = nx - 1 - d.i; i > d.i ; i-- ) {
    //      grid[i+j*nx] = grid[ (i - d.i) + (j+d.j)*nx ] ;
    //   }
    //}
    cout << "down and right shift not implemented yet\n";
    cout.flush();
  }
  else if (d.i < 0 && d.j < 0) {
    // case - shift down and left
    // note that since d.i and d.j are negative, we need to change signs
    for (j = 0 ; j < this->ny - 1 + d.j ; j++ ) {
       for ( i = 0 ; i < this->nx - 1 + d.i ; i++ ) {
          grid[i+j*this->nx] = (T) grid[ (i - d.i) + (j - d.j)*this->nx ] ;
       }
    }
  }
  else {
    // shift left and up
    cout << "up and left  shift not implemented yet\n";
    cout.flush();
    //for (j = ny - 1 - d.j ; j > d.j ; j-- ) {
    //   for ( i = d.i; i < nx - 1 - d.i ; i++ ) {
    //      grid[i+j*nx] = grid[ (i + d.i) + (j-d.j)*nx ] ;
    //   }
    // }
  }

  return *this;
}
template<class T> 
grid2_base<T> & grid2_base<T>::subset(const int x1, const int y1, const int x2, const int y2) {
  int i, j, indexin, indexout;

  if (local != (grid2_base<T> *) NULL) {
    cout <<"subset: need to delete local\n";
    cout.flush();
    delete local;
  }
  local = new grid2_base<T>(x2 - x1 + 1, y2 - y1 + 1);

  #ifdef VERBOSE
    printf("In subset, nx, ny = %d %d\n",this->nx, this->ny); fflush(stdout);
  #endif
  for (j = 0; j < y2 - y1 + 1; j++ ) {
    for (i = 0; i < x2-x1+1; i++) {
      indexin = (x1+i)+this->nx*(y1+j);
      indexout = i + local->nx*j;
      local->grid[indexout] = grid[indexin];
    }
  }
  #ifdef VERBOSE
    cout << "Returning local\n";
    cout.flush();
  #endif
  return *local;
}
//This variant avoids problems with memory creep:
template<class T>
int grid2_base<T>::subset(grid2_base<T> &x, int x1, int y1, int x2, int y2) {
  int i, j, indexin, indexout;
  for (j = 0; j < y2 - y1 + 1; j++ ) {
    for (i = 0; i < x2-x1+1; i++) {
      indexin = (x1+i)+x.xpoints()*(y1+j);
      indexout = i + this->nx*j;
      grid[indexout] = x.grid[indexin];
    }
  }
  return 0;
}

template<class T>
void grid2_base<T>::enter(grid2_base<T> &x, int x1, int y1, int x2, int y2) {
  int i, j;
  if (x2 - x1 > x.xpoints() - 1 ) {
     cout << "too large an x range\n";
    cout.flush();
  }
  if (y2 - y1 > x.ypoints() - 1 ) {
     cout << "too large a y range\n";
    cout.flush();
  }
  for (i = 0; i < x.xpoints(); i++) {
  for (j = 0; j < x.ypoints(); j++) {
    this->grid[x1+i + (y1+j)*this->nx] = x[i+j*x.xpoints()];
  }
  }
  return;
}


template<class T> 
grid2_base<T> &grid2_base<T>::subset(const ijpt &ll, const ijpt &ur) {
  int i, j, indexin, indexout;

  if (local != (grid2_base<T> *) NULL) {
    delete local;
  }
  local = new grid2_base<T>(ur.i-ll.i+1, ur.j - ll.j + 1);

  for (j = 0; j < ur.j - ll.j + 1; j++ ) {
    for (i = 0; i < ur.i-ll.i+1; i++) {
      indexin = (ll.i+i)+this->nx*(ll.j+j);
      indexout = i + local->nx*j;
      local->grid[indexout] = grid[indexin];
    }
  }
  return *local;
}
template<class T>
grid2_base<T> & grid2_base<T>::magnify(const int mag) {
  int i, j, k, kj;
  int indexin, indexout;

  #ifdef VERBOSE
    cout << "about to test on local being null \n";
    cout.flush();
  #endif 
  if (local != (grid2_base<T> *) NULL) {
    cout << "local is not null, trying to clear the grid\n";
    cout.flush();
    delete local;
  }
  #ifdef VERBOSE
    cout << "about to new for local\n";
    cout.flush();
  #endif 
  local = new grid2_base<T>(mag*this->nx, mag*this->ny);
  #ifdef VERBOSE
    cout << "successfully new local\n";
    cout.flush();
  #endif 

  for (j = 0; j < this->ny; j++) {
    for (i = 0; i < this->nx; i++) {
      indexin  = i + j*this->nx;
      for (kj = 0; kj < mag; kj++) {
        for (k = 0; k < mag; k++) {
          indexout = i*mag+k + local->nx*(j*mag+kj);
          local->grid[indexout] = grid[indexin];
        }
      }
    }
  }
  #ifdef VERBOSE
    cout << "about to return from grid2_base::magnify\n";
    cout.flush();
  #endif
  return *local;
}
// Added 7 May 2010
template<class T>
void grid2_base<T>::magnify(const int mag, grid2_base<T> &x) {
  int i, j, k, kj;
  int indexin, indexout;

  for (j = 0; j < this->ny; j++) {
  for (i = 0; i < this->nx; i++) {
      indexin  = i + j*this->nx;
      for (kj = 0; kj < mag; kj++) {
      for (k  = 0; k  < mag; k++) {
        indexout = i*mag+k + x.nx*(j*mag+kj);
        x.grid[indexout] = this->grid[indexin];
      }
      }
  }
  }
  #ifdef VERBOSE
    cout << "about to return from void grid2_base::magnify\n";
    cout.flush();
  #endif
}
// Added 7 May 2013
template<class T>
void grid2_base<T>::flip(void) {
  grid2_base<T> tmp(this->nx,this->ny);
  ijpt loc, tloc;

  for (loc.j = 0; loc.j < this->ny; loc.j++) {
    tloc.j = this->ny - 1 - loc.j;
  for (loc.i = 0; loc.i < this->nx; loc.i++) {
    tloc.i = loc.i;
    tmp[loc] = this->operator[](tloc);
    //printf("in flip %d %d  %d %d  %f %f\n",loc.i, loc.j, tloc.i, tloc.j,
    //  tmp[loc], this->operator[](tloc));
  }
  }
  for (loc.j = 0; loc.j < this->ny; loc.j++) {
  for (loc.i = 0; loc.i < this->nx; loc.i++) {
    this->operator[](loc) = tmp[loc];
  }
  }
  return;
}
template<class T>
void grid2_base<T>::flop(void) {
  grid2_base<T> tmp(this->nx,this->ny);
  ijpt loc, tloc;

  for (loc.j = 0; loc.j < this->ny; loc.j++) {
    tloc.j = loc.j;
  for (loc.i = 0; loc.i < this->nx; loc.i++) {
    tloc.i = this->nx - 1 - loc.i;
    tmp[loc] = this->operator[](tloc);
  }
  }
  
  for (loc.j = 0; loc.j < this->ny; loc.j++) {
  for (loc.i = 0; loc.i < this->nx; loc.i++) {
    this->operator[](loc) = tmp[loc];
  }
  }

  return;
}


// Grid/Vector Operations
template <class T>
void grid2_base<T>::put_mvector(mvector<T> & x , int i) {
// Put a mvector in to column i
  int j, xlim;
  if (x.xpoints() != this->ny ) {printf("put_mvector mismatch, %d vs %d\n", 
                                              x.xpoints(), this->ny); }
  xlim = x.xpoints();
  for (j = 0; j < xlim; j++) {
     grid[i+j*this->nx] = x[j];
  }
  return;
}

template<class T>
void grid2_base<T>::get_transect(const ijpt & x1, const ijpt & x2, mvector<T> &y) {
// Get data along a line.  Resolution is specified by the mvector.
  int i, xlim;
  float di, dj;
  ijpt outpt;

  xlim = y.xpoints();
  di = (x2.i - x1.i) / (xlim - 1);
  dj = (x2.j - x1.j) / (xlim - 1);
  for (i = 0; i < xlim; i++) {
     outpt.i = x1.i + (int) (di * i + 0.5) ;
     outpt.j = x1.j + (int) (dj * i + 0.5) ;
     y[i] = this->operator[](outpt); 
  }

  return;
}

template<class T>
void grid2_base<T>::put_transect(const ijpt & x1, const ijpt & x2, mvector<T> &y) {
// Put data from mvector into the grid, along a particular line.
  int i, xlim;
  float di, dj;
  ijpt outpt;

  xlim = y.xpoints();
  di = (x2.i - x1.i) / (xlim - 1);
  dj = (x2.j - x1.j) / (xlim - 1);
  for (i = 0; i < xlim; i++) {
     outpt.i = x1.i + (int) (di * i + 0.5) ;
     outpt.j = x1.j + (int) (dj * i + 0.5) ;
     this->operator[](outpt) = y[i];
  }

  return;
}


  


// Grid level functional operations++++++++++++++++++++++++++++++++++++++++++++

template <class T>
inline bool grid2_base<T>::in(const fijpt  &x) {
// Return true if fijpt is inside the grid
// Robert Grumbine 5/12/97
// Modify: True if within 1/2 grid point (will round to the grid)
// Modify: Permit cyclic grids 3/25/99
  bool tmp=true;
  
  if ( !cyclicx ) {
    tmp = tmp && (x.i < (float) this->nx - 0.5 ) && (x.i > -0.5);
  }
  if ( !cyclicy  ) {
    tmp = tmp && (x.j < (float) this->ny - 0.5 ) && (x.j > -0.5);
  }
  return tmp;
} 

template <class T>
inline bool grid2_base<T>::in(const ijpt  &x) {
// Return true if ijpt is inside the grid
// Derived from in(fijpt)
// Robert Grumbine 6/24/99
  bool tmp=true;

  if ( !cyclicx ) {
    tmp = tmp && (x.i < this->nx ) && (x.i >= 0);
  }
  if ( !cyclicy ) {
    tmp = tmp && (x.j < this->ny ) && (x.j >= 0);
  }
  return tmp;
}
    

template<class T>
inline bool grid2_base<T>::mask(const T flagval, const fijpt  &x) {
// Return true if any of the points adjacent are mask points
// Round the location
// Also return true if any of the adjacent points are off the grid!
  fijpt p1, p2((T) 0.,(T) 1.), p3((T) 1.,(T) 1.), p4((T) 1.,(T) 0.);
  bool retcode = false;

  p1.i = (int) x.i ; p1.j = (int) x.j ;
  if (in(p1)) { retcode = (retcode || (this->operator[](p1) == flagval)) ;}
    else { return true; }

  p2 += p1; 
  if (in(p2)) { retcode = (retcode || (this->operator[](p2) == flagval)) ;}
    else {return true; }

  p4 += p1;
  if (in(p4)) { retcode = (retcode || (this->operator[](p4) == flagval)) ;}
    else {return true; }

  p3 += p1; 
  if (in(p3)) { retcode = (retcode || (this->operator[](p3) == flagval)) ;}
    else {return true; }

  return retcode;

} 


template<class T>
int grid2_base<T>::anyof(T type, int range, ijpt  &center){
//Debug -- not necessarily handling boundaries properly.
  int count = 0, imin, jmin, imax, jmax;
  ijpt tloc;

  imin = max(0, center.i - range);
  imax = min(this->nx-1, center.i + range);
  jmin = max(0, center.j - range);
  jmax = min(this->ny-1, center.j + range);
  #ifdef VERBOSE
    printf("anyof input %d %d  %d\n",center.i, center.j, range);
    printf("limits i %d %d  j %d %d\n", imin, imax, jmin, jmax);
  #endif

  for ( tloc.j = jmin; tloc.j <= jmax; tloc.j++) {
  for ( tloc.i = imin; tloc.i <= imax; tloc.i++) {
     #ifdef VERBOSE2
       printf("anyof %d %d\n",tloc.i, tloc.j); fflush(stdout);
     #endif
     if (this->operator[](tloc) == type ) { count += 1 ; }
  }
  }

  #ifdef VERBOSE
     printf("count in anyof = %d\n",count);
  #endif
  return count;
}

template <class T>
T grid2_base<T>::nonmask(grid2_base<T> & mask, const T flagval, const T nonval, const fijpt  &center) {
// Return the nearest non-masked value.  This is exceedingly
//   ugly due to the casewise nature of the problem/
  T dum = nonval;
  T v1, v2, v3, v4;
  fijpt loccenter;
  ijpt loc1, loc2, loc3, loc4;
  bool mask1, mask2, mask3, mask4;
  float tdist, dist1, dist2, dist3, dist4;
  float fx, fy, fxy, deltax, deltay;
  float w1=0., w2=0., w3=0., w4=0.;
  int yes = (1==1), set;
//

  v1 = 0.;
  v2 = 0.;
  v3 = 0.;
  v4 = 0.;
  if (this->nx != mask.nx || this->ny != mask.ny) {
    cout << "Mask file and data file are not from the same type of grid!\n";
    cout << "  Results will be bad or worse!!\n";
    cout.flush();
  }

//Preliminary work:
  loccenter.i = center.i;
  loccenter.j = center.j;
  if (loccenter.i < 0. && this->iscyclicx() ) {
    loccenter.i += this->xpoints() ;
  }
  if (loccenter.i > this->xpoints() - 1 && this->iscyclicx() ) {
    loccenter.i -= this->xpoints() ;
  }


  loc1.i = (int) loccenter.i;
  loc1.j = (int) loccenter.j;
  if (loc1.i > this->xpoints() - 1 && this->iscyclicx()) {
    loc1.i -= this->xpoints() - 1 ;
  }
  mask1 = mask.mask(flagval, loc1);

  loc2.i = loc1.i + 1;
  if (loc2.i > this->xpoints() - 1 && this->iscyclicx()) {
    loc2.i -= this->xpoints() - 1 ;
  }
  loc2.j = loc1.j;
  mask2 = mask.mask(flagval, loc2);

  loc3.i = loc1.i;
  loc3.j = loc1.j + 1;
  if (loc3.j > this->ypoints() - 1 && this->iscyclicy()) {
    loc3.i -= this->ypoints() - 1 ;
    mask3 = mask.mask(flagval, loc3);
  }
  else {
    mask3 = true;
  }

  loc4.i = loc1.i + 1;
  if (loc4.i > this->xpoints() - 1 && this->iscyclicx()) {
    loc4.i -= this->xpoints() - 1 ;
  }
  loc4.j = loc1.j + 1;
  if (loc4.j > this->ypoints() - 1 && this->iscyclicy()) {
    loc4.i -= this->ypoints() - 1 ;
    mask4 = mask.mask(flagval, loc4);
  }
  else {
    mask4 = true;
  }

//Most trivial case, all points are masked:
  #ifdef VERBOSE
  printf("maskcount %1d %d %d %d %d\n",mask1+mask2+mask3+mask4, mask1, mask2, mask3, mask4);
  #endif
  if (mask1 && mask2 && mask3 && mask4 ) {
    dum =  nonval; 
    return dum;
  }
//Straightforward case, all points are not masked:
  if (!mask1 && !mask2 && !mask3 && !mask4) {
    v1 =  this->operator[](loc1);
    v2 =  this->operator[](loc2);
    v3 =  this->operator[](loc3);
    v4 =  this->operator[](loc4);
    #ifdef OLDVERSION
    dist1 = (loc1.i-loccenter.i)*(loc1.i-loccenter.i) +
            (loc1.j-loccenter.j)*(loc1.j-loccenter.j);
    dist2 = (loc2.i-loccenter.i)*(loc2.i-loccenter.i) +
            (loc2.j-loccenter.j)*(loc2.j-loccenter.j);
    dist3 = (loc3.i-loccenter.i)*(loc3.i-loccenter.i) +
            (loc3.j-loccenter.j)*(loc3.j-loccenter.j);
    dist4 = (loc4.i-loccenter.i)*(loc4.i-loccenter.i) +
            (loc4.j-loccenter.j)*(loc4.j-loccenter.j);
    dist1 = sqrt(dist1);
    dist2 = sqrt(dist2);
    dist3 = sqrt(dist3);
    dist4 = sqrt(dist4);
    if (fabs(dist1) < 1.e-6 ) {
      dum = v1;
      return dum;
    }
    else if (fabs(dist2) < 1.e-6 ) {
      cout << "very near dist2\n";
    cout.flush();
      dum = v2;
      return dum;
    }
    else if (fabs(dist3) < 1.e-6 ) {
      cout << "very near dist3\n";
    cout.flush();
      dum = v3;
      return dum;
    }
    else if (fabs(dist4) < 1.e-6 ) {
      cout << "very near dist4\n";
    cout.flush();
      dum = v4;
      return dum;
    }
    dum  = v1/dist1 ;
    dum += v2/dist2 ;
    dum += v3/dist3 ;
    dum += v4/dist4 ;
    tdist = 1./dist1 + 1./dist2 + 1./dist3 + 1./dist4;
    dum = dum / tdist;
    // end of old version
    #else
    deltax = center.i - loc1.i; if (deltax > 1) {
        cout << "wraparound, help!\n";
    cout.flush();
    }
    deltay = center.j - loc1.j; 
    fxy = v3 + v2 - (v1 + v4);
    fy  = v3 - v1 - deltax*fxy;
    fx  = v2 - v1 - deltay*fxy;
    dum = v1 + deltax*fx + deltay*fy + deltax*deltay*fxy;
    #endif
     
    return dum;
  }

//Otherwise, compute distances and see who is unmasked and near:
  set = !yes;

//4 cases in which only one point is unmasked:
  if (!mask1 && mask2 && mask3 && mask4 ) {
    dum =  this->operator[](loc1); set = yes;
  }
  else if (mask1 && !mask2 && mask3 && mask4 ) {
    dum =  this->operator[](loc2); set = yes;
  }
  else if (mask1 && mask2 && !mask3 && mask4 ) {
    dum =  this->operator[](loc3); set = yes;
  }
  else if (mask1 && mask2 && mask3 && !mask4 ) {
    dum =  this->operator[](loc4); set = yes;
  }
  if (set) return dum;

// In the remaining cases, must settle by distance (6 cases for 2 pts unmasked,
//    4 cases for 3 pts unmasked.  The function won't be called if
//    all four points are unmasked.
    v1 =  this->operator[](loc1);
    v2 =  this->operator[](loc2);
    v3 =  this->operator[](loc3);
    v4 =  this->operator[](loc4);
  dist1 = (loc1.i-loccenter.i)*(loc1.i-loccenter.i) + 
          (loc1.j-loccenter.j)*(loc1.j-loccenter.j);
  dist2 = (loc2.i-loccenter.i)*(loc2.i-loccenter.i) + 
          (loc2.j-loccenter.j)*(loc2.j-loccenter.j);
  dist3 = (loc3.i-loccenter.i)*(loc3.i-loccenter.i) + 
          (loc3.j-loccenter.j)*(loc3.j-loccenter.j);
  dist4 = (loc4.i-loccenter.i)*(loc4.i-loccenter.i) + 
          (loc4.j-loccenter.j)*(loc4.j-loccenter.j);
  dist1 = sqrt(dist1);
  dist2 = sqrt(dist2);
  dist3 = sqrt(dist3);
  dist4 = sqrt(dist4);
  if (!mask1 && v1 != flagval ) w1 = 1.;
  if (!mask2 && v2 != flagval ) w2 = 1.;
  if (!mask3 && v3 != flagval ) w3 = 1.;
  if (!mask4 && v4 != flagval ) w4 = 1.;
  if ( (w1 + w2 + w3 + w4) == 0.) {
    dum = nonval;
    return dum;
  }

  //Direct from weights, will drop casewise business, note that we're in
  // trouble if any of the dists is == 0.  1 June 1999 
  if (fabs(dist1) < 1.e-6 && !mask1) {
    dum = v1;
    return dum;
  }
  else if (fabs(dist2) < 1.e-6 && !mask2) {
    cout << "very near dist2\n";
    cout.flush();
    dum = v2;
    return dum;
  }
  else if (fabs(dist3) < 1.e-6 && !mask3) {
    cout << "very near dist3\n";
    cout.flush();
    dum = v3;
    return dum;
  }
  else if (fabs(dist4) < 1.e-6 && !mask4) {
    cout << "very near dist4\n";
    cout.flush();
    dum = v4;
    return dum;
  }
  else if (dist1 < 1.e-6 || dist2 < 1.e-6 || dist3 < 1.e-6 || dist4 < 1.e-6) {
    dum = nonval;
    return dum;
  }

  dum  = v1*w1/dist1 ;
  dum += v2*w2/dist2 ;
  dum += v3*w3/dist3 ;
  dum += v4*w4/dist4 ;
  tdist = w1/dist1 + w2/dist2 + w3/dist3 + w4/dist4;
  dum = dum / tdist;

  #ifdef VERBOSE
  if ( (dum == nonval) || (dum > 1.e6) || (dum < -10.0) ) {
    printf("new method returning %f, tdist = %6.2f w1 w2 w3 w4 %2.0f %2.0f %2.0f %2.0f  %6.2f %6.2f %6.2f %6.2f  %1d %1d %1d %1d\n",
      dum, tdist, w1, w2, w3, w4, v1, v2, v3, v4, mask1, mask2, mask3, mask4);
  }
  #endif

  return dum;

}

//          ///////////////////////////////Define grid2_base Operators:

template <class T>
grid2_base<T> & grid2_base<T>::operator=(const grid2_base<T> &x) {
  int j;
  #ifdef VERBOSE
    cout << "In grid2_base operator = \n";
    cout.flush();
  #endif
  if (this->nx != x.nx || this->ny != x.ny ) {
    cout << "size mismatch in operator=\n";
    cout.flush();
    if (this->nx == 0 && this->ny == 0 ) {
      cout << "Need to resize from grid2_base operator=, currently size 0\n";
    cout.flush();
      this->resize(x.nx, x.ny);
      if (grid == (T*)NULL) {
        cout << "failed to create grid space\n";
    cout.flush();
      }
      this->local = (grid2_base<T> *) NULL;
    }
    else {
      printf("Error in = , sizes don't match %d %d vs %d %d\n",this->nx, this->ny,
                  x.nx, x.ny);
    }
  }
//Note that memcpy is vastly more efficient, but doesn't work 
  for (j = 0; j < this->ny*this->nx; j++ ) {
      grid[j] = x.grid[j] ;
  }
  //this->local = (grid2_base<T> *) NULL;
  cyclicx=x.cyclicx; cyclicy=x.cyclicy;
  return *this;
}


// Begin logical functions
template <class T>
bool grid2_base<T>::operator==(const grid2_base<T> &y) {
  bool truth = true;
  int j;
  for (j = 0; j < this->ny*this->nx; j++ ) {
      truth = truth && ( grid[j] == y.grid[j] ) ;
  }
  return truth;
}


// Utility functions, not class members:
template <class T, class U>
void conv(grid2_base<T> &x, grid2_base<U> &y) ;

template <class T, class U>
void conv(grid2_base<T> &x, grid2_base<U> &y) {
  ijpt loc;
  for (loc.j = 0; loc.j < y.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints() ; loc.i++) {
     y[loc] = (U)x[loc];
  }
  }
  return;
}


#endif
