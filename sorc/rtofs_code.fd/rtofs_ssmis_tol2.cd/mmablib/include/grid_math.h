#include <cstdio>
#include <limits>
#include <climits>
#include <cfloat>
#include <cmath>
//Declare a class for 2d grids on which one may do mathematics.  This
//  now inherits from the grid2_base class.
//Robert Grumbine 
//  Oldest on hand 17 June 1998, must be one of oldest in actual writing
//
//Modifications:
//    3 Dec 1998: Add flag values, copy constructor
//    4 Oct 1999: Work on resize, reduce, move to new from malloc
//   20 Jun 2001: Add colorproc, move xpoints references outside loop control
//   10 May 2005: (by then) add flagged rms, max, min
//   10 May 2005 Add const copy constructor
//   23 Sep 2005: ifndef GRIDH added
//    9 Jul 2007: 'thisification' -- this-> references to variables rather than bare references
//    4 Feb 2009: member initialization

#ifndef GRIDH

#define GRIDH

#ifndef GRID2_BASE
  #include "grid_base.h"
#endif

#ifndef COLORH
  #include "color.h"
#endif

template<class T>
class grid2 : public grid2_base<T> {
    
//Data Section
  private:
    grid2<T> *local;

  public:
//Constructor/destructor, initialization
   grid2();
   grid2(const int , const int = 30);
   grid2(grid2<T> &);
   grid2(const grid2<T> &);
   
   virtual ~grid2();

// Grid2 Operators
// X= operators
   grid2<T> & operator+=(const grid2<T>&);
   grid2<T> & operator-=(const grid2<T>&);
   grid2<T> & operator*=(const grid2<T>&);
   grid2<T> & operator/=(const grid2<T>&);
   grid2<T> & operator+=(const T  );
   grid2<T> & operator-=(const T );
   grid2<T> & operator*=(const T );
   grid2<T> & operator/=(const T );

// Now simple binary operators
   grid2<T>& operator+(const grid2<T>&);
   grid2<T>& operator-(const grid2<T>&);
   grid2<T>& operator*(const grid2<T>&);
   grid2<T>& operator/(const grid2<T>&);

   grid2<T> & operator+(const  T );
   grid2<T> & operator-(const  T );
   grid2<T> & operator*(const  T );
   grid2<T> & operator/(const  T );

   grid2<T>& operator=(grid2_base<T> &) ;
   grid2<T>& operator=(const grid2<T> &) ;
   grid2<T>& operator=(const T ) ;

// Grid level functional operations
   T gridmax();
   T gridmin();
   T average();
   T rms();
   T average(const T ); //exclude flagged points from average
   T gridmax(const T );
   T gridmin(const T );
   T rms(const T );
   void laplace(grid2<T> &);
   void gradsq(grid2<T> &);
   grid2<T>& laplace();
   grid2<T>& gradsq();
   void scale();
   void scale(T, T);
   void grib_scale(const int &, int & , grid2<float> &);  // Do grib scaling

// IO on grid2's:
   void printer(FILE *);  //assumes conversion to float exists for all T's
   void reader(FILE *);   //assumes data written by printer
   void xpm(char *, int , grid2<T> &, palette<unsigned char> );
   void xpm(char *, int , palette<unsigned char> );
   void colorproc(grid2<T> &, int, int, T (*x)(T, T, int, int)) ; //Function to accept
                //a second grid and two integers as arguments, and then operate
                //on every element of invoking grid.  Origin is from the color
                //table.  Last argument is a function which returns a T.  Declare as:
                //unsigned char (*x)(unsigned char, unsigned char, int, int) ;
                // x = &fun1; -- where fun1 is a function with these arguments
// Logical grid operations
   void crit(T  , grid2<T> & );
   void crit(T  );
   grid2<T>& reduce(grid2_base<T> & );
   grid2<T>& reduce(grid2_base<T> &, grid2_base<T> &, T maskval );

};

// Constructor/destructor:
template<class T>
grid2<T>::grid2(const grid2<T> &x) {
  int j;
  this->nx = x.nx;
  this->ny = x.ny;
  if (x.grid != (T *) NULL) {
    if (this->grid == (T *) NULL) {
      this->grid = new T[this->nx*this->ny];
    }
    if (this->nx+this->ny != 0) {
      for (j = 0; j < this->ny*this->nx; j++) {
         this->grid[j] = x.grid[j];
      }
    }
  }
  else {
    this->grid = (T *) NULL;
  }
  local = (grid2<T> *) NULL;
}
template<class T>
grid2<T>::grid2(grid2<T> &x) {
  int j;
  this->nx = x.nx;
  this->ny = x.ny;

  if (x.grid != (T *) NULL) {
    if (this->grid == (T *) NULL) {
      this->grid = new T[this->nx*this->ny];
    }
    if (this->nx+this->ny != 0) {
      for (j = 0; j < this->ny*this->nx; j++) {
         this->grid[j] = x.grid[j];
      }
    }
  }
  else {
    this->grid = (T *) NULL;
  }

  local = (grid2<T> *) NULL;
}
// Member Initialization
template<class T>
grid2<T>::grid2(void): local((grid2<T> *) NULL)  {
 //Grid2 does nothing new from grid2_base
  #ifdef VERBOSE
    cout << "In grid2::grid2(void) \n";
    cout.flush();
  #endif
}

template<class T>
grid2<T>::grid2(int n1, int n2): local((grid2<T> *) NULL)  {
  if (this->nx == 0 && this->ny == 0) {
     #ifdef VERBOSE
         cout <<"Calling grid2_base resizer from grid2\n";
    cout.flush();
     #endif
     grid2_base<T>::resize(n1, n2); //Note that this is actually invoking 
     //the grid2_base constructor, which is 
     //what we want, but may look a little odd
  }
}

template<class T>
grid2<T>::~grid2() {
  #ifdef VERBOSE
    cout << "In ::~grid2\n";
    cout.flush();
  #endif
  if (local != (grid2<T> *) NULL) {
    #ifdef VERBOSE
      cout << "Destroying the grid2::local\n";
    cout.flush();
    #endif
    delete local;
  }
}

// IO on grid2's:++++++++++++++++++++++++++++++++++++++++++++++++

template<class T> 
void grid2<T>::printer(FILE *fout) {
  int i, j;
  for (j = 0; j < this->ny; j++) {
    for (i = 0; i < this->nx; i++) {
      // Future : Griddable types need a float() operator
      // Future : alternatively, need to use iostream and appropriate >> 
      //             operator.
      fprintf(fout, "%8d %8d %f\n", i, j, (float) this->grid[i+j*this->nx] );
    }
  }

  return;
}
template<class T> 
void grid2<T>::reader(FILE *fout) {
  int i, j, di, dj;
  float tempor;
  #ifdef VERBOSE
    printf("In reader, nx, ny = %d %d\n",this->nx, this->ny);
  #endif
  for (j = 0; j < this->ny; j++) {
    for (i = 0; i < this->nx; i++) {
      // Future : Griddable types need a float() operator
      // Future : alternatively, need to use iostream and appropriate >> 
      //             operator.
      fscanf(fout, "%d %d %f\n", &di, &dj, &tempor);
      this->grid[i+j*this->nx] = (T) tempor ;
    }
  }

  return;
}

template <class T>
void grid2<T>::colorproc(grid2<T> &yland, int cres, int cbase,
  T (*x)(T, T, int, int) ) {

  int i;
  for (i = 0; i < this->nx*this->ny; i++) {
     this->grid[i] = x(this->grid[i], yland[i], cres, cbase);
  }

}

/* Convert from a raster map to an .xpm (X pixmap) formatted file.  Probably
     several uses, but the one that prompts it is to create a file exportable
     to the www.
   Robert Grumbine 28 July 1995
*/
template<class T>
void grid2<T>::xpm(char *fname, int cres, grid2<T> & sland, palette<unsigned char> gg) {

  int i, j, index;
  FILE *sout;

  sout = fopen(fname, "w");
  if (sout == NULL) {
    cout << "failed to open xpm output file, " << fname << endl;
    cout.flush();
    return ;
  }

/* XPM header */ 
  fprintf(sout,"/* XPM */\n");
  fprintf(sout,"static char * %s [] = {\n",fname);
  fprintf(sout,"\"%d %d %d %d\",\n",this->nx, this->ny, gg.ncol , 1);
  fprintf(sout,"/* columns rows colors chars-per-pixel */\n"); //9 May 2008
  for (j = 0; j <  gg.ncol ; j++) {
    fprintf(sout, "\"%c c #%02x%02x%02x\",\n", (char) (j+ gg.cbase), 
            (int) gg.pal[j ].i, (int) gg.pal[ j ].j, 
            (int) gg.pal[ j ].k );  
  }
  fprintf(sout,"/* pixels */\n"); //9 May 2008

/* Print out the pix map */
  for (j = this->ny - 1; j >= 0; j-- ) {
    fprintf(sout,"\"");
    for (i = 0; i < this->nx; i++) {
      index = i + j*this->nx;
      fprintf(sout,"%c", (int)this->grid[index] / cres + gg.cbase);
    }
    fprintf(sout,"\",\n");
  }
  fprintf(sout," }; \n");
  fflush(sout);
  fclose(sout);
    
  return;

}
template<class T>
void grid2<T>::xpm(char *fname, int cres, palette<unsigned char> gg) {

  int i, j, index;
  FILE *sout;

  sout = fopen(fname, "w");
  if (sout == NULL) {
    cout << "failed to open xpm output file\n" << fname << endl;
    cout.flush();
    return ;
  }

/* XPM header */ 
  fprintf(sout,"/* XPM */\n");
  fprintf(sout,"static char * %s [] = {\n",fname);
  fprintf(sout,"\"%d %d %d %d\",\n",this->nx, this->ny, gg.ncol , 1);
  fprintf(sout,"/* columns rows colors chars-per-pixel */\n"); //9 May 2008
  for (j = 0; j <  gg.ncol ; j++) {
    fprintf(sout, "\"%c c #%02x%02x%02x\",\n", (char) (j+ gg.cbase), 
            (int) gg.pal[j ].i, (int) gg.pal[ j ].j, 
            (int) gg.pal[ j ].k );  
  }
  fprintf(sout,"/* pixels */\n"); //9 May 2008

/* Print out the pix map */
  for (j = this->ny - 1; j >= 0; j-- ) {
    fprintf(sout,"\"");
    for (i = 0; i < this->nx; i++) {
      index = i + j*this->nx;
      fprintf(sout,"%c", (int)this->grid[index] / cres + gg.cbase);
    }
    fprintf(sout,"\",\n");
  }
  fprintf(sout," }; \n");
  fflush(sout);
  fclose(sout);
    
  return;

}


// Logical grid/point operations++++++++++++++++++++++++++++++++++++++++++
template<class T>
void grid2<T>::crit(T cut, grid2<T> & x) {
  int j;

  for (j = 0; j < this->ny*this->nx; j++) {
    if (  this->grid[j] > cut) {
        x.grid[j] = this->grid[j];
    }
    else {
      x.grid[j] = (T) 0;
    }
  }
}
          
template<class T>
void grid2<T>::crit(T cut) {
  int j;

  for (j = 0; j < this->ny*this->nx; j++) {
    if (  this->grid[j] > cut) {
       // do nothing
    }
    else {
      this->grid[j] = 0;
    }
  }
}

template<class T> 
grid2<T>& grid2<T>::reduce(grid2_base<T> &x, grid2_base<T> &mask, T maskval) {
// N:1 resolution reducer working on a grid2
// Robert Grumbine 1 March 1995 
  int k, l;
  int nxout, nyout, nxin, nyin;
  int nreduce, count;
  double avg;
  ijpt center, orig;
  
   nreduce = (int)(0.5 + (float)x.xpoints() / (float)this->nx);
   nxin = x.xpoints(); 
   nyin = x.ypoints();
   nxout = this->nx;
   nyout = this->ny;
   if (nxin % nreduce != 0 || nyin % nreduce != 0) {
      printf("Reduced map is not an integral subset of the original, \
will crop %d in %d %d out %d %d\n",nreduce,nxin, nyin, nxout, nyout);
   }
   
   for (center.j = 0; center.j < nyout; center.j++) {
      for ( center.i = 0; center.i < nxout; center.i++) {
        avg = 0.;
        count = 0;

        for (k = 0; k < nreduce; k++) {  
             orig.j =  (center.j*nreduce + k);
          for (l = 0; l < nreduce; l++) { 
             orig.i = (center.i*nreduce + l);
             if (mask.in(orig) ) {
             if (mask[orig] != maskval) {
               avg  += x[orig];
               count += 1;
             }
             }
          }
        }

        if (count == 0) {
          avg = 0.0;
        } 
        else {
          avg = avg / (double)count;
        }
        this->operator[](center)  =  (T) avg;
      }        
    }

    return *this;
}        

template<class T> 
grid2<T>& grid2<T>::reduce(grid2_base<T> &x) {
// N:1 resolution reducer working on a grid2
// Robert Grumbine 1 March 1995 
  int k, l;
  int nxout, nyout, nxin, nyin;
  int nreduce, count;
  double avg;
  ijpt center, orig;
  
  #ifdef VERBOSE
  cout << "Entered grid2::reduce\n";
    cout.flush();
  #endif
  if (this->nx > x.xpoints() || this->ny > x.ypoints() ) {
    cout << "The argument grid must be larger than the invoking grid\n";
    cout.flush();
    return *this;
  }
   nreduce = (int)(0.5 + (float)x.xpoints() / (float)this->nx);
   nxin = x.xpoints(); 
   nyin = x.ypoints();
   nxout = this->nx;
   nyout = this->ny;
   if (nxin % nreduce != 0 || nyin % nreduce != 0) {
      printf("Reduced map is not an integral subset of the original, \
will crop %d in %d %d out %d %d\n",nreduce,nxin, nyin, nxout, nyout);
   }
   
   for (center.j = 0; center.j < nyout; center.j++) {
      for ( center.i = 0; center.i < nxout; center.i++) {
        avg = 0.;
        count = 0;

        for (k = 0; k < nreduce; k++) {  
             orig.j =  (center.j*nreduce + k);
          for (l = 0; l < nreduce; l++) { 
             orig.i = (center.i*nreduce + l);
             if (x.in(orig)) {
               avg  += x.operator[](orig);
               count += 1;
             }
          }
        }

        if (count == 0) {
          avg = 0.;
        } /* end of major then clause */
        else {
          avg = avg / (double)count;
        }
         
        this->operator[](center)  =  (T) avg;
      }        
    }

    return *this;
}        

// Grid/Vector Operations
//In grid2_base class

// Grid level functional operations++++++++++++++++++++++++++++++++++++++++++++
template <class T>
T grid2<T>::average(void) {
// Compute the average value for a grid (no masking)
// Robert Grumbine 5/12/97
  int j;
  double temp=0.0;
  for (j = 0; j < this->ny*this->nx; j++ ) {
       temp += (double) this->grid[j];
  }
  temp = temp / (double) (this->nx*this->ny);
  return ( (T) temp ); 
}
template <class T>
T grid2<T>::average(const T mask) {
// Compute the average value for a grid (no masking)
// Robert Grumbine 5/12/97
  int j, n=0;
  double temp = 0.0;

  for (j = 0; j < this->ny*this->nx; j++ ) {
     if (this->grid[j] != mask) {
       temp += this->grid[j];
       n += 1;
     }
  }
  if (n != 0) {
    temp = temp / n;
  }
  else {
    temp = 0.0;
  }
  return ( (T) temp ); 
}
  
  
template <class T>
T grid2<T>::gridmax(const T flag) {
// Compute the max value for a grid (use masking)
// Robert Grumbine 18 October 2001
  int j;
  double temp = -FLT_MAX;
  T ret;
  for (j = 0; j < this->ny*this->nx; j++ ) {
       if (this->grid[j] != flag) temp = max (temp, this->grid[j] );
  }
  ret = (T) temp;
  return ret;
}
template <class T>
T grid2<T>::gridmax(void) {
// Compute the max value for a grid (no masking)
// Robert Grumbine 5/13/97
  int j;
  double temp = -FLT_MAX; 
  T ret;
  for (j = 0; j < this->ny*this->nx; j++ ) {
       temp = max (temp, (double) this->grid[j] );
       //temp = max (temp, this->grid[j] );
  }
  ret = (T) temp;
  return ret;
}
template <class T>
T grid2<T>::gridmin(const T flag) {
// Compute the min value for a grid (with masking)
// Robert Grumbine 18 October 2001
  int j;
  double temp = FLT_MAX;
  for (j = 0; j < this->ny*this->nx; j++ ) {
       if (this->grid[j] != flag) temp = min ( temp, (double) this->grid[j] );
  }
  return ((T) temp );
}
template <class T>
T grid2<T>::gridmin(void) {
// Compute the min value for a grid (no masking)
// Robert Grumbine 5/13/97
  int j;
  double temp = FLT_MAX;
  for (j = 0; j < this->ny*this->nx; j++ ) {
       temp = min ( temp, (double) this->grid[j] );
  }
  return ((T) temp ); 
}

template <class T>
T grid2<T>::rms(const T flag) {
// Compute the rms value for a grid (with masking)
// Robert Grumbine 18 October 2001
  int i, j = 0;
  double temp;
  temp = 0;
  for ( i = 0; i < this->nx*this->ny; i++ ) {
     if (this->grid[i] != flag) {
       temp += (double) this->grid[i]*this->grid[i];
       j += 1;
     }
  }
  if (j != 0) {
   temp = sqrt(temp / (double) (j) );
  }
  else {
   temp = 0;
  }
  return ((T) temp );
}
template <class T>
T grid2<T>::rms(void) {
// Compute the rms value for a grid (no masking)
// Robert Grumbine 5/12/97
  int i;
  double temp;
  temp = 0;
  for ( i = 0; i < this->nx*this->ny; i++ ) {
     temp += (double) this->grid[i]*this->grid[i];
  }
  temp = sqrt(temp / (double) (this->nx*this->ny) );
  return ((T) temp );
}


template<class T>
void grid2<T>::grib_scale(const int &prec, int &nbit, grid2<float> &round) {
  int i;
  float mul, range;
  double rprec;

  #ifdef VERBOSE
    printf("Entered grib_scale, prec, nbit = %d %d\n",prec, nbit); 
    fflush(stdout);
  #endif
  rprec = (double) prec;
  mul = pow(10, rprec);
  for (i = 0; i < this->nx*this->ny; i++) {
    #ifdef LINUX
      round[i] = rint(this->grid[i] * mul) / mul;
    #else
      round[i] = ( (int) (this->grid[i] * mul + 0.5) ) / mul;
    #endif
  }
  #ifdef VERBOSE
    printf("completed looping over the grids, round max, min = %f %f\n",
             round.gridmax(), round.gridmin() ); fflush(stdout);
  #endif
 
  range = round.gridmax() - round.gridmin();
  if (range > 0.0) {
    nbit = (int) (log10( range*mul+0.9 ) / log10(2.) + 1.) ;
  }
  else {
    nbit = 0;
  }

  return;
}
  

template<class T> 
void grid2<T>::scale(void) {
// Scale a grid2 to be in 0-127 range 
  float fmin, fmax;
  int j;

  fmin = this->gridmin();
  fmax = this->gridmax();
  if (fmin == fmax) { this->operator=((T) 100); return; }
  
  for (j = 0; j < this->ny*this->nx; j++) {
       this->grid[j] = (T) (0.5+127.*((float)(this->grid[j] - fmin)) / 
                                    ((float)(fmax-fmin))  );
  }

  return;
}

template<class T>
void grid2<T>::scale(T low, T high) {
  float fmin, fmax;
  int j;
  fmin = (float) low;
  fmax = (float) high;

  if (fmin == fmax) { this->operator=((T) 100); return; }
  
  for (j = 0; j < this->ny*this->nx; j++) {
       this->grid[j] = (T) (0.5+127.*((float)(this->grid[j] - fmin)) /
                                    ((float)(fmax-fmin))  );
  }

  return;
}
      

template<class T>
grid2<T>& grid2<T>::laplace() {
  int i, j, index, indexip, indexjp, indexim, indexjm;

  #ifdef VERBOSE
  if (local != (grid2<T> *) NULL) {
    cout << "Clearing local in grid2::laplace prior to use\n";
    cout.flush();
    delete local;
  }
  #endif
  local = new grid2<T>(this->nx, this->ny);

  local->set((T) 0);

  for (j = 1; j < this->ny-1 ; j++) {
    for (i = 1; i < this->nx-1 ; i++) {
       index = i+j*this->nx;
       indexip = index + 1;
       indexjp = index + this->nx;
       indexim = index - 1;
       indexjm = index - this->nx;

       local->grid[index] = (T)(  this->grid[indexip] + 
                this->grid[indexjp] +  this->grid[indexim] + 
                this->grid[indexjm] -  this->grid[index]*4.);
    }
  }
  #ifdef VERBOSE
    cout << "In laplace that returns a grid2\n";
    cout.flush();
  #endif
  return *local;
}
template<class T>
void grid2<T>::laplace(grid2<T> &lapl) {
  int i, j, index, indexip, indexjp, indexim, indexjm;

  lapl.set((T) 0);

  for (j = 1; j < this->ny-1 ; j++) {
    for (i = 1; i < this->nx-1 ; i++) {
       index = i+j*this->nx;
       indexip = index + 1;
       indexjp = index + this->nx;
       indexim = index - 1;
       indexjm = index - this->nx;

       lapl.grid[index] = (T)( this->grid[indexip] + this->grid[indexjp] + 
                          this->grid[indexim] + this->grid[indexjm] - 4. * this->grid[index]);
    }
  }

  return ;
}

template<class T>
grid2<T>& grid2<T>::gradsq( ) {
  int i, j, index, indexip, indexjp, indexim, indexjm;

  if (local != (grid2<T> *) NULL) {
    cout << "Clearing local in grid2::gradsq prior to use\n";
    cout.flush();
    delete local;
  }
  local = new grid2<T>(this->nx, this->ny);
  local->set((T) 0);

  for (j = 1; j < this->ny-1 ; j++) {
    for (i = 1; i < this->nx-1 ; i++) {
       index = i+j*this->nx;
       indexip = index + 1;
       indexjp = index + this->nx;
       indexim = index - 1;
       indexjm = index - this->nx;

       local->grid[index] = (T)
   .25*(this->grid[indexip] - this->grid[indexim])*(this->grid[indexip] - this->grid[indexim]) +
   .25*(this->grid[indexjp] - this->grid[indexjm])*(this->grid[indexjp] - this->grid[indexjm]) ; 

    }
  }

  return *local;
}

template<class T>
void grid2<T>::gradsq(grid2<T> &grad) {
  int i, j, index, indexip, indexjp, indexim, indexjm;

  grad.set((T) 0);

  for (j = 1; j < this->ny-1 ; j++) {
    for (i = 1; i < this->nx-1 ; i++) {
       index = i+j*this->nx;
       indexip = index + 1;
       indexjp = index + this->nx;
       indexim = index - 1;
       indexjm = index - this->nx;

       grad.grid[index] = (T)
   .25*(this->grid[indexip] - this->grid[indexim])*(this->grid[indexip] - this->grid[indexim]) +
   .25*(this->grid[indexjp] - this->grid[indexjm])*(this->grid[indexjp] - this->grid[indexjm]) ; 
    }
  }

  return ;
}


//          ///////////////////////////////Define Grid2 Operators:
template<class T>
grid2<T>& grid2<T>::operator=(const T x) {
  int j, npts = this->ypoints()*this->xpoints();
  for (j = 0; j < npts; j++) {
      this->grid[j] = x;
  }
  return *this;
}
template<class T>
grid2<T>& grid2<T>::operator=(const grid2<T> &x) {
  int j, npts;

  if (this->nx != x.nx ||  this->ny != x.ny ) {
  #ifdef VERBOSE
    printf("In grid2::operator=grid2, sizes not originally equal, %d %d vs %d %d, resizing the invoking grid2 to the rhs of equality\n",
             this->nx, this->ny, x.nx, x.ny);
  #endif
    grid2_base<T>::resize(x.nx, x.ny );
  }

  if (this == (grid2<T>*) NULL) {
    this->nx = x.nx;
    this->ny = x.ny;
    this->grid = new T[this->nx*this->ny];
  }
  npts = this->xpoints() * this->ypoints();
  for (j = 0; j < npts ; j++) {
      this->grid[j] = x.grid[j];
  }
  this->local = (grid2<T> *) NULL;
  #ifdef VERBOSE
    cout << "Leaving grid2::operator=\n";
    cout.flush();
  #endif
  return *this;
}

template<class T>
grid2<T>& grid2<T>::operator=(grid2_base<T> &x) {
  int j, npts;
  #ifdef VERBOSE
    printf("In grid2::operator=grid2_base %d %d vs %d %d\n",
           this->nx, this->ny, x.xpoints(), x.ypoints()); fflush(stdout);
  #endif
  if (this == (grid2<T> *) NULL) {
    cout << "'this' is null in grid2=grid2_base\n"; cout.flush();
    this->nx = x.xpoints();
    this->ny = x.ypoints();
    this->grid = new T[this->nx*this->ny];
  }
  if (this->nx != x.xpoints() || this->ny != x.ypoints() ) {
    if (this->grid != NULL) { delete []this->grid;} 
    this->nx = x.xpoints();
    this->ny = x.ypoints();
    this->grid = new T[this->nx*this->ny];
  }
  npts = x.xpoints() * x.ypoints();
  for (j = 0; j < npts ; j++) {
      this->grid[j] = x[j];
  }
  this->local = (grid2<T> *) NULL;
  #ifdef VERBOSE
    cout << "about to return from grid2::operator=grid2_base\n"; cout.flush();
  #endif
  return *this;
}



// Operator X= groups
template <class T>
grid2<T> & grid2<T>::operator+=(const grid2<T>&  x) {
  int j;
  for (j = 0; j < this->ny*this->nx ; j++ ) {
      this->grid[j] += x.grid[j] ;
  }
  return *this;
}
template <class T>
grid2<T>& grid2<T>::operator-=(const grid2<T> & x) {
  int j;
  for (j = 0; j < this->ny*this->nx ; j++ ) {
      this->grid[j] -= x.grid[j] ;
  }
  return *this;
}
template <class T>
grid2<T>& grid2<T>::operator*=(const grid2<T> & x) {
  int j;
  for (j = 0; j < this->nx*this->ny ; j++ ) {
      this->grid[j] *= x.grid[j] ;
  }
  return *this;
}
template <class T>
grid2<T>& grid2<T>::operator/=(const grid2<T> & x) {
//Note that this is a dangerous operator, no division by zero check
  int j;
  for (j = 0; j < this->nx*this->ny ; j++ ) {
      this->grid[j] /= x.grid[j] ;
  }
  return *this;
}

template <class T>
grid2<T>& grid2<T>::operator+=(const T  x) {
  int j;
  for (j = 0; j < this->ny*this->nx ; j++ ) {
      this->grid[j] += x ;
  }
  return *this;
}
template <class T>
grid2<T>& grid2<T>::operator-=(const T  x) {
  int j;
  for (j = 0; j < this->ny*this->nx ; j++ ) {
      this->grid[j] -= x ;
  }
  return *this;
}
template <class T>
grid2<T>& grid2<T>::operator/=(const T x) {
  int j;
  if (x == 0) {
    cout << "error in grid2::operator/=, division by 0 requested\n";
    cout.flush();
  }
  #ifdef VERBOSE
    cout << "verbose division by "; cout << x; cout << " \n";
    cout.flush();
  #endif
  for (j = 0; j < this->ny*this->nx ; j++ ) {
      this->grid[j] /= x;
  }
  return *this;
}
template <class T>
grid2<T>& grid2<T>::operator*=(const T x) {
  int j;
  for (j = 0; j < this->ny*this->nx ; j++ ) {
      this->grid[j] *= x;
  }
  return *this;
}



//Simple binary operators
// Grid-level should be avoided due to issues of their return variables.  RG  4 Feb 2009


//..with scalars.  These and the other operations are ok.
template <class T>
grid2<T> & grid2<T>::operator*(const T x) {
  int j;

  if (local != (grid2<T> *) NULL) {
    cout << "Clearing local in grid2::operator* x prior to use\n";
    cout.flush();
    delete local;
  }
  local = new grid2<T>(this->nx, this->ny);
  for (j = 0; j < this->ny*this->nx ; j++ ) {
      local->grid[j] = this->grid[j];
      local->grid[j] *= x;
  }
  return *local;
}
template <class T>
grid2<T> & grid2<T>::operator/(const T x) {
  int j;

  if (local != (grid2<T> *) NULL) {
    cout << "Clearing local in grid2::operator/ x prior to use\n";
    cout.flush();
    delete local;
  }
  local = new grid2<T>(this->nx, this->ny);
  for (j = 0; j < this->ny*this->nx ; j++ ) {
      local->grid[j] = this->grid[j];
      local->grid[j] /= x;
  }
  return *local;
}
template <class T>
grid2<T> & grid2<T>::operator+(const T x) {
  int j;

  if (local != (grid2<T> *) NULL) {
    cout << "Clearing local in grid2::operator+ x prior to use\n"; cout.flush();
    printf("nx, ny = %d %d\n",local->nx, local->ny); fflush(stdout);
    delete local;
  }
  local = new grid2<T>(this->nx, this->ny);
  for (j = 0; j < this->ny*this->nx ; j++ ) {
      local->grid[j] = this->grid[j];
      local->grid[j] += x;
  }
  return *local;
}
template <class T>
grid2<T> & grid2<T>::operator-(const T x) {
  int j;

  if (local != (grid2<T> *) NULL) {
    cout << "Clearing local in grid2::operator- x prior to use\n";
    cout.flush();
    delete local;
  }
  local = new grid2<T>(this->nx, this->ny);
  for (j = 0; j < this->ny*this->nx ; j++ ) {
      local->grid[j] = this->grid[j];
      local->grid[j] -=  x;
  }
  return *local;
}

#endif
