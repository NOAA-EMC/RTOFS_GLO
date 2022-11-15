#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <limits>
#include <climits>
#include <cfloat>
#include <iostream>
using namespace std;

// Class for working with mathematical vectors (mvectors)
// Robert Grumbine (oldest extant 1997/11/26)
//
// Modifications:
//   1998/06/17 add printing
//   1998/06/27 use references to vectors as arguments rather than 
//                vectors (avoid recopying large data spans)
//   1998/09/30 start developing a time-series capable class
//   1998/12/03 add many vector operators
//   1999/      begin move to iostream, add otislev
//   1999/      add nodc levels, Levitus levels
//   1999/10/04 template otis, levitus levels
// On 2 April 2004 change everything from vector.h to mvector.h for the
// mathematical vectors.  Vector.h is actually part of the standard
// template library and it is time to stop conflicting with it.
//   2005/03/16
//     Add operator==constant
//     Add crosscorrel(timeseries)
//   20 Nov 2007: 'thisification'
//   2009/11/05
//     Add type cast to double (for metricgrid<mvector<T> > situations)
//     member initialization (per effc++)
//     make random vector, shuffle vector, construct histograms to 
//       specified resolution
//   7 May 2013: Extract time_series and the data_vectors to own files


#ifndef MVECTORH
  #define MVECTORH

  #ifndef MAXMIN
    #define MAXMIN
    template <class T, class U>
    T max(T x, U y ) {
      if (x > y) {
        return x;
      }
      else {
        return (T) y;
      }
    }
    template <class T, class U>
    T min(T x, U y ) {
      if (x > y) {
        return (T) y;
      }
      else {
        return x;
      }
    }
  #endif


template <class T>
class mvector {
  private : // Should not be necessary for anyone to mess around with this
    int nx;
    T *vec;
  protected:
    mvector<T> *local;
    void setsize(const int );   // method for derived classes to initialize data vec.
  public :
//CTOR - DTOR
    //member init mvector(int n = 1) { nx = n; vec = new T[nx];
    //member init      #ifdef VERBOSE
    //member init      printf("constructed %d pt mvector\n",nx); fflush(stdout);
    //member init      #endif
    //member init  }
    mvector(int n = 1);
    ~mvector()         { if (vec != (T *) NULL) { delete []vec; }  }
    mvector(mvector<T> &);  //copy constructor
    mvector(const mvector<T> &);  // Added copy ctor
    void resize(const int );
//Interrogation and assignment -- first two are automatic inline functions
    int xpoints()              {return (nx);}
    T& operator[](const int x) {return (vec[x]) ;}
    mvector<T>& operator=(const mvector<T> &);
    mvector<T>& operator=(const T );
    bool operator==(const mvector<T> &);
    bool operator==(const T &y) { 
         bool res = true; 
         for (int i = 0; i < nx; i++) {
             res = res && (vec[i] == y);
         }
         return res;
         }

//IO
    void binout(FILE *);
    void binin (FILE *);
    void printer(FILE *);

//Mathematical operators
    mvector<T>& operator*=(const mvector<T>& );
    mvector<T>& operator/=(const mvector<T>& );
    mvector<T>& operator+=(const mvector<T>& );
    mvector<T>& operator-=(const mvector<T>& );
    mvector<T>& operator*=(const T );
    mvector<T>& operator/=(const T );
    mvector<T>& operator+=(const T );
    mvector<T>& operator-=(const T );
//  Note that non (op)= operators are not defined.  This is to avoid the
//    memory catastrophes which can occur in creating new objects without
//    any way of ensuring their deletion.

// Vector level functions that return mathematical information:
// These are all candidates for externalization to the class?
// 'const' correcting?
    double norm(int = 2);        //Compute the L-N norm of the mvector
    T rms();
    T maximum();
    T minimum();
    T average();
    T average(T );
    float complete(T );
    void histogram(mvector<int> &, float&);
    operator double();

// Vector level functions which modify the mvector:
    inline void fill(T );
    mvector<T> & rescale(T &x) { mvector<T>::operator/=(x); return *this; }
    mvector<T> & normalize();
    mvector<T> & normalize(T );
    // Note that these two random functions are seeded with a constant at
    //   this point (8 May 2008).  In future, version with seed
    void random(const T, const T);
    void shuffle();

};
// Declaration for better standard compliance when constructing static
//   mvector.  move to library.h, library.C
mvector<float> nullvec() {
  mvector<float> x;
  return x;
}

// added 8 May 2008
// Shuffle the input vector -- destroys original
template <class T>
void mvector<T>::shuffle() {
  mvector<int> x(this->xpoints() );
  int i, j, n = this->xpoints(), nx = this->xpoints();

  srand48(1);
  for (i = 0; i < nx ; i++) {
    x[i] = -1;
  }

  for (i = 0; i < nx ; i++) {
    j = (int) (n*drand48());
    x[i] = this->operator[](j);

    if (x[i] >= nx ) {
      fprintf(stderr, "out of bound value %d vs %d j = %d\n",x[i], nx, j ); 
        fflush(stderr);
      for (int k = 0; k < nx ; k++) {
        printf("%d x %d orig %d\n",k,x[k],(int)this->operator[](k) ); 
        fflush(stdout);
      }
    }

    this->operator[](j) = this->operator[](n-1);
    n -= 1;
  }

  for (i = 0; i < x.xpoints(); i++) {
    this->operator[](i) = x[i];
  }

  return;
}


// added 8 May 2008
// Construct a uniformly random value vector given input max, min
// Future: pass in seed
//       : construct gaussian random by option
template <class T>
void mvector<T>::random(const T min, const T max) {
  T delta = (max - min);

  srand48(1);
  for (int i = 0; i < this->xpoints(); i++) {
    this->operator[](i) = delta*drand48() + min;
  }

  return;
}

// added 8 May 2008
// Construct a histogram from input data
template <class T>
void mvector<T>::histogram(mvector<int> &datout, float &res ) {
  int n;
  float rmin, rmax, delta, resi = res;

  rmin =  (float) (this->minimum() );
  rmax =  (float) (this->maximum() );
  delta = rmax - rmin;
  while (delta < res) { res *= 10; }  //Rescale to get at least one full bin
  #ifdef VERBOSE
    printf("res = %f\n",res);
  #endif

  n = (int) (0.5 + (rmax - rmin)/res + 1);
  if (res != resi || n > datout.xpoints() ) {
    datout.resize(n);
    #ifdef VERBOSE
      cout << "resized datout in histogram\n";
    cout.flush();
    #endif
  }
  for (n = 0; n < datout.xpoints(); n++) {
    datout[n] = 0;
  }

  for (n = 0; n < this->xpoints(); n++) {
    datout[ (int)((this->operator[](n) - rmin)/res + 0.5) ] += 1;
  }

  return;
}

template <class T>
float mvector<T>::complete(T maskval) {
  int i, tot=0;
  for (i = 0; i < xpoints(); i++) {
    if (vec[i] != maskval) tot+= 1;
  }
  return (float) tot / (float) xpoints();
}
template <class T>
T mvector<T>::average() {
  int i;
  double sum=0.0;
  for (i = 0; i < nx; i++) {
    sum += (double) vec[i];
  }
  if (nx > 0) {
    sum /= (double) nx;
    return (T) sum;
  }
  else {
    return (T) 0;
  }

  return (T) 0;
}
template <class T>
T mvector<T>::average(T maskval) {
  int i, good=0;
  double sum=0.;
  for (i = 0; i < nx; i++ ) {
    if (vec[i] != maskval) { good += 1; sum += (double) vec[i]; }
  }
  if (good > 0) {
    return sum / (double) good;
  }
  else {
    return maskval;
  }
}
template <class T>
T mvector<T>::maximum() {
  T lim = -FLT_MAX;
  int i;
  for (i = 0; i < nx; i++) {
     lim = max(lim, vec[i]);
  }
  return lim;
}
template <class T>
T mvector<T>::minimum() {
  T lim = FLT_MAX;
  int i;
  for (i = 0; i < nx; i++) {
     lim = min(lim, vec[i]);
  }
  return lim;
}
template <class T>
mvector<T>::operator double() {
  double x = (double) this->rms();
  return x;
}

template <class T>
T mvector<T>::rms() {
  int i;
  double sum=0.;
  for (i = 0; i < nx; i++ ) {
    sum += (double) vec[i]*vec[i];
  }
  if (nx > 0) {
    return (T) sqrt(sum / (double) nx);
  }
  else {
    return (T) 0;
  }
}


template <class T>
double mvector<T>::norm(int n) {
  int i;
  double sum=0., tpow;

  #ifdef VERBOSE
    printf("Computing norm nx = , n = %d %d\n",nx, n);
  #endif
  //Note special cases for n = 1, 2 which are more quickly handled without
  // reference to 'pow'.
  switch (n) {
    case 0:
      sum = nx;
      return  sum;
      break;
    case 1:
      for (i = 0; i < nx; i++) {
        sum += vec[i];
      }
      break;
    case 2:
      for (i = 0; i < nx; i++) {
        sum += vec[i]*vec[i];
      }
      break;
    default:
      for (i = 0; i < nx; i++) {
        sum += pow((double) vec[i], n);
      }
  }

  sum /= (double) nx;
  if (n != 0) {
    tpow = 1./(double) n;
    sum = pow(sum, tpow);
  }
  return  sum;
}

template <class T>
mvector<T> & mvector<T>::normalize() {
  T scale;
  scale = norm(2);
  this->rescale(scale);
  return *this;
}
template <class T>
mvector<T> & mvector<T>::normalize(T flag) {
  int i;
  double sum = 0.;
  for (i = 0; i < xpoints(); i++) {
     if (this->operator[](i) != flag) sum += this->operator[](i)*this->operator[](i) ;
  }
  sum = sqrt(sum);
  for (i = 0; i < xpoints(); i++) {
     if (this->operator[](i) != flag) this->operator[](i) /= sum;
  }
  return *this;
}

template <class T>
inline void mvector<T>::fill(T flag) {
// Fill in flag gaps by linear interpolation
  int i, j, ifirst, inext;
  float del;

  i = 0;
  ifirst = 0;
  while (ifirst < nx - 1) {

    while ( (vec[i] == flag || vec[i+1] != flag) && i != (nx -1) ) {
      i += 1;
    }

    if (i == nx - 1) {return ;}
    ifirst = i;

    i += 1;
    while (vec[i] == flag && i < nx - 1 ) {
      i+= 1;
    }
    if (i == nx - 1) { return ; }
    inext = i;

    del = (float) (inext - ifirst) ;
//  The following is generating a compiler on GCC if left in:
//  if (del < (float) 2) { printf("error delta = %f i = %d \n",del, i); return;}

    if ( inext < ifirst+2) {printf("error, delta = %d i = %d\n",
                                        inext - ifirst,i); return;    }
    for (j = 1; j < (int) del; j++) {
       vec[j+ifirst] = (vec[inext] - vec[ifirst])/del*j + vec[ifirst];
    }
    ifirst = inext;
    i = ifirst - 1;  // the -1 is so the repeat will execute on inext;

  } // end of going through for new points

  return;
}


template <class T>
mvector<T>::mvector(const mvector<T> &y) {
  int i;
  if (this == &y) { return;}
  nx = y.nx;
  vec = new T[nx];
  for (i = 0; i < nx; i++) {
    vec[i] = y.vec[i];
  }
}
//15 May 2008 template <class T>
template <class T>
mvector<T>::mvector(mvector<T> &y) {
  int i;
  if (this == &y) { return;}
  nx = y.nx;
  vec = new T[nx];
  for (i = 0; i < nx; i++) {
    vec[i] = y.vec[i];
  }
}

template <class T>
void mvector<T>::setsize(const int n) {
  #ifdef VERBOSE
    printf("Setting size in mvector, %d\n",n);
  #endif
  nx = n;
  vec = new T[nx];
}
template <class T>
void mvector<T>::binin(FILE *fout) {
  fread(vec, sizeof(T), nx, fout);
  return;
}
template <class T>
void mvector<T>::binout(FILE *fout) {
  fwrite(vec, sizeof(T), nx, fout);
  return;
}
template <class T>
void mvector<T>::printer(FILE *fout) {
  int i;
  for (i = 0; i < nx; i++) {
    // This should be done via iostream and >> operators, since vec may be
    //   something other than a float.
    fprintf(fout, "%d %f\n", i, (float) vec[i]);
  }
  return;
}

template <class T>
mvector<T>& mvector<T>::operator=(const T val) {
  int i;
  for (i = 0; i < nx; i++) {
    vec[i] = val;
  }
  return *this;
}
template <class T>
mvector<T>& mvector<T>::operator*=(const mvector<T>& y) {
  int i;
  for (i = 0; i < nx; i++ ) {
     vec[i] *= y.vec[i];
  }
  return *this;
}
template <class T>
mvector<T>& mvector<T>::operator/=(const mvector<T>& y) {
  int i;
  for (i = 0; i < nx; i++ ) {
     vec[i] /= y.vec[i];
  }
  return *this;
}
template <class T>
mvector<T>& mvector<T>::operator/=(const T y) {
  int i;
  for (i = 0; i < nx; i++ ) {
     vec[i] /= y;
  }
  return *this;
}
template <class T>
mvector<T>& mvector<T>::operator*=(const T y) {
  int i;
  for (i = 0; i < nx; i++ ) {
     vec[i] *= y;
  }
  return *this;
}
template <class T>
mvector<T>& mvector<T>::operator+=(const mvector<T> &y) {
  int i;
  for (i = 0; i < nx; i++ ) {
     vec[i] += y.vec[i];
  }
  return *this;
}
template <class T>
mvector<T>& mvector<T>::operator+=(const T y) {
  int i;
  for (i = 0; i < nx; i++ ) {
     vec[i] += y;
  }
  return *this;
}
template <class T>
mvector<T>& mvector<T>::operator-=(const mvector<T>& y) {
  int i;
  for (i = 0; i < nx; i++ ) {
     vec[i] -= y.vec[i];
  }
  return *this;
}
template <class T>
mvector<T>& mvector<T>::operator-=(const T y) {
  int i;
  for (i = 0; i < nx; i++ ) {
     vec[i] -= y;
  }
  return *this;
}


template <class T>
mvector<T>& mvector<T>::operator=(const mvector<T>& y) {
  int i, ynx = y.nx;
  if (this == &y) { return *this; }
  if (nx != ynx) {
    #ifdef VERBOSE
      cout << "Unequal mvector lengths in mvector assignment!\n";
    cout.flush();
      printf(" %d %d\n", nx, ynx);
    #endif
    for (i = 0; i < min(nx,ynx ) ; i++) {
       vec[i] = y.vec[i];
    }
    return *this;
  }
  for (i = 0; i < nx; i++) {
     vec[i] = y.vec[i];
  }
  return *this;
}
template <class T>
bool mvector<T>::operator==(const mvector<T> &y) {
  int j ;
  bool i ;
  if (nx != y.nx) {
    cout << "Unequal mvector lengths in mvector equality test!\n";
    cout.flush();
    printf(" %d %d\n", nx, y.nx);
    return false;
  }
  i = (vec[0] == y.vec[0]);
  for (j = 1; j < nx; j++) {
    i = i && ( vec[i] == y.vec[i] );
  }

  return i;
}

template <class T>
void mvector<T>::resize(const int n) {
  #ifdef VERBOSE
    cout << "Entered mvector::resize\n"; cout.flush();
  #endif
  if (vec != (T * ) NULL) {
  #ifdef VERBOSE
    cout << "About to delete an old vec\n"; cout.flush();
  #endif
    delete []vec ;
  }
  nx = n;
  #ifdef VERBOSE
    cout << "About to new the new mvector\n"; cout.flush();
  #endif
  vec = new T[n];
  return;
}


//////////////////////////////////////////////////////////////////
// Declare the class for metric mvector -- those for which there is some
//   'distance' relation between points.  In this case, we're doing it
//   by carrying another mvector.
// Note that with metric being private, there is no way to find out what
//   metric[23] (e.g.) is.  ?Do users need to?
template <class T>
class metricvector : public mvector<T> {
  private:
    mvector<T> *metric;
    metricvector(const metricvector<T>&y){ if (this == &y) return ;
                                           *metric = *(y.metric);
                                           mvector<T>::operator=(y); }
  public :
    metricvector(int n = 1) { metric = new mvector<T>(n); this->setsize(n);
             #ifdef VERBOSE
                 cout << "constructed metricvector "; cout << n; cout << "\n"; 
                 cout.flush();
             #endif
                             }
    metricvector<T>& operator=(const T y) {mvector<T>::operator=(y); return *this; }
    void set_metric(const mvector<T> &y) {*metric = y; return; }
    mvector<T>& get_metric(mvector<T> &y) {return *metric;}
    metricvector<T>& operator=(const mvector<T> &y) { if (this == &y) return *this;
                                                  mvector<T>::operator=(y);
                                                  return *this;}
    metricvector<T>& operator=(const metricvector<T> &y) { if (this == &y) return *this;
                                                  mvector<T>::operator=(y);
                                                  *metric = *(y.metric);
                                                  return *this; }
    inline void interp(metricvector<T> &, T );
};

template <class T>
inline void metricvector<T>::interp(metricvector<T> &in, T flag) {
   int i, j, imax;
   float tmp;

   i = 0;
   imax = in.xpoints() - 1;
   tmp = 0.0;
   for (j = 0; j < this->xpoints(); j++) {
      if (metric->operator[](j) <= in.metric->operator[](0) ) {
        mvector<T>::operator[](0) = in[0] ;
        continue;
      }
      while (in.metric->operator[](i+1) < metric->operator[](j) && i < imax && in[i+1] != flag )
      {
        i += 1;
      }
      if (i == imax) {
        mvector<T>::operator[](j) = in[i];
        continue;
      }
      if (in[i+1] == flag ) {
        mvector<T>::operator[](j) = in[i];
        continue;
      }
      if (in.metric->operator[](i+1) == in.metric->operator[](i) ){
        mvector<T>::operator[](j) = flag;
      }
      else {
        tmp = (in.metric->operator[](i+1) - metric->operator[](j) ) /
              (in.metric->operator[](i+1) - in.metric->operator[](i) );
        mvector<T>::operator[](j) = (1.-tmp)*in[i+1] + tmp*in[i] ;
      }
   }

   return;
}

// Member initialization:
template<class T>
mvector<T>::mvector(int n): nx(n), vec (new T[n]), local() {
     #ifdef VERBOSE
       cout << "constructed mvector "; cout << nx; cout << "\n"; 
       cout.flush();
     #endif
}
#endif
