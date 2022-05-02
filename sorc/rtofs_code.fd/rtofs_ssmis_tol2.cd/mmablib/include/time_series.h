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
// Extract time series to own include file 7 May 2013


#ifndef MVECTORH
  #include "mvector.h"
#endif
#ifndef TIMESERIESH
  #define TIMESERIESH

  #ifndef MAXMIN
    #define MAXMIN
    template <class T, class U>
    T max(T x, U y ) {
      if (x > y) {
        return x;
      }
      else {
        return y;
      }
    }
    template <class T, class U>
    T min(T x, U y ) {
      if (x > y) {
        return y;
      }
      else {
        return x;
      }
    }
  #endif


//////////////////////////////////////////////////
// Time series class, descended of mvector
template <class T>
class time_series : public mvector<T> {
  public :
    T mean;
    time_series(int=15000);
    time_series(time_series<T> &);
    void set(T );  // set series to a constant, t
    void set(mvector<T> & );  // transfer a mvector to a time series
    void set(T *);  // transfer a set of data pointed to into series

    float autocovary(int) ;    //compute autocovariance, no mask
    float autocovary(T, int) ; //compute autocovariance, mask
    T crossvary(time_series<T> &, T, int = 1) ; //compute cross-covariance, mask
    T crossvary(time_series<T> &, int = 1) ;    //compute cross-covariance, no mask
    T crosscorrel(time_series<T> &, int = 1) ;  //compute cross-correl, no mask
    void fft(mvector<T> &, mvector<T> &);       //compute the fft of a data set,
                                           //  passing back real and im parts
    void ifft(mvector<T> &, mvector<T> &); //compute the inverse fft of a
                //data set, given real and im parts
};
template <class T>
time_series<T>::time_series(int n) {
  this->setsize(n); //Sets the data mvector size and nx
  #ifdef VERBOSE
    cout << "Constructing time series with ";
    cout << n;
    cout << " points\n";
    cout.flush();
  #endif
//  *this = (T) 0;
  mean = this->average();
}

template <class T>
time_series<T>::time_series(time_series<T> &x) {
  mvector<T>::operator=(x);
  mean = this->average();
}

template <class T>
void time_series<T>::set(T x ) {  // set series to a constant, t
  int i;
  for (i = 0; i < this->xpoints(); i++) {
    this->operator[](i) = x;
  }
  mean = this->average();
  return;
}

template <class T>
void time_series<T>::set(mvector<T> &x) {  // transfer a mvector to a time series
  int i;
  for (i = 0; i < x.xpoints(); i++) {
    this->operator[](i) = x[i];
  }
  mean = this->average();
  return;
}

template <class T>
void time_series<T>::set(T *x) {  // transfer a set of data pointed to into series
  int i;
  for (i = 0; i < this->xpoints(); i++) {
    this->operator[](i) = x[i];
  }
  mean = this->average();
  return;
}

//Compute the autocovariance at lag u, in the presence of masks
template <class T>
float time_series<T>::autocovary(T maskval, int u) {
  double sum = 0.;
  int i;
  if (this->xpoints() > u+1) {
    mean = this->average(maskval);
    if (mean == maskval) return 0.;
    for (i = 0; i < this->xpoints() - u; i++) {
       if (this->operator[](i) != maskval && this->operator[](i+u) != maskval) {
           sum += (this->operator[](i) - mean)*(this->operator[](i+u) - mean);
       }
    }
    return (sum / (double)this->xpoints());
  }
  else {
    return  0.0;
  }
}
//Compute the autocovariance at lag u
template <class T>
float time_series<T>::autocovary(int u) {
  double sum = 0., c0 = 1;
  int i;
  if (this->xpoints() > u+1) {
    //if (u > 0) c0 = this->autocovary(0);
    mean = this->average();
    for (i = 0; i < this->xpoints() - u; i++) {
       sum += (this->operator[](i) - mean)*(this->operator[](i+u) - mean);
    }
    if (u == 0) {
      return sum / (double)this->xpoints() ;
    }
    else {
      return sum / c0 / (double)this->xpoints() ;
    }
        
  }
  else {
    return  0.0;
  }
}

// Attach to the header that manages the fft call to four1 - Numerical recipes
//  routine
extern "C" void four1_(float *f, int *nn, int *isign);

template<class T>
void time_series<T>::fft(mvector<T> &re, mvector<T> &im) {
  mvector<float> *f;
  int i, nn, nx2;
  int npts = this->xpoints();

// Ensure that there are 2**N points by zero filling as needed
  nn = lrintf( log((double) npts)/log(2.) );
  if (pow(2., (double) nn) == npts) {
    f = new mvector<float> (2*npts);
    nx2 = npts;
  }
  else {
    nn = (int) (log((double) npts)/log(2.) );
    nx2 = (int) pow(2., (double) nn+1.);
    f = new mvector<float> (nx2*2);
    if (re.xpoints() != nx2) {
      cout << "Resizing re from "; cout << re.xpoints(); cout << " ";
      cout << " to "; cout << nx2; cout << " points\n";
    cout.flush();
      re.resize(nx2);
    }
    if (im.xpoints() != nx2) { im.resize(nx2); }
  }
  nn = nx2;

// Transfer data to temporary mvector for transforming
//  Note that four1 assumes a 'complex' input
  for (i = 0; i < nx2; i++) {
    f->operator[](2*i) = this->operator[](i);
    f->operator[](2*i+1) = 0.0;
  }
  cout << "Must fix the usage of fft:four1_\n"; cout.flush();

  for (i = 0; i < nx2; i++) {
    re[i] = f->operator[](2*i);
    im[i] = f->operator[](2*i+1);
  }
}
template<class T>
void time_series<T>::ifft(mvector<T> &re, mvector<T> &im) {
  mvector<float> f(2*this->xpoints());
  int i;

// Transfer data to temporary mvector for transforming
  for (i = 0; i < this->xpoints(); i++) {
    f[2*i]   = re[i];
    f[2*i+1] = im[i];
  }
// Compute transform
  cout << "Must fix the usage of ifft:four1_\n"; cout.flush();

  for (i = 0; i < this->xpoints(); i++) {
    this->operator[](i) = f[2*i] / (double) this->xpoints();
  }

}


//Compute the cross covariance at lag u in presence of masks
template <class T>
T time_series<T>::crossvary(time_series<T> &x2, T maskval, int u) {
  double sum = 0.;
  int i;
  double mean1, mean2;
  mean1 = (double) this->average(maskval);
  mean2 = (double) x2.average(maskval);
  if (mean1 == maskval || mean2 == maskval) return (T) 0;
  if (u >= 0) {
    if (x2.xpoints() > u+1) {
      for (i = 0; i < x2.xpoints() - u; i++) {
         if (this->operator[](i) != maskval && x2[i+u] != maskval) {
           sum += (this->operator[](i) - mean1)*(x2[i+u] - mean2);
         }
      }
      return (T)(sum / (double)x2.xpoints()-u);
    }
    else {
      return (T) 0.0;
    }
  } // end positive lag case
  else {
    if (x2.xpoints() > -u-1) {
      for (i = 0; i < x2.xpoints() + u; i++) {
         if (this->operator[](i-u) != maskval && x2[i] != maskval) {
           sum += (this->operator[](i-u) - mean1)*(x2[i] - mean2);
         }
      }
      return (T)(sum / (double)x2.xpoints());
     }
     else {
       return (T) 0.0;
    }
  }

  return (T) 0.0;
}
//Compute the cross covariance at lag u
template <class T>
T time_series<T>::crossvary(time_series<T> &x2, int u) {
  double sum = 0.;
  int i; 
  double mean1, mean2;
  
  mean1 = (double) this->average();
  mean2 = (double) x2.average();
  
  if (u >= 0) {
    if (x2.xpoints() > u+1) {
      for (i = 0; i < x2.xpoints() - u; i++) {
         //sum += (this->operator[](i) - mean1)*(x2[i+u] - mean2);
         sum += (this->operator[](i) )*(x2[i+u] );
      }
      sum /= (double)(x2.xpoints()-u);
      sum -= mean1*mean2;
      //return (T)(sum / (double)x2.xpoints()  );
      return (T) sum;
    }
    else {
      return (T) 0.0;
    }
  } // end positive lag case
  else {
    if (x2.xpoints() > -u-1) {
      for (i = 0; i < x2.xpoints() + u; i++) {
         sum += (this->operator[](i-u) )*(x2[i] );
      }
      sum /= (double)(x2.xpoints()-u);
      sum -= mean1*mean2;
      //return (T)(sum / (double)x2.xpoints()  );
      return (T) sum;
     }
     else {
       return (T) 0.0;
    }
  }
  
  return (T) 0.0;
}

//Compute the cross correlation at lag u
template <class T>
T time_series<T>::crosscorrel(time_series<T> &x2, int u) {
  double sum = 0.;
  int i;
  double mean1, mean2, var1 = 0, var2 = 0;

  mean1 = (double) this->average();
  mean2 = (double) x2.average();
  for (i = 0; i < x2.xpoints(); i++) {
    var1 += this->operator[](i)*this->operator[](i);
    var2 += x2[i]*x2[i];
  }
  var1 /= x2.xpoints();
  var2 /= x2.xpoints();
  var1 -= mean1*mean1;
  var2 -= mean2*mean2;
  
  if (u >= 0) {
    if (x2.xpoints() > u+1) {
      for (i = 0; i < x2.xpoints() - u; i++) {
         sum += (this->operator[](i) )*(x2[i+u] );
      }
      sum /= (double)x2.xpoints();
      sum -= mean1*mean2;
      return (T) sum / sqrt(var1*var2);
    }
    else {
      return (T) 0.0;
    }
  } // end positive lag case
  else {
    if (x2.xpoints() > -u-1) {
      for (i = 0; i < x2.xpoints() + u; i++) {
         sum += (this->operator[](i-u) )*(x2[i] );
      }
      sum /= (double)x2.xpoints();
      sum -= mean1*mean2;
      return (T) sum / sqrt(var1*var2);
     }
     else {
       return (T) 0.0;
    }
  }

  return (T) 0.0;
}

#endif
