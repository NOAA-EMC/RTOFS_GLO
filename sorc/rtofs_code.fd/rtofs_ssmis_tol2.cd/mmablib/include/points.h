#include <cmath>

#ifndef POINTH

#define POINTH

// Robert Grumbine 2 December 1997
// OMB C++ library: classes: point3, ijpt, fijpt 
// These classes declare useful 'point' classes.
//   A point3 is an ordered triple of things of type T,
//     with member values i, j, k.
//   ijpt and fijpt are restricted to be integer and floating point
//     point3's.
//   A structure, latpt, is defined here.  This is a structure
//     rather than a class member, so operations cannot be performed
//     on latpts (the reason being that the latpts name members
//     differently).  One can set an fijpt = latpt, and operate
//     on the latpts.
//Operations in the class are:
//     construction, destruction
//     Arithmetic with a single element of type T (say a float)
//         (=, +, -, *, / )  Operation is applied to each element of
//         the point3.
//     Arithmetic with another point3 (=, +, -, +=, -=, *=, element by element)
//     logical operations (element-by element.  ==, !=, >=, <=, >, < )
//     type casting (to int, float, double.  The value cast is the
//        L2 norm of the 3 vector.)
// Additional notes:
//     It is generally proper to compound classes.  A point3<float> is itself a 
//       'thing of type T', in the parlance above.  This makes it
//       legal to declare a variable which is a point3< point3<float> >.
//       Such a structure is fundamentally a 3x3 matrix, and mathematically
//       suitable for discussing a tensor.  Unfortunately, since we've only
//       declared this as a single template class, a point3< point3<float> >
//       can only be operted on (add, subtract, etc.) with a point3<float>.
//       You can't multiply a point3<point3<float> > by a scalar until we
//       have a doubly-templated class.  (Worse: you can, in fact, do this
//       but you won't get the answer you expect!!)  Compound classes 
//       with care.
//
// Modifications:
//   3 Dec 1997 Rename class to point3 from point, file to points.h from point.h
//  12 Dec 1997 Template the class, define latpt, expand fijpt ijpt
//   3 Dec 1998 Add substantial commentary
//              operators return reference to a point3
//  24 Jun 1999 Template returns
//              Pass references to point3's, rather than point3's, for 
//              speed and memory
//  04 Oct 1999 Remove commented out code
//  20 Nov 2000 Minor changes to magnitude and normalize -- specifying
//              conversion to atomic types on ASP
//  13 Jul 2004 go to more explicit copy constructor
//  15 May 2008 Member initialization lists rather than oldstyle constructors
//  15 May 2008 operators returning a *local deleted -- memory leak problems
//    Robert Grumbine

#ifndef LATPT
#define LATPT
typedef struct {
  float lat, lon;
} latpt;
#endif

template <class T>
class point3 {
  public:
   T i, j, k;
  private:
   //point3<T> *local;

  public:
   //member point3(void) { i =  0; j =  0; k =  0; local = (point3<T>*) NULL; return; }
   point3(void);
   point3(const T)  ;
   point3(const T, const T);
   point3(const T, const T, const T);
   //~point3(void) {};
   point3(const point3<T> &x);

   T magnitude();
   void normalize();

   point3<T> & operator=(T);
   point3<T> & operator=(const latpt &x) {i = (int) x.lon; j = (int) x.lat; k=0; return *this; };
   point3<T> & operator=(const point3<T> &x) { i = x.i; j = x.j; k = x.k; return *this; };

   point3<T> & operator+=(const point3<T> &x) { i += x.i; j += x.j; k += x.k; return *this;} ;
   point3<T> & operator-=(const point3<T> &x) { i -= x.i; j -= x.j; k -= x.k; return *this;} ;
   point3<T> & operator*=(const point3<T> &x) { i *= x.i; j *= x.j; k *= x.k; return *this;} ;
   point3<T> & operator/=(const point3<T> &x) { i /= x.i; j /= x.j; k /= x.k; return *this;} ;

   bool operator==(const point3<T> &);
   bool operator!=(const point3<T> &);
   bool operator>=(const point3<T> &);
   bool operator<=(const point3<T> &);
   bool operator>(const point3<T> &);
   bool operator<(const point3<T> &);
  
   operator int();
   operator float();
   operator double();
};

template <class T>
point3<T>::operator double() {
  double x = (double) this->magnitude();
  return x;
}
template <class T>
point3<T>::operator float() {
  float x = (float) this->magnitude();
  return x;
}
template <class T>
point3<T>::operator int() {
  T temp = this->magnitude();
  int x = (int) (temp);
  return x;
}

//IO

//Operators with points

//Operators with scalars
template <class T>
point3<T> & point3<T>::operator=(T x) {
  i = x;
  j = x;
  k = x;
  return *this;
}

template <class T>
bool point3<T>::operator==(const point3<T> &x) {
  bool res = (x.i == i) && (x.j == j) && (x.k == k) ;
  return res;
} 
template <class T>
bool point3<T>::operator!=(const point3<T> &x) {
  bool res = ! ((x.i == i) && (x.j == j) && (x.k == k)) ;
  return res;
} 

template <class T>
bool point3<T>::operator<=(const point3<T> &x) {
  bool res = (x.i <= i) && (x.j <= j) && (x.k <= k) ;
  return res;
} 

template <class T>
bool point3<T>::operator>=(const point3<T> &x) {
  bool res = (x.i >= i) && (x.j >= j) && (x.k >= k) ;
  return res;
} 
template <class T>
bool point3<T>::operator>(const point3<T> &x) {
  bool res = (x.i > i) && (x.j > j) && (x.k > k) ;
  return res;
} 
template <class T>
bool point3<T>::operator<(const point3<T> &x) {
  bool res = (x.i < i) && (x.j < j) && (x.k < k) ;
  return res;
} 


//Function which can operate on a point
template <class T>
T point3<T>::magnitude(void) {
// Results will not be ideal for ints and the like
  double sum;
  T sumout;
  sum = ((double) i)* ((double)(i));
  sum += ((double) j)* ((double)(j));
  sum += ((double) k)* ((double)(k));
  sum = sqrt(sum);
  sumout = (T) sum;
  return ( sumout );
}
template <class T>
void point3<T>::normalize(void) {
// Construct the unit vector
// Results will not be ideal for ints and the like
  float mag; 
  mag = (float) this->magnitude();
  i = (T) ( (float) i / mag);
  j = (T) ( (float) j / mag);
  k = (T) ( (float) k / mag);
}


//Constructors
template <class T>
point3<T>::point3(const T x): i(x),j(x),k(x) {}

template <class T>
point3<T>::point3(const T x, const T y): i(x), j(y), k(0) {}

template <class T>
point3<T>::point3(const T x, const T y, const T z): i(x),j(y),k(x) {}

template <class T>
point3<T>::point3(const point3<T> &x): i(x.i), j(x.j), k(x.k) {}

/////////////////////////////////////////////////
class fijpt : public point3<float> {
  public:
    fijpt() {};
    fijpt(fijpt &);
    fijpt (float, float);
    fijpt & operator=(latpt &);
    fijpt & operator=(fijpt &);
    fijpt & operator=(point3<float> &);
};
// Global variable for misc. usage
fijpt global_fijpt;

class ijpt : public point3<int> {
  public:
    ijpt() {};
    ijpt(int ii, int jj) {i = ii; j = jj;};
    ijpt(int ii, int jj, int kk) {i = ii; j = jj; k = kk;} ;
    //~ijpt() {};
//    ijpt& operator=(fijpt &);
    ijpt& operator=(fijpt );
    ijpt& operator=(point3<int> &);
};
/////////////////////////////////////////////////
inline ijpt& ijpt::operator=(fijpt x) {
  i = (int)(x.i+0.5);
  j = (int)(x.j+0.5);
  k = (int)(x.k+0.5);
  return *this;
}
inline ijpt& ijpt::operator=(point3<int> &x) {
  i = x.i;
  j = x.j;
  k = x.k;
  return *this;
}
inline fijpt & fijpt::operator=(fijpt &x) {
  i = x.i;
  j = x.j;
  k = x.k;
  return *this;
}
inline fijpt & fijpt::operator=(latpt &x) {
  i = x.lon;
  j = x.lat;
  k = 0.;
  return *this;
}
inline fijpt::fijpt(fijpt &x) {
  i = x.i;
  j = x.j;
  k = x.k;
}
inline fijpt::fijpt(float x, float y) {
  i = x;
  j = y;
  k = 0.;
}
inline fijpt & fijpt::operator=(point3<float> &x) {
  i = x.i;
  j = x.j;
  k = x.k;
  return *this;
}


//Member initializers
template <class T>
point3<T>::point3(void): i(0),j(0),k(0) {}

#endif
