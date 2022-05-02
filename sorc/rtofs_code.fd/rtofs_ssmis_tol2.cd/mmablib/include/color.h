#include <cstdio>
// Class for working with colors and palettes of colors
// 13 May 1997
// Robert Grumbine
//
// Modifications -- palette class:
//    3 Dec 1998: Add color palette options beyond construction
//   16 Jun 1999: Drop stdlib.h, macros.h
//   24 Jun 1999: Convert palette to an arbitrary template, vs. <unsigned char>
//    3 Feb 2000: -- completed
//   10 May 2005: Add 'lighten' to palette class, operator=, copy constructor
//   15 May 2008: member initialization list method
// Color Class
//   25 Feb 2000: Base code as 'colortest.h'
//    4 Feb 2009: Added to color.h, operational, file.
//                RG Note: At this point, the palette doesn't pay attention to 
//                   the color class.  This is something to revise!

#ifndef COLORH

#define COLORH
#ifndef POINTH
  #include "points.h"
#endif
#ifndef MVECTORH
  #include "mvector.h"
#endif

//Reference point: typedef point3<unsigned char> color;

#define RED i
#define GREEN j
#define BLUE k

template<class T>
class palette {
  public:
    int ncol, cbase;
    point3<T> *pal;
  public:
    palette();
    palette(const int );
    palette(const int , const int);
    palette(const palette<T> &);
    palette<T>& operator=(const palette<T> &);
// Operate on whole palettes:
    void resize(const int );
    void color_bar(const int, const int);
    void print();
    void set_color(const point3<T> &);
    void lighten(const float );
    void lighten(const int, const float );
    void darken(const float );
    void invert();
// Operate on single colors (would be better to declare color class):
    void set_color(const int, const point3<T> &);
    void set_color(const int, const int, const int, const int );
// looking inside:
    void get_color(const int, point3<T> &);
};

template<class T>
palette<T>& palette<T>::operator=(const palette<T> &x) {
  ncol = x.ncol;
  cbase = x.cbase;
  pal = new point3<T>[x.ncol];  
  for (int i = 0; i < ncol; i++) {
    pal[i] = x.pal[i];
  } 
  return *this;
}
template<class T>
palette<T>::palette(const palette<T> &x) {
  ncol = x.ncol;
  cbase = x.cbase;
  pal = new point3<T>[x.ncol];  
  for (int i = 0; i < ncol; i++) {
    pal[i] = x.pal[i];
  } 
}
template<class T>
void palette<T>::resize(const int n) {
  delete pal;
  ncol = n;
  cbase = 65;
  pal   = new point3<T>[ncol] ;
}

template<class T>
palette<T>::palette(): ncol(0), cbase(65), pal((point3<T> *) NULL) {
//palette<T>::palette() {
  #ifdef VERBOSE 
    cout << "Constructing a null palette\n"; cout.flush();
  #endif
//  ncol = 0;
//  cbase = 65;
//  pal = (point3<T> *) NULL;
}
template<class T>
palette<T>::palette(const int n) {
  point3<T> white;
  white.RED = 255; white.GREEN = 255; white.BLUE = 255;
  #ifdef VERBOSE 
    cout << "Constructing a palette with N colors\n"; cout.flush();
  #endif
  ncol = n;
  cbase = 65;
  pal   = new point3<T>[ncol] ;
  this->set_color(white);
}


/* Create the National Geographic ice color bar 8 January 1996 */
template<class T>
palette<T>::palette(const int a, const int b): ncol(a), cbase(b), pal(new point3<T>[a]) {
//palette<T>::palette(const int a, const int b) {
  int j;

  point3<T> pal1[19] ; 
  point3<T> pal10(  0, 55, 80), pal11(  0, 70,100), pal12( 10, 85,100);
  point3<T> pal13( 20, 85, 80), pal14( 25,100, 80), pal15( 35,100,100);
  point3<T> pal16( 50,100, 90), pal17( 40,100, 50), pal18( 65,100, 40);
  point3<T> pal19( 85, 95, 40), pal110(100, 70, 30), pal111(100, 50, 20);
  point3<T> pal112(100, 35, 35), pal113(100,  0,  0), pal114(100,100,100); 
  point3<T> pal115(100, 91,100), pal116(100, 75,100), pal117( 50, 50, 50); 
  point3<T> pal118(  0,  0,  0 );

  #ifdef VERBOSE 
    cout << "Constructing national geographic palette \n"; cout.flush();
  #endif

  pal1[0] = pal10;
  pal1[1] = pal11;
  pal1[2] = pal12;
  pal1[3] = pal13;
  pal1[4] = pal14;
  pal1[5] = pal15;
  pal1[6] = pal16;
  pal1[7] = pal17;
  pal1[8] = pal18;
  pal1[9] = pal19;
  pal1[10] = pal110;
  pal1[11] = pal111;
  pal1[12] = pal112;
  pal1[13] = pal113;
  pal1[14] = pal114;
  pal1[15] = pal115;
  pal1[16] = pal116;
  pal1[17] = pal117;
  pal1[18] = pal118;
  #ifdef VERBOSE 
    cout << "Finished setting vector \n"; cout.flush();
  #endif

// Note that you shouldn't declare the elements of the class, they're
//   already around!
  //ncol = a;
  //cbase = b;
  //pal   = new point3<T>[ncol] ; // constructor
  #ifdef VERBOSE 
    cout << "newed the palette pointer\n"; cout.flush();
  #endif

  for ( j = 0; j < min(ncol,19) ; j++ ) {
     pal[(ncol - 1) - j].RED   = (T) (((float)pal1[j].RED   * 255 
                                                    +0.5 )/ 100 );
     pal[(ncol - 1) - j].GREEN = (T) (((float)pal1[j].GREEN * 255 
                                                    +0.5 )/ 100 );
     pal[(ncol - 1) - j].BLUE  = (T) (((float)pal1[j].BLUE  * 255 
                                                    +0.5 )/ 100 );
  }

// Fix to odd issue with Blue 7 March 2011
   pal[5].BLUE =    0 ;
   pal[6].BLUE =   89 ;
   pal[7].BLUE =   51 ;
   pal[8].BLUE =   76 ;
   pal[9].BLUE =  102 ;
   pal[10].BLUE = 102 ;
   pal[11].BLUE = 127 ;
   pal[12].BLUE = 229 ;
   pal[13].BLUE = 255 ;
   pal[14].BLUE = 204 ;
   pal[15].BLUE = 204 ;
   pal[16].BLUE = 255 ;
   pal[17].BLUE = 255 ;
   pal[18].BLUE = 204 ;


  #ifdef VERBOSE
    cout << "Leaving palette::(int, int)\n"; cout.flush();
  #endif
  return;
}

template<class T>
void palette<T>::print() {
  int k;
  printf("ncol = %d charbase = %d\n", ncol, cbase);
  for (k = 0; k < ncol; k++) {
     printf("rgb %d %d %d  \n",
        (int) pal[k].RED, (int) pal[k].GREEN, (int) pal[k].BLUE);
  }
  return;
}


template<class T>
void palette<T>::color_bar(const int celli, const int cellj) {
// Print out blocks of color (celli*cellj) for constructing color bars
  int k, j, i;
  char fname[80];
  FILE *sout;

  for (k = 0; k < ncol; k++) {
    sprintf(fname, "dcolcell%d.xpm", k);
    sout = fopen(fname,"w");
 
    /* XPM header */ 
    fprintf(sout,"/* XPM */\n");
    fprintf(sout,"static char * %s [] = {\n",fname );
    fprintf(sout,"\"%d %d %d %d\",\n", celli, cellj, 1 , 1);
    fprintf(sout, "\"%c c #%02x%02x%02x\",\n", (char) (0+ cbase), 
              (int)pal[k ].RED, (int)pal[ k ].GREEN, (int)pal[ k ].BLUE );  

    /* Print out the pix map */
    for (j = cellj - 1; j >= 0; j-- ) {
      fprintf(sout,"\"");
      for (i = 0; i < celli; i++) {
        fprintf(sout,"%c", 0  +  cbase);
      }
      fprintf(sout,"\",\n");
    }
    fprintf(sout," }; \n");
    fclose(sout);
    
  }
/* End of printing out the color cells */

  sout = fopen("bar2.html","w");
  for (k = 0; k < ncol; k++) {
    fprintf(sout, "<dt><img src=\"dcolcell%d.gif\"> Level %d rgb %d %d %d\n", 
              k, k,
              (int)pal[k ].RED, (int)pal[ k ].GREEN, (int)pal[ k ].BLUE );  
  }
  fclose(sout);

}

////////////////////////////////////////////////
// Tools to create palettes and modify colors:
template<class T>
void palette<T>::set_color(const point3<T> &x) {
  int i;
  for (i = 0; i < ncol; i++) {
    pal[i] = x;
  }
  return;
}
template<class T>
void palette<T>::set_color(const int n, const point3<T> &x) {
  pal[n] = x;
  return;
}
template<class T>
void palette<T>::set_color(const int n, const int red, const int green, const int blue) {
  //pal[n].RED   = max(red, 0);
  //pal[n].GREEN = max(green, 0);
  //pal[n].BLUE  = max(blue, 0);
// Ensure that max is 255 -- will want to reconsider for some color codes.
  pal[n].RED   = min( max(red, 0), 255);
  pal[n].GREEN = min( max(green, 0), 255);
  pal[n].BLUE  = min( max(blue, 0), 255);

  return;
}
template<class T>
void palette<T>::lighten(const int n, const float frac) {
//Added 10 May 2005
  point3<T> white, del;
  white.RED = 255; white.GREEN = 255; white.BLUE = 255;
  del = white; del -= pal[n] ;
  del.RED   = (T) (del.RED   * frac + 0.5);
  del.GREEN = (T) (del.GREEN * frac + 0.5);
  del.BLUE  = (T) (del.BLUE  * frac + 0.5);
  pal[n] += del;
  return;
}
template<class T>
void palette<T>::lighten(const float frac) {
  point3<T> white, del;
  int i;
  white.RED = 255; white.GREEN = 255; white.BLUE = 255;
  for (i = 0; i < ncol; i++) {
    del = white; del -= pal[i] ;
    del.RED   = (int) (del.RED   * frac + 0.5);
    del.GREEN = (int) (del.GREEN * frac + 0.5);
    del.BLUE  = (int) (del.BLUE  * frac + 0.5);
    pal[i] += del;
  }
  return;
}
template<class T>
void palette<T>::darken(const float frac) {
  int i;
  for (i = 0; i < ncol; i++) {
    pal[i].RED   =  (T) ((1.-frac)*pal[i].RED   + 0.5);
    pal[i].GREEN =  (T) ((1.-frac)*pal[i].GREEN + 0.5);
    pal[i].BLUE  =  (T) ((1.-frac)*pal[i].BLUE  + 0.5);
  }
  return;
}
template<class T>
void palette<T>::invert() {
  point3<T> white, tmp;
  int i;
  white.RED = 255; white.GREEN = 255; white.BLUE = 255;
  for (i = 0; i < ncol; i++) {
    tmp = white; tmp -= pal[i];
    pal[i] = tmp;
  }
  return;
}

// 4 February 2009 Bring in old color class:
// RG Note: Note that this is assuming that we're working with unsigned char.
class color {
  public :
    unsigned char red, green, blue;
    color& operator=(point3<unsigned char> &);
    color operator+=(color );
    color operator-=(color );
    float magnitude();
    void brighten(float );
    void brightness(float );
    void darken(float );
    color negate();
};
color& color::operator=(point3<unsigned char> &x) {
  this->red   = x.i;
  this->green = x.j; 
  this->blue  = x.k;
  return *this;
} 
color color::operator+=(color x) {
  color y;
  y.red   = (unsigned char) min(255, (int)x.red + (int)red);
  y.green = (unsigned char) min(255, (int)x.green + (int)green);
  y.blue  = (unsigned char) min(255, (int)x.blue + (int)blue);
  return y;
}
color color::operator-=(color x) {
  color y;
  y.red   = (unsigned char) min(0, (int)x.red - (int)red);
  y.green = (unsigned char) min(0, (int)x.green - (int)green);
  y.blue  = (unsigned char) min(0, (int)x.blue - (int)blue);
  return y;
}
void color::brighten(float x) {
  red   = min( (unsigned char) 255,
               (unsigned char) ( 0.5 + ( (float)red *x)) );
  green = min( (unsigned char) 255,
               (unsigned char) ( 0.5 + ( (float)green  * x)) );
  blue  = min( (unsigned char) 255,
               (unsigned char) ( 0.5 + ( (float)blue  * x)) );
} 
void color::darken(float x) {
  red = (unsigned char) ( 0.5 + ( (float)red/x));
  green = (unsigned char) ( 0.5 + ( (float)green / x));
  blue  = (unsigned char) ( 0.5 + ( (float)blue / x));
}
float color::magnitude() {
  point3<float> x;
  x.i = red;
  x.j = green;
  x.k = blue;
  return (x.magnitude());
}
void color::brightness(float x) {
  float mag, mag2;
  mag = this->magnitude();
  mag2 = x / mag ; 
  this->brighten(mag2);
}
color color::negate() {
  color x;
  x.red   = 255 - red;
  x.green = 255 - green;
  x.blue  = 255 - blue;
  return x;
}

template<class T>
void palette<T>::get_color(const int n, point3<T> &x) {
  x = pal[n];
  return;
}

#endif
