#include <math.h>

void mapxy(float *alat, float *along, int i, int j, const float xorig,
            const float yorig, const float dx, const float dy,
            const float slat, const float slon, const float sgn,
            const float rearth, const float eccen2)
{
/* After V. Troisi program.  Given grid coordinates, compute latitude
  and longitude on a polar stereographic grid */
/* Robert Grumbine */
/* Last Modified 3 June 1994. */


  const float cdr = 57.29577951;
  const float pi  = 3.141592654;
  float e, e2;

  float x, y, sl, rho, cm, t, chi;

/*  printf("%3d %3d %8.1f %8.1f %8.1f %8.1f %5.1f %5.1f %4.1f %f %f \n",
    i, j, xorig, yorig, dx, dy, slat, slon, sgn, rearth, eccen2); */

  x = i * dx + xorig;
  y = j * dy + yorig;

  sl = slat / cdr;
  rho = sqrt(x*x+y*y);
  e   = sqrt(eccen2);
  e2  = eccen2;
  
  if (rho <= 0.1) {
	*alat  = 90. * sgn;
	*along = 0.0;
	return;
  }

  cm = cos(sl)/ sqrt(1.0-e2*sin(sl)*sin(sl) );

  t  = tan(pi/4. - sl/2.) /
	   pow(  ((1.0 - e*sin(sl))/(1.0+e*sin(sl))), e/2.);

  if ( fabs(slat - 90) < 1.e-5 ) {
	t = rho*sqrt( pow(1.+e, 1.+e) * pow(1.-e, 1.-e) ) / 2. / rearth;
  }
  else {
	t = rho * t / (rearth * cm);
  }
  
  chi = (pi/2.) - 2.*atan(t);
  *alat = chi +
		 ( e2/2. + 5.*e2*e2/24. + e2*e2*e2/12.) * sin(2.*chi) +
		 ( 7.*e2*e2/48. + 29.*e2*e2*e2/240.)    * sin(4.*chi) +
		 ( 7.*e2*e2*e2/120.                )    * sin(6.*chi) ;

  *alat = sgn* (*alat) *cdr;
  *along = sgn*atan2(sgn*y, sgn*x)*cdr - slon;

  return;
}
