#ifndef _GRID_H
#define _GRID_H

typedef unsigned char mask_t;

typedef struct {
  int     self;
  int     type;                   /* grid type                      */
  int     prec;                   /* grid precision                 */
  mask_t *mask;
  mask_t *mask_gme;
  double *xvals;
  double *yvals;
  double *area;
  double *xbounds;
  double *ybounds;
  double  xfirst, yfirst;
  double  xlast, ylast;
  double  xinc, yinc;
  double  lcc_originLon;          /* Lambert Conformal Conic        */
  double  lcc_originLat;
  double  lcc_lonParY;
  double  lcc_lat1;
  double  lcc_lat2;
  double  lcc_xinc;
  double  lcc_yinc;
  int     lcc_projflag;
  int     lcc_scanflag;
  int     lcc_defined;
  double  lcc2_lon_0;             /* Lambert Conformal Conic 2      */
  double  lcc2_lat_0;
  double  lcc2_lat_1;
  double  lcc2_lat_2;
  double  lcc2_a;
  int     lcc2_defined;
  double  laea_lon_0;             /* Lambert Azimuthal Equal Area   */
  double  laea_lat_0;
  double  laea_a;
  int     laea_defined;
  double  xpole, ypole, angle;    /* rotated north pole             */
  int     isCyclic;               /* TRUE for global cyclic grids   */
  int     isRotated;              /* TRUE for rotated grids         */
  int     xdef;                   /* 0: undefined 1:xvals 2:x0+xinc */
  int     ydef;                   /* 0: undefined 1:yvals 2:y0+yinc */
  int     nd, ni, ni2, ni3;       /* parameter for GRID_GME         */
  int     number, position;       /* parameter for GRID_REFERENCE   */
  char   *reference;
  int     trunc;                  /* parameter for GRID_SPECTEAL    */
  int     nvertex;
  int    *rowlon;
  int     nrowlon;
  int     size;
  int     xsize;
  int     ysize;
  int     locked;
  int     lcomplex;
  char    xname[256];
  char    yname[256];
  char    xlongname[256];
  char    ylongname[256];
  char    xstdname[256];
  char    ystdname[256];
  char    xunits[256];
  char    yunits[256];
}
grid_t;


const double *gridInqXvalsPtr(int gridID);
const double *gridInqYvalsPtr(int gridID);

double *gridInqXboundsPtr(int gridID);
double *gridInqYboundsPtr(int gridID);
const double *gridInqAreaPtr(int gridID);

int gridCompare(int gridID, grid_t grid);
int gridGenerate(grid_t grid);

#endif
