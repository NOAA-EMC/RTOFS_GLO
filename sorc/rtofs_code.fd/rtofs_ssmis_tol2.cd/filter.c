#include <stdio.h>


#define NFREQS 8

#define LAND 157
#define MIXED 195

/* For C++ emulation from c */
typedef int bool;
#define false (1==0)
#define true  (1==1)

int tb_filter(float *tb, float *concentration, int *flag) ;
int waters(float tb1, float tb2, bool alpha, float crit, bool under, float *concentration, int *qc, float *land) ;
int lands (float tb1, float tb2, bool alpha, float crit, bool under, float *concentration, int *qc, float *land) ;

int tb_filter(float *tb, float *concentration, int *flag) {

/* Derived from F17: */
  float thot[NFREQS] = {275., 260., 270., 268., 260., 268., 272., 271.} ; 
  float tcold[NFREQS] = {155., 50., 175., 180., 130., 172., 170., 160.};
/* F18 note: cannot use 150 GHz
  float thot[NFREQS] = {275., 260., 270., 268., 260., 268., 272., 271.} ; 
  float tcold[NFREQS] = {155., 50., 175., 180., 130., 172., 170., 160.};
*/

  int i, j;
  bool hot = false, cold = false, cold4 = false, is_land = false, mixed = false;
  *flag = 0;
  *concentration = 2.24;

  for (j = 0; j < NFREQS; j++) {
    if (tb[j] > thot[j]) {
      hot = true;
      if (j == 1 || j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
        is_land = true;
      }
      else {
        mixed = true;
      }
    }
    if (tb[j] < tcold[j]) {
      if (j != 4) {
        cold = true;
      }
      else {
        cold4 = true;
      }
    }

    } /* nfreqs */

  if (is_land ) {
    *flag = LAND;
    *concentration = 0;
  }
  else if (mixed) {
    *flag = MIXED;
    *concentration = 0;
  }
  if (cold) {
    *flag = LAND;
    *concentration = 0;
  }
  else if (cold4) {
    *flag = MIXED;
    *concentration = 0;
  }

  return 0;
}
/* //////////////////////////////////////////////////////////////// */
int waters(float tb1, float tb2, bool alpha, float crit, bool under, float *concentration, int *qc, float *land) {
  float dr = (tb1-tb2)/(tb1+tb2);

/* better than alpha vs beta is to use dr vs. -dr ? */

/* Filters to flag never-ice water points: */
  if (alpha ) { /* normal sense */
    if (!under ) {
      if ( dr > crit) {
        *land = 0.01;
        *qc   = 4;
        *concentration = 0.0;
        return 1;
      }
    }
    else {
      if ( dr < -crit) {
        *land = 0.02;
        *qc   = 4;
        *concentration = 0.0;
        return 1;
      }
    }
  }
  else { /* beta */
    printf("in beta branch in waters -- error\n"); fflush(stdout);
    return -1;
  }

  return 0;
}
/* //////////////////////////////////////////////////////////////////////// */
int lands(float tb1, float tb2, bool alpha, float crit, bool under, float *concentration, int *qc, float *land) {
  float dr = (tb1-tb2)/(tb1+tb2);

/* Filters to flag never-ice land points: */
  if (alpha ) { /* normal sense */
    if (!under ) {
      if ( dr > crit) {
        *land = 0.97;
        *qc   = 4;
        *concentration = 0.0;
        return 1;
      }
    }
    else {
      if ( dr < -crit) {
        *land = 0.98;
        *qc   = 4;
        *concentration = 0.0;
        return 1;
      }
    }
  }
  else { /* beta */
    if (!under) {
      if ( dr > -crit) {
        *land = 0.96;
        *qc   = 4;
        *concentration = 0.0;
        return 1;
      }
    }
    else if ( dr < crit) {
        *land = 0.99;
        *qc   = 4;
        *concentration = 0.0;
        return 1;
    }
  }

  return 0;
}
