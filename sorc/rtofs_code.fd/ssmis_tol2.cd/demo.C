#include <stdio.h>

#define MAXOBS (5*1000*1000)
#define NFREQS 8

#include "ncepgrids.h"
#include "mvector.h"

int acquire(mvector<float> &lat, mvector<float> &lon, mvector<float> &sflag, mvector<float> &ncconc, mvector<float> *tb, int &count) ;
int waters(float tb1, float tb2, bool alpha, float crit, bool under, float *concentration, int *qc, float *land) ;
int lands (float tb1, float tb2, bool alpha, float crit, bool under, float *concentration, int *qc, float *land) ;



int main(int argc, char *argv[]) {
  mvector<float> lat(MAXOBS), lon(MAXOBS);
  mvector<float> sflag(MAXOBS);
  mvector<float> ncconc(MAXOBS);
  mvector<float> tb[NFREQS];

  int ret, count;
  int i;
/////
  ret = acquire(lat, lon, sflag, ncconc, tb, count);
  printf("tb acquire = %d count = %d\n",ret, count); fflush(stdout);

  FILE *fin;
  global_12th<float> icec;
  global_12th<unsigned char> land;
  global_12th<float> distance;

  fin = fopen("icec", "r");
  icec.binin(fin);
  fclose(fin);
  fin = fopen("seaice_gland5min","r");
  land.binin(fin);
  fclose(fin);
  fin = fopen("seaice_alldist.bin","r");
  distance.binin(fin);
  fclose(fin);

  latpt ll;
  fijpt floc;
  ijpt  loc;
  int j, k;
//      flag testing
   int w1 = 0, w2 = 0, w3 = 0, w4 = 0;
   int l1 = 0, l2 = 0, l3 = 0, l4 = 0;
   float alpha, crit;
   bool under;
   float flagland, concentration;
   int qc;

  for (k = 0; k < count; k++) {

    ll.lat = lat[k];
    ll.lon = lon[k];
    floc = icec.locate(ll);
    printf("%6.2f %7.2f  %4.2f %3d %6.1f  %4.2f %4.2f ", lat[k], lon[k], sflag[k], land[floc], distance[floc]/1000., ncconc[k], icec[floc]);

    #ifndef FILTER
      printf(" %5.2f \n",ncconc[k] - icec[floc]);
    #else
// Water filters:
     w1 = 0, w2 = 0, w3 = 0, w4 = 0;
     flagland = 0.0;
// From Sept 23, 2018:
//     j = 2; i = 3; alpha = true; crit = 0.05; under = false;
//     j = 0; i = 5; alpha = true; crit = 0.11; under = true;
//     j = 0; i = 4; alpha = true; crit = 0.28; under = true;
//     j = 3; i = 4; alpha = true; crit = 0.20; under = true;
// From 20 August 2018
//alpha.07:O 2 3  123564 0.073  
//alpha.28:U 0 4   61666 0.036 
//alpha.20:O 4 7   47025 0.028
//alpha.20:U 3 4   21734 0.013

j = 2; i = 3; alpha = true; under = false; crit = 0.07;
     w1 =  waters(tb[i][k], tb[j][k], alpha, crit, under, &concentration, &qc, &flagland);
j = 0; i = 4; alpha = true; under = true; crit = 0.28;
     w2 =  waters(tb[i][k], tb[j][k], alpha, crit, under, &concentration, &qc, &flagland);
j = 4; i = 7; alpha = true; under = false; crit = 0.20;
     w3 =  waters(tb[i][k], tb[j][k], alpha, crit, under, &concentration, &qc, &flagland);
j = 3; i = 4; alpha = true; under = true; crit = 0.20;  
     w4 =  waters(tb[i][k], tb[j][k], alpha, crit, under, &concentration, &qc, &flagland);
     printf("w %1d %1d %1d %1d %1d  %4.2f",(w1+w2+w3+w4), w1, w2, w3, w4, flagland);
// Land filters:
// From 23 Sep 2018
//     j = 1; i = 2; alpha = false; crit = 0.008; under = true;
//     j = 1; i = 3; alpha = false; crit = 0.002; under = true;
//     j = 4; i = 5; alpha = false; crit = 0.005; under = true;
// From 20 Aug 2018
//beta.005:U 1 2   78593 0.046  
//beta.005:U 4 5   65519 0.039 
//alpha.002:U 1 3   32038 0.019 
     flagland = 0.0;
     l1 = 0, l2 = 0, l3 = 0, l4 = 0;
j = 1; i = 2; alpha = false; under = true; crit = 0.005;
     l1 =  lands(tb[i][k], tb[j][k], alpha, crit, under, &concentration, &qc, &flagland);
j = 4; i = 5; alpha = false; under = true; crit = 0.005;
     l2 =  lands(tb[i][k], tb[j][k], alpha, crit, under, &concentration, &qc, &flagland);
j = 1; i = 3; alpha = true;  under = true; crit = 0.002;
     l3 =  lands(tb[i][k], tb[j][k], alpha, crit, under, &concentration, &qc, &flagland);
     printf(" l %1d %1d %1d %1d %1d  %4.2f\n",(l1+l2+l3+l4), l1, l2, l3, l4, flagland);

    #endif

  }

  return 0;
}

////////////////////////////////////////////////////////////////
int acquire(mvector<float> &lat, mvector<float> &lon, mvector<float> &sflag, mvector<float> &ncconc, mvector<float> *tb, int &count) {
  int qc;
  double clat, clon;
  int nfreqs = NFREQS;
  float tmp[nfreqs], top[nfreqs], bottom[nfreqs];
  float conc, land, flag = 330.0;
  FILE *flatout;
  int i;

  count = 0;
  for (i = 0; i < nfreqs; i++) {
    top[i] = 0.0;
    bottom[i] = flag;
    tb[i].resize(MAXOBS);
    tb[i] = flag; 
  }

  flatout = fopen("flatout","r");
  if (flatout == (FILE*) NULL) {
    printf("failed to open flatout\n");
    return 1;
  }

  while (!feof(flatout)) {
    fread(&clon, sizeof(clon), 1, flatout);
    fread(&clat, sizeof(clat), 1, flatout);
    fread(&conc, sizeof(conc), 1, flatout);
    fread(&qc, sizeof(qc), 1, flatout);
    fread(&land, sizeof(land), 1, flatout);
    fread(&tmp[0], sizeof(float), nfreqs, flatout);
    for (i = 0; i < nfreqs; i++) {
      if (tmp[i] > top[i]   ) top[i]    = tmp[i];
      if (tmp[i] < bottom[i]) bottom[i] = tmp[i];
    }
    
    if (!feof(flatout)) {
      lat[count] = (float) clat;
      lon[count] = (float) clon;
      sflag[count] = land;
      ncconc[count] = conc;
      for (i = 0; i < nfreqs; i++) {
        tb[i][count] = tmp[i];
      }
       
      count++;
    }
  }
  fclose(flatout);

  return 0;
}
/////////////////////////////////////

////////////////////////////////////////////////////////////////////////
int lands(float tb1, float tb2, bool alpha, float crit, bool under, float *concentration, int *qc, float *land) {
  float dr = (tb1-tb2)/(tb1+tb2);

// Filters to flag never-ice land points: 
  if (alpha ) { // normal sense 
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
  else { // beta
    if (!under) {
      if ( dr > -crit) {
        *land = 0.96;
        *qc   = 4;
        *concentration = 0.0; 
        return 1;
      }
    }
    else if ( dr < crit) {
        *land = 0.98;
        *qc   = 4;
        *concentration = 0.0; 
        return 1;
    }
  }

  return 0;
}
int waters(float tb1, float tb2, bool alpha, float crit, bool under, float *concentration, int *qc, float *land) {
  float dr = (tb1-tb2)/(tb1+tb2);

// better than alpha vs beta is to use dr vs. -dr ?

// Filters to flag never-ice water points: 
  if (alpha ) { // normal sense 
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
  else { // beta
    printf("in beta branch in waters -- error\n"); fflush(stdout);
    return -1;
  }

  return 0;
}
