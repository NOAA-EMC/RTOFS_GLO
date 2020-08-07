#include <stdio.h>

#define MAXOBS (5*1000*1000)
#define NFREQS 8

#include "ncepgrids.h"
#include "mvector.h"

int acquire(mvector<float> &lat, mvector<float> &lon, mvector<float> &sflag, mvector<float> *tb, int &count) ;

int main(int argc, char *argv[]) {
  mvector<float> lat(MAXOBS), lon(MAXOBS);
  mvector<float> sflag(MAXOBS);

  mvector<float> tb[NFREQS];

  int ret, count;
  float flag = 330.0;
  float tover, tunder;

  int i;
/////
  ret = acquire(lat, lon, sflag, tb, count);
  printf("tb acquire = %d\n",ret);

  FILE *fin;
  global_12th<float> icec;
  global_12th<unsigned char> land;
  global_12th<float> distance;

  tover  = atof(argv[1]);
  tunder = atof(argv[2]);
  printf("over is > %f under is < %f\n",tover, tunder);

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
  int index, j, k, ncoast = 0, nwater = 0, nice = 0, nland = 0;

  grid2<int> nover(NFREQS,NFREQS), nunder(NFREQS,NFREQS);
  grid2<int> nover_land(NFREQS,NFREQS), nover_water(NFREQS,NFREQS), nover_coast(NFREQS,NFREQS), nover_ice(NFREQS,NFREQS);
  grid2<int> nunder_land(NFREQS,NFREQS), nunder_water(NFREQS,NFREQS), nunder_coast(NFREQS,NFREQS), nunder_ice(NFREQS,NFREQS);

  grid2<float> dr(NFREQS,NFREQS);

  nwater = 0;
  for (i = 0; i < NFREQS; i++) { 
  for (j = 0; j < NFREQS; j++) { 
    index = i + j*NFREQS;
    nover[index] = 0;
    nunder[index] = 0;
    nover_land[index] = 0;
    nunder_land[index] = 0;
    nover_water[index] = 0;
    nunder_water[index] = 0;
    nover_coast[index] = 0;
    nunder_coast[index] = 0;
    nover_ice[index] = 0;
    nunder_ice[index] = 0;
  }
  }

  int nused = 0;
  for (k = 0; k < count; k++) {
    // should be taken care of already if (lat[i] < 24.0 && lat[i] > -45.0) continue; // skip the 'tropics'
    if (sflag[k] != 0.0) continue; // skip points prior filter is confident are not ice
    //printf("k %d %f\n",k,sflag[k]);

    nused++;

    ll.lat = lat[k];
    ll.lon = lon[k];
    floc = icec.locate(ll);
    if (land[floc] == 195) {
      ncoast++;
    }
    if (land[floc] == 157) {
      nland++;
    }
    if (land[floc] == 0) {
      nwater++;
    }
    if (icec[floc] > 0 && icec[floc] < 1.28) {
      nice++;
    }

    for (j = 0; j < NFREQS-1; j++) {
    for (i = j+1; i < NFREQS; i++) {
      index = i + j*NFREQS;
      dr[index] = (tb[i][k]-tb[j][k])/(tb[i][k]+tb[j][k]);

      if (dr[index] > tover) nover[index]++;
      if (land[floc] == 157 && dr[index] > tover) nover_land[index]++;
      if (land[floc] == 195 && dr[index] > tover) nover_coast[index]++;
      if (land[floc] == 0  && dr[index] > tover && icec[floc] == 0.) nover_water[index]++;
      if (land[floc] == 0  && dr[index] > tover && icec[floc] != 0.0) { nover_ice[index]++; }

      if (dr[index] < tunder) nunder[index]++;
      if (land[floc] == 157 && dr[index] < tunder) nunder_land[index]++;
      if (land[floc] == 195 && dr[index] < tunder) nunder_coast[index]++;
      if (land[floc] == 0  && dr[index] < tunder && icec[floc] == 0.) nunder_water[index]++;
      if (land[floc] == 0  && dr[index] < tunder && icec[floc] != 0.0) { nunder_ice[index]++; }
    }
    }

  }
  printf("nused = %d count = %d\n",nused, count);

  float pland  =  (float) nland / (float) nused;
  float pcoast =  (float) ncoast / (float) nused;
  float pwater =  (float) nwater / (float) nused;
  float pice   =  (float) nice / (float) nused;
  grid2<float> pover(NFREQS,NFREQS), punder(NFREQS, NFREQS);
  grid2<float> pover_land(NFREQS,NFREQS), pover_water(NFREQS,NFREQS), pover_coast(NFREQS,NFREQS), pover_ice(NFREQS,NFREQS);
  grid2<float> punder_land(NFREQS,NFREQS), punder_water(NFREQS,NFREQS), punder_coast(NFREQS,NFREQS), punder_ice(NFREQS,NFREQS);

  printf("pland, coast, water, ice %f %f %f %f\n",pland, pcoast, pwater, pice);
  
  for (j = 0; j < NFREQS-1; j++) {
  for (i = j+1; i < NFREQS; i++) {
    index = i + j*NFREQS;

    pover[index] =  (float)nover[index] / (float) nused;
    pover_water[index] = (float) nover_water[index] / (float) nwater;
    pover_land [index] = (float) nover_land [index] / (float) nland;
    pover_coast [index] = (float) nover_coast [index] / (float) ncoast;
    pover_ice [index] = (float) nover_ice [index] / (float) nice;

    punder[index] =  (float)nunder[index] / (float) nused;
    punder_water[index] = (float) nunder_water[index] / (float) nwater;
    punder_land [index] = (float) nunder_land [index] / (float) nland;
    punder_coast [index] = (float) nunder_coast [index] / (float) ncoast;
    punder_ice [index] = (float) nunder_ice [index] / (float) nice;

    if (nover[index] > 0 && nover[index] != nused) {
    printf("O %1d %1d %7d %5.3f  %6.4f %6.4f %6.4f %f   water %f land %f coast %f ice %f\n",
      j,i,nover[index], pover[index], 
      pover_water[index], pover_land[index], pover_coast[index], pover_ice[index],
          pover_water[index]*pwater/pover[index],
          pover_land [index]*pland /pover[index],
          pover_coast[index]*pcoast /pover[index],
          pover_ice  [index]*pice  /pover[index]
      );
    }

    if (nunder[index] > 0 && nunder[index] != nused) {
    printf("U %1d %1d %7d %5.3f  %6.4f %6.4f %6.4f %f   water %f land %f coast %f ice %f\n",
      j,i,nunder[index], punder[index], 
      punder_water[index], punder_land[index], punder_coast[index], punder_ice[index],
          punder_water[index]*pwater/punder[index],
          punder_land [index]*pland /punder[index],
          punder_coast[index]*pcoast /punder[index],
          //punder_water[index]*pwater/punder[index] + punderland [index]*pland /punder[index] + pundercoast[index]*pcoast /punder[index],
          punder_ice  [index]*pice  /punder[index]
      );
    }
  }
  }


  return 0;
}

////////////////////////////////////////////////////////////////
int acquire(mvector<float> &lat, mvector<float> &lon, mvector<float> &sflag, mvector<float> *tb, int &count) {
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

  int freq = 1000;
  while (!feof(flatout)) {
    //if ((count% freq) == 0) {  printf("%d *1k obs\n",count/ freq); fflush(stdout); }
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
      for (i = 0; i < nfreqs; i++) {
        tb[i][count] = tmp[i];
      }
       
      count++;
    }
  }
  fclose(flatout);
  printf("sflag max min = %f %f\n",sflag.maximum(), sflag.minimum() );
  printf("lon max min = %f %f\n",lon.maximum(), lon.minimum() );

  return 0;
}
