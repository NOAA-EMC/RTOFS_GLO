#include <stdio.h>
#include <netcdf.h>

/* For C++ emulation from C */
typedef int bool;
#define false (1==0)
#define true (1==1)

#include "ssmisu.h"
/* Items for the concentration algorithm: */
#define SSMIS_ANTENNA 0
#define SSMIS_GR37LIM 0.05
#define SSMIS_GR22LIM 0.045
#define BAD_DATA 166
#define WEATHER  177
extern float nasa_team(float t19v, float f19h, float t22v, float t37v, float t37h,
                float t92v, float t92h, float t150h, const char pole, const int ant,
                const int satno);
#define NFREQS 8
#define LAND 157
#define MIXED 195

int l1b_to_l2(ssmisupt *a, float *concentration, int *qc, float *land);

extern int tb_filter(float *tb, float *concentration, int *flag) ;
extern int waters(float tb1, float tb2, bool alpha, float crit, bool under, float *concentration, int *qc, float *land) ;
extern int lands (float tb1, float tb2, bool alpha, float crit, bool under, float *concentration, int *qc, float *land) ;

/* For the algorithm */
int bad_low = 0;
int bad_high = 0;
int crop_low = 0;
int filt37   = 0;
int filt22   = 0;

/* New version 22 August 2018 --
  Write out netcdf
*/
#define NDIMS 1
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); fflush(stdout); }

/* ////////////////  For intercepting binary and writing out flat /////////// */
#ifdef BINOUT
FILE *flatout;
#endif

#ifdef IBM
int close_nc(int *ncid) {
#elif LINUX
int close_nc_(int *ncid) {
#else
int close_nc_(int *ncid) {
#endif
  return nc_close(*ncid);
}

#ifdef IBM
int open_nc(int *unit, int *ncid, int *platform, int *varid) {
#elif LINUX
int open_nc_(int *unit, int *ncid, int *platform, int *varid) {
#else
int open_nc_(int *unit, int *ncid, int *platform, int *varid) {
#endif
  char fname[24];
  int rec_dimid, dimids[NDIMS];
  float freqs[NFREQS] = {150.0, 19.35, 19.35, 22.235, 37.0, 37.0, 91.655, 91.655};
  int polar[NFREQS] = {0, 0, 1, 1, 0, 1, 1, 0};
  int nfreqs = NFREQS;

  int varid_tb19v, varid_tb19h, varid_tb22v, varid_tb37v, varid_tb37h;
  int varid_tb92v, varid_tb92h, varid_tb150h;
  int varid_lat, varid_lon, varid_sic, varid_qc, varid_land, varid_dtg1, varid_dtg2;
  int retcode;

  #ifdef BINOUT
  flatout = fopen("flatout","w");
  #endif

  sprintf(fname,"l2out.f%03d.%02d.nc",*platform, *unit);

  retcode = nc_create(fname, NC_CLOBBER, ncid);
  printf("create retcode %d\n",retcode); fflush(stdout);
  if (retcode != 0) ERR(retcode);

  nc_def_dim(*ncid, "nobs", NC_UNLIMITED, &rec_dimid);
  dimids[0] = rec_dimid;
/*  header information (past nobs) */
   nc_put_att_int(*ncid, NC_GLOBAL, "platform_id", NC_INT, 1, platform);
   nc_put_att_int(*ncid, NC_GLOBAL, "n_frequencies", NC_INT, 1, &nfreqs);
   nc_put_att_int(*ncid, NC_GLOBAL, "polarizations", NC_INT, nfreqs, &polar[0]);
   nc_put_att_float(*ncid, NC_GLOBAL, "frequencies", NC_FLOAT, nfreqs, &freqs[0]);
/* info for each obs: */
  nc_def_var(*ncid, "longitude", NC_DOUBLE, NDIMS, dimids, &varid_lon);
  nc_def_var(*ncid, "latitude", NC_DOUBLE, NDIMS, dimids, &varid_lat);
  nc_def_var(*ncid, "ice_concentration", NC_FLOAT, NDIMS, dimids, &varid_sic);
  nc_def_var(*ncid, "quality", NC_INT, NDIMS, dimids, &varid_qc);

  retcode = nc_def_var(*ncid, "land_flag", NC_FLOAT, NDIMS, dimids, &varid_land);
  printf("def var land retcode = %d\n",retcode); fflush(stdout);
  if (retcode != 0) ERR(retcode);

  retcode = nc_def_var(*ncid, "dtg_yyyymmdd", NC_INT, NDIMS, dimids, &varid_dtg1);
  if (retcode != 0) ERR(retcode);
  retcode = nc_def_var(*ncid, "dtg_hhmm", NC_INT, NDIMS, dimids, &varid_dtg2);
  if (retcode != 0) ERR(retcode);

  nc_def_var(*ncid, "tb_19V", NC_FLOAT, NDIMS, dimids, &varid_tb19v);
  nc_def_var(*ncid, "tb_19H", NC_FLOAT, NDIMS, dimids, &varid_tb19h);
  nc_def_var(*ncid, "tb_22V", NC_FLOAT, NDIMS, dimids, &varid_tb22v);
  nc_def_var(*ncid, "tb_37V", NC_FLOAT, NDIMS, dimids, &varid_tb37v);
  nc_def_var(*ncid, "tb_37H", NC_FLOAT, NDIMS, dimids, &varid_tb37h);
  nc_def_var(*ncid, "tb_92V", NC_FLOAT, NDIMS, dimids, &varid_tb92v);
  nc_def_var(*ncid, "tb_92H", NC_FLOAT, NDIMS, dimids, &varid_tb92h);
  nc_def_var(*ncid, "tb_150H", NC_FLOAT, NDIMS, dimids, &varid_tb150h);
/* exit definition section */

  nc_enddef(*ncid);
  varid[0] = varid_lon;
  varid[1] = varid_lat;
  varid[2] = varid_sic;
  varid[3] = varid_qc;
  varid[4] = varid_land;
  varid[5] = varid_dtg1;
  varid[6] = varid_tb19v;
  varid[7] = varid_tb19h;
  varid[8] = varid_tb22v;
  varid[9] = varid_tb37v;
  varid[10] = varid_tb37h;
  varid[11] = varid_tb92v;
  varid[12] = varid_tb92h;
  varid[13] = varid_tb150h;
  varid[14] = varid_dtg2;

  return 0;
}

/* ssmisout */
#ifdef IBM
void ssmisout_nc(double *hdr, double *ident, double *chan, int *ncid, int *varid, int *nwrites) {
#elif LINUX
void ssmisout_nc_(double *hdr, double *ident, double *chan, int *ncid, int *varid, int *nwrites) {
#else
void ssmisout_nc_(double *hdr, double *ident, double *chan, int *ncid, int *varid, int *nwrites) {
#endif

  ssmisupt x;
  int bad = 0, k, j, retcode;
  int dtg1, dtg2;
  float conc, land, tb[NFREQS];
  int nobs, qc, nfreqs = NFREQS;

  x.year   = (short int)     hdr[1];
  x.month  = (unsigned char) hdr[2];
  x.day    = (unsigned char) hdr[3];
  x.hour   = (unsigned char) hdr[4];
  x.minute = (unsigned char) hdr[5];
  x.second = (unsigned char) hdr[6];
  x.satid = (short int) ident[0];

  x.clat = hdr[7];
  x.clon = hdr[8];
  if (x.clat > 90 || x.clon > 360 || x.clon < -360) bad += 1;
  k = 0;
  for (j = 0; j < 24; j++) {
    /* We're only extracting a few channels, hence looking for j */
    if (j == 7 || (j >= 11 && j <= 17) ) {
      x.obs[k].tmbr    = chan[j*4 + 1];
      tb[k] = chan[j*4 + 1];
      if (x.obs[k].tmbr > 350) bad += 1;
      k += 1;
    }
  }

  size_t start[NDIMS]; start[0] = (size_t) *nwrites;
  size_t count[NDIMS]; count[0] = 1;
  int nerr = 0;
  land = 0.0;

  if (bad == 0 && (x.clat > 24.0 || x.clat < -30.0) ) {
/* Can call the algorithm from here -- thence to write out netcdf */
    nerr = 0;
    nerr = process_bufr(&x);
    if (nerr == 0) {
  
/* could be filtering land, water here, then let l1b_to_l2 do concentration/weather filtering */
     nobs = l1b_to_l2(&x, &conc, &qc, &land);

      dtg1  = x.year;
      dtg1 *= 100; dtg1 += x.month;
      dtg1 *= 100; dtg1 += x.day;

      dtg2 = x.hour;
      dtg2 *= 100; dtg2 += x.minute;

      nc_put_vara_double(*ncid, varid[0], start, count, &x.clon);
      nc_put_vara_double(*ncid, varid[1], start, count, &x.clat);
      nc_put_vara_float(*ncid, varid[2], start, count, &conc);
      nc_put_vara_int  (*ncid, varid[3], start, count, &qc);
      nc_put_vara_float(*ncid, varid[4], start, count, &land);
      retcode = nc_put_vara_int (*ncid, varid[5], start, count, &dtg1);
      if (retcode != 0) ERR(retcode);
     
      nc_put_vara_float(*ncid, varid[6], start, count, &tb[0]);
      nc_put_vara_float(*ncid, varid[7], start, count, &tb[1]);
      nc_put_vara_float(*ncid, varid[8], start, count, &tb[2]);
      nc_put_vara_float(*ncid, varid[9], start, count, &tb[3]);
      nc_put_vara_float(*ncid, varid[10], start, count, &tb[4]);
      nc_put_vara_float(*ncid, varid[11], start, count, &tb[5]);
      nc_put_vara_float(*ncid, varid[12], start, count, &tb[6]);
      nc_put_vara_float(*ncid, varid[13], start, count, &tb[7]);

      retcode = nc_put_vara_int (*ncid, varid[14], start, count, &dtg2);
      if (retcode != 0) ERR(retcode);

/* For flat binary: */
      #ifdef BINOUT
      fwrite(&x.clon, sizeof(x.clon), 1, flatout);
      fwrite(&x.clat, sizeof(x.clat), 1, flatout);
      fwrite(&conc  , sizeof(conc)  , 1, flatout);
      fwrite(&qc    , sizeof(qc)    , 1, flatout);
      fwrite(&land  , sizeof(land)  , 1, flatout);
      fwrite(&tb[0] , sizeof(float) , nfreqs, flatout);
      #endif
/* /////////////////////////////////////////////////////////////////////// */

      *nwrites += 1;
    }
  }

 return;
}
int l1b_to_l2(ssmisupt *a, float *concentration, int *qc, float *land) {
  float tb[NFREQS];
  int nobs;

  tb[0] = (float)a->obs[0].tmbr;
  tb[1] = (float)a->obs[1].tmbr;
  tb[2] = (float)a->obs[2].tmbr;
  tb[3] = (float)a->obs[3].tmbr;
  tb[4] = (float)a->obs[4].tmbr;
  tb[5] = (float)a->obs[5].tmbr;
  tb[6] = (float)a->obs[6].tmbr;
  tb[7] = (float)a->obs[7].tmbr;

  *concentration = 1.0;
  *qc = 5;      /* 1-5, 5 is worst */
  *land = 1./4. + 1./16.; /* want this to be about 0.30, but do this for exactness */
  int i, j, flag = 0, sum = 0;

/* tb filter: */
   tb_filter(tb, concentration, &flag);
   if (flag == LAND) {
     *land = 0.99;
     *qc   = 3;
     sum += 1;
   }
   else if (flag == MIXED) {
     *land = 0.50;
     *qc   = 3;
     sum += 1;
   }

/* for delta filtering: */
   int w1 = 0, w2 = 0, w3 = 0, w4 = 0;
   int l1 = 0, l2 = 0, l3 = 0, l4 = 0;
   float crit;
   bool alpha, under;

/* Water filters:
 From Sept 23, 2018:
     j = 2; i = 3; alpha = true; crit = 0.05; under = false;
     j = 0; i = 5; alpha = true; crit = 0.11; under = true;
     j = 0; i = 4; alpha = true; crit = 0.28; under = true;
     j = 3; i = 4; alpha = true; crit = 0.20; under = true;
 From 20 August 2018
alpha.07:O 2 3  123564 0.073
alpha.28:U 0 4   61666 0.036
alpha.20:O 4 7   47025 0.028
alpha.20:U 3 4   21734 0.013
*/

     w1 = 0, w2 = 0, w3 = 0, w4 = 0;
j = 2; i = 3; alpha = true; under = false; crit = 0.07;
     w1 =  waters(tb[i], tb[j], alpha, crit, under, concentration, qc, land);
j = 0; i = 4; alpha = true; under = true; crit = 0.28;
     w2 =  waters(tb[i], tb[j], alpha, crit, under, concentration, qc, land);
j = 4; i = 7; alpha = true; under = false; crit = 0.20;
     w3 =  waters(tb[i], tb[j], alpha, crit, under, concentration, qc, land);
j = 3; i = 4; alpha = true; under = true; crit = 0.20;
     w4 =  waters(tb[i], tb[j], alpha, crit, under, concentration, qc, land);

/* Land filters:
 From 23 Sep 2018
     j = 1; i = 2; alpha = false; crit = 0.008; under = true;
     j = 1; i = 3; alpha = false; crit = 0.002; under = true;
     j = 4; i = 5; alpha = false; crit = 0.005; under = true;
 From 20 Aug 2018
beta.005: U 1 2   78593 0.046
beta.005: U 4 5   65519 0.039
alpha.002:U 1 3   32038 0.019
*/
     l1 = 0, l2 = 0, l3 = 0, l4 = 0;
j = 1; i = 2; alpha = false; under = true; crit = 0.005;
     l1 =  lands(tb[i], tb[j], alpha, crit, under, concentration, qc, land);
j = 4; i = 5; alpha = false; under = true; crit = 0.005;
     l2 =  lands(tb[i], tb[j], alpha, crit, under, concentration, qc, land);
j = 1; i = 3; alpha = true;  under = true; crit = 0.002;
     l3 =  lands(tb[i], tb[j], alpha, crit, under, concentration, qc, land);

/* prep for regular usage of algorithm -- restore land to 0 */
  if ((*land) == (1./4. + 1./16.) ) {
    *land = 0.0;
  }
  /* printf("skel %f\n",*land); */
  if (*land != 0.0) return 1;
/* only proceed if we didn't filter the point already */

/* //////////////////////////////////////////////////////////////
// done with delta ratio filters and tb filters, ready to call algorithm
////////////////////////////////////////////////////////////// */
  float tlat, tlon;
  float t19v, t19h, t22v, t37v, t37h, t92v, t92h, t150h;
  float nasa;
  int satno, ant = SSMIS_ANTENNA;
  char pole;

  satno = a->satid ;

  tlat = ( (float)a->clat);
  tlon = ( (float)a->clon);
  t19v = ( (float)a->obs[SSMIS_T19V].tmbr );
  t19h = ( (float)a->obs[SSMIS_T19H].tmbr );
  t22v = ( (float)a->obs[SSMIS_T22V].tmbr );
  t37v = ( (float)a->obs[SSMIS_T37V].tmbr );
  t37h = ( (float)a->obs[SSMIS_T37H].tmbr );
  t92v = ( (float)a->obs[SSMIS_T92V].tmbr );
  t92h = ( (float)a->obs[SSMIS_T92H].tmbr );
  t150h = ( (float)a->obs[SSMIS_T150H].tmbr );
  if (tlat > 0) {
    pole = 'n';
  }
  else {
    pole = 's';
  }
  nasa = nasa_team(t19v, t19h, t22v, t37v, t37h, t92v, t92h, t150h, pole, ant, satno);
  if (nasa == BAD_DATA || nasa == WEATHER || nasa < 0. ) {
    *qc = 5;
    *land = 0.5;
    *concentration = 0.0;
  }
  else { /* valid concentration: */
    *qc = 1;
    *land = 0.0;
    *concentration = nasa / 100.;
    if (*concentration > 1.  ) *concentration = 1.0;
    if (*concentration < 0.15) *concentration = 0.0;
  }

  return sum;
}
