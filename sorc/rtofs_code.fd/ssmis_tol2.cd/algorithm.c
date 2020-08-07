#include <stdio.h>
/* Various defined values/flags/...  Moved here from ncepgrids 2006 Jan 30 */
#define LAND     157
#define COAST    195
#define BAD_DATA 166
#define WEATHER  177
#define NO_DATA  224
#define OCEAN      0
#define MIN_CONC  15
#define MAX_CONC 127
#define MAX_ICE  128
/* Note that these indices refer to the ssmisupt, rather than the */
/*   ice spot values I'll be using later */
#define SSMIS_T150H 0
#define SSMIS_T19H 1
#define SSMIS_T19V 2
#define SSMIS_T22V 3
#define SSMIS_T37H 4
#define SSMIS_T37V 5
#define SSMIS_T92V 6
#define SSMIS_T92H 7
/* Items for the concentration algorithm: */
#define SSMIS_ANTENNA 0
#define SSMIS_GR37LIM 0.05
#define SSMIS_GR22LIM 0.045

/* For error/filtration statistics */
extern int bad_low;
extern int bad_high;
extern int crop_low;
extern int filt37;
extern int filt22;


void antenna(float *t19v, float *t19h, float *t22v, float *t37v, float *t37h,
             float *t92v, float *t92h, float *t150h);

void sat_regress(float t19v, float t19h, float t22v, float t37v, float t37h,
              float t92v, float t92h, float t150h,
              float *nt19v, float *nt19h, float *nt22v, float *nt37v, 
              float *nt37h, float *nt92v, float *nt92h, float *nt150h, int satno);

float nasa_team(float t19v, float t19h, float t22v, 
                float t37v, float t37h, 
                float t92v, float t92h, float t150h, 
                const char pole, const int ant, const int satno) {
/* Implementation of the NASA Team algorithm with 22 GHz weather
     filter.  C version by Robert Grumbine, based on Fortran code by
     M. Martino.  3/23/94.

C                                                                
C   THIS PROGRAM USES THE TEAM ALGORITHM TO CALCULATE THE TOTAL ICE
C   CONCENTRATION AND THE MULTIYEAR ICE CONCENTRATION.  INPUT ARE 
C   19 VERT, 19 HORZ AND 37 VERT, 37 HORZ BRIGHTNESS TEMPERATURES.
C                                                                 
C     GR FILTER IS .05                        
C    SSMI TIE POINTS  (1=19H, 4=37V)

C   Using the recalibrated tie points for SSMI-S, F17, by Walt Meier
C 6 October 2011
C                              
*/
/* North Pole tie points */
      /* Order is 19H, 19V, 22V, 37V */
      float t0n[7] = {116.5, 182.2, 00.0, 206.5, 0., 0., 0.};
      float tfn[7] = {235.4, 251.7, 00.0, 242.7, 0., 0., 0.};
      float tmn[7] = {199.0, 223.4, 00.0, 188.1, 0., 0., 0.};

/* SOUTH POLE TIE POINTS */
      float t0s[7] = {118.4, 187.7, 00.0, 208.9, 0., 0., 0.};
      float tfs[7] = {241.1, 256.2, 00.0, 246.4, 0., 0., 0.};
      float tms[7] = {214.8, 246.9, 00.0, 212.6, 0., 0., 0.};

/* tie points within function */
      float *T0, *TF, *TM;
      float DW[2], DF[2], DM[2];
      float SW[2], SF[2], SM[2];

/* Local variables */
      float gr37, gr22, polar;
      float nf19, nm19, dd19, fy, my, total;
      float a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1 ;
      float nt19v, nt19h, nt22v, nt37v, nt37h, nt92v, nt92h, nt150h;

/* For new weather filtering 18 August 2010 */
      float grYY;

/* Preliminary check on whether the data are useable */
       if ( (t22v + t19v) == 0. || (t19v + t19h) == 0. || (t37v + t19v)==0. )
       {
         return (float) BAD_DATA;
       }

/*
C   CALCULATE PARAMETERS FOR ICE CONCENTRATION ALGORITHM          
*/                                                                 
                           
      if (pole == 'n') {
        T0 = &t0n[0];
        TF = &tfn[0];
        TM = &tmn[0];
      }
      else if (pole == 's') {
        T0 = &t0s[0];
        TF = &tfs[0];
        TM = &tms[0];
      }
      else {
        printf("specified a pole that doesn't exist!!\n");
        return -1.;
      }  

       DW[0]=T0[1]-T0[0];
       DF[0]=TF[1]-TF[0];
       DM[0]=TM[1]-TM[0];

       SW[0]=T0[1]+T0[0];
       SF[0]=TF[1]+TF[0];
       SM[0]=TM[1]+TM[0];

       DW[1]=T0[3]-T0[1];
       DF[1]=TF[3]-TF[1];
       DM[1]=TM[3]-TM[1];

       SW[1]=T0[3]+T0[1];
       SF[1]=TF[3]+TF[1];
       SM[1]=TM[3]+TM[1];

       a1=DM[0]*DW[1]-DM[1]*DW[0];
       b1=DM[1]*SW[0]-DW[1]*SM[0];
       c1=DW[0]*SM[1]-DM[0]*SW[1];
       d1=SM[0]*SW[1]-SM[1]*SW[0]; 
/*       d1=SM[0]*SW[0]-SM[1]*SW[0]; */

       i1=DF[1]*DW[0]-DF[0]*DW[1];
       j1=DW[1]*SF[0]-DF[1]*SW[0];
       k1=SW[1]*DF[0]-DW[0]*SF[1];
       l1=SF[1]*SW[0]-SF[0]*SW[1];

       e1=DF[0]*(DM[1]-DW[1])+DW[0]*(DF[1]-DM[1])+DM[0]*(DW[1]-DF[1]); 
       f1=DF[1]*(SM[0]-SW[0])+DW[1]*(SF[0]-SM[0])+DM[1]*(SW[0]-SF[0]); 
       g1=DF[0]*(SW[1]-SM[1])+DW[0]*(SM[1]-SF[1])+DM[0]*(SF[1]-SW[1]); 
/*       g1=DF[0]*(SW[0]-SM[1])+DW[0]*(SM[1]-SF[1])+DM[0]*(SF[1]-SW[1]); */
       h1=SF[1]*(SW[0]-SM[0])+SW[1]*(SM[0]-SF[0])+SM[1]*(SF[0]-SW[0]);

/* Recompute the brightness temperatures, if needed, via the Abdalati, 
   1995 calibration of F-8 versus F-11, or Grumbine verification of
   F-11 to F-8 being ok for F13,14,15 as well*/
/* Regressions for F-18, F-16 to F-17 now, too 9 July 2014 */
         sat_regress(t19v, t19h, t22v, t37v, t37h, t92v, t92h, t150h,
                  &nt19v, &nt19h, &nt22v, &nt37v, &nt37h, &nt92v, &nt92h, &nt150h,
                  satno); 

/* Add 19 August 2010 -- recover utility of F15 even though team
     weather filter relies on 22V channel that is corrupted */
/* Leaving in the old filter as-is since it will only result in over-filtering
     of ice, vs. letting bad data through */
/* Note that this type of filter is probably a good idea for all SSMI, and
     for reruns */
      if (satno == 15) {
        grYY = (t37v-t92v)/(t37v+t92v);
        if (t92h > 266.875610 || grYY < -0.085 ) {
          total = 0;
          return total;
        }
      }


/* Now, finally, compute the ice concentration */
       gr22  =  (nt22v - nt19v) / (nt22v + nt19v);
       polar =  (nt19v - nt19h) / (nt19v + nt19h);
       gr37  =  (nt37v - nt19v) / (nt37v + nt19v);

       total = (float) NO_DATA;
       if ( (gr37 <= (float) SSMIS_GR37LIM) && (gr22 <= (float) SSMIS_GR22LIM) ) {
           nf19=a1+b1*polar+c1*gr37+d1*polar*gr37;
           nm19=i1+j1*polar+k1*gr37+l1*polar*gr37;
           dd19=e1+f1*polar+g1*gr37+h1*polar*gr37;
           if (dd19 == 0.) {  printf("zero divisor for concentrations\n");
                              return (float) BAD_DATA; }
           fy = nf19/dd19;
           my = nm19/dd19;
           total = (my + fy)*100.;
           if (total < -20.0 ) {
              bad_low += 1;
              return (float) BAD_DATA;
           }
           if (total <   0.0 ) {
              crop_low += 1;
              total = 0.; 
           }
           if (total > MAX_CONC) {
              bad_high += 1;
              return (float) BAD_DATA;
           }
       }
       else {
/* Set weather filtered points to weather flag 
   Tracking of which filter was triggered added 16 March 2004 */
        if (gr37 > SSMIS_GR37LIM) filt37 += 1;
        if (gr22 > SSMIS_GR22LIM) filt22 += 1;
        total = (float) WEATHER; 
       }
      
      return total;
}
void antenna(float *TB19V, float *TB19H, float *TB22V, 
             float *TB37V, float *TB37H, float *TB92V, float *TB92H, float *TB150H) {
      float YES19V, YES19H, YES22V, YES37V, YES37H, YES92V, YES92H;

      float AP19V = 0.969, AP19H = .969, AP22V = .974;
      float AP37V = .986, AP37H = .986,  AP92V = .988, AP92H = .988;
      float BP19V = .00473, BP19H = .00415, BP22V = .0107; 
      float BP37V = .0217, BP37H = .02612, BP92V = .01383, BP92H = .01947;

      float C19V, C19H, C22V, C37V, C37H, C92V, C92H;
      float D19V, D19H, D22V, D37V, D37H, D92V, D92H;

      C19V = 1/(AP19V*(1-BP19V));
      C19H = 1/(AP19H*(1-BP19H));
      C22V = 1/(AP22V*(1-BP22V));
      C37V = 1/(AP37V*(1-BP37V));
      C37H = 1/(AP37H*(1-BP37H));
      C92V = 1/(AP92V*(1-BP92V));
      C92H = 1/(AP92H*(1-BP92H));
      D19V = C19V*BP19V;
      D19H = C19H*BP19H;
      D22V = C22V*BP22V;
      D37V = C37V*BP37V;
      D37H = C37H*BP37H;
      D92V = C92V*BP92V;
      D92H = C92H*BP92H;

        YES19V = C19V * (*TB19V) - D19V * (*TB19H);
        YES19H = C19H * (*TB19H) - D19H * (*TB19V);
        YES22V = C22V * (*TB22V) - D22V * (0.653 * (*TB19H) + 96.6);
        YES37V = C37V * (*TB37V) - D37V * (*TB37H);
        YES37H = C37H * (*TB37H) - D37H * (*TB37V);
        YES92V = C92V * (*TB92V) - D92V * (*TB92H);
        YES92H = C92H * (*TB92H) - D92H * (*TB92V);

     *TB19V = YES19V;
     *TB19H = YES19H;
     *TB22V = YES22V;
     *TB37V = YES37V;
     *TB37H = YES37H;
     *TB92V = YES92V;
     *TB92H = YES92H;

     return;
}     
void sat_regress(float t19v, float t19h, float t22v, float t37v, float t37h,
              float t92v, float t92h, float t150h, 
              float *nt19v, float *nt19h, float *nt22v, 
              float *nt37v, float *nt37h,
              float *nt92v, float *nt92h, float *nt150h, int satno) {
/* Recompute the brightness temperatures if needed for, say, new instruments */
/* At this point (7 February 2012) no need to do so as F17 is the reference
    instrument
 9 July 2014: Add in F18 (satno = 286), F16 (satno = 249) regressions
 F17 = 285
*/
   if (satno == 249) {
     *nt150h = -1.803e+00 + 1.0233898*t150h;
     *nt19h  =  3.719e-01 + 1.0031472*t19h;
     *nt19v  = -2.501e-01 + 1.0042363*t19v;
     *nt22v  = -8.727e-02 + 1.0050771*t22v;
     *nt37h  = -4.698e-01 + 0.9978330*t37h;
     *nt37v  = -1.469e-01 + 0.9991854*t37v;
     *nt92h  = -3.138e+00 + 1.0050135*t92h;
     *nt92v  = -2.920e+00 + 0.9991504*t92v;
     return;
   }
   if (satno == 286) {
     *nt150h =  0 + 0*t150h; /* F18 150 GHz is garbage */
     *nt19h  =  1.216e+00 + 0.9970090*t19h;
     *nt19v  =  1.879e-01 + 1.0002307*t19v;
     *nt22v  =  4.054e-01 + 0.9988976*t22v;
     *nt37h  =  1.039e+00 + 0.9893275*t37h;
     *nt37v  =  8.352e-01 + 0.9952053*t37v;
     *nt92h  = -1.167e+00 + 1.0120497*t92h;
     *nt92v  =  7.059e-01 + 0.9996586*t92v;
     return;
   }

   *nt19h = t19h;
   *nt19v = t19v;
   *nt22v = t22v;
   *nt37h = t37h;
   *nt37v = t37v;
   *nt92v = t92v;
   *nt92h = t92h;
   *nt150h = t150h;

   return;
}
