#include "resops.h"
#include "params.h"
#include "genes.h"

#define MAXPTS 1000
#define POPULATION 50
#define GENMAX 100

////////////////////////////////////////////////////////////////////////
#include "get_gulfstream.C"
#include "vector_calculus.C"
#include "dist.C"
#include "scorer.C"
#include "maxontrace.C"
#include "maxperim.C"
#include "set_offset.C"
#include "crossing.C"
#include "casting.C"
////////////////////////////////////////////////////////////////////////
void genes(hycom<float> &ssh, hycom<float> &gradsq, hycom<float> &tlapl, 
           mvector<fijpt> &navy, int navycount, FILE *fout, FILE* gsout, 
           FILE* lcout) ;
bool lethal(hycom<float> &x, mvector<fijpt> &points, int npts) ;
////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[]) {
  hycom<float> mask, ssh, sst;
  hycom<float> gradsq_ssh, diverg_ssh, ssh_dx, ssh_dy, u, v;
  hycom<float> gradsq_sst, sst_dx, sst_dy;
  FILE *fin, *fout, *gsout, *lcout;
  float flag2, tflag ;
  ijpt loc;
  latpt ll;
  mvector<fijpt> gspoints(MAXPTS), lcpoints(MAXPTS);
  int i, navycount;
  mvector<fijpt> navypoints(MAXPTS);
  char dum2[900], dum3[900], dum4[900];

// Get the gulf stream from file nout:
  get_gulfstream(ssh, navycount, navypoints, dum2, dum3, dum4);
// Working with ssh from grib, so C binary read
  fin = fopen(argv[1],"r");
  ssh.binin(fin);
  fclose(fin);
  fin = fopen(argv[2],"r");
  sst.binin(fin);
  fclose(fin);
  tflag = sst.gridmax();
  printf("sst max, min, average %f %f %f\n",sst.gridmax(tflag), sst.gridmin(tflag), sst.average(tflag) );


  flag2 = ssh.gridmax();

// Mask out peripheral sea surface heights due to model problems and fact
//  that they're far from the gulf stream.
  int buffer = 90;
  mask.set((float) 0.);
  for (loc.j = 0; loc.j < ssh.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ssh.xpoints(); loc.i++) {
    ll = ssh.locate(loc);
 
    // flag buffer zone around upper edges -- 
    //   kludge for hycom model error 4/13/2005
    if (loc.i < ssh.xpoints() - buffer && loc.j < ssh.ypoints() - buffer ) {
      // do nothing: ssh[loc] = val[loc];
    }
    else {
      ssh[loc] = flag2;
    }
                                                                                
    if (ssh[loc] == flag2) mask[loc] = flag2;

    if (sst[loc] == tflag) sst[loc] = 273.15;
  }
  }
  sst -= 273.15;
  printf("sst max, min, average %f %f %f\n",sst.gridmax(), sst.gridmin(), sst.average() );

  vector_calculus(ssh, ssh_dx, ssh_dy, flag2);
  vector_calculus(sst, sst_dx, sst_dy, flag2);
  diverg_ssh.div(ssh_dx, ssh_dy);
  for (loc.j = 0; loc.j < ssh.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ssh.xpoints(); loc.i++) {
    gradsq_sst[loc] = sqrt(
       sst_dx[loc]*sst_dx[loc] + sst_dy[loc]*sst_dy[loc]   );
    gradsq_ssh[loc] = sqrt(
       ssh_dx[loc]*ssh_dx[loc] + ssh_dy[loc]*ssh_dy[loc]   );
  }
  }

  gradsq_sst *= 1.e3; // rescale to approx mag of gradsq_ssh
  gradsq_ssh *= 1.e5; // rescale to proportional to approx geostrophic velocity
  diverg_ssh *= 1.e9; // rescale to approx nondimensional vorticity (lap = div(grad))
  printf("ssh max, min, average %f %f %f\n",ssh.gridmax(flag2), 
                    ssh.gridmin(flag2), ssh.average(flag2) );
  printf("ssh_ddx max, min, average %e %e %e\n",ssh_dx.gridmax(flag2), 
                    ssh_dx.gridmin(flag2), ssh_dx.average(flag2) );
  printf("ssh_ddy max, min, average %e %e %e\n",ssh_dy.gridmax(flag2), 
                    ssh_dy.gridmin(flag2), ssh_dy.average(flag2) );
  printf("sst gradsq max, min, average %e %e %e\n",gradsq_sst.gridmax(), 
                    gradsq_sst.gridmin(), gradsq_sst.average() );
  printf("gradsq max, min, average %e %e %e\n",gradsq_ssh.gridmax(), 
                    gradsq_ssh.gridmin(), gradsq_ssh.average() );
  printf("vorticity max, min, average %e %e %e\n",diverg_ssh.gridmax(), 
                    diverg_ssh.gridmin(), diverg_ssh.average() );


// Genes portion:
   //genes(ssh, gradsq_ssh, diverg_ssh, navypoints, navycount);
   fout = fopen(argv[3], "w"); // file to write out genes
   gsout = fopen(argv[4], "w"); // file for gs listing
   lcout = fopen(argv[5], "w"); // file for lc listing
   if (fout == (FILE*) NULL || gsout == (FILE *) NULL || 
       lcout == (FILE*) NULL) {
     printf("failed to open one or more of genes out, gulf stream out, or loop current output files\n");
     return 1;
   }
   genes(gradsq_sst, gradsq_ssh, diverg_ssh, navypoints, navycount, 
            fout, gsout, lcout);

  return 0;
}

//////////////////////////////////////////////////
// Everything else will have been set up in caller, start working on genetics:
void genes(hycom<float> &ssh, hycom<float> &gradsq, hycom<float> &tlapl, 
           mvector<fijpt> &navy, int navycount, FILE *fout, FILE* gsout, 
           FILE* lcout) {
  mvector<fijpt> gspoints(MAXPTS), lcpoints(MAXPTS);
  int i, ngs, nlc;
  latpt ll;
  hycom<float> tmp, tmp2, tmp3, alt;

// Genes:
  float best, mean = 0;
  int generation=0, genmax = GENMAX;
  genetic_code x(3);
  mvector<united> weights;
  mvector<mvector<int> > genome(POPULATION);
  mvector<float> scores(POPULATION);  
  united f1, f2;

  f1.fval = 0; f2.fval = 10.;
  x.newgene(0, 8, FLOAT_TYPE, f1, f2); //weight for gradsq
  f1.fval = -10.; f2.fval = 10.;
  x.newgene(1, 8, FLOAT_TYPE, f1, f2); //weight for lapl
  f1.fval = -10; f2.fval = 10.;
  x.newgene(2, 8, FLOAT_TYPE, f1, f2); //weight for alt field

// Fresh start:
  weights.resize(x.ncodes);
  #ifdef VERBOSE
    printf("code length = %d\n",x.code_length); fflush(stdout);
  #endif

  for (i = 0; i < POPULATION; i++) {
     genome[i].resize(x.code_length);
     newgenes(genome[i]);
     #ifdef VERBOSE
       transcriber(genome[i], weights, x);
       showgenes(stdout, genome[i], x);
       fflush(stdout);
     #endif
  }

  scores = (float) 0.;
  generation = 0;

// Construct alternate field for 3rd weight:
//  alt.set((float) 0.0);
//  ijpt loc;
//  for (loc.j = 0; loc.j < alt.ypoints(); loc.j++) {
//  for (loc.i = 0; loc.i < alt.xpoints(); loc.i++) {
//    //alt[loc] = sqrt(gradsq[loc]);
//    alt[loc] = gradsq[loc]*gradsq[loc];
//  }
//  }
  alt = ssh;

  do {
    #ifdef VERBOSE
    printf("in while loop, generation = %d\n",generation); fflush(stdout);
    fflush(stdout);
    #endif
    for (i = 0; i < POPULATION; i++) {
      #ifdef VERBOSE
        printf("in do loop, i = %d\n",i); fflush(stdout);
        fflush(stdout);
      #endif
      //if (scores[i] == 0.0) {
        transcriber(genome[i], weights, x);
        
        tmp  = gradsq;  tmp *= weights[0].fval;
        //tmp  = gradsq;  //tmp *= weights[0].fval;
        tmp2 = tlapl;  tmp2 *= weights[1].fval;
        tmp3 = alt;    tmp3 *= weights[2].fval;
        tmp += tmp2;
        tmp += tmp3;
        castings(tmp, gspoints, ngs);
        scores[i] = 14.77/scorer(tmp, gspoints, ngs, navy, navycount);
        if (lethal(tmp, gspoints, ngs)) scores[i] = 0.;

        #ifdef VERBOSE
          printf("trial %f ", scores[i]); fflush(stdout);
          showgenes(stdout, genome[i], x); fflush(stdout);
        #endif
      //}
      
    }
    best = scores.maximum();
    mean = scores.average();
    printf("epoch %d generation %d scores: %f %f %f %f\n", 0, generation,
       scores.maximum(),  scores.average(),  scores.rms(),  scores.norm(4) );
    fflush(stdout);

    reproducer(genome, scores);
    order(genome, scores);

    printf("\ngeneration %3d top 15 list\n", generation);
    for (i = 0; i < 15; i++) {
      printf("score %f ",scores[i]);
      showgenes(stdout, genome[i], x);
        fflush(stdout);
    }
    printf("\n");

    grazer(genome, scores);
    generation += 1;

  } while ( generation < genmax );
  printf("final best, mean %f %f ratio %f generation %d\n",best, mean,
                        best/mean, generation);

// Now get and print out the best one:
  transcriber(genome[0], weights, x);
  tmp  = gradsq;  tmp *= weights[0].fval;
  //tmp  = gradsq;  //tmp *= weights[0].fval;
  tmp2 = tlapl;  tmp2 *= weights[1].fval;
  tmp3 = alt;    tmp3 *= weights[2].fval;
  tmp += tmp2;
  tmp += tmp3;
// Separate version to get loop current as well
  castings(tmp, gspoints, ngs, lcpoints, nlc);
  printf("RMS grid point error of best genes: %f, score %f\n",
                  scorer(tmp, navy, navycount, gspoints, ngs),
            14.7/ scorer(tmp, navy, navycount, gspoints, ngs) );
  for (i = 0; i < ngs; i++) {
    ll = tmp.locate(gspoints[i]);
    printf("%4d %f %f\n",i, ll.lat, ll.lon);
    fprintf(gsout, "%4d %f %f\n",i, ll.lat, ll.lon);
  }
  for (i = 0; i < nlc; i++) {
    ll = tmp.locate(lcpoints[i]);
    printf("%4d %f %f\n",i, ll.lat, ll.lon);
    fprintf(lcout, "%4d %f %f\n",i, ll.lat, ll.lon);
  }

  x.write(fout);
  for (i = 0; i < genome.xpoints(); i++) {
    showgenes(fout, genome[i], x);
  } 

  return;
}

//  Consider it a lethal mutation if the curve goes west of 83 or
//  north of 50
bool lethal(hycom<float> &x, mvector<fijpt> &points, int npts) {
  float westmax = -83, northmax = 48;
  latpt ll;
  for (int i = 0; i < npts; i++) {
     ll = x.locate(points[i]);
     if (ll.lat > northmax || ll.lon < 360.+westmax) return true;
  }
  return false;
}
