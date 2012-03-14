#include "resops.h"
#include "params.h"
#include "genes.h"

#define MAXPTS 1000
#define POPULATION 50
#define GENMAX 100

// Program to trace gulf stream and loop current using already defined
//   genetics
// Robert Grumbine 14 September 2006
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
void trace(hycom<float> &ssh, hycom<float> &gradsq, hycom<float> &tlapl, 
           mvector<fijpt> &navy, int navycount, FILE *fout, FILE* gsout, 
           FILE* lcout) ;
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
   fout = fopen(argv[3], "r");  // file to write out/read in genes
   gsout = fopen(argv[4], "w"); // file for gs listing
   lcout = fopen(argv[5], "w"); // file for lc listing
   if (fout == (FILE*) NULL || gsout == (FILE *) NULL || 
       lcout == (FILE*) NULL) {
     printf("failed to open one or more of genes out, gulf stream out, or loop current output files\n");
     if (fout == (FILE*) NULL ) {
       printf(" -- failed on arg3 -- genes in/out, %s\n",argv[3]);
     }
     if (gsout == (FILE *) NULL) {
       printf(" -- failed on arg4 -- gs listing, %s\n",argv[4]);
     }
     if (lcout == (FILE *) NULL) {
       printf(" -- failed on arg5 -- lc listing, %s\n",argv[5]);
     }
     return 1;
   }
   trace(gradsq_sst, gradsq_ssh, diverg_ssh, navypoints, navycount, 
            fout, gsout, lcout);

  return 0;
}

//////////////////////////////////////////////////
// Everything else will have been set up in caller, start working on genetics:
// For ofs_trace, just read and evaluate, no genetic work is done
void trace(hycom<float> &ssh, hycom<float> &gradsq, hycom<float> &tlapl, 
           mvector<fijpt> &navy, int navycount, FILE *fout, FILE* gsout, 
           FILE* lcout) {
  mvector<fijpt> gspoints(MAXPTS), lcpoints(MAXPTS);
  int i, ngs, nlc;
  latpt ll;
  hycom<float> tmp, tmp2, tmp3, alt;

// Genes:
  genetic_code x(3);
  mvector<united> weights;
  mvector<mvector<int> > genome(POPULATION);
  mvector<float> scores(POPULATION);  

// Read in the best genome
  x.read(fout);
  x.write(stdout); // DEBUG
  weights.resize(x.ncodes);
  fscanf(fout,"%f %f %f\n",&weights[0].fval, &weights[1].fval, &weights[2].fval);
  retroscribe(genome[0], weights, x);

  alt = ssh;
  tmp  = gradsq;  tmp *= weights[0].fval;
  tmp2 = tlapl;  tmp2 *= weights[1].fval;
  tmp3 = alt;    tmp3 *= weights[2].fval;
  tmp += tmp2;
  tmp += tmp3;

// Separate version to get loop current as well
  castings(tmp, gspoints, ngs, lcpoints, nlc);
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

  return;
}
