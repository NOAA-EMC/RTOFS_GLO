#include "resops.h"

//Will be reading in degribbed native horizontal grid:
int main(int argc, char *argv[]) {
  hycom<float> ssh, sst, mld;
  hycom<float> avging;
  int i;
  FILE *fin;

  ssh.set((float) 0.);
  sst.set((float) 0.);
  mld.set((float) 0.);

// Get the surface elevations and average them:
  fin = fopen(argv[1], "r");
  for (i = 0; i < 25; i++) {
    avging.binin(fin);
    ssh += avging;
  } 
  fclose(fin);
  ssh /= 25;

  fin = fopen(argv[2], "w");
  ssh.binout(fin);
  fclose(fin);
   
  return 0;
}
