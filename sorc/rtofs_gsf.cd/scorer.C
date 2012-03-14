template <class T>
float scorer(hycom<float> &ssh, mvector<T> &navy, int ncount, 
                                mvector<T> &ofspoints, int npts) ;
template <class T>
float scorer(hycom<float> &ssh, mvector<T> &navy, int ncount, 
                                mvector<T> &ofspoints, int npts) {
  float tdist, sumsqdist = 0.;
  int j = 0;
  latpt ll;
  float westend = -77., eastend = -65.;

  #ifdef VERBOSE
  printf("entered scorer, navycount = %d, nofspoints=%d npts=%d\n",
               navy.xpoints(), ofspoints.xpoints(), npts);
  fflush(stdout);
  #endif
  for (int i = 0 ; i < ncount;  i++) {
    if (navy[i].i == 0 && navy[i].j == 0) break;
    #ifdef VERBOSE
    printf("%f %f\n",navy[i].i, navy[i].j); fflush(stdout);
    #endif
    ll = ssh.locate(navy[i]);
    if (ll.lon < 0) ll.lon += 360.;
    if (ll.lon < 360+westend) continue;
    if (ll.lon > 360+eastend) break;    // Skip out once we're past the navy limit
    //tdist = dist(ofspoints[i], navy, ncount);
    tdist = dist(navy[i], ofspoints, npts);
    sumsqdist += tdist*tdist;
    j += 1;
  }

  //printf("counted %d points\n",j); fflush(stdout);

  if (sumsqdist <= 0.) {
    return 999.;
  }
  else {
    return sqrt(sumsqdist/ j);
  }
}
