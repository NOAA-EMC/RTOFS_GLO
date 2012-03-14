void castings(hycom<float> &x, mvector<fijpt> &gspoints, int &ngs);
void castings(hycom<float> &x, mvector<fijpt> &gspoints, int &ngs,
                               mvector<fijpt> &lcpoints, int &nlc);

void castings(hycom<float> &u, hycom<float> &v, hycom<float> &x, 
              mvector<fijpt> &gspoints, int &ngs, mvector<fijpt> &lcpoints, int &nlc) ;
///////////////////////////////////
void castings(hycom<float> &u, hycom<float> &v, hycom<float> &x, 
              mvector<fijpt> &gspoints, int &ngs, mvector<fijpt> &lcpoints, int &nlc) {
// Take a field, x, and a
//   geostrophic velocity field and then trace down the maxima in x.
// Start by finding the maximum in the Florida Strait to Cuba and then
//   advect down the x field.  Then go back, and advect in the opposite
//   direction.  The forward computation is the Gulf Stream, the back
//   calculation is the Loop Current
  fijpt trial;
  fijpt current, offset;
  latpt ll;
  int i;

// First, get seed points from the Florida Strait
//  maxontrace(x, -81.2, 26.0, -81.2, 22.0, gspoints[0]);
//  maxontrace(x, -81.0, 26.0, -81.0, 22.0, gspoints[1]);
  maxontrace(x, -82.0, 28.0, -77.0, 28.0, gspoints[0]);
  maxontrace(x, -82.0, 28.2, -77.0, 28.2, gspoints[1]);

  current = gspoints[1];
  offset = gspoints[1]; offset -= gspoints[0];
  offset.i /= (float) 10.;
  offset.j /= (float) 10.;
  #ifdef VERBOSE
  printf("loc 0 %f %f\n",gspoints[0].i, gspoints[0].j);
  printf("loc 1 %f %f\n",gspoints[1].i, gspoints[1].j);
  printf("offset = %f %f\n",offset.i, offset.j); fflush(stdout); 
  #endif

  for (i = 2; i < gspoints.xpoints(); i++) {
    trial = current;
    trial += offset;
    //printf("initial trial %f %f ",trial.i, trial.j);
    crossing(x, offset, trial);

    if (!x.in(trial)) break;

    ll = x.locate(trial);
    if (ll.lon > (360 - 50.)) break; // tunable, stop looking if you reach 50 W
    #ifdef VERBOSE
    printf("loc %d %f %f  %f %f\n",i, ll.lat, ll.lon, trial.i, trial.j); 
    fflush(stdout);
    #endif
    current = trial;
    gspoints[i] = trial;

    set_offset(gspoints, offset, i-1, i);
    fflush(stdout); 

  }
  ngs = i - 1;

  // Loop current branch:
// First, get seed points from the Florida Strait
  maxontrace(x, -81.5, 26.0, -81.5, 22.0, lcpoints[0]);
  maxontrace(x, -82.0, 26.0, -82.0, 22.0, lcpoints[1]);
  current = lcpoints[1];
  offset = lcpoints[1] ; offset -=lcpoints[0];
  offset.i /= (float) 10.;
  offset.j /= (float) 10.;
  #ifdef VERBOSE
  printf("loc 0 %f %f\n",lcpoints[0].i, lcpoints[0].j);
  printf("loc 1 %f %f\n",lcpoints[1].i, lcpoints[1].j);
  printf("offset = %f %f\n",offset.i, offset.j); fflush(stdout); 
  #endif

  for (int i = 2; i < lcpoints.xpoints(); i++) {
    trial = current;
    trial += offset;
    //printf("initial trial %f %f ",trial.i, trial.j);
    crossing(x, offset, trial);

    if (!x.in(trial)) break;

    ll = x.locate(trial);
    // Stop looking if we go south of 20 N
    if (ll.lat < 20.0) break;
    #ifdef VERBOSE
    printf("loc %d %f %f  %f %f\n",i, ll.lat, ll.lon, trial.i, trial.j); 
    fflush(stdout);
    #endif
    current = trial;
    lcpoints[i] = trial;

    set_offset(lcpoints, offset, i-1, i);
  }
  nlc = i - 1;

  //printf("leaving castings\n"); fflush(stdout);
  return;

}
//  Version best suited to genetics use:
void castings(hycom<float> &x, mvector<fijpt> &gspoints, int &ngs) {
// Take a field, x, and a
//   geostrophic velocity field and then trace down the maxima in x.
// Start by finding the maximum in the Florida Strait to Cuba and then
//   advect down the x field.  Then go back, and advect in the opposite
//   direction.  The forward computation is the Gulf Stream, the back
//   calculation is the Loop Current
  fijpt starter, trial;
  fijpt current, offset;
  latpt ll;
  int i, nstart = 40, npts;
  
// Clear the gspoints vector:
  starter.i = 0; starter.j = 0;
  for (i = 0; i < gspoints.xpoints(); i++) {
    gspoints[i] = starter;
  }
    
// First, get seed points from SW portion 
  float wlon = -82, elon = -78.5;
  float blat = 26.5, dlat = 0.1;
  for (i = 0; i < nstart; i++) {
    maxontrace(x, wlon, blat + i*dlat, elon, blat + i*dlat, gspoints[i]);
  }

  wlon = -80;
  elon = -73.;
  //blat += nstart*dlat;
  for (i = nstart; i < 2*nstart; i++) {
    maxontrace(x, wlon, blat + i*dlat, elon, blat + i*dlat, gspoints[i]);
  }
  npts = i;

  current = gspoints[npts-1];
  offset = gspoints[npts-1] ; offset -=gspoints[npts-2];
  offset.i /= (float) 10.;
  offset.j /= (float) 10.;
  #ifdef VERBOSE
    printf("offset = %f %f\n",offset.i, offset.j); fflush(stdout); 
  #endif

  for (i = npts; i < gspoints.xpoints(); i++) {
    trial = current;
    trial += offset;
    //printf("initial trial %f %f ",trial.i, trial.j);
    crossing(x, offset, trial);

    if (!x.in(trial)) break;

    ll = x.locate(trial);
    if (ll.lon > (360 - 50.)) break; // tunable, stop looking if you reach 50 W
    if (ll.lat < 20) break; // also stop if south of 20
    #ifdef VERBOSE
    printf("loc %d %f %f  %f %f\n",i, ll.lat, ll.lon, trial.i, trial.j); 
    fflush(stdout);
    #endif
    current = trial;
    gspoints[i] = trial;

    set_offset(gspoints, offset, i-1, i);
    fflush(stdout); 

  }
  ngs = i - 1;

  return;

}
//  Version with loop current included:
void castings(hycom<float> &x, mvector<fijpt> &gspoints, int &ngs,
                               mvector<fijpt> &lcpoints, int &nlc) {
// Take a field, x, and a
//   geostrophic velocity field and then trace down the maxima in x.
// Start by finding the maximum in the Florida Strait to Cuba and then
//   advect down the x field.  Then go back, and advect in the opposite
//   direction.  The forward computation is the Gulf Stream, the back
//   calculation is the Loop Current
  fijpt starter, trial;
  fijpt current, offset;
  latpt ll;
  int i, nstart = 40, npts;
  
// Clear the gspoints vector:
  starter.i = 0; starter.j = 0;
  for (i = 0; i < gspoints.xpoints(); i++) {
    gspoints[i] = starter;
  }
    
// First, get seed points from SW portion 
  float wlon = -82, elon = -78.5;
  float blat = 26.5, dlat = 0.1;
  for (i = 0; i < nstart; i++) {
    maxontrace(x, wlon, blat + i*dlat, elon, blat + i*dlat, gspoints[i]);
  }

  wlon = -80;
  elon = -73.;
  //blat += nstart*dlat;
  for (i = nstart; i < 2*nstart; i++) {
    maxontrace(x, wlon, blat + i*dlat, elon, blat + i*dlat, gspoints[i]);
  }
  npts = i;

  current = gspoints[npts-1];
  offset = gspoints[npts-1] ; offset -=gspoints[npts-2];
  offset.i /= (float) 10.;
  offset.j /= (float) 10.;
  #ifdef VERBOSE
    printf("offset = %f %f\n",offset.i, offset.j); fflush(stdout); 
  #endif

  for (i = npts; i < gspoints.xpoints(); i++) {
    trial = current;
    trial += offset;
    //printf("initial trial %f %f ",trial.i, trial.j);
    crossing(x, offset, trial);

    if (!x.in(trial)) break;

    ll = x.locate(trial);
    if (ll.lon > (360 - 50.)) break; // tunable, stop looking if you reach 50 W
    if (ll.lat < 20) break; // also stop if south of 20
    #ifdef VERBOSE
    printf("loc %d %f %f  %f %f\n",i, ll.lat, ll.lon, trial.i, trial.j); 
    fflush(stdout);
    #endif
    current = trial;
    gspoints[i] = trial;

    set_offset(gspoints, offset, i-1, i);
    fflush(stdout); 

  }
  ngs = i - 1;

// Loop current branch:
// First, get seed points from the Florida Strait
  maxontrace(x, -81.5, 26.0, -81.5, 22.0, lcpoints[0]);
  maxontrace(x, -82.0, 26.0, -82.0, 22.0, lcpoints[1]);
  current = lcpoints[1];
  offset = lcpoints[1]; offset -= lcpoints[0];
  offset.i /= (float) 10.;
  offset.j /= (float) 10.;
  #ifdef VERBOSE
  printf("loc 0 %f %f\n",lcpoints[0].i, lcpoints[0].j);
  printf("loc 1 %f %f\n",lcpoints[1].i, lcpoints[1].j);
  printf("offset = %f %f\n",offset.i, offset.j); fflush(stdout); 
  #endif

  for (int i = 2; i < lcpoints.xpoints(); i++) {
    trial = current;
    trial += offset;
    //printf("initial trial %f %f ",trial.i, trial.j);
    crossing(x, offset, trial);

    if (!x.in(trial)) break;

    ll = x.locate(trial);
    // Stop looking if we go south of 20 N
    if (ll.lat < 20.0) break;
    #ifdef VERBOSE
    printf("loc %d %f %f  %f %f\n",i, ll.lat, ll.lon, trial.i, trial.j); 
    fflush(stdout);
    #endif
    current = trial;
    lcpoints[i] = trial;

    set_offset(lcpoints, offset, i-1, i);
  }
  nlc = i - 1;

  //printf("leaving castings\n"); fflush(stdout);
  return;

}
