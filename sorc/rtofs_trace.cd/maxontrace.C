void maxontrace(hycom<float> &x, float lon1, float lat1, 
                                 float lon2, float lat2, fijpt &maxpt) ;
void maxontrace(hycom<float> &x, float lon1, float lat1, 
                                 float lon2, float lat2, fijpt &maxpt) {
// Find the max along a line determines by lat-long bounds.  
  fijpt starter;
  fijpt tloc;
  latpt ll;
  float tmax, dlon, dlat;
  int nsteps = 64;

  ll.lon = min(lon1, lon2);
  ll.lat = min(lat1, lat2);
  dlon = (max(lon1, lon2) - ll.lon ) /nsteps;
  dlat = (max(lat1, lat2) - ll.lat ) /nsteps;

  starter.i = 0; starter.j = 0;
  tloc = x.locate(ll);
  tmax = x[tloc];
  starter = tloc;
  #ifdef VERBOSE
  printf("starter %f %f max %e lat lon %f %f\n",starter.i, starter.j,
          tmax, ll.lat, ll.lon);
  fflush(stdout);
  printf("min max lon %f %f min max lat %f %f dlon dlat %f %f\n",min(lon1, lon2), max(lon1, lon2),
          min(lat1, lat2), max(lat1, lat2), dlon, dlat );
  fflush(stdout);
  #endif 
  for (int i = 0; i < nsteps; i++) {
    tloc = x.locate(ll);
    //printf("ll.lat %f\n",ll.lat); fflush(stdout);
    if (x[tloc] > tmax) {
      #ifdef VERBOSE
      printf("starter %f %f max %e lat lon %f %f\n",starter.i, starter.j,
              tmax, ll.lat, ll.lon);
      fflush(stdout);
      #endif
      tmax = x[tloc];
      starter = tloc;
    }
    ll.lon += dlon;
    ll.lat += dlat;
  }

  maxpt = starter;
  return;
}
