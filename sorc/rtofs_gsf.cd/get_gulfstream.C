int get_gulfstream(hycom<float> &sst, int &count, mvector<fijpt> &gspoints,
                    char *arg2, char *arg3, char *arg4) ;

int get_gulfstream(hycom<float> &sst, int &count, mvector<fijpt> &gspoints,
                    char *arg2, char *arg3, char *arg4) {
  float lonmin, lonmax;
  float tlat, tlon;
  fijpt fijtmp;
  latpt ll;
  FILE *gsin;

// Inputs are degrees west, shift to standard west = -east
  //lonmin = atof(arg4);
  //lonmax = atof(arg3);
  lonmin = 77;
  lonmax = 65;
  lonmin = -lonmin;
  lonmax = -lonmax;
  count = 0;

  gsin = fopen("nout", "r");
  if (gsin == (FILE *) NULL) {
    printf("failed to open gspts file %s\n",arg2);
    exit(3);
  }
  while (!feof(gsin) && count < MAXPTS) {
    fscanf(gsin, "%f %f \n", &tlat, &tlon);
  
    ll.lat = tlat;
    ll.lon = -tlon; // convert to degrees east, with negatives for west
    fijtmp = sst.locate(ll);

    if (sst.in(fijtmp) && (ll.lon <= lonmax) && (ll.lon >= lonmin) ) {
      gspoints[count] = fijtmp;
      count += 1;
    }
   
    #ifdef VERBOSE2
      printf("%f %f  %f %f  %f %f  %d\n",ll.lat, ll.lon, fijtmp.i, fijtmp.j,
            lonmin, lonmax, 
            (sst.in(fijtmp) && (ll.lon <= lonmax) && (ll.lon >= lonmin)) );
    #endif

  }
  printf("Found %d navy points to work with\n",count); fflush(stdout);

  if (count == 0) {
    printf("cannot run with zero points, aborting\n");
    fflush(stdout);
    // should probably be a throw to running without the gsfile
    exit(3);
  }

  #ifdef VERBOSE
    printf("Listing from getgs of points:\n"); 
    for (int i = 0; i < count; i++) {
      printf("%d  %6.2f %6.2f\n",i, gspoints[i].i, gspoints[i].j);
    }
    fflush(stdout);
  #endif

  return count;
}
