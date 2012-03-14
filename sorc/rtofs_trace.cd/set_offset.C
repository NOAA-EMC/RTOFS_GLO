void set_offset(mvector<fijpt> &locs, fijpt &offset, int n1, int n2) ;

// Try to ensure that the step size does not get either too large or 
//   too small:
void set_offset(mvector<fijpt> &locs, fijpt &offset, int n1, int n2) {
  offset = locs[n2] ; offset -=locs[n1];
  float tmag = offset.magnitude();
  float tmax = 2.50, tmin = 1.0;
  if (tmag < tmax && tmag > tmin) return;

  if (tmag > tmax) {
    //printf("tmax branch\n"); fflush(stdout);
    float mratio = tmag / tmax;
    offset.i /= mratio;
    offset.j /= mratio;
    return;
  }
  if (tmag != 0) {
    float sratio = tmag / tmin;
    offset.i /= sratio;
    offset.j /= sratio;
    return;
  }

  if (n1 >= 1) {
    set_offset(locs, offset, n1-1, n2);
  }
  else {
    printf("fatal, cannot back up far enough to get a valid offset %d\n",n2);
    offset.i = 1.0;
    offset.j = 1.0;
  }

  return;
}

