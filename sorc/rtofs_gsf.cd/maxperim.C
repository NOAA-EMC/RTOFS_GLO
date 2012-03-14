void maxperim(hycom<float> &x, float lon1, float lat1, float radius, 
                               fijpt &maxpt) ;
// This finds the 2 maximal points along the perimeter (problem if adjacent?!)
void maxperim(hycom<float> &x, float lon1, float lat1, float radius, 
                               fijpt &direction) {
  fijpt loc, tloc;
  latpt ll;
  float di, dj, angle;
  mvector<float> perims(900);
  mvector<int> maxima(900);
  int i, nmaxima = 0;
  int steps = 24;

  ll.lat = lat1;
  ll.lon = lon1;
  loc = x.locate(ll);

  for (int i = 0; i < steps; i++) {
    angle = i*(360./steps);
    di = radius*cos(angle*3.1416/180.);
    dj = radius*sin(angle*3.1416/180.);
    tloc.i = loc.i + di;
    tloc.j = loc.j + dj;
    perims[i] = x.accurate(tloc);
  }
// note: if max enters but does not leave, then only 1 relative maximum on
//  perimeter
  if (perims[0] > perims[1] && perims[0] > perims[11]) {
    maxima[nmaxima] = 0;
    nmaxima += 1;
  }

  for (i = 1; i < steps; i++) {
    if (perims[i] > perims[(i+1)%steps] && perims[i] > perims[i-1]) {
      maxima[nmaxima] = i;
      nmaxima += 1;
    }
  } 
  printf("nmax = %d\n",nmaxima);
    
  return ;
} 
