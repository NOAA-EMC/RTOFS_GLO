float dist(fijpt &x, mvector<fijpt> &gspoints, int ngs) ;
float dist(ijpt &x, mvector<fijpt> &gspoints) ;
float dist(ijpt &x, mvector<ijpt> &gspoints) ;

float dist(ijpt &x, mvector<fijpt> &gspoints) {
  float a, b, c, tdist = 9.e20, point = 9.e20; 
  int i, imin = 0;
  fijpt test;

  test.i = x.i; test.j = x.j;

  for (i = 0; i < gspoints.xpoints() ; i++) {
    tdist = sqrt(
                (gspoints[i].i - test.i)*(gspoints[i].i - test.i) +
                (gspoints[i].j - test.j)*(gspoints[i].j - test.j) ) ;
    if (tdist < point) {
      imin = i;
      point = tdist; 
    }
  }

  float x0, x1, y0, y1, tdist1, tdist2;
  tdist = 9.e20;
  if (i == 0) { 
    printf("must skip tdist2\n");
  }
  else if (i == gspoints.xpoints()-1 ) {
    printf("must skip tdist1\n");
  }
  else {
  i = imin;
    y1 = gspoints[i+1].j;  x1 = gspoints[i+1].i;
    y0 = gspoints[i].i  ;  x0 = gspoints[i].i;
    b = x1 - x0;
    a = - (y1 - y0);
    c = - (y0*(x1-x0) - x0*(y1-y0) ); 
    tdist1 = fabs( (a*test.i + b*test.j + c)/sqrt(a*a+b*b) );
  i = imin - 1;
    y1 = gspoints[i+1].j;  x1 = gspoints[i+1].i;
    y0 = gspoints[i].j  ;  x0 = gspoints[i].i;
    b = x1 - x0;
    a = - (y1 - y0);
    c = - (y0*(x1-x0) - x0*(y1-y0) ); 
    tdist2 = fabs( (a*test.i + b*test.j + c)/sqrt(a*a+b*b) );
    
    tdist = min(tdist1, tdist2);
  }
  if (point < tdist) {
    //printf("error in dist %f vs %f\n",point, tdist);
    return point;
  }
  //printf("%f %f\n",point, tdist);
  return tdist;
}
float dist(ijpt &x, mvector<ijpt> &gspoints) {
  float a, b, c, tdist = 9.e20, point = 9.e20;
  int i, imin = 0;
  fijpt test;

  test.i = x.i; test.j = x.j;

  for (i = 0; i < gspoints.xpoints() ; i++) {
    tdist = sqrt(
                (gspoints[i].i - test.i)*(gspoints[i].i - test.i) +
                (gspoints[i].j - test.j)*(gspoints[i].j - test.j) ) ;
    if (tdist < point) {
      imin = i;
      point = tdist;
    }
  }

  float x0, x1, y0, y1, tdist1, tdist2;
  tdist = 9.e20;
  if (i == 0) {
    printf("must skip tdist2\n");
  }
  else if (i == gspoints.xpoints()-1 ) {
    printf("must skip tdist1\n");
  }
  else {
  i = imin;
    y1 = gspoints[i+1].j;  x1 = gspoints[i+1].i;
    y0 = gspoints[i].i  ;  x0 = gspoints[i].i;
    b = x1 - x0;
    a = - (y1 - y0);
    c = - (y0*(x1-x0) - x0*(y1-y0) );
    tdist1 = fabs( (a*test.i + b*test.j + c)/sqrt(a*a+b*b) );
  i = imin - 1;
    y1 = gspoints[i+1].j;  x1 = gspoints[i+1].i;
    y0 = gspoints[i].j  ;  x0 = gspoints[i].i;
    b = x1 - x0;
    a = - (y1 - y0);
    c = - (y0*(x1-x0) - x0*(y1-y0) );
    tdist2 = fabs( (a*test.i + b*test.j + c)/sqrt(a*a+b*b) );

    tdist = min(tdist1, tdist2);
  }
  if (point < tdist) {
    return point;
  }

  return tdist;
}
float dist(fijpt &x, mvector<fijpt> &gspoints, int ngs) {
  float a, b, c, tdist = 9.e20, point = 9.e20;
  int i, imin = 0;
  fijpt test;

  test.i = x.i; test.j = x.j;

  for (i = 0; i < ngs ; i++) {
    tdist = sqrt(
                (gspoints[i].i - test.i)*(gspoints[i].i - test.i) +
                (gspoints[i].j - test.j)*(gspoints[i].j - test.j) ) ;
    if (tdist < point) {
      imin = i;
      point = tdist;
    }
  }

  float x0, x1, y0, y1, tdist1, tdist2;
  tdist = 9.e20;
  if (i == 0) {
    printf("must skip tdist2\n");
  }
  else if (i == ngs-1 ) {
    printf("must skip tdist1\n");
  }
  else {
  i = imin;
    y1 = gspoints[i+1].j;  x1 = gspoints[i+1].i;
    y0 = gspoints[i].i  ;  x0 = gspoints[i].i;
    b = x1 - x0;
    a = - (y1 - y0);
    c = - (y0*(x1-x0) - x0*(y1-y0) );
    tdist1 = fabs( (a*test.i + b*test.j + c)/sqrt(a*a+b*b) );
  i = imin - 1;
    y1 = gspoints[i+1].j;  x1 = gspoints[i+1].i;
    y0 = gspoints[i].j  ;  x0 = gspoints[i].i;
    b = x1 - x0;
    a = - (y1 - y0);
    c = - (y0*(x1-x0) - x0*(y1-y0) );
    tdist2 = fabs( (a*test.i + b*test.j + c)/sqrt(a*a+b*b) );

    tdist = min(tdist1, tdist2);
  }
  if (point < tdist) {
    return point;
  }

  return tdist;
}
