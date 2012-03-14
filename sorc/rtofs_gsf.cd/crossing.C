void crossing(hycom<float> &x, fijpt offset, fijpt &trial);

// Given a field to find maxima, a direction of travel (offset), and a starting
//  point, check perpendicular to the direction for nearby relative maxima 
//  crosswise to the flow.  Reset the trial location to that maximum.
void crossing(hycom<float> &x, fijpt offset, fijpt &trial) {
  fijpt perp1, perp2, tmp, lmax;
  float scale, tmax;
  #define MM 1.0
  float steps = 20.;
  perp1.i = -offset.j;
  perp1.j =  offset.i;
  perp2.i =  offset.j;
  perp2.j = -offset.i;
  scale = sqrt(offset.i*offset.i + offset.j*offset.j);
  perp1.i *= MM/steps/scale; // span M grid points in 10 steps
  perp1.j *= MM/steps/scale; 
  perp2.i *= MM/steps/scale; 
  perp2.j *= MM/steps/scale; 
  tmp = trial;
  lmax = trial;
  tmax = fabs(x.accurate(trial));
  for (int i = 0; i < steps; i++) {
    tmp += perp1;
    if (fabs(x.accurate(tmp)) > tmax) {
      lmax = tmp;
      tmax = fabs(x.accurate(tmp));
    }
    //printf("%f %f  %f %f ",tmp.i, tmp.j, x.accurate(tmp), tmax); fflush(stdout);
  }
  tmp = trial;
  for (int i = 0; i < steps; i++) {
    tmp += perp2;
    if (fabs(x.accurate(tmp)) > tmax) {
      lmax = tmp;
      tmax = fabs(x.accurate(tmp));
    }
  }

  if (lmax.i != trial.i || lmax.j != trial.j) {
    //printf("crossing loc %f %f vs. %f %f\n",lmax.i, lmax.j, trial.i-lmax.i, trial.j-lmax.j);
    //fflush(stdout);
  }

  trial = lmax;

  return;
}
