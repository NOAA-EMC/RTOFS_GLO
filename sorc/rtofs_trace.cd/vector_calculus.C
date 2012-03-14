void vector_calculus(hycom<float> &x, hycom<float> &ddx, hycom<float> &ddy, float flagval);

void vector_calculus(hycom<float> &x, hycom<float> &ddx, hycom<float> &ddy, 
                      float flagval) {
  //palette<unsigned char> gg(19, 65);

  x.grad(ddx, ddy, flagval);

  return;
}
