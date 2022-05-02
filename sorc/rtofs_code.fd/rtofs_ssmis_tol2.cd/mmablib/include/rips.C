#include "ncepgrids.h"
// Test program for RIPS grid
// Robert Grumbine 2015--

int main(void) {
  rips<float> x;
  ijpt loc;
  latpt ll;

  loc.i = 0; loc.j = 0;
  ll = x.locate(loc);

  printf("ll: %f %f\n",ll.lat, ll.lon);
  ll.lat = 90.0;
  ll.lon = 0.0;
  loc = x.locate(ll);
  printf("np: %d %d\n",loc.i, loc.j);

  return 0;
}
