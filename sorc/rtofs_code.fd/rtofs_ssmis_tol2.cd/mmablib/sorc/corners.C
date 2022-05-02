#include "points.h"
#include "mvector.h"
// Given a vector of latpts, find the extremal corners n/s/e/w
// Robert Grumbine
// 9 Mary 2005
//
void corners(mvector<latpt> &x, float &north, float &south, 
                                float &east, float &west)   {
  north = -95.0;
  south = +95.0;
  east = -400.0;
  west =  400.0;
  for (int i = 0; i < x.xpoints(); i++) {
    north = max(north, x[i].lat);
    south = min(south, x[i].lat);
    east  = max(east, x[i].lon);
    west  = min(west, x[i].lon);
  }  

  return ;
}
