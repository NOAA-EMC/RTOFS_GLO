// low-level test of all .h files in the mmablib for immediate compiler complaints
// Robert Grumbine
// 11 June 2018

#include "params.h"
#include "gshhs.h"
#include "points.h"
#include "icegrids.h"

#include "mvector.h"
#include "time_series.h"

#include "genes.h"

#include "buoy.h"
#include "date.h"
#include "f2c_files.h"
#include "clib8.h"
#include "clib4.h"
#include "clib.h"
#include "color.h"

#include "grid_base.h"
#include "grid_math.h"
#include "grib.h"
#include "grid3.h"

#include "metric.h"
//#include "fromall.h"
//#include "from.h"
#include "ncepgrids.h"

// Map projections
#include "gaussian.h"
#include "eta.h"
#include "mercator.h"
#include "psgrid.h"
#include "llgrid.h"
#include "lambert.h"
#include "resops.h"

#include "walcc.h"

// Satellite-related
// SSMI
#include "ssmiclass.h"
#include "ssmi.h"
#include "icessmi.h"
// SSMI-S
#include "icessmis.h"
#include "ssmis.h"
#include "ssmisclass.h"
//#include "icessmisu.h"
//#include "ssmisu.h"
//#include "ssmisuclass.h"
// AMSR-e and AMSR2
#include "amsr.h"
#include "amsr2.h"
#include "amsrice.h"
// AVHRR
#include "avhrr.h"



// Legacies only for reference:
//  #include "legacy.h"
//  #include "cofs.h"
//  #include "fromcfs.h"

int main(void) {
  return 0;
}
