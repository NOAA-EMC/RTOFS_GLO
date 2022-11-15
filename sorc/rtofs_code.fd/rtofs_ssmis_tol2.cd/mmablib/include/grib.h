#include <cstdio>
#include <cmath>
#include <iostream>
using namespace std;

//Class for working with grib1
//Oldest extant 17 June 1998
//Robert Grumbine
//
//04 Aug 1998:
//  Declare types for functions
//  Fix management of depth arguments to pds
//
//12 Mar 1999: Drop metric.h from inclusion
//16 Jun 1999: Inline many, add show_date
//25 Jun 1999: add an epsilon as protection in setting precision

// Put this inside the appropriate class, assuming metric for now:
// Assume, for now, that we're going to call w3fi72, a-la- gribit,
//  means that pds and gds are integer arrays, rather than the
//  WMO byte counts (parenthetical comments will describe these).

#ifndef GRIBH

#define GRIBH

class grib_pds {
   public:
     int pds[25];
   public:
     grib_pds();

     int &get_precision() { return pds[24]; }
     void show_date();

     void fix_year();
     void fix_century(int );
     void set_time(int, int, int, int, int = 0);
     void set_precision(float );
     void set_table(int=3);
     void set_center(int=7);
     void set_process(int n)      { pds[3] = n; }
     //void set_gridid(int n)       { pds[4] = n; }
     void set_gridid(int );
     void set_parmno(int parmno ) { pds[7] = parmno; }
     void set_layer(int n)        { pds[8] = n; }
     void set_depth(int );
     void set_fcst_lead(int lead) { pds[17] = lead; }
};
void grib_pds::set_gridid(int n) {
  pds[4] = n;
}
void grib_pds::set_table(int n) {
  pds[1] = n;  //table 3 is NCEP, 128 is OMB
}
void grib_pds::set_center(int n) {
  pds[2] = n;  //NCEP is 7, FNMOC is 58
}
void grib_pds::set_depth(int depth) {
  pds[9] = 0;
  pds[10] = depth; //by default, set lower depth to same
}
void grib_pds::show_date() {
  printf("Century %2d\n", pds[22]);
  printf("Year   %3d\n", pds[11]);
  printf("Month   %2d\n", pds[12]);
  printf("Day     %2d\n", pds[13]);
  printf("Hour    %2d\n", pds[14]);
  printf("Minute  %2d\n", pds[15]);
  printf(" \n");
}

void grib_pds::set_precision(float x) {
  //Provide at least enough precision to represent x.
  int y;
  x *= (1.+1.e-5); // The extra is to avoid round off problems from reals
  y = (int) floor(log10(x));
  pds[24] = -y;  // Note the negative.  We divide the data by 10**y,
                 //   so multiply by 10**(-y).  GRIB multiplies.  
  #ifdef VERBOSE
  printf("precision set to %d from x = %f\n",y, x);
  #endif
}  
  
void grib_pds::set_time(int year, int month, int day, int hour, int minute) {
  fix_century(year);
  pds[12] = month;
  pds[13] = day;
  pds[14] = hour;
  pds[15] = minute;
}
void grib_pds::fix_year() {
// This is in the event that the year is set, and cleans up to 
//   set the century
  int year;
  year = pds[11];
  if (year <= 100) {
//  Assume 20th century
    pds[22] = 20;
    return;
  }
  if (year > 1899) {
      pds[22] = (year-1) / 100 + 1;
      pds[11] = (int) year - (pds[22] - 1)*100;
  }
  else {
    cout << "Impossible year -- it is not:\n";
    cout << "  less than or equal to 100 (assumed to be 1900-2000)\n";
    cout << "  greater than 1899 (assumed to be a real year) \n";
    cout.flush();
    pds[11] = 00;
    pds[22] = 00;
  } 
  return;
}

void grib_pds::fix_century(int year) {
  if (year <= 100) {
//  Assume 20th century
    pds[22] = 20;
    pds[11] = year;
    return;
  }
  if (year > 1899) {
      pds[22] = (year-1) / 100 + 1;
      pds[11] = year - (pds[22] - 1)*100;
  }
  else {
    cout << "Impossible year -- it is not:\n";
    cout << "  less than or equal to 100 (assumed to be 1900-2000)\n";
    cout << "  greater than 1899 (assumed to be a real year) \n";
    cout.flush();
    pds[11] = 00;
    pds[22] = 00;
  } 
  return;
}


grib_pds::grib_pds() {
   pds[0] = 28;  // length in octets of pds
   pds[1] = 3;   //128 for omb table;
   pds[2] = 7;   //center id
   pds[3] = 255; // model (process) id
   pds[4] = 255; // grid id
   pds[5] = 1;   // gds flag
   pds[6] = 0;   // bms flag (will do own check)
   pds[7] = 255; // parameter number (table B) (will override)
   pds[8] = 160; // ocean depth (layer type) (will often override)
   pds[9] = 0;   // 0 for single level, depth in m (need to override)
   pds[10] = 0;  // second depth (or depth of level)
   pds[11] = 1998; // override - year, need to reset
   pds[12] = 1;    // override - month
   pds[13] = 1;    // override - day
   pds[14] = 0;    // override - hour
   pds[15] = 0;    // override - minute
   pds[16] = 1;    // forecast time unit 1 -> hours
   pds[17] = 0;    // override - number of fcst units
   pds[18] = 0;    // if averaged, number of units in average
   pds[19] = 10;    // 10 -> valid at initial time + pds[17] time, 2 byte time
   pds[20] = 0;    // if averaged, number included
   pds[21] = 0;    // if averaged, number missing from average
   pds[22] = 20;   // !! Century -- always fix this by WMO logic
   pds[23] = 0;    // reserved
   pds[24] = 0;    // will usually override -- decimal scaling
}
 

//// GDS definition and operations
class grib_gds {
  public:
    int gdslen;
    int gds[18];
    grib_gds();
    //friend set_gds(grib_gds &, llgrid<float> &);
};

grib_gds::grib_gds(): gdslen(32) {
//grib_gds::grib_gds() {
  //gdslen = 32;
  gds[0] = 0;       //number of vertical coords
  gds[1] = 255;     // vertical coord flag
  gds[2] = 0; //  IDRT;    // OVERRIDE -- set in friend
  gds[3] = 0; //  NX;      // OVERRIDE -- set in friend
  gds[4] = 0; //  NY;      //  ""
  gds[5] = 0; //  LAT1;    //  ""
  gds[6] = 0; //  LON1;    //  ""
  gds[7] = 0; //  IRESFL;  // ""
  gds[8] = 0; //  IGDS09;  // ""
  gds[9] = 0; //  IGDS10;  // ""
  gds[10] = 0; //  IGDS11; // ""
  gds[11] = 0; //  IGDS12; // ""
  gds[12] = 0;      // Scanning mode in most, intercut lat for mercator
                    //   -- set by friend, from dx, dy sign.
  gds[13] = 0;      // Scanning mode for mercator, unused else.
  gds[14] = 0;      // unused 
  gds[15] = 0;      // unused
  gds[16] = 0;      // unused
  gds[17] = 0;      // unused
}
  
#endif
