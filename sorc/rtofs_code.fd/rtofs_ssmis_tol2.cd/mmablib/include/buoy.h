// Declare a class to handle buoy data
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <cmath>
#include <iostream>
using namespace std;

#ifndef BUOYH
#define BUOYH

// Declare two classes in this file: 
//    buoy_report -- a buoy report which may have physical parameters 
//                      defined as well 
//    avbuoy      -- construct which can be used for averaging time and place
//                      information (currently, with physical averaging to
//                      come in the future)
// Robert Grumbine 23 April 2001
//
// Modifications:
//   14 Apr 2000: add iabpread, as distinct from CPC file format
//   24 May 2000: Add another argument type (ABSOFT) for arcdis call
//   20 Jun 2001: Inline set_*_range, some minor readability changes
//   04 Dec 2002: Close the #ifdef BUOYH
//   05 Apr 2004: Change to new from malloc
//   31 Jan 2006: explicit ::buoy_report(void) constructor
//                move initialization of time and space range to constructor
//Oldest extant 3 Dec 1998
//
//12 Mar 1999: Major additions ...
//  Add copy ctor, equating
//  Add set_time, get_time
//  Add isbuoy, isdrifter, isfixed, isship, is_moving_ship, is_other_fixed
//  Add synoptic
//  Add many varieties of 'near' (argument-selected -- distance, time, 
//     time and distance, user-specified time and distance tolerances,
//     near a point, near a buoy of same name) 
//
//25 Feb 2000:
//  Add 'avbuoy' class
//  Add resetting of default time and space ranges
//  Add get and set seconds, logical 'same_id'
//  Add buoy read, write
//20 March 2014
//  Add 'matchup' class, for drift model verification


#ifndef POINTSH
  #include "points.h" //needed for latpt
#endif
#ifndef PARAMETERS
  #include "params.h" //needed for km per degree
#endif

/////////////////////////////////////////////////////
inline char * strhelp(char *x, int n1, int n2, char *y) ;
//Helper: take characters from n1 to n2 and cut them out of a string 
inline char * strhelp(char *x, int n1, int n2, char *y) {
  int i;

  for (i = n1; i <= n2; i++) {
    y[i-n1] = x[i-1];
  }
  y[n2-n1+1] = '\0';
  return y;
}

#ifndef ARCDIS_READY
  #define ARCDIS_READY
  #ifdef ABSOFT
    extern "C" float arcdis(float &long1, float &lat1, float &long2, 
                            float &lat2 );
    #define ARCDIS arcdis
  #else
    extern "C" float arcdis_(float &long1, float &lat1, float &long2, 
                             float &lat2 );
    #define ARCDIS arcdis_
  #endif
#endif

/////////////////////////////////////////////////////
class buoy_report {
  public:
    //Physical information + platform info:
    float slp, wind_dir, wind_speed, air_temp, dewpt_depression;
    char  cloud_cover;
    float sst;
    int   report_type; 
#define MAX_ID_LEN 26
    char  station_id[MAX_ID_LEN];

    //Time, platform, location information:
    //Note that data like this should be hidden and only accessed through
    // date_set and date_get routines.
    float latitude, longitude;
    time_t obs_secs;
    int   year, month, day;
    float hour;
  private:
    tm obs_time;
    static time_t report_change_time; 
    static time_t time_range ;  // This and the next establish a couple of 
    static float  space_range ; //   defaults for testing nearness
    
  public:
    buoy_report(void);
    ~buoy_report() {}
    buoy_report(buoy_report &); //add copy constructor
    buoy_report & operator=(buoy_report &);

// Resetting default ranges:
    void set_space_range(const float y) { space_range = y; }
    void set_time_range(const time_t y) { time_range = y; }

// Time functions:
    void   set_time();
    tm     get_time() { return this->obs_time; }
    time_t get_secs() { return this->obs_secs; } 
    void   set_secs(const time_t &x) { this->obs_secs = x; }
   
//  Read in a buoy report from CPC format and translate to internal format.
    buoy_report& read(FILE *);
    void write(FILE *);

    buoy_report& iabpread(FILE *fin) ;

//  Check on types of reports 
    bool same_id(buoy_report &x) {
       return (strncmp(station_id, x.station_id, MAX_ID_LEN) == 0 );
    }
    bool isbuoy()    { if (obs_secs < report_change_time) {
                         return (report_type == 62 || report_type == 61) ; }
                       else { 
                         return (report_type/10 == 2 || report_type/10 == 3);
                       }
                     }
    bool isdrifter() { if (obs_secs < report_change_time) {
                         return (report_type == 62 ) ; }
                       else {
                         return (report_type/10 == 2);
                       }
                     }
    bool isfixed()   { if (obs_secs < report_change_time) {
                         return (report_type == 61); }
                       else {
                         return (report_type/10 == 3);
                       }
                     }
    bool isship()    { if (obs_secs < report_change_time ) {
                         return (report_type == 22 || report_type == 23
                                    || report_type == 21) ; }
                       else {
                          return (report_type/10 == 1);
                       }
                     }
    bool is_moving_ship() { if (obs_secs< report_change_time ) {
                              return (report_type == 22 || report_type == 23); }
                            else {
                              return (report_type/10 == 1) ; 
                            } // note that modern can't distinguish between 
                          }   // moving and stationary ships
    bool is_other_fixed() { if (obs_secs < report_change_time) {
                              return (report_type == 31 || report_type == 51); }
                            else {
                              return (report_type/10 == 4);
                            }
                          }

//Say whether you're near a point (space, time, or space-time), 
//       given tolerances.  Many are inlined.
    bool near(tm &, time_t);
    bool near(buoy_report &, const char *, time_t); //near in time and same id

    bool poleward(float cutoff) {
      if (cutoff > 0.) { return (this->latitude >= cutoff); }
      else { return (this->latitude <= cutoff) ; }
    }
    bool synoptic(float del) { return (hour <= del || (24. - hour) <= del) ; }
    bool near(buoy_report &x, time_t toler) { 
      return this->near(x.obs_time, toler); 
    }
    bool near(latpt &x, float toler) { 
      if (fabs(x.lat - latitude)*parameters::km_per_degree > toler) { return false; } 
      else { 
        return (fabs(arcdis_(x.lon, x.lat, longitude, latitude)) 
                 < toler) ;
      }
    }
    bool near(buoy_report &x, float toler) { 
      if (fabs(x.latitude - latitude)*parameters::km_per_degree > toler) { return false; }
      else {
        return (fabs(arcdis_(x.longitude, x.latitude, longitude, latitude)) 
                 < toler) ; 
      }
    }
    bool near(buoy_report &x, time_t toler, float distance) { return 
      (this->near(x.obs_time,toler) && this->near(x, distance) ); }

// Start thinking about QC:
    bool ok(void) {
      return ( fabs(latitude) < 90.1);
    }

};

//The following initialized the date at which the interpretation of 
//  report_types changed, March 1, 1997
time_t fn(int yy, int mm, int dd) { tm tmp;
         tmp.tm_year = yy; tmp.tm_mon  = mm - 1;
         tmp.tm_mday = dd;  tmp.tm_hour = 0;
         tmp.tm_min  = 0;    tmp.tm_sec  = 0;
         return mktime(&tmp); 
         }
time_t buoy_report::report_change_time = fn(97, 3, 1);
time_t buoy_report::time_range = (time_t)3600;
float  buoy_report::space_range = 50.0;


//////////////////////// Begin support code:
void buoy_report::set_time() {
//construct an obs time, given that there are month day year entries already
// in the buoy report.
  obs_time.tm_year = year;
  obs_time.tm_mon     = month - 1;
  obs_time.tm_mday    = day;
  obs_time.tm_hour    = (int) hour;
  obs_time.tm_min     = (int) (60.*(hour - (int)hour) );
  obs_time.tm_sec     = (int) (3600.*(hour - (int)hour) - 60*obs_time.tm_min);
  obs_secs = mktime(&obs_time);
}
buoy_report::buoy_report(void) {
  #ifdef VERBOSE
    cout << "In void buoy_report constructor\n";
    cout.flush();
  #endif
  year = 0; month=0; day=0; hour = 0;
  latitude = -100; longitude = 0;
  report_type = 0;
  slp = 0;
  wind_dir=0; wind_speed=0;
  air_temp=0; dewpt_depression=0;
  cloud_cover = 0;
  sst = 0;
  strncpy(station_id, "\n", MAX_ID_LEN);
  obs_time.tm_sec = 0;
  obs_time.tm_min = 0;
  obs_time.tm_hour = 0;
  obs_time.tm_mday = 0;
  obs_time.tm_mon = 0;
  obs_time.tm_year = 0;
  obs_time.tm_wday = 0;
  obs_time.tm_yday = 0;
  obs_time.tm_isdst = 0;
  obs_secs = mktime(&obs_time);
}

buoy_report::buoy_report(buoy_report &x) {
  #ifdef VERBOSE
    cout << "In copy constructor\n";
    cout.flush();
  #endif
  year = x.year; month=x.month; day=x.day;
  hour = x.hour;
  latitude = x.latitude; longitude = x.longitude;
  report_type = x.report_type; 
  slp = x.slp; 
  wind_dir=x.wind_dir; wind_speed=x.wind_speed; 
  air_temp=x.air_temp; dewpt_depression=x.dewpt_depression;
  cloud_cover = x.cloud_cover;
  sst = x.sst;
  strncpy(station_id, x.station_id, MAX_ID_LEN);
  obs_time.tm_sec = x.obs_time.tm_sec;
  obs_time.tm_min = x.obs_time.tm_min;
  obs_time.tm_hour = x.obs_time.tm_hour;
  obs_time.tm_mday = x.obs_time.tm_mday;
  obs_time.tm_mon = x.obs_time.tm_mon;
  obs_time.tm_year = x.obs_time.tm_year;
  obs_time.tm_wday = x.obs_time.tm_wday;
  obs_time.tm_yday = x.obs_time.tm_yday;
  obs_time.tm_isdst = x.obs_time.tm_isdst;
  obs_secs = mktime(&obs_time);
}

buoy_report & buoy_report::operator=(buoy_report &x) {
    #ifdef VERBOSE
        cout << "buoy_report::operator=\n"; cout.flush();
    cout.flush();
    #endif 
    year = x.year;
    month=x.month;
    day=x.day;
    hour = x.hour;
    latitude = x.latitude;
    longitude = x.longitude;
    report_type = x.report_type; 
    slp = x.slp; 
    wind_dir=x.wind_dir;
    wind_speed=x.wind_speed; 
    air_temp=x.air_temp;
    dewpt_depression=x.dewpt_depression;
    cloud_cover = x.cloud_cover;
    sst = x.sst;
    obs_time.tm_sec = x.obs_time.tm_sec;
    obs_time.tm_min = x.obs_time.tm_min;
    obs_time.tm_hour = x.obs_time.tm_hour;
    obs_time.tm_mday = x.obs_time.tm_mday;
    obs_time.tm_mon = x.obs_time.tm_mon;
    obs_time.tm_year = x.obs_time.tm_year;
    obs_time.tm_wday = x.obs_time.tm_wday;
    obs_time.tm_yday = x.obs_time.tm_yday;
    obs_time.tm_isdst = x.obs_time.tm_isdst;
    #ifdef VERBOSE
       cout << "Done adding all the numeric data, now trying to equate names\n";
       cout.flush();
    #endif
    strncpy(station_id, x.station_id, MAX_ID_LEN);
    #ifdef VERBOSE
       cout << "Equated names, now trying to set obs_secs\n";
       cout.flush();
    #endif
    obs_secs = mktime(&obs_time);
    #ifdef VERBOSE
       cout << "Trying to leave operator=\n";
       cout.flush();
    #endif

  return *this;
}

#define MAXBUOYLINE 90
#define FLAG  9999
void buoy_report::write(FILE *fout) {
  //Note: this is a destructive write -- after write the data are no
  //  longer in a format to be used.
  //Future: fix to non-destructive
  int tyear; 

  if (slp == FLAG) {
    slp = 9999.;
  }
  else {
    slp = (slp - 900)*10.+0.5;
  }
  if (wind_dir == FLAG) wind_dir = 999.;
  if (wind_speed == FLAG) wind_speed = 999.;
  if (air_temp == FLAG) {
    air_temp = 9999.;
  }
  else {
    air_temp = 10.*air_temp + 0.5;
  }
  if (dewpt_depression == FLAG) {
    dewpt_depression = 999.;
  }
  else {
    dewpt_depression = 10.*dewpt_depression+0.5;
  }
  if (sst == FLAG) {
    sst = 999.;
  }
  else {
    sst = 10.*sst+0.5;
  }

  // Fix the year 
  if (year >= 100) {
    tyear = year - 100;
  }
  else if (year < 0) {
    tyear = year + 100;
  }
  else {
    tyear = year;
  }

  //             Y  M  D  H  LA LO R  Stn Id.......... SLP WD WS Ta Td C SST
  fprintf(fout,"%2d%2d%2d%4ld%5ld%5ld%2d%c%c%c%c%c %4ld%3ld%3ld%4ld%3ld%1c%3ld\n",
    tyear, month, day, lrint(100*hour),
    lrint(100*latitude), lrint(100.*longitude), report_type, 
    station_id[0],station_id[1], station_id[2], station_id[3], station_id[4],
    lrint(slp),
    lrint(wind_dir), lrint( wind_speed),lrint( air_temp),
    lrint(dewpt_depression), cloud_cover, lrint( sst)  );
} 
 
//Future: The selection of read types should be invisible to the user
buoy_report& buoy_report::iabpread(FILE *fin) {
  int yy, mm, dd, hh;
  float lat, lon;
  char id[MAX_ID_LEN];
  //bool bad_data = false;

  fscanf(fin, "%d %d %d %d %s %f %f\n",&yy, &mm, &dd, &hh, &id[0], &lat, &lon);
  //if (fabs(lat) >= 90. || fabs(lon) >= 999. || lat == 0 ) {
  //  bad_data = true;
  //}

  year      = yy;
  if (year > 1800) { year -= 1900; } // Change to 2 digits for mktime
  month     = mm;
  day       = dd;
  hour      = (float) hh;
  latitude  = lat;
  longitude = lon;
  if (longitude < 0.) longitude += 360.;
  sprintf(station_id, "%s",id);
 
//The following are not in the IABP 'C' data set
  slp         = FLAG;
  wind_dir    = FLAG;
  wind_speed  = FLAG;
  air_temp    = FLAG;
  sst         = FLAG;
  dewpt_depression = FLAG;

//Construct the tm variable for making comparisons
  obs_time.tm_year    = year;
  obs_time.tm_mon     = month - 1;
  obs_time.tm_mday    = day;
  obs_time.tm_hour    = (int) hour;
  obs_time.tm_min     = (int) (60.*(hour - (int)hour) );
  obs_time.tm_sec     = (int) (3600.*(hour - (int)hour) - 60*obs_time.tm_min);
  obs_secs = mktime(&obs_time);

//This puts to accord with CPC format
  if (obs_secs < report_change_time) {
     report_type = 62 ;
  }
  else {
     report_type = 29;
  }
 
  return *this;
}

buoy_report& buoy_report::read(FILE *fin) {
  char line[MAXBUOYLINE];
  char *y;

  y = new char [MAXBUOYLINE];
  fgets(line, MAXBUOYLINE, fin);
  #ifdef VERBOSE
    printf("line = %s",line); fflush(stdout);
  #endif

  strhelp(line, 1, 2, y);
  year        = atoi(y);
// Add the following because of bouy file treatment upstream:
  if (year < 50) year += 100;

  strhelp(line, 3, 4, y);
  month       = atoi(y);
  strhelp(line, 5, 6, y);
  day         = atoi(y);
  
  strhelp(line, 7, 10, y);
  hour        = atof(y);
  hour /= 100.;
  #ifdef VERBOSE
    printf("ymd h %2d %2d %2d %f %s\n",year, month, day, hour, y);
    fflush(stdout);
  #endif

  strhelp(line, 11, 15, y);
  latitude    = atof(y);
  latitude /= 100.;

  strhelp(line, 16, 20, y);
  longitude   = atof(y);
  longitude /= 100.;

  strhelp(line, 21, 22, y);
  report_type = atoi(y);

  strhelp(line, 23, 27, y);
  strncpy(station_id, y, MAX_ID_LEN);
  //station_id[5]  = '\0';

//Note: there is a space after the station id
  strhelp(line, 29, 32, y);
  slp         = atof(y);
  if (slp == 9999.) slp = FLAG;
  if (slp != FLAG) {
    slp = 900. + slp/10.;
  }
  
  strhelp(line, 33, 35, y);
  wind_dir    = atof(y);
  if (wind_dir == 999.) wind_dir = FLAG;

  strhelp(line, 36, 38, y);
  wind_speed  = atof(y);
  if (wind_speed == 999.) {wind_speed = FLAG; 
  }

  strhelp(line, 39, 42, y);
  air_temp    = atof(y);
  if (air_temp == 9999.)  { air_temp = FLAG; }
  else {
    air_temp /= 10.;
  } 

  strhelp(line, 43, 45, y);
  dewpt_depression = atof(y);
  if (dewpt_depression == 999.) { dewpt_depression = FLAG; }
  else {
     dewpt_depression /= 10;
  }

  strhelp(line, 46, 46, y);
  strncpy(&cloud_cover, y, 1); 
    
  strhelp(line, 47, 49, y);
  sst         = atof(y);
  if (sst == 999.) { sst = FLAG; }
  else {
    sst /= 10.;
  }
  
//Construct the tm variable for making comparisons
  obs_time.tm_year = year;
  obs_time.tm_mon     = month - 1;
  obs_time.tm_mday    = day;
  obs_time.tm_hour    = (int) hour;
  obs_time.tm_min     = (int) (60.*(hour - (int)hour) );
  obs_time.tm_sec     = (int) (3600.*(hour - (int)hour) - 60*obs_time.tm_min);
  obs_secs = mktime(&obs_time);
 
  if (y != (char *) NULL) { 
    delete(y); 
  }
  return *this;
}

bool buoy_report::near(buoy_report &x, const char *y, time_t toler) {
  time_t t1, t2;
  #ifdef VERBOSE
    printf("%s %s %s %d\n",station_id, x.station_id, y, 
                             strncmp(x.station_id, y, MAX_ID_LEN) );
  #endif
  if (strncmp(x.station_id, y, MAX_ID_LEN) == 0) {
    t1 = obs_secs;
    t2 = x.obs_secs;
    #ifdef VERBOSE
      printf("t1 t2 %d %d %d\n",(int)t1, (int)t2, (int)toler);
    #endif
    if ( abs(t2-t1) > toler || t1 < 0 || t2 < 0 ) {
      return false;
    }
    else {
      return true;
    }
  }
  else {
    return false;
  }
}
bool buoy_report::near(tm &x, time_t toler) {
  time_t t1, t2;

  t1 = this->obs_secs;
  t2 = mktime(&x );
  if ( abs(t2-t1) > toler || t1 < 0 || t2 < 0 ) {
    return false;
  }
  else {
    return true;
  }
}

/////////////////////////////////////////////////////
class avbuoy : public buoy_report {
   private:
    float alatitude, alongitude; 
    int count;
    time_t reftime; // set to the first ob time
    time_t delta_time; // delta's relative to the reference time
   public:
    avbuoy();
    avbuoy & operator=(buoy_report &);
    avbuoy & operator=(avbuoy &);
    avbuoy & operator+=(buoy_report &);
    avbuoy & average();
    void write(FILE *fout);
    avbuoy & read(FILE *fin);
};
avbuoy::avbuoy() {
  latitude = 0.0;
  longitude = 0.0;
  alatitude = 0.0;
  alongitude = 0.0;
  count = 0;
  reftime = 0; delta_time = 0;
  sprintf(&station_id[0],"%s","blank");
}
avbuoy & avbuoy::operator=(avbuoy &x) {

  count = x.count;

  alatitude = x.latitude;
  alongitude = x.longitude; // Make averaging sbr figure the difficult parts

  reftime = x.get_secs();
  set_secs(reftime);
  delta_time = (time_t) 0;

  strncpy(&station_id[0], &x.station_id[0], MAX_ID_LEN);
  
  return *this;

}

avbuoy & avbuoy::operator=(buoy_report &x) {
  count = 1;

  alatitude = x.latitude;
  alongitude = x.longitude; // Make averaging sbr figure the difficult parts

  reftime = x.get_secs();
  delta_time = (time_t) 0;

  strncpy(&station_id[0], &x.station_id[0], MAX_ID_LEN);

// Buoy basic date info:
  year     = x.year;
  month    = x.month;
  day      = x.day;
  hour     = x.hour;
  // obs_time is currently not being copied over
// Buoy data parameters:
  slp              = x.slp;
  wind_dir         = x.wind_dir;
  wind_speed       = x.wind_speed;
  air_temp         = x.air_temp;
  dewpt_depression = x.dewpt_depression;
  cloud_cover      = x.cloud_cover;
  sst              = x.sst;
  report_type      = x.report_type;

  return *this;
}
// Add in a buoy report
avbuoy& avbuoy::operator+=(buoy_report &x) {
  // Only add if same id, and only if within 1 degree (100 km) latitude
  if (strncmp(x.station_id, station_id, MAX_ID_LEN) != 0) return *this;
  if (fabs(alatitude/count - x.latitude) > 1.0) return *this;

  delta_time += x.get_secs() - reftime;
  alatitude   += x.latitude;

  if (fabs(x.longitude-alongitude/count) > 180.) {
    if(alongitude/count > 180.) {
      alongitude += (360. + x.longitude);
    }
    else {
      alongitude += (x.longitude - 360.);
    }
  }
  else {
    alongitude += x.longitude;
  }

  // Cannot increment until after the addition of longitudes!
  count      += 1;
  return *this;
}
avbuoy& avbuoy::average() {
  time_t t;
  if (count > 1) {
    delta_time /= count;
    alatitude   /= count;
    alongitude  /= count;
    count = 1;
  }
  latitude  = alatitude;
  longitude = alongitude;
  t = reftime + delta_time;
  set_secs(t);
  return *this;
}
void avbuoy::write(FILE *fout) {
  tm *out_time;
  time_t av_time ;
  float hour;

  // Ensure that averaging has been performed:
  if (count != 1) this->average();

  // For output purposes, rewrite to full time (out_time)
  av_time = reftime + delta_time;
  out_time = localtime(&av_time); //must use localtime as mktime
                                  //uses local clock
  hour = out_time->tm_hour + out_time->tm_min/60. + out_time->tm_sec/3600.;
  if (hour > 23.995) {
    av_time += (time_t) (.005 * 3600.) + 1;
    out_time = localtime(&av_time);
    hour = out_time->tm_hour + out_time->tm_min/60. + out_time->tm_sec/3600.;
  }

  //Fix up longitude:
  if (alongitude <    0.0) alongitude += 360.0;
  if (alongitude >= 360.0) alongitude -= 360.0;
  latitude = alatitude;
  longitude = alongitude;

  // Change over: write out in the same format as read in, preserve all
  // original information.  4 April 2000.
  this->buoy_report::write(fout);

}
avbuoy & avbuoy::read(FILE *fin) {
  int year, month, mday;
  float hour;
  tm tmvar;

  count = 1;
  fscanf(fin, "%d %d %d %f %f %f %s\n", &year, &month, &mday, &hour, &alatitude,
    &alongitude, &station_id[0]);
  latitude  = alatitude;
  longitude = alongitude;
  tmvar.tm_year = year - 1900;
  tmvar.tm_mon  = month - 1;
  tmvar.tm_mday = mday;
  tmvar.tm_hour = (int) hour;
  tmvar.tm_min  = (int) ((hour - (int) hour)*60.);
  tmvar.tm_sec  = (int) ((hour - (int) hour)*3600. - 60.*tmvar.tm_min);
  reftime  = mktime(&tmvar);
  if (reftime == -1) {
    cout << "Failed to set up a valid time in mktime!!\n";
    cout.flush();
  }
  set_secs(reftime);

  return *this;
}
  
#endif
/////////////////////////////////////////////////////

#ifndef MATCHUP_H
  #define MATCHUP_H
class matchup {
  public:
     int year, month, day;
     int skpt, lead;
     float lat1, lon1, lat2, lon2;
     float obs_dist, obs_dir, fcst_dist, fcst_dir;
     float speed;

     matchup(void);
     int read(FILE *);
};

matchup::matchup(void): year(98), month(1), day(1), skpt(0), lead(0),
                        lat1(0), lon1(0), lat2(0), lon2(0),
                        obs_dist(0), obs_dir(0), fcst_dist(0), fcst_dir(0) {}
//matchup::matchup(void) {
////  year = 98; month = 1; day = 1;
////  skpt = 0; lead = 0;
////  lat1 = 0.; lon1 = 0.; lat2 = 0.; lon2 = 0.;
////  obs_dir = 0.; obs_dist = 0.;
////  fcst_dir = 0.; fcst_dist = 0.;
////

int matchup::read(FILE *fin) {
  char *id;
  id=new char[MAX_ID_LEN];

  if ( feof(fin) ) {
    return 0;
  }
  fscanf(fin, "%d %d %d %d %d %f %f to %f %f %f %f %f %f %f %s\n",
    &year, &month, &day, &skpt, &lead, &lat1, &lon1, &lat2, &lon2,
    &obs_dist, &obs_dir, &fcst_dist, &fcst_dir, &speed, id);
  #ifdef VERBOSE2
  printf(
   "%2d %2d %2d %3d %2d %5.2f %6.2f to %5.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",
    year, month, day, skpt, lead, lat1, lon1, lat2, lon2,
    obs_dist, obs_dir, fcst_dist, fcst_dir);
  #endif

  delete id;

  return 1;
}
#endif
