#include <time.h>
/* Robert Grumbine */
/* 2005 */
/* Date declarations for NARR */

#ifndef DATEH
  #define DATEH
typedef struct {
  unsigned int year   : 12;
  unsigned int month  :  4;
  unsigned int day    :  6;
  unsigned int hour   :  5;
  unsigned int minute :  6;
  unsigned int second :  6;
} packed_bufr_date;

typedef struct {
  short int year;
  unsigned char month, day, hour, minute, second;
} bufr_date;

typedef struct tm unix_date;
typedef time_t unix_secs;


#endif
