#ifndef SSMIS_INCLUDE
#define SSMIS_INCLUDE

/* Robert Grumbine 2008 -- present */
/* Set up data types/classes as appropriate for the AVHRR instruments on */
/*   NOAA-17, 18, and METOP-A                                            */

/* Translate to AMSR-suitable 10 April 2009                              */
/* Translate to SSMI-S 15 October 2010                                   */

/* Frequencies are . 19.35, 22.235, 37, 91.655, 150 GHz                  */
/*   horizontal, then vertical, polarization except 22 and 150 which     */
/*   are H only                                                          */
/* Note that we are ignoring the sounding channels                       */

typedef struct {
  float tmbr;       /* to 0.01 K, note no channel number or latinfo 
                       here.  Simpler system */
} ssmisu_spots;

typedef struct {
  short int satid;

  short int year;
  unsigned char month, day, hour, minute, second;

  double clat, clon;

  ssmisu_spots obs[8]; 

} ssmisupt;

/* Note that these indices refer to the ssmisupt, rather than the */
/*   ice spot values I'll be using later */
#define SSMIS_T150H 0
#define SSMIS_T19H 1
#define SSMIS_T19V 2
#define SSMIS_T22V 3
#define SSMIS_T37H 4
#define SSMIS_T37V 5
#define SSMIS_T92V 6
#define SSMIS_T92H 7


/* RG: Look this up -- currently copying ssmi value */
#ifndef SSMIS_MAX_LATITUDE
  #define SSMIS_MAX_LATITUDE 88.5
#endif
#define NSPOTS 1000
/* Structures which relate to bufr decoding */

typedef struct {
    ssmisupt full[NSPOTS];
} scan_points;

extern int process_bufr(ssmisupt *b);
extern int check_bufr(ssmisupt *b);
extern void zero_bufr(ssmisupt *b);
#endif
