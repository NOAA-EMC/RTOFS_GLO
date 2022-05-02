/* Information for decoding the SDR orbit by orbit files */
/* Robert Grumbine 16 December 1994 */
/* Extraneous definitions removed 18 November 1998 */
/* -- extraneous due to changes in input data from SDR to BUFR */

#ifndef SSMI_INCLUDE

  #define SSMI_INCLUDE

/* Define the characteristics of the DMSP F-11 satellite orbit */
/* Orbit time = 1:41:57 */
/* Orbit inclination =  */
/* Swath width = 1600? */
/* Maximum observed latitude = */
  #define SSMI_MAX_LATITUDE  87.5

/* Satellite Altitude = */
/* Antenna size = */
/* Exact operating frequencies = */

/* Define the characteristics of the data file structure */
  #define NORBITS          1
  #define NSCANS           64

/* Structures which relate to bufr decoding */
  typedef struct {
      int kwrit;
      float latitude, longitude;
      int sftg, posn;
      float t85v, t85h;
  } ssmi_short_bufr;
 
  typedef struct {
      int scan_counter;
      float latitude, longitude;
      int surface_type;
      int position_num;
      float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
      ssmi_short_bufr hires[3];
  } ssmi_bufr_point;

  typedef struct {
      int satno, year, month, day, hour, mins, secs, scan_no;
      ssmi_bufr_point full[NSCANS];
  } ssmi_bufr_line;

/* Declare BUFR function prototypes */
#ifdef CPLUS
  extern "C" int process_bufr(ssmi_bufr_line *b);
#else
  extern int process_bufr(ssmi_bufr_line *b);
#endif
extern int check_bufr(ssmi_bufr_line *b);
extern int check_short_bufr(ssmi_short_bufr *b);
extern void zero_bufr(ssmi_bufr_line *b, int i);

#endif
