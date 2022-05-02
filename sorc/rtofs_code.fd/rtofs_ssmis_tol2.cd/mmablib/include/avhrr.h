
/* Robert Grumbine 2009 -- present

// Set up data types/classes as appropriate for the AVHRR instruments on
//   NOAA-17, 18, and METOP-A
*/

typedef struct {
  unsigned char channel;
  unsigned char albedo; /* 255 is flag value      */
  double tmbr;          /* 0.01 degree precision  */
} avhrr_spots;

typedef struct {
  short int year;
  unsigned char month, day, hour, minute, second;
  short int satid;
  double clat, clon;
  double soza, saza;
  avhrr_spots obs[5];

  unsigned char sstype, sstsrc;
  double sst;
  unsigned char irel; /* 38-150 range  */
  short int rms;     /* 0-655 range  */
  double solazi;
  int opth;           /* 0-655 range  */
} avhrrpt;

/* Variant for GAC tank AVCL18 */
typedef struct {
  short int year;
  unsigned char month, day, hour, minute, second;
  short int satid;
  double clat, clon;
  double soza, saza;
  avhrr_spots obs[5];

  double fovn, clavr;
} gacpt;
