#ifndef _VLIST_H
#define _VLIST_H

#include <stddef.h>  /* size_t */

#ifndef _CDI_LIMITS_H
#  include "cdi_limits.h"
#endif


/*
 * CDI attribute
 */
typedef struct {
  size_t    xsz;	  /* amount of space at xvalue                      */
  size_t    namesz;       /* size of name                                   */
  char     *name;         /* attribute name                                 */
  int       indtype;	  /* internal data type of xvalue (INT, FLT or TXT) */
  int       exdtype;      /* external data type                             */
                          /* indtype    exdtype                             */
                          /* TXT        TXT                                 */
                          /* INT        INT16, INT32                        */
                          /* FLT        FLT32, FLT64                        */
  size_t    nelems;    	  /* number of elements                             */
  void     *xvalue;       /* the actual data                                */
} cdi_att_t;


typedef struct {
  size_t     nalloc;		/* number allocated >= nelems */
  size_t     nelems;		/* length of the array */
  cdi_att_t  value[MAX_ATTRIBUTES];
} cdi_atts_t;


typedef struct
{
  int      flag;
  int      index;
  int      mlevelID;
  int      flevelID;
}
levinfo_t;


typedef struct
{
  int         flag;
  int         nlevs;
  int         isUsed;
  int         mvarID;
  int         fvarID;
  int         param;
  int         gridID;
  int         zaxisID;
  int         timeID;    /* TIME_VARIABLE or TIME_CONSTANT */
  int         datatype;  /* DATATYPE_PACKX for GRIB data, else DATATYPE_FLT32 or DATATYPE_FLT64 */
  int         instID;
  int         modelID;
  int         tableID;
  int         tsteptype; /* Time step type: TSTEP_INSTANT, TSTEP_AVG ... */
  int         timave;
  int         timaccu;
  int         missvalused; /* TRUE if missval is defined */
  char       *name;
  char       *longname;
  char       *stdname;
  char       *units;
  double      missval;
  double      scalefactor;
  double      addoffset;
  levinfo_t  *levinfo;
  int         ztype;
  int         zlevel;
  cdi_atts_t  atts;
}
var_t;


typedef struct
{
  int         self;
  int         used;
  int         nlock;
  int         nvars;        /* number of variables                */
  int         ngrids;
  int         nzaxis;
  int         ntsteps;
  int         taxisID;
  int         tableID;
  int         instID;
  int         modelID;
  int         varsAllocated;
  int         gridIDs[MAX_GRIDS_PS];
  int         zaxisIDs[MAX_ZAXES_PS];
  var_t      *vars;
  cdi_atts_t  atts;
}
vlist_t;


vlist_t *vlist_to_pointer(int vlistID);
int      vlistNlock(int vlistID);
void     vlistLock(int vlistID);
void     vlistUnlock(int vlistID);
char    *vlistInqVarNamePtr(int vlistID, int varID);
char    *vlistInqVarLongnamePtr(int vlistID, int varID);
char    *vlistInqVarStdnamePtr(int vlistID, int varID);
char    *vlistInqVarUnitsPtr(int vlistID, int varID);
void     vlistDestroyVarName(int vlistID, int varID);
void     vlistDestroyVarLongname(int vlistID, int varID);
void     vlistDestroyVarUnits(int vlistID, int varID);
void     vlistDefVarTime(int vlistID, int varID, int timeID);
int      vlistInqVarMissvalUsed(int vlistID, int varID);
int      vlistHasTime(int vlistID);

int      vlistDelAtts(int vlistID, int varID);
int      vlistCopyVarAtts(int vlistID1, int varID_1, int vlistID2, int varID_2);

#endif  /* _VLIST_H */