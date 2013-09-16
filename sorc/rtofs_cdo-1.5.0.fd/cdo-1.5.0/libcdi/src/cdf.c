#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "cdf.h"
#include "cdi.h"
#include "stream_int.h"
#include "cdf_int.h"


const char *cdfLibraryVersion(void)
{
#if  defined  (HAVE_LIBNETCDF)
  return (nc_inq_libvers());
#else
  return ("library undefined");
#endif
}

#if  defined(HAVE_LIBHDF5)
#if defined(__cplusplus)
extern "C" {
#endif
  int H5get_libversion(unsigned *, unsigned *, unsigned *);
#if defined(__cplusplus)
}
#endif
#endif

const char *hdfLibraryVersion(void)
{
#if  defined(HAVE_LIBHDF5)
  static char hdf_libvers[256];
  unsigned majnum, minnum, relnum;

  H5get_libversion(&majnum, &minnum, &relnum);

  sprintf(hdf_libvers, "%u.%u.%u", majnum, minnum, relnum);

  return (hdf_libvers);
#else
  return ("library undefined");
#endif
}


int CDF_Debug   = 0;    /* If set to 1, debugging           */


void cdfDebug(int debug)
{
  CDF_Debug = debug;

  if ( CDF_Debug )
    Message("debug level %d", debug);
}

static
void cdfComment(int ncid)
{
#if  defined  (HAVE_LIBNETCDF)
  static char comment[256] = "Climate Data Interface version ";
  static int init = 0;
  char *blank;
  int size = 0;

  if ( ! init )
    {
      init = 1;
      blank = strchr(cdiLibraryVersion(), ' ');
      if ( blank ) size = blank - cdiLibraryVersion();

      if ( size == 0 || ! isdigit((int) *cdiLibraryVersion()) )
	strcat(comment, "??");
      else
	strncat(comment, cdiLibraryVersion(), size);
      strcat(comment, " (http://code.zmaw.de/projects/cdi)");
    }

  cdf_put_att_text(ncid, NC_GLOBAL, "CDI", strlen(comment), comment);
  cdf_put_att_text(ncid, NC_GLOBAL, "Conventions", 6, "CF-1.0");
#endif
}


int cdfOpenFile(const char *filename, const char *mode, int version)
{
  int ncid = -1;
#if  defined  (HAVE_LIBNETCDF)
  int fmode;
  int writemode = NC_CLOBBER;
  int readmode = NC_NOWRITE;
  int status;

  if ( filename == NULL )
    ncid = CDI_EINVAL;
  else
    {
      switch (*mode)
	{
	case 'r':
	case 'R':
	  fmode = 'r';
	  status = cdf_open(filename, readmode, &ncid);
	  if ( status > 0 && ncid < 0 ) ncid = CDI_ESYSTEM;
#if  defined  (NC_NETCDF4)
	  /*
	  else
	    {
	      int format;
	      (void) nc_inq_format(ncid, &format);
	      if ( format == NC_FORMAT_NETCDF4 )
		{
		  cdf_close(ncid);
		  ncid = CDI_EUNC4;
		}
	    }
	  */
#endif
	  break;
	case 'w':
	case 'W':
	  fmode = 'w';
#if  defined  (NC_64BIT_OFFSET)
	  if ( version == 2 ) writemode = NC_CLOBBER | NC_64BIT_OFFSET;
#endif
#if  defined  (NC_NETCDF4)
	  if ( version == 4 ) writemode = NC_CLOBBER | NC_NETCDF4 /*| NC_CLASSIC_MODEL*/;
#endif
	  cdf_create(filename, writemode, &ncid);
	  cdfComment(ncid);
	  break;
	case 'a':
	case 'A':
	  fmode = 'a';
	  cdf_open(filename, NC_WRITE, &ncid);
	  break;
	default:
	  ncid = CDI_EINVAL;
	}
    }
#endif

  return (ncid);
}


int cdfOpen(const char *filename, const char *mode)
{
  int fileID = 0;

  if ( CDF_Debug )
    Message("open %s with mode %c", filename, *mode);

  fileID = cdfOpenFile(filename, mode, 1);

  if ( CDF_Debug )
    Message("file %s opened with id %d", filename, fileID);

  return (fileID);
}


int cdfOpen64(const char *filename, const char *mode)
{
  int fileID = -1;

  if ( CDF_Debug )
    Message("open %s with mode %c", filename, *mode);

#if  defined  (HAVE_LIBNETCDF)
#if  ! defined  (NC_64BIT_OFFSET)
  fileID = CDI_ELIBNAVAIL;
  return (fileID);
#endif
#endif

  fileID = cdfOpenFile(filename, mode, 2);

  if ( CDF_Debug )
    Message("file %s opened with id %d", filename, fileID);

  return (fileID);
}


int cdf4Open(const char *filename, const char *mode)
{
  int fileID = -1;

  if ( CDF_Debug )
    Message("open %s with mode %c", filename, *mode);

#if  defined  (HAVE_LIBNETCDF)
#if  ! defined  (NC_NETCDF4)
  fileID = CDI_ELIBNAVAIL;
  return (fileID);
#endif
#endif

  fileID = cdfOpenFile(filename, mode, 4);

  if ( CDF_Debug )
    Message("file %s opened with id %d", filename, fileID);

  return (fileID);
}


void cdfCloseFile(int fileID)
{
#if  defined  (HAVE_LIBNETCDF)
  cdf_close(fileID);
#endif
}

void cdfClose(int fileID)
{
  cdfCloseFile(fileID);
}
