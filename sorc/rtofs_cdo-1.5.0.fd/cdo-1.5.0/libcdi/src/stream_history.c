#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "dmemory.h"
#include "cdi.h"
#include "stream_int.h"
#include "stream_cdf.h"


void streamDefHistory(int streamID, int length, const char *history)
{
  stream_t *streamptr;

  streamptr = stream_to_pointer(streamID);

  if ( streamptr->filetype == FILETYPE_NC  ||
       streamptr->filetype == FILETYPE_NC2 ||
       streamptr->filetype == FILETYPE_NC4 )
    {
      char *histstring;
      size_t len;
      if ( history )
	{
	  len = strlen(history);
	  if ( len )
	    {
	      histstring = strdupx(history);
	      cdfDefHistory(streamID, length, histstring);
	      free(histstring);
	    }
	}
    }
}


int streamInqHistorySize(int streamID)
{
  int size = 0;
  stream_t *streamptr;

  streamptr = stream_to_pointer(streamID);

  if ( streamptr->filetype == FILETYPE_NC  ||
       streamptr->filetype == FILETYPE_NC2 ||
       streamptr->filetype == FILETYPE_NC4 )
    {
      size = cdfInqHistorySize(streamID);
    }

  return (size);
}


void streamInqHistoryString(int streamID, char *history)
{
  stream_t *streamptr;

  streamptr = stream_to_pointer(streamID);

  if ( streamptr->filetype == FILETYPE_NC  ||
       streamptr->filetype == FILETYPE_NC2 ||
       streamptr->filetype == FILETYPE_NC4 )
    {
      cdfInqHistoryString(streamID, history);
    }
}
