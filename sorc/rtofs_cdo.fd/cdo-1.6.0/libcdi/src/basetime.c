#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stdio.h>

#include "error.h"
#include "cdi.h"
#include "basetime.h"

#undef  UNDEFID
#define UNDEFID  CDI_UNDEFID

void basetimeInit(BaseTime *basetime)
{
  if ( basetime == NULL )
    Error("Internal problem! Basetime not allocated.");

  (*basetime).ncvarid       = UNDEFID;
  (*basetime).ncdimid       = UNDEFID;
  (*basetime).ncvarboundsid = UNDEFID;
  (*basetime).lwrf          = 0;
}
