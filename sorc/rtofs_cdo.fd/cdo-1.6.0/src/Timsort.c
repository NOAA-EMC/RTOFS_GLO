/*
  This file is part of CDO. CDO is a collection of Operators to
  manipulate and analyse Climate model Data.

  Copyright (C) 2003-2011 Uwe Schulzweida, Uwe.Schulzweida@zmaw.de
  See COPYING file for copying and redistribution conditions.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
*/

/*
   This module contains the following operators:

     Timsort    timsort         Sort over the time
*/


#if defined (_OPENMP)
#  include <omp.h>
#endif

#include <cdi.h>
#include "cdo.h"
#include "cdo_int.h"
#include "pstream.h"


#define  NALLOC_INC  1024

static
int cmpdarray(const void *s1, const void *s2)
{
  int cmp = 0;
  double *x = (double *) s1;
  double *y = (double *) s2;
  /*
  printf("%d %d  %d %d\n", x->code, y->code, x, y);
  */
  if      ( *x < *y ) cmp = -1;
  else if ( *x > *y ) cmp =  1;

  return (cmp);
}


void *Timsort(void *argument)
{
  int gridsize;
  int nrecs;
  int gridID, varID, levelID, recID;
  int tsID;
  int i;
  int nts;
  int nalloc = 0;
  int streamID1, streamID2;
  int vlistID1, vlistID2, taxisID1, taxisID2;
  int nmiss;
  int nvars, nlevel;
  int *vdate = NULL, *vtime = NULL;
  int ompthID;
  double missval;
  double **sarray = NULL;
  field_t ***vars = NULL;

  cdoInitialize(argument);

  streamID1 = streamOpenRead(cdoStreamName(0));

  vlistID1 = streamInqVlist(streamID1);
  vlistID2 = vlistDuplicate(vlistID1);

  taxisID1 = vlistInqTaxis(vlistID1);
  taxisID2 = taxisCreate(TAXIS_ABSOLUTE);
  vlistDefTaxis(vlistID2, taxisID2);

  streamID2 = streamOpenWrite(cdoStreamName(1), cdoFiletype());

  streamDefVlist(streamID2, vlistID2);

  nvars = vlistNvars(vlistID1);

  tsID = 0;
  while ( (nrecs = streamInqTimestep(streamID1, tsID)) )
    {
      if ( tsID >= nalloc )
	{
	  nalloc += NALLOC_INC;
	  vdate = (int *) realloc(vdate, nalloc*sizeof(int));
	  vtime = (int *) realloc(vtime, nalloc*sizeof(int));
	  vars  = (field_t ***) realloc(vars, nalloc*sizeof(field_t **));
	}

      vdate[tsID] = taxisInqVdate(taxisID1);
      vtime[tsID] = taxisInqVtime(taxisID1);

      vars[tsID] = (field_t **) malloc(nvars*sizeof(field_t *));

      for ( varID = 0; varID < nvars; varID++ )
	{
	  gridID  = vlistInqVarGrid(vlistID1, varID);
	  missval = vlistInqVarMissval(vlistID1, varID);
	  nlevel  = zaxisInqSize(vlistInqVarZaxis(vlistID1, varID));

	  vars[tsID][varID] = (field_t *) malloc(nlevel*sizeof(field_t));

	  for ( levelID = 0; levelID < nlevel; levelID++ )
	    {
	      vars[tsID][varID][levelID].grid    = gridID;
	      vars[tsID][varID][levelID].missval = missval;
	      vars[tsID][varID][levelID].ptr     = NULL;
	    }
	}

      for ( recID = 0; recID < nrecs; recID++ )
	{
	  streamInqRecord(streamID1, &varID, &levelID);
	  gridID   = vlistInqVarGrid(vlistID1, varID);
	  gridsize = gridInqSize(gridID);
	  vars[tsID][varID][levelID].ptr = (double *) malloc(gridsize*sizeof(double));
	  streamReadRecord(streamID1, vars[tsID][varID][levelID].ptr, &nmiss);
	  vars[tsID][varID][levelID].nmiss = nmiss;
	}

      tsID++;
    }

  nts = tsID;

  sarray = (double **) malloc(ompNumThreads*sizeof(double *));
  for ( i = 0; i < ompNumThreads; i++ )
    sarray[i] = (double *) malloc(nts*sizeof(double));

  for ( varID = 0; varID < nvars; varID++ )
    {
      if ( vlistInqVarTime(vlistID1, varID) == TIME_CONSTANT ) continue;

      gridID   = vlistInqVarGrid(vlistID1, varID);
      gridsize = gridInqSize(gridID);
      nlevel   = zaxisInqSize(vlistInqVarZaxis(vlistID1, varID));
      for ( levelID = 0; levelID < nlevel; levelID++ )
	{
#if defined (_OPENMP)
#pragma omp parallel for default(shared) private(i, ompthID, tsID)
#endif
	  for ( i = 0; i < gridsize; i++ )
	    {
#if defined (_OPENMP)
	      ompthID = omp_get_thread_num();
#else
	      ompthID = 0;
#endif
	      for ( tsID = 0; tsID < nts; tsID++ )
		sarray[ompthID][tsID] = vars[tsID][varID][levelID].ptr[i];

	      qsort(sarray[ompthID], nts, sizeof(double), cmpdarray);  	      

	      for ( tsID = 0; tsID < nts; tsID++ )
		vars[tsID][varID][levelID].ptr[i] = sarray[ompthID][tsID];
	    }
	}
    }

  for ( i = 0; i < ompNumThreads; i++ )
    if ( sarray[i] ) free(sarray[i]);

  if ( sarray ) free(sarray);

  for ( tsID = 0; tsID < nts; tsID++ )
    {
      taxisDefVdate(taxisID2, vdate[tsID]);
      taxisDefVtime(taxisID2, vtime[tsID]);
      streamDefTimestep(streamID2, tsID);

      for ( varID = 0; varID < nvars; varID++ )
	{
	  nlevel = zaxisInqSize(vlistInqVarZaxis(vlistID1, varID));
	  for ( levelID = 0; levelID < nlevel; levelID++ )
	    {
	      if ( vars[tsID][varID][levelID].ptr )
		{
		  nmiss = vars[tsID][varID][levelID].nmiss;
		  streamDefRecord(streamID2, varID, levelID);
		  streamWriteRecord(streamID2, vars[tsID][varID][levelID].ptr, nmiss);
		  free(vars[tsID][varID][levelID].ptr);
		}
	    }
	  free(vars[tsID][varID]);
	}
      free(vars[tsID]);      
    }

  if ( vars  ) free(vars);
  if ( vdate ) free(vdate);
  if ( vtime ) free(vtime);

  streamClose(streamID2);
  streamClose(streamID1);

  cdoFinish();

  return (0);
}