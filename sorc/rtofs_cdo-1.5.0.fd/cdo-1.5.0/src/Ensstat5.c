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
   Ensstat5       enscrps          Ensemble cumulative ranked probability score
*/

#if defined (_OPENMP)
#  include <omp.h>
#endif

#include <cdi.h>
#include "cdo.h"
#include "statistic.h"
#include "cdo_int.h"
#include "pstream.h"
#include "util.h"
#include "merge_sort2.h"

void *Ensstat5(void *argument)
{
  int operatorID;
  int operfunc, datafunc;
  int i,k;
  int nvars,nrecs = 0, nrecs0, nmiss, nens, nfiles,nlevs,valcount;
  int cmpflag;
  int cum;
  int levelID, varID, recID, tsID, binID, ensID;
  int gridsize = 0;
  int gridID, gridID2;
  int have_miss = 0;
  int streamID = 0, streamID2;
  int vlistID, vlistID1, vlistID2;
  int taxisID1, taxisID2;
  int zaxisID1,zaxisID2;
  int ompthID;
  int *varID2;
  int xsize,ysize;
  double missval;
  double *alpha, *beta, *alpha_weights, *beta_weights;
  double xval=0; double yval=0;
  double *val;
  double *weights, sum_weights;
  double reli, crps_pot,sprd, crps;
  double heavyside0, heavysideN;
  double g,o,p;
  
  int fileID;
  const char *ofilename;

  typedef struct
  {
    int streamID;
    int vlistID;
    double *array;
  } ens_file_t;
  ens_file_t *ef = NULL;
  
  cdoInitialize(argument);
  
  cdoOperatorAdd("enscrps",   0,  0,   NULL);
  
  operatorID = cdoOperatorID();
  operfunc = cdoOperatorF1(operatorID);
  datafunc = cdoOperatorF2(operatorID);

  nfiles = cdoStreamCnt() - 1;
  nens = nfiles-1;

  val = (double *) calloc ( nfiles,sizeof(double) );
  alpha=(double *) calloc ( nens+1,sizeof(double) );
  beta =(double *) calloc ( nens+1,sizeof(double) );
  alpha_weights=(double *) calloc ( nens+1,sizeof(double) );
  beta_weights =(double *) calloc ( nens+1,sizeof(double) );

  if ( cdoVerbose )
    cdoPrint("Ensemble over %d files (Ensstat5).", nfiles-1);

  ofilename = cdoStreamName(nfiles);

  if ( !cdoSilentMode && !cdoOverwriteMode )
    if ( fileExist(ofilename) )
      if ( !userFileOverwrite(ofilename) )
	cdoAbort("Outputfile %s already exist!", ofilename);

  ef = (ens_file_t *) malloc(nfiles*sizeof(ens_file_t));
  
  for ( fileID = 0; fileID < nfiles; fileID++ )
    {
      streamID = streamOpenRead(cdoStreamName(fileID));

      vlistID = streamInqVlist(streamID);
      
      ef[fileID].streamID = streamID;
      ef[fileID].vlistID = vlistID;
    }

  if ( cdoVerbose ) 
    cdoPrint("Opened %i Input Files for Ensemble Operator",nfiles);
  
  /* check for identical contents of all ensemble members */
  nvars = vlistNvars(ef[0].vlistID);
  if ( nvars == 1 ) 
    cmpflag = CMP_NAME | CMP_GRIDSIZE | CMP_NLEVEL | CMP_GRID;
  else 
    cdoAbort("Only single-variable files supported");

  for ( fileID = 1; fileID < nfiles; fileID++ )
    vlistCompare(ef[0].vlistID, ef[fileID].vlistID, cmpflag);

  vlistID1 = ef[0].vlistID;

  zaxisID1 = vlistInqVarZaxis(vlistID1,0);
  nlevs    = zaxisInqSize(zaxisID1);
  zaxisID2 = zaxisDuplicate(zaxisID1);

  vlistID2 = vlistCreate();
  varID2 = (int *) malloc( 2*sizeof(int));
  gridID2 = gridCreate(GRID_LONLAT, 1);
  gridDefXsize(gridID2, 1);
  gridDefYsize(gridID2, 1);
  gridDefXvals(gridID2, &xval);
  gridDefYvals(gridID2, &yval);

  if ( cdoVerbose )
    cdoPrint("Setup Grid for vlistID2");

  varID2[0] = vlistDefVar(vlistID2, gridID2, zaxisID2, TIME_VARIABLE);
  vlistDefVarName(vlistID2, varID2[0], "Reli");
  vlistDefVarLongname(vlistID2, varID2[0], "CRPS Reliability");
  vlistDefVarUnits(vlistID2, varID2[0], "1");
  
  varID2[1] = vlistDefVar(vlistID2, gridID2, zaxisID2, TIME_VARIABLE);
  vlistDefVarName(vlistID2, varID2[0], "CRPS_pot");
  vlistDefVarLongname(vlistID2, varID2[0], "Potential CRPS score");
  vlistDefVarUnits(vlistID2, varID2[0], "1");
  
  taxisID1 = vlistInqTaxis(vlistID1);
  taxisID2 = taxisDuplicate(taxisID1);
  vlistDefTaxis(vlistID2, taxisID2);
  
  streamID2 = streamOpenWrite(ofilename, cdoFiletype());
  streamDefVlist(streamID2, vlistID2);
  
  gridsize = vlistGridsizeMax(vlistID1);
  weights=(double*) malloc (gridsize*sizeof(double));

  gridID = vlistInqVarGrid(vlistID1, 0);
  xsize = gridInqXsize(gridID);
  ysize = gridInqYsize(gridID);

  /*  if ( xsize > 1 && ysize > 1 )  {
    gridWeights(gridID, weights);
    sum_weights=0;
    for ( i=0; i<gridsize; i++ )  
      sum_weights += weights[i];
  }
  else*/ {
    for ( i=0; i< gridsize; i++ )
      weights[i] = 1./gridsize;
    sum_weights=1.;
  }

  if ( cdoVerbose ) 
    cdoPrint(" sum_weights %10.6f\n",sum_weights);

  for ( fileID = 0; fileID < nfiles; fileID++ )
    ef[fileID].array = (double *) malloc(gridsize*sizeof(double));
  
  tsID = 0;
  do
    {
      nrecs0 = streamInqTimestep(ef[0].streamID, tsID);
      for ( fileID = 1; fileID < nfiles; fileID++ )
	{
	  streamID = ef[fileID].streamID;
	  nrecs = streamInqTimestep(streamID, tsID);
	  if ( nrecs != nrecs0 )
	    cdoAbort("Number of records changed from %d to %d at time Step", nrecs0, nrecs, tsID);
	}

      taxisCopyTimestep(taxisID2, taxisID1);
      if ( nrecs0 > 0 ) streamDefTimestep(streamID2, tsID);

      for ( recID = 0; recID < nrecs0; recID++ )
	{
	  for ( fileID = 0; fileID < nfiles; fileID++ )
	    {
	      streamID = ef[fileID].streamID;
	      streamInqRecord(streamID, &varID, &levelID);
	      streamReadRecord(streamID, ef[fileID].array, &nmiss);
	    }

	  gridID   = vlistInqVarGrid(vlistID1, varID);
	  gridsize = gridInqSize(gridID);
	  missval  = vlistInqVarMissval(vlistID1, varID);

	  nmiss = 0;
	  valcount = 0;
	  for ( i = 0; i < gridsize; i++ )
	    {
	      have_miss = 0;
	      for ( fileID = 0; fileID < nfiles; fileID++ )
		{
		  val[fileID] = ef[fileID].array[i];
		  if ( DBL_IS_EQUAL(val[fileID], missval) ) 
		    { have_miss = 1; break; }
		}
	      
	      if ( ! have_miss )  // only process if no missing value in ensemble
		{
		  double xa=val[0];                              /* 1st file contains reference  */
		  double *x = &val[1];                           /* Ensembles start at 2nd   file*/
		  sort_iter_single(nens,x,1);                    /* Sort The Ensemble Array      */

		  if ( xa < x[0] ) {                             /* Consider outliers            */
		    beta[0] += (x[0]-xa)*weights[i];
		    beta_weights[0] += weights[i];
		    heavyside0 += 1.;
		  }
		  if ( xa > x[nens-1] )  {
		    alpha[nens] += (xa-x[nens-1])*weights[i];
		    alpha_weights[nens] += weights[i];
		    heavysideN += 1.;
		  }

 		  /* Loop start at zero ==> 1st ensemble (c-indexing) */
		  for ( k=0; k<nens-1; k++ ) {                   /* Cumulate alpha and beta      */
		    if ( xa > x[k+1] )                           /* left of heavyside            */
		      alpha[k+1]+= (x[k+1]-x[k]) * weights[i];   /*                              */
		    else if ( xa < x[k] )                        /* right of heavyside           */
		      beta[k+1] += (x[k+1]-x[k]) * weights[i];   /*                              */
		    else if ( x[k+1] >= xa && xa >= x[k] ) {     /* hitting jump pf heavyside    */
		      alpha[k+1]+= (xa    -x[k]) * weights[i];   /* (occurs exactly once!)       */
		      beta[k+1] += (x[k+1]-xa)   * weights[i];   /* **************************** */
		    }
		  }
		}
	    }        // for ( i=0; i<gridsize; i++ )

	  // First Bin
	  p=0.; g=0.;
	  o = heavyside0/gridsize;
	  if ( o > 0. ) {
	    g = beta[0]/o;
	  }
	  reli    = g * (o - p) * (o - p);
	  crps_pot= g*o*(1.-o);
	  crps    = g*( (1.-o)*p*p  +  o*(1.-p)*(1.-p) );	  

	  // Middle Bins
	  for ( k=1; k<nens; k++ ) {
	    p = (double)k/(double)nens;
	    
	    if ( ! DBL_IS_EQUAL(sum_weights,1.) ) {
	      alpha[k] /= sum_weights; 
	      beta[k] /= sum_weights;
	    }
	    
	    g = alpha[k]+beta[k];
	    o = beta[k] / ( alpha[k] + beta[k] ); 

	    reli    += g * (o - p) * (o - p);
	    crps_pot+= g*o*(1.-o);
	    crps    += g*( (1.-o)*p*p  +  o*(1.-p)*(1.-p) );	  

	  }
	  
	  // Last Bin
	  p=1.; g=0.;
	  o = 1. - heavysideN/gridsize; 
	  if ( o != 1. ) {
	    g = alpha[nens] / (1-o);
	    reli    += g * (o-p) * (o-p);
	    crps_pot+= g*o*(1-o);
	    crps    += g*( (1-o)*p*p  +  o*(1-p)*(1-p) );	  
	  }

	  fprintf(stdout, "%12.6g %12.6g %12.6g %12.6g\n", reli,crps_pot, reli+crps_pot, crps);

	  streamDefRecord(streamID2,varID2[0],levelID);
	  streamWriteRecord(streamID2,&reli,have_miss);
	  streamDefRecord(streamID2,varID2[1],levelID);
	  streamWriteRecord(streamID2,&crps_pot,have_miss);
	  
	  memset(alpha, 0, (nens+1) * sizeof(double) );
	  memset(beta,  0, (nens+1) * sizeof(double) ); 
	  heavyside0=0;
	  heavysideN=0;

	}   // for ( recID = 0; recID < nrecs; recID++ ) 
      tsID++;
    }  while ( nrecs );
  
  for ( fileID = 0; fileID < nfiles; fileID++ )
    {
      streamID = ef[fileID].streamID;
      streamClose(streamID);
    }
  
  if ( operfunc != func_roc ) 
    streamClose(streamID2);
  
  for ( fileID = 0; fileID < nfiles; fileID++ )
    if ( ef[fileID].array ) free(ef[fileID].array);
  
  if ( ef ) free(ef);
  
  cdoFinish();
 
  return (0);
}


