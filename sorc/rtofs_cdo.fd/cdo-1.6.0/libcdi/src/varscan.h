#ifndef _VARSCAN_H
#define _VARSCAN_H

#ifndef _GRID_H
#  include "grid.h"
#endif


void varAddRecord(int recID, int param, int gridID, int zaxistype, int lbounds,
		  int level1, int level2, int prec,
		  int *pvarID, int *plevelID, int tsteptype, int numavg, int ltype,
		  const char *name, const char *longname, const char *units);

void varDefVCT(size_t vctsize, double *vctptr);

int  varDefGrid(int vlistID, grid_t grid, int mode);
int  varDefZaxis(int vlistID, int zaxistype, int nlevels, double *levels, int lbounds,
		 double *levels1, double *levels2, int vctsize, double *vct, char *name,
		 char *longname, char *units, int prec, int mode, int ltype);

void varDefMissval(int varID, double missval);
void varDefZtype(int varID, int ztype);
void varDefZlevel(int varID, int zlevel);
void varDefInst(int varID, int instID);
int  varInqInst(int varID);
void varDefModel(int varID, int modelID);
int  varInqModel(int varID);
void varDefTable(int varID, int tableID);
int  varInqTable(int varID);

int  zaxisCompare(int zaxisID, int zaxistype, int nlevels, int lbounds, double *levels, char *longname, char *units, int ltype);

#endif