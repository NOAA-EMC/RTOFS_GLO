#ifndef _TABLEPAR_H
#define _TABLEPAR_H

typedef struct
{
  int   id;	     /* Parameter number (GRIB) */
  char *name;	     /* Parameter name */
  char *longname;    /* Parameter long name */
  char *units;	     /* Parameter units */
}
PAR;


void tableLink(int tableID, PAR *pars, int npars);
int tableDef(int modelID, int tablegribID, const char *tablename);

#endif
