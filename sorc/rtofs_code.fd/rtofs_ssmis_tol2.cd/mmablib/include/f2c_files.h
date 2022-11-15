#include <stdio.h>
/* standard interface to open files named fort.nn by c, from fortran call. */
/* Robert Grumbine 21 January 2016 */

/* n.b.: this structure, with a global 'fout' assumes that there will only be */
/* one file open at a time. */

#ifdef IBM
int openout(int *unit) ;
#elif LINUX
int openout_(int *unit) ;
#else
int openout_(int *unit) ;
#endif

FILE *fout;

#ifdef IBM
int openout(int *unit) {
#elif LINUX
int openout_(int *unit) {
#else
int openout_(int *unit) {
#endif
  char fname[80];
  sprintf(fname,"fort.%02d",*unit);
  fout = fopen(fname, "w");
  if (fout == (FILE *) NULL) {
    printf("Failed to open output unit %d\n",*unit);
    return 1;
  }
  return 0;
}

