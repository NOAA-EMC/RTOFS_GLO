#ifndef FROMALLH

#define FROMALLH
template <>
void metricgrid<float>::fromall(metricgrid<float> &llt, 
    metricgrid<float> &mask, float landval, float nonval) {

// Robert Grumbine 1998 -- present

// llt is the input data grid
// mask is a mask to the data grid
// landval is the value in the mask grid which flags that the point
//   should not be used.
// nonval is the value for the routine to fill in points for which 
//   no valid interpolation can be done
// llt is so named because the first version of this routine was
//  converting a lat-long temperature field between grids.

//Interpolation control:
// ncyc = 0 for noncyclic
//      = 1 for cyclic in i, noncyclic in j
//      = 2 for not cyclic in i, cyclic in j
//      = 3 for cyclic in i, cyclic in j
// interp = 1 for linear
//        = !=1 for quadratic

//Oldest on hand 17 June 1998, but must be much older than that.
//
//1998-9 -- add calling patterns to w3ft01 for different systems
//          add diagnostic messages, some under #VERBOSE control
// Added 12 April 2000:
//  locmask masks out all points which are masked in _either_ the mask file,
//  or which have nonvalues in the input data field
//25 May 2004: add checking for points that are within 0.5 of the domain
//20 Apr 2005: Add a version to run without mask files 

  int lltnx, lltny;
  ijpt outij;
  fijpt locfrom;
  latpt outll;
  int interp=1, ncyc=0, xlim = this->xpoints(), ylim = this->ypoints();
  float dum;
  grid2<float> locmask(llt.xpoints(), llt.ypoints() );

  #ifdef VERBOSE
     printf("Entered fromall, flag = %f nonval = %f\n",landval, nonval);
     fflush(stdout);
  #endif
  if (llt.iscyclicx()) {
    #ifdef VERBOSE
      cout << "is cyclic in x\n";
      cout.flush();
    #endif
    ncyc += 1;
  }
  if (llt.iscyclicy()) { ncyc += 2; }

  lltnx = llt.xpoints();
  lltny = llt.ypoints();
  
// Construct the total mask file:
  for (outij.j = 0; outij.j < lltny; outij.j++) {
  for (outij.i = 0; outij.i < lltnx; outij.i++) {
     if (mask[outij] == landval || llt[outij] == nonval) {
       locmask[outij] = landval;
     }
     else {
       locmask[outij] = landval + 10.0;
     }
  }
  }
  #ifdef VERBOSE
  printf("constructed the mask file\n"); fflush(stdout);
  #endif

// Loop over all points on the local (destination) grid
  for (outij.j = 0; outij.j < ylim; outij.j++ ) {
     for (outij.i = 0; outij.i < xlim;  outij.i++) {
       #ifdef FROMTEST
         printf("outij = %d %d\n",outij.i, outij.j); fflush(stdout);
       #endif
       dum = nonval;

       outll = this->locate(outij); //Find lat long in local grid
       locfrom = llt.locate(outll); //Convert to (floating point) i,j of the 
                                    // input data grid.
       #ifdef FROMTEST
         printf("ptfrom = %f %f in? %d\n",locfrom.i, locfrom.j, llt.in(locfrom) ); fflush(stdout);
       #endif
                                    
       // deal with edge round off -- cyclic will handle things > 0.5 if it's cyclic
         if (locfrom.i > -0.5 && locfrom.i < 0) locfrom.i = 0;
         if (locfrom.j > -0.5 && locfrom.j < 0) locfrom.j = 0;
         if (locfrom.i < (llt.xpoints() - 0.5) && locfrom.i > llt.xpoints()-1 ) locfrom.i = llt.xpoints() - 1;
         if (locfrom.j < (llt.ypoints() - 0.5) && locfrom.j > llt.ypoints()-1 ) locfrom.j = llt.ypoints() - 1;

       // In cyclic grids, some extra remediation is possible for otherwise
       //   off grid points
       if (!llt.in(locfrom) ) {
         #ifdef VERBOSE
         printf("trying to deal with a point not in grid\n"); 
         #endif

         if (locfrom.i < -0.5 && ((ncyc%2) == 1) ) { 
             locfrom.i += llt.xpoints();
         }
         if (locfrom.i > llt.xpoints()-.5  && ((ncyc%2) == 1) ) { 
             locfrom.i -= llt.xpoints();
         }
  
         if (locfrom.j < -0.5 && ncyc >= 2) {
           locfrom.j += llt.ypoints();
         }
         if (locfrom.j > llt.ypoints()-.5  && ncyc >= 2) {
             locfrom.j -= llt.ypoints();
         }
       }

//Now go through and extract fields
       if (llt.in(locfrom)) {
           #ifdef VERBOSE
           if (locfrom.j < 0 ) {  
             printf("Should not be here 1 %f %f\n",locfrom.i, locfrom.j ); 
             locfrom.j = 0; 
           }
           if (locfrom.j > llt.ypoints() - 1 ) { 
             printf("Should not be here 2 %f %f\n",locfrom.i, locfrom.j ); 
             locfrom.j = llt.ypoints() - 1;
           }
           if (locfrom.i < 0) {
             printf("Should not be here 3 %f %f\n",locfrom.i, locfrom.j ); 
             locfrom.i = 0;
           }
           if (locfrom.i > llt.xpoints() - 1) {
             printf("Should not be here 4 %f %f\n",locfrom.i, locfrom.j ); 
             locfrom.i = llt.xpoints() - 1;
           }
           fflush(stdout);
           #endif

         #ifdef VOLDVERSION
                // the very old version used w3ft01, newer does not
         if ( ! locmask.mask(landval, locfrom) ) {
              printf("hello from voldversion\n");
         #else
         if ( false ) {
              printf("hello from false\n");
         #endif

           locfrom.i += 1;
           locfrom.j += 1;  // add 1,1 because w3ft expects 1..N !
           printf("w3ft01 locs %f %f %f\n",locfrom.i, locfrom.j, dum);
         #ifdef LINUX
           w3ft01_(&locfrom.i, &locfrom.j, llt.grid, &dum, &lltnx, &lltny, 
                                                     &ncyc, &interp);
         #elif MACOSX
           w3ft01_(&locfrom.i, &locfrom.j, llt.grid, &dum, &lltnx, &lltny, 
                                                     &ncyc, &interp);
         #elif SGI
           w3ft01_(&locfrom.i, &locfrom.j, llt.grid, &dum, &lltnx, &lltny, 
                                                     &ncyc, &interp);
         #elif IBM
           w3ft01(&locfrom.i, &locfrom.j, llt.grid, &dum, &lltnx, &lltny, 
                                                     &ncyc, &interp);
         #else
           W3FT01(&locfrom.i, &locfrom.j, llt.grid, &dum, &lltnx, &lltny, 
                                                     &ncyc, &interp);
         #endif
         }
         else { 
           #ifdef VERBOSE
             printf("masked pt %d %d  %f %f of %d %d  %f\n",outij.i, outij.j, locfrom.i, locfrom.j, locmask.xpoints(), locmask.ypoints(), landval);
           #endif
           dum = llt.nonmask(locmask, landval, nonval, locfrom);
         }
       }
       else { // Point is not in the grid being worked from, give dummy
        dum = nonval;
       }

       // Regardless of whether the point was masked or not present,
       // assign value to cfs.  use of nonval will make this come out
       // sensibly.
       if (dum == nonval) { }
       this->grid[outij.i + outij.j * xlim ] = dum;
   }
 }

}

//////////// Version to run without mask file 
template <>
void metricgrid<float>::fromall(metricgrid<float> &llt, 
    float landval, float nonval) {

// llt is the input data grid
// landval is the value in the mask grid which flags that the point
//   should not be used.
// nonval is the value for the routine to fill in points for which 
//   no valid interpolation can be done
// llt is so named because the first version of this routine was
//  converting a lat-long temperature field between grids.

//Interpolation control:
// ncyc = 0 for noncyclic
//      = 1 for cyclic in i, noncyclic in j
//      = 2 for not cyclic in i, cyclic in j
//      = 3 for cyclic in i, cyclic in j
// interp = 1 for linear
//        = !=1 for quadratic

// Added 12 April 2000:
//  locmask masks out all points which are masked in _either_ the mask file,
//  or which have nonvalues in the input data field

  int lltnx, lltny;
  ijpt outij;
  fijpt locfrom;
  latpt outll;
  int interp=1, ncyc=0, xlim = xpoints(), ylim = ypoints();
  float dum;

  #ifdef VERBOSE
     printf("Entered fromall, flag = %f nonval = %f\n",landval, nonval);
     fflush(stdout);
  #endif
  if (llt.iscyclicx()) {
    #ifdef VERBOSE
      cout << "is cyclic in x\n";
      cout.flush();
    #endif
    ncyc += 1;
  }
  if (llt.iscyclicy()) { 
                       ncyc += 2; }

  lltnx = llt.xpoints();
  lltny = llt.ypoints();
  
// Loop over all points on the local (destination) grid
  for (outij.j = 0; outij.j < ylim; outij.j++ ) {
     for (outij.i = 0; outij.i < xlim;  outij.i++) {
       #ifdef FROMTEST
         printf("outij = %d %d\n",outij.i, outij.j); fflush(stdout);
       #endif
       dum = nonval;

       outll = this->locate(outij); //Find lat long in local grid
       locfrom = llt.locate(outll); //Convert to (floating point) i,j of the 
                                    // input data grid.
       // In cyclic grids, some extra remediation is possible for otherwise
       //   off grid points
       if (!llt.in(locfrom) ) {
         if (locfrom.i < -0.5 && (ncyc%2 == 1) ) { 
             locfrom.i += llt.xpoints();
         }
         if (locfrom.i > llt.xpoints()-.5  && (ncyc%2 == 1) ) { 
             #ifdef VERBOSE
               printf("fromall overloaded locfrom.i %f\n",locfrom.i);
             #endif
             locfrom.i -= llt.xpoints();
         }
  
         if (locfrom.j < -0.5 && ncyc >= 2) {
           locfrom.j += llt.ypoints();
         }
         if (locfrom.j > llt.ypoints()-.5  && ncyc >= 2) {
             locfrom.j -= llt.ypoints();
         }
       }
       // Clean up after the i's that are 'in' but are not in correct range:
         if (locfrom.i < -0.5 && (ncyc%2 == 1) ) {
             locfrom.i += llt.xpoints();
         }
         if (locfrom.i > llt.xpoints()-.5  && (ncyc%2 == 1) ) {
             #ifdef VERBOSE
               printf("Working on overloaded locfrom.i %f\n",locfrom.i);
             #endif
             locfrom.i -= llt.xpoints();
         }

//Now go through and extract fields
       if (llt.in(locfrom)) {
           if (locfrom.j < 0 ) {  
           #ifdef VERBOSE
             printf("Should not be here 1 %f %f\n",locfrom.i, locfrom.j ); 
           #endif
             locfrom.j = 0; 
           }
           // Note, need the -2 because w3ft expects to add 1
           #define LIM 1
           if (locfrom.j > llt.ypoints() - LIM ) { 
           #ifdef VERBOSE
             printf("Should not be here 2 %f %f\n",locfrom.i, locfrom.j ); 
           #endif
             locfrom.j = llt.ypoints() - LIM;
           }
           if (locfrom.i < 0) {
             locfrom.i = 0;
           }
           if (locfrom.i > llt.xpoints() - 1) {
           #ifdef VERBOSE
             printf("Should not be here 3 %f %f\n",locfrom.i, locfrom.j ); 
           #endif
             locfrom.i = llt.xpoints() - 1;
           }


           locfrom.i += 1;
           locfrom.j += 1;  // add 1,1 because w3ft expects 1..N !
         #ifdef LINUX
           w3ft01_(&locfrom.i, &locfrom.j, llt.grid, &dum, &lltnx, &lltny, 
                                                     &ncyc, &interp);
         #elif MACOSX
           w3ft01_(&locfrom.i, &locfrom.j, llt.grid, &dum, &lltnx, &lltny, 
                                                     &ncyc, &interp);
         #elif SGI
           w3ft01_(&locfrom.i, &locfrom.j, llt.grid, &dum, &lltnx, &lltny, 
                                                     &ncyc, &interp);
         #elif IBM
           w3ft01(&locfrom.i, &locfrom.j, llt.grid, &dum, &lltnx, &lltny, 
                                                     &ncyc, &interp);
         #else
           W3FT01(&locfrom.i, &locfrom.j, llt.grid, &dum, &lltnx, &lltny, 
                                                     &ncyc, &interp);
         #endif

       }
       else { // Point is not in the grid being worked from, give dummy
        dum = nonval;
       }

       // Regardless of whether the point was masked or not present,
       // assign value to cfs.  use of nonval will make this come out
       // sensibly.
       if (dum == nonval) {
       }
       this->grid[outij.i + outij.j * xlim ] = dum;
   }
 }

}
#endif
