// void fromcfs(cfslevel<T> llt, cfsgrid<T> mask, T flagval, T nonval, 
//                cfsreg<T> x)

// Note that this is a very special include file.  It contains the
//   core of a routine that converts between grids.  The argument list
//   is
// What follows is identical for all input grid types and all output
//   grid types.  The isomorphism is possible due to making
//   locate(ijpt) and locate(latpt) mandatory members of all
//   metric grids, and the fact that the output is carried by an
//   argument.

// This is not as good as having the 'from' function, for which this
//   is the core, fully templated.  The templating is problematic
//   because templating is already being used for the grid2's
// Each metricgrid needs to specify its own from functions, which
//   will be overloaded in the first two arguments (data grid, mask grid).
// At a future date, these should be pure virtual functions of the
//   metricgrid class.

// Note that this works on the arguments, which saves creating arrays
//   locally.
// Robert Grumbine 12 May 1997
// Version to work with masked array inputs

// This is a legacy file -- the COFS model was retired from operations in ~2006.
// Last modification no later than 2004 Apr 29

  int xnx, xny, lltnx, lltny;
  ijpt outij;
  fijpt locfrom;
  latpt outll;
  int interp=1, ncyc=1;
  T dum;

  xnx = x.xpoints();
  xny = x.ypoints();
  lltnx = llt.xpoints();
  lltny = llt.ypoints();

// Loop over all points on the local grid
  for (outij.j = 0; outij.j < xny; outij.j++ ) {
     for (outij.i = 0; outij.i < xnx; outij.i++) {
       dum = nonval;

       outll = x.locate(outij);     //Find lat long in local grid
       locfrom = llt.locate(outll); //Convert to (floating point) i,j of the 
                                    // input data grid.

//Now go through and extract fields
       if (llt.in(locfrom)) {
         if ( ! mask.mask(flagval, locfrom) ) {
           locfrom.i += 1;
           locfrom.j += 1;  // add 1,1 because w3ft expects 1..N !
         #ifdef LINUX
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
         else { // Some point is masked, must invoke the special routine
           dum = llt.nonmask(mask, flagval, nonval, locfrom);
         }
       }
       else { // Point is not in the grid being worked from, give dummy
         dum = nonval;
       }

       // Regardless of whether the point was masked or not present,
       // assign value to cfs.  use of nonval will make this come out
       // sensibly.
       x[outij] = dum;
     }
  }
