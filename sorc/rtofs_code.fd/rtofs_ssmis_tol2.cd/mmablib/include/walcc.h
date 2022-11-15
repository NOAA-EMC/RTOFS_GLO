// Robert Grumbine 2012-present
// Grids for the Western Alaska LCC project domain

#ifndef METRICH
  #include "metric.h"
#endif

#ifndef WALCCH
  #define WALCCH
template <class T>
class walcc : public llgrid<T> {
  public:
    walcc( float=2.);
};
template <class T>
walcc<T>::walcc(float degreefrac) {
  this->dlat =  1./degreefrac;
  this->dlon =  1./degreefrac;

  this->firstlat =  40.0 - this->dlat / 2.;
  this->firstlon = 150.0 - this->dlon / 2.;

  this->nx = lrintl( (210.-125.)*degreefrac + 1.); // note that this is using W lon
  this->ny = lrintl( ( 80.- 40.)*degreefrac + 1.);

  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new an walcc\n"; }

}
#endif
