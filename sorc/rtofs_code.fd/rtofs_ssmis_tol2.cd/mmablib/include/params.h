#ifndef PARAMETERS
#define PARAMETERS

#include <cmath>

// Robert Grumbine ~2000 -- present
// params.h begun 2 Nov 2001 as a class to hold static information like the
//    radius of the earth.
// 25 May 2004: Added non-spherical earth, math constants, and some 
//     constants of use in computing lat-long <-> polar stereographic 
//     grid remappings
// 25 Jan 2005: move to cmath from math.h
// 30 January 2006: Add defines to here from ncepgrids.h
//  4 Feb 2009: add constant and function definitions here from 
//                   19 Apr 2000 test file 'parameters.h'.

class parameters {
  public:
// Physical Parameters of the earth:
     static const double a;  //radius of an equivalent spherical earth, in m
     static const double eccen2;
     static const double rearth;

// Mapping function constants:
     static const double eccen;
     static const double m_per_degree;
     static const double km_per_degree;
     static const float nmtokm ;
     // Precomputed terms for polar stereographic grid computation efficiency
     static const double ps_chi1, ps_chi2, ps_chi3;
     static const double ps_ll;

// Mathematical parameters:
     static const double degrees_per_radian;
     static const double radians_per_degree;

// Following physical parameters and functions are taken from Gill, A. E., 
//    Atmosphere-Ocean Dynamics, Academic Press, 1982, 662 pgs.
// Physical parameters -- universal
     static const float universal_gas_constant ; //  J/mol/K 
     static const float stefan_boltzman        ; //  W/m^2/K^4
//  Earth Orbital parameters
     static const float year_days, tropical_year, sidereal_year, anomalistic_year; // days
// Physical parameters -- Dry and moist air
     static const float cp_air               ;
     static const float latent_fusion        ;
     static const float latent_vaporization  ;
     static const float latent_sublimation   ;
     static const float molecular_mass_air   ;
     static const float molecular_mass_water ;
     static const float molecular_ratio      ;
// Physical parameters -- the earth
     static const float g_bar                ;  //  m/s^2
     static const float omega                ;  // s^-1
     float g(float lat) { return 9.78032
                         + 0.005172*sin(lat)*sin(lat)
                         - 0.00006*sin(2.*lat)*sin(2.*lat) ; } //lat in radians
     float g(float lat, float height) {return g(lat)*pow((1+height/a), -2); }
// Physical parameters -- water
     float tfreeze(float S) { return -0.0575*S + 1.710523e-3*pow((double) S,1.5) -
                                     2.154996e-4*S*S; }
     float tfreeze(float S, float P) { return tfreeze(S) - 7.53e-3*P ; }



  parameters() {
  }

};

// Mathematical constants:
#define DEGREES_PER (180./M_PI)
const double parameters::degrees_per_radian = DEGREES_PER ;
const double parameters::radians_per_degree = M_PI / 180.;

// Basics of the size and shape of the earth
const double parameters::a = 6371.2e3;         //Value from IPLIB
#define SPHEREA 6371.2e3
#ifdef WGS84
  const double parameters::rearth  = 6378.137e3; 
  const double parameters::eccen2 = 0.00669438; 
  #define ECCEN2 0.00669438
#else
  const double parameters::rearth  = 6378.160e3; //earth definition is from
  const double parameters::eccen2 = 0.006694604; //GRIB standard
  #define ECCEN2 0.006694604
#endif

// For efficiency of polar stereographic conversions:
const double parameters::eccen  = sqrt(ECCEN2);
const double parameters::ps_chi1  = ( ECCEN2/2. + 5.*ECCEN2*ECCEN2/24. + ECCEN2*ECCEN2*ECCEN2/12.);
const double parameters::ps_chi2  = ( 7.*ECCEN2*ECCEN2/48. + 29.*ECCEN2*ECCEN2*ECCEN2/240.);
const double parameters::ps_chi3  = ( 7.*ECCEN2*ECCEN2*ECCEN2/120. );
const double parameters::ps_ll    = pow( pow(1.+eccen,1.+eccen) * pow(1.-eccen,1.-eccen) , 1./2.); 

// Misc. Utility variables, mapping related:
const double parameters::m_per_degree = SPHEREA / DEGREES_PER;
const double parameters::km_per_degree = SPHEREA / DEGREES_PER / 1000;
const float  parameters::nmtokm = 1.85318;

// Physical Parameters:
const float parameters::universal_gas_constant = 8.31436; //  J/mol/K 
const float parameters::stefan_boltzman        = 5.67051E-8; //  W/m^2/K^4
const float parameters::cp_air                 = 1004.6;
const float parameters::latent_fusion          = 3.32e5;
const float parameters::latent_vaporization    = 2.5e6;
const float parameters::latent_sublimation     = parameters::latent_fusion + parameters::latent_vaporization;
const float parameters::molecular_mass_air     = 28.966;
const float parameters::molecular_mass_water   = 18.016;
const float parameters::molecular_ratio        = 0.62197;
//      PARAMETER (EPSI   = 0.6219886)  !CRC #72
const float parameters::g_bar                  = 9.7976;  //  m/s^2
//      PARAMETER (GRAV   = 9.8062 )    !NMC Handbook
const float parameters::omega                  = 7.292e-5;  // s^-1
////////////////////////////////////////////
//  Earth Orbital parameters
const float parameters::year_days        = 365.259635;  // default to anomalistic, days
const float parameters::tropical_year    = 365.24219;
const float parameters::sidereal_year    = 365.256363;
const float parameters::anomalistic_year = 365.259635;

////////////////////////////////////////////
// Various defined values/flags/...  Moved here from ncepgrids 2006 Jan 30
#define LAND     157 
#define COAST    195 
#define BAD_DATA 166
#define WEATHER  177
#define NO_DATA  224
#define OCEAN      0
#define MIN_CONC  15
#define MAX_CONC 127
#define MAX_ICE  128

#endif
//C     Parameters known to high precision
//      PARAMETER (KAPPA  = 2. / 7.)
//      PARAMETER (TMELT  = 273.16)     !NMC Handbook
//      PARAMETER (F0     = 2.*OMEGA)
//
//C     Parameters constrained, but not high precision
//      PARAMETER (VONKAR = 0.4)

//      PARAMETER (RHOICE = 9.1E2)
//      PARAMETER (RHOSNO = 3.3E2)
//      PARAMETER (RHOWAT = 1.028E3)

//      PARAMETER (CON    = 2.1656)  ! Thermal Conductivity of ice
//      PARAMETER (CONSN  = 0.31)    ! Thermal Conductivity of snow
//      PARAMETER (TFREZ  = -1.84)   ! Tf for water at 34.5 psu, Gill
//      PARAMETER (CC     = 4.217E6) ! Specific heat of pure water, Gill

//      PARAMETER (RHOAIR = 1.29)
