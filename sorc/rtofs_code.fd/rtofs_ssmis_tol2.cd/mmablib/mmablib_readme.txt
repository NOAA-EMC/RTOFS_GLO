MMABLIB overview:
Robert Grumbine

The roots of this library extend to the mid-1990s, at which time fortran 90 was not yet available on NWS computers.  Consequently, elements were developed in C and C++ to take advantage of structures and classes.  C largely migrated to C++.  In the C++ aspect, the design choice was made to template essentially all classes and components.  The positive to this is that classes may operate on many different types of data (minimally, at least integer, floating point, different precisions thereof; and on classes, particularly locations and buoys).  The drawback is that for the C++ classes, there is not a library to link against – the new programs must compile against the includes themselves.

The purpose was and is to have a general library of tools, with limited re-invention of wheels.  A few minor redundancies vs. other libraries are still present – some elementary statistics, earth geometry, and the like, are present here.  In each case, these are algorithmically trivial, and introduced for portability to other systems.  Development of the elements in the classes and sources has been driven by operational need.  This means that many items which are logically obvious extensions don't (yet) exist in the library.

The structure is:
sorc – source code (Fortran and C) which is compiled to libombf and libombc
include – include files, mostly C++ classes but also some files for Fortran definitions of grids

The C++ classes of about 2000 are discussed in detail in http://polar.ncep.noaa.gov/mmab/papers/tn185/ and http://polar.ncep.noaa.gov/mmab/papers/tn186/
(one being the classes, the other being why one might want to use object-oriented programming).


Source:
Statistics:
sumx – sum a vector
sumxy – sum the product of two vectors
sumx2 – sum the square of a vector
fit – 
correl – compute correlation, ...

iagree – compute the index of agreement between two vectors
ciagree –  compute the index of agreement between two complex-valued vectors
VCC – compute the Vector correlation (Crosby, Breaker, Gemmill, 199x) between two sets of wind vectors

ssanaly2 – given two vectors of drift distance and direction, return correlation of distances, index of agreement, and vector correlation

For gaussian grids:
gaulat
bsslz1

Mapping on polar stereographic grids:
mapxy.c, mapxy.f – 
mapll.c – 

Distance on a sphere:
arcdis – distances
darcdis – double precision computation


Utility:
vectorize – convert magnitude+direction to u,v components
tfreez – freezing point of sea water as function of salinity
wdir – compute wind direction in meteorological convention given winds in geographic orientation
tvap – saturation vapor pressure for air/ice/water (Max Planck, Achim Stoessel, 1992)
w3ft01 – bilinear or bicubic interpolation on a grid

Legacy:
cfsread
gribit

control:
gridtypecheck.sh – run through all grids and print out their corner points
makefile – build and execute each test programming

sources:
The intent is for each source to exercise every member function for the class it names, in such a way that the output can be read for successful execution.  e.g., the results have 'should be X actually is ' Y statements, where Y is the value computed.


Include

Fortran grid definitions
icegrid.global – global 30' lat-long grid
icegrid.global5min – global 5' lat-long grid
icegrid.north – northern hemisphere 25.4 km polar stereographic sea ice grid 
icegrid.north12 – northern hemisphere 12.7 km polar stereographic sea ice grid 
icegrid.south – southern hemisphere 25.4 km polar stereographic sea ice grid 
icegrid.south12 – southern hemisphere 12.7 km polar stereographic sea ice grid 

Fortran definitions for grib (may be supercedable by more modern tools)
clib.inc
locale.inc

Class Library:
Level 00 – include only standard libraries, declare no classes

Definitions, constants, …
params.h – universal constants
icegrids.h – northern + southern hemisphere operational sea ice grids (deprecated)

Satellite instruments:
amsr.h – 
ssmi.h – 
ssmisu.h – 
avhrr.h – 

Interpolation between grids (code)
fromall.h – 
fromcfs.h –
from.h – 

External:
gshhs.h – for SOEST/GMT 



Level 0 – include nothing (nonstandard), declare a class

grib.h – relict grib1 class
mvector.h – mathematical vectors
points.h – points (numeric triplets, used for indexing grids)


Level 1 – include only Level 0, 00
(terminal – included by no other includes)
time_series.h ← mvector.h – some elementary time series computations
datavectors.h ← mvector.h – some vectors of data, e.g., NODC standard depths
genes.h ← points.h, mvector.h – basics for genetic algorithms, mostly not class based.
buoy.h ← points.h, params.h – elements for working with 'buoy' data, though 'buoy' is strictly shorthand; includes buoys, cman, ships, …

ssmisclass 	← class for ssmi-s processing
ssmiclass	← class for ssmi processing


(nonterminal)
color.h 	← points.h, mvector.h 	– palette and color classes
grid_base.h 	← points.h, mvector.h 	– a basic 2d grid class.  2 dimensional grid of 'things'.  Includes non-mathematical operations such as I/O, copying, subsetting, grid flipping


Level 2
grid_math.h 	← grid_base.h	– class for doing mathematics on grids of data 


Level 3
grid3.h	← grid_math.h	– 3d grids of data.
Amsrice.h	← grid_math.h	– amsr ice analysis
icessmis.h	← grid_math.h, ssmisclass.h	– ssmis ice analysis
icessmi.h	← grid_math.h, ssmiclass.h	– ssmi ice analysis





High Level:
ncepgrid.h	Definitions built on metric.h for specific NCEP grids
walcc.h
legacy.h 	legacy grids
cofs.h	Legacy


metric.h	Virtual Class for grids which have a mapping between I,j coordinates and latitude-longitude


lambert.h	Lambert Conformal Grids
eta.h
nmmb.h

gaussian.h	gaussian grids

llgrid.h	regularly spaced latitude-longitude grids

mercator.h	mercator projection
psgrid.h	polar stereographic grids

resops.h	grids specified by latitude-longitude at each point.  'REgular Set of PointS'




