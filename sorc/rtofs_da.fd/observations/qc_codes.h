c
c  Ocean Data Quality Control Codes
c  - used by 3D multivariate optimum interpolation system
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c     ..all QC codes are even integers greater than 1
c
c       flags indicate failure of an observation to a specific QC
c       test.  the flags are stored separate from the likelihood
c       an observation contains a random error determined in the
c       decision making algorithm.  
c
c       for profile observations the likelihood of random errors
c       are also computed at level "j".  separate arrays are 
c       maintained for profile temperature and salinity random
c       errors.  
c
c     ..definitions
c
c       flag values greater than 1000 are considered fatal, and
c       observations (or profile levels) with these flags should
c       be ignored
c
c       flag values greater than 100 but less than 1000 should be
c       considered suspect, and observations (or profile levels)
c       with these flags should be used with caution
c
c       flag values greater than 1 but less than 100 are for
c       informational purposes only.  observations with these
c       flags should be used based on the random error probability
c
c     ..profile observations
c
c       DUP_DEPTH - profile contains duplicate depth levels
c       DEPTH_ERR - profile depth is not strictly increasing
c       SAL_MISS - missing salinity reported value
c       TMP_MISS - missing temperature reported value
c       TMP_SAL_MISS - missing temperature and salinity reported values
c       TMP_OUTLIER - profile temperature not on planet earth
c       SAL_OUTLIER - profile salinity not on planet earth
c       RESPN - profile within climate mixed layer depth contain
c               spurious positive temperature gradients
c       SPIKE - profile contains anomalous temperature gradients
c               (either positive or negative)
c       STRETCH - profile contains a positive temperature gradient
c                 near the bottom that is not in the climatology
c                (climatology expected a negative temperature
c                 gradient or nearly neutral stratification).
c       INVRSN - profile contains a density inversion.  computed 
c                only for profiles that report both temperature
c                and salinity.  density inversions are corrected
c                for profiles with derived salinities, but are not
c                corrected for observed salinity profiles.
c       VERT_GRAD_MN - profile vertical gradient exceeds expected
c                      vertical gradient variability, and the 
c                      deviation is not considered significant
c       VERT_GRAD_MX - profile vertical gradient exceeds expected
c                      vertical gradient variability, and the
c                      deviation is considered significant
c
c       notes:
c         INVRSN flags can be added to already defined RESPN, SPIKE
c         or STRETCH flags (and their commbinations) resulting in
c         a composite flag that is not in the following parameter
c         statements.  
c
c     ..drifting buoy surface velocity (position error test)
c       
c       SPEED_ERROR - drifting buoy velocity out of range compared 
c                     to linear regression of adjacent along-track
c                     velocities
c
c     ..infrared satellite sst retrievals 
c
c       DIURNAL_SST - indicates a warming event is likely taking place  
c          LAND_SST - sst observation is over land
c
c     ..ssmi sea ice retrievals
c
c       LAND_DST - sea ice observation too close to the coast, which 
c                  likely indicates the MW BTs are land contaminated
c       LAND_ICE - sea ice observations over land
c         FW_ICE - sea ice observations in fresh water lakes.
c       WARM_SST - sea ice observations greater than 0% where the
c                  co-located sst is greater than 1 deg C
c
c     ..sss retrieval observations
c
c        ICE_SSS - ice covered seas
c         FW_SSS - observed value is in fresh water
c       SHLW_SSS - shallow water retrieval
c       LAND_SSS - retrieval over land
c
c       notes:
c         SSS flags can be added together to form a composite
c         flag that is not defined in the parameter statements
c
c.......................................................................
c
c DESCRIPTON:
c   what all observations should be
c
      integer, parameter :: GOOD = 0
c
c DESCRIPTON:
c   codes for preliminary profile checks
c
      integer, parameter :: TMP_MISS = 1002
      integer, parameter :: SAL_MISS = 1004
      integer, parameter :: TMP_SAL_MISS = 1006
      integer, parameter :: DUP_DEPTH = 1010
      integer, parameter :: DEPTH_ERR = 1020
      integer, parameter :: DEC_DEPTH = 1030
      integer, parameter :: TMP_OUTLIER = 1040
      integer, parameter :: SAL_OUTLIER = 1050
      integer, parameter :: TMP_SAL_OUTLIER = 1060
c
c DESCRIPTON:
c   codes for instrumentation profile checks
c
      integer, parameter :: RESPN = 110
      integer, parameter :: SPIKE = 130
      integer, parameter :: STRETCH = 150
      integer, parameter :: SPIKE_RESPN = 170
      integer, parameter :: STRETCH_RESPN = 190
      integer, parameter :: STRETCH_SPIKE = 210
      integer, parameter :: STRETCH_SPIKE_RESPN = 230
c
c DESCRIPTON:
c   code for temperature vertical gradient checks
c
      integer, parameter :: VERT_GRAD_MN = 26
      integer, parameter :: VERT_GRAD_MX = 36
c
c DESCRIPTON:
c   code for profile density inversion
c
      integer, parameter :: INVRSN = 10
c
c DESCRIPTON:
c   code for derived salinity
c
      integer, parameter :: DERIVED_SAL = 12
c
c DESCRIPTON:
c   code for surface obs out of range valid value check
c
      integer, parameter :: OUTLIER = 1280
c
c DESCRIPTION:
c   code for drifting buoy velocity errors
c
      integer, parameter :: SPEED_ERROR = 1420
c
c DESCRIPTON:
c   code for infrared satellite SST retrievals 
c
      integer, parameter :: DIURNAL_SST = 600
      integer, parameter :: LAND_SST = 690
c
c DESCRIPTON:
c   code for SSM/I sea ice retrievals
c
      integer, parameter :: WARM_SST = 410
      integer, parameter :: LAND_DST = 430
      integer, parameter :: LAND_ICE = 440
      integer, parameter :: FW_ICE   = 470
c
c DESCRIPTON:
c   codes for satellite SSS retrievals
c
      integer, parameter :: ICE_SSS  = 510
      integer, parameter :: SHLW_SSS = 512
      integer, parameter :: LAND_SSS = 514
      integer, parameter :: FW_SSS   = 516
c
