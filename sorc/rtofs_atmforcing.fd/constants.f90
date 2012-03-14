!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                       !!
!!                   GNU General Public License                          !!
!!                                                                       !!
!! This file is part of the Flexible Modeling System (FMS).              !!
!!                                                                       !!
!! FMS is free software; you can redistribute it and/or modify           !!
!! it and are expected to follow the terms of the GNU General Public     !!
!! License as published by the Free Software Foundation.                 !!
!!                                                                       !!
!! FMS is distributed in the hope that it will be useful,                !!
!! but WITHOUT ANY WARRANTY; without even the implied warranty of        !!
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         !!
!! GNU General Public License for more details.                          !!
!!                                                                       !!
!! You should have received a copy of the GNU General Public License     !!
!! along with FMS; if not, write to:                                     !!
!!          Free Software Foundation, Inc.                               !!
!!          59 Temple Place, Suite 330                                   !!
!!          Boston, MA  02111-1307  USA                                  !!
!! or see:                                                               !!
!!          http://www.gnu.org/licenses/gpl.txt                          !!
!!                                                                       !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE constants_mod

!-----------------------------------------------------
!
!  Defines useful constants for Earth in mks units.
!
!-----------------------------------------------------

!use fms_mod, only: write_version_number

IMPLICIT NONE
PRIVATE

CHARACTER(len=128) :: version='$Id: constants.f90,v 1.1 2002/03/27 22:43:06 alangenh Exp $'
CHARACTER(len=128) :: tagname='$Name: galway $'
LOGICAL :: do_log = .TRUE.
LOGICAL :: module_is_initialized = .FALSE.
!-----------------------------------------------------------------------
!------------ physical constants ---------------

REAL, PUBLIC, PARAMETER :: RADIUS = 6376.e3      !  radius of the earth (meters)
REAL, PUBLIC, PARAMETER :: OMEGA  = 7.292e-5     !  rotation rate of planet (1/sec)
REAL, PUBLIC, PARAMETER :: GRAV   = 9.80         !  acceleration due to gravity (m/s2)
REAL, PUBLIC, PARAMETER :: RDGAS  = 287.04       !  gas constant for dry air (J/Kg/deg)
REAL, PUBLIC, PARAMETER :: KAPPA  = 2./7.        !  RDGAS / CP
REAL, PUBLIC, PARAMETER :: cp     = RDGAS/KAPPA  !  spec heat cap of dry air (J/kg/deg)

!------------ water vapor constants ---------------

REAL, PUBLIC, PARAMETER :: RVGAS = 461.50        !  gas constant for water vapor (J/Kg/deg)
REAL, PUBLIC, PARAMETER :: DENS_H2O = 1000.      !  density of liquid water (Kg/m3)
REAL, PUBLIC, PARAMETER :: HLV = 2.500e6         !  latent heat of evaporation (J/Kg)
REAL, PUBLIC, PARAMETER :: HLF = 3.34e5          !  latent heat of fusion (J/Kg)
REAL, PUBLIC, PARAMETER :: HLS = 2.834e6         !  latent heat of sublimation (J/Kg)
REAL, PUBLIC, PARAMETER :: TFREEZE = 273.16      !  temp where fresh water freezes (deg K)

!------------ miscellaneous constants ---------------

REAL, PUBLIC, PARAMETER :: STEFAN  =  5.6734e-8  !  Stefan-Boltzmann constant (W/m2/deg4)
REAL, PUBLIC, PARAMETER :: VONKARM =  0.40       !  Von Karman constant
REAL, PUBLIC, PARAMETER :: PI      =  3.14159265358979323846 ! is it enough?
!-----------------------------------------------------------------------

PUBLIC constants_init

CONTAINS

! optional initialization routine
! only purpose is to write version to log file

SUBROUTINE constants_init

  IF (module_is_initialized) RETURN
  module_is_initialized = .TRUE.

  IF (.NOT.do_log) RETURN
!  call write_version_number (version,tagname)
  do_log = .FALSE.

END SUBROUTINE constants_init

!-----------------------------------------------------------------------

END MODULE constants_mod

