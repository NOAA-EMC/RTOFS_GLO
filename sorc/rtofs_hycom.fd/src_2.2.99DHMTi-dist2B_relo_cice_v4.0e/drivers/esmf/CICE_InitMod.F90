! #
! DISTRIBUTION STATEMENT B: Distribution authorized to U.S. Government
! agencies based upon the reasons of possible Premature Distribution
! and the possibility of containing Software Documentation as listed
! on Table 1 of DoD Instruction 5230.24, Distribution Statements on
! Technical Documents, of 23 August 2012. Other requests for this
! document shall be made to Dr. Ruth H. Preller, Superintendent,
! Oceanography Division, U.S. Naval Research Laboratory, DEPARTMENT
! OF THE NAVY, John C. Stennis Space Center, MS 39529-5004; (228)
! 688-4670 (voice); ruth.preller@nrlssc.navy.mil (e-mail).
! #
!=======================================================================
!
!BOP
!
! !MODULE: CICE_InitMod - performs CICE initialization
!
! !DESCRIPTION:
!
!  This module contains the CICE initialization routine that sets model
!  parameters and initializes the grid and CICE state variables.
!
! !REVISION HISTORY:
!  SVN:$Id: CICE_InitMod.F90 140 2008-07-25 20:15:53Z eclare $
!
!  authors Elizabeth C. Hunke, LANL
!          William H. Lipscomb, LANL
!          Philip W. Jones, LANL
!
! 2006: Converted to free form source (F90) by Elizabeth Hunke
!
! !INTERFACE:
!
      module CICE_InitMod
!
! !USES:
!
#ifdef USE_ESMF
      use esmf_mod
#endif
      use ice_age
      use ice_calendar
      use ice_communicate
!pgp
      use ice_coupling
      use ice_diagnostics
      use ice_domain
      use ice_dyn_evp
      use ice_exit
      use ice_fileunits
      use ice_flux
      use ice_forcing
      use ice_grid
      use ice_history
      use ice_restart
      use ice_init
      use ice_itd
      use ice_kinds_mod
      use ice_mechred
      use ice_meltpond
      use ice_ocean
      use ice_orbital
      use ice_shortwave
      use ice_therm_itd
      use ice_therm_vertical
      use ice_timers
      use ice_transport_driver
      use ice_transport_remap
      use ice_work
#ifdef popcice
      use drv_forcing, only: sst_sss
#endif

      implicit none
      private
      save
!pgp
      integer, save, public :: nts_day ! set in CICE_initialize

! !PUBLIC MEMBER FUNCTIONS:

      public :: CICE_Initialize, cice_init

!
!EOP
!
!=======================================================================

      contains

!=======================================================================
!BOP
!
! !ROUTINE: CICE_Initialize - initialize CICE model
!
! !DESCRIPTION:
!
!  Initialize the basic state, grid and all necessary parameters for
!  running the CICE model.  Return the initial state in routine
!  export state.
!  Note: This initialization driver is designed for standalone and
!        CCSM-coupled applications, with or without ESMF.  For other
!        applications (e.g., standalone CAM), this driver would be
!        replaced by a different driver that calls subroutine cice_init,
!        where most of the work is done.
!
! !REVISION HISTORY: same as module
!
! !INTERFACE:
!

      subroutine CICE_Initialize(CICE_Comp,  importState, exportState, &
                                 synchClock, errorCode)
!
! !USES:
!
!
! !INPUT/OUTPUT PARAMETERS:
!

   !--------------------------------------------------------------------
   ! Argument types depend on whether the model is using ESMF.
   !--------------------------------------------------------------------

#ifdef USE_ESMF

      type (ESMF_GridComp) :: &
           CICE_Comp            ! defined ESMF component for CICE

      type (ESMF_State) :: &
           importState, &       ! CICE import state
           exportState          ! CICE export state

      type (ESMF_Clock) :: &
           synchClock           ! ESMF clock to check init time

      integer, intent(out) :: &
           errorCode            ! returns an error code if any init fails

      integer rc

#else
! declare as integer dummy arguments

      integer (int_kind) , intent(inout), optional :: &
           CICE_Comp  , &       ! dummy argument
           importState, &       ! dummy argument
           exportState, &       ! dummy argument
           synchClock , &       ! dummy argument
           errorCode            ! dummy argument

#endif
!
!EOP
!BOC
!
   !--------------------------------------------------------------------
   !  initialize return flag
   !--------------------------------------------------------------------

#ifdef USE_ESMF
      call ESMF_LogWrite("CICE Initialize routine called", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)

      errorCode = ESMF_SUCCESS
#endif

   !--------------------------------------------------------------------
   ! model initialization
   !--------------------------------------------------------------------
!pgp
!      call cice_init
      call cice_init(CICE_Comp)

#ifdef USE_ESMF
      call ESMF_LogWrite("cice_init routine returned", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)

   !--------------------------------------------------------------------
   !  initialize and fill the export state with initial fields
   !--------------------------------------------------------------------

      call CICE_CoupledInit(importState, exportState, errorCode)

      if (errorCode /= ESMF_Success) then
         write(nu_diag,*) &
              '(ice) CICE_Initialize: error filling export state'
         return
      endif

      call ESMF_LogWrite("CICE Initialize routine returned", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)

#endif

!
!EOC
!
      end subroutine CICE_Initialize

!=======================================================================
!BOP
!
! !ROUTINE: cice_init - initialize CICE model
!
! !DESCRIPTION:
!
!  Initialize CICE model.
!
! !REVISION HISTORY: same as module
!
! !INTERFACE:
!
!pgp      subroutine cice_init
      subroutine cice_init(gridComp)
      type (ESMF_GridComp), intent(inout) :: gridComp

!
!EOP
!

      integer rc

      call init_communicate     ! initial setup for message passing
      call init_fileunits       ! unit numbers
      call input_data           ! namelist variables
      call init_work            ! work arrays

#ifdef USE_ESMF
      call ESMF_LogWrite("init_domain_blocks routine called", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)
#endif
      call init_domain_blocks   ! set up block decomposition
#ifdef USE_ESMF
      call ESMF_LogWrite("init_domain_blocks routine returned", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)
#endif
      call init_grid1           ! domain distribution
#ifdef USE_ESMF
      call ESMF_LogWrite("init_grid1 routine returned", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)
#endif
      call init_ice_timers      ! initialize all timers
      call ice_timer_start(timer_total)   ! start timing entire run
!pgp      call init_grid2           ! grid variables
#ifdef USE_ESMF
      call ESMF_LogWrite("init_grid2 routine called", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)
#endif
      call init_grid2(gridComp)     ! grid variables
#ifdef USE_ESMF
      call ESMF_LogWrite("init_grid2 routine returned", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)
#endif

      call init_transport       ! initialize horizontal transport
      call init_calendar        ! initialize some calendar stuff
!ajw
      call init_diags           ! initialize diagnostic output points
!pgp
      nts_day = nint(secday/dt) ! time step per day for ESMF coupling
      call init_hist (dt)       ! initialize output history file
      call init_evp (dt)        ! define evp dynamics parameters, variables
      call init_coupler_flux    ! initialize fluxes exchanged with coupler
#ifdef USE_ESMF
      call ESMF_LogWrite("init_coupler_flux routine returned", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)
#endif
#ifdef popcice
      call sst_sss              ! POP data for CICE initialization
#endif
      call init_thermo_vertical ! initialize vertical thermodynamics
      call init_itd             ! initialize ice thickness distribution
      call calendar(time)       ! determine the initial date

      call init_forcing_ocn(dt) ! initialize sss and sst from data
      call init_state           ! initialize the ice state

      if (runtype == 'continue') then ! start from core restart file
         call restartfile()           ! given by pointer in ice_in
         call calendar(time)          ! update time parameters
      else if (restart) then          ! ice_ic = core restart file
         call restartfile (ice_ic)    !  or 'default' or 'none'
      endif         

      ! tracers
      if (tr_iage) call init_age        ! ice age tracer
      if (tr_pond) call init_meltponds  ! melt ponds

      call init_history_therm   ! initialize thermo history variables
      call init_history_dyn     ! initialize dynamic history variables


      ! Initialize shortwave components using swdn from previous timestep 
      ! if restarting. These components will be scaled to current forcing 
      ! in prep_radiation.
      if (runtype == 'continue' .or. restart) &
         call init_shortwave    ! initialize radiative transfer

         istep  = istep  + 1    ! update time step counters
         istep1 = istep1 + 1
         time = time + dt       ! determine the time and date
         call calendar(time)    ! at the end of the first timestep

   !--------------------------------------------------------------------
   ! coupler communication or forcing data initialization
   !--------------------------------------------------------------------
!pgp
      call init_forcing_atmo    ! initialize atmospheric forcing (standalone)

#ifndef coupled
!      call get_forcing_atmo     ! atmospheric forcing from data
!      call get_forcing_ocn(dt)  ! ocean forcing from data
#endif

      if (runtype == 'initial' .and. .not. restart) &
         call init_shortwave    ! initialize radiative transfer using current swdn

      call init_flux_atm        ! initialize atmosphere fluxes sent to coupler
      call init_flux_ocn        ! initialize ocean fluxes sent to coupler
#ifdef USE_ESMF
      call ESMF_LogWrite("init_flux_ocn routine returned", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)
#endif

      call ice_write_hist(dt)   ! write initial conditions if write_ic = T

      end subroutine cice_init

!=======================================================================

      end module CICE_InitMod

!=======================================================================
