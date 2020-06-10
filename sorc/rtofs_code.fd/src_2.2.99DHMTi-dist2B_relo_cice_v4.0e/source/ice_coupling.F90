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
! CVS: $Id: ice_coupling.F,v 1.11 2004/02/10 23:17:12 lipscomb Exp $
! CVS: $Source: /home/climate/CVS-COSIM/cice/source/ice_coupling.F,v $
! CVS: $Name:  $
!=======================================================================
!
!BOP
!
! !MODULE: ice_coupling - message passing to and from the coupler
!
! !DESCRIPTION:
!
! Message passing to and from the coupler
!
! !REVISION HISTORY:
!
! author: Elizabeth C. Hunke, LANL
!         Tony Craig, NCAR, Dec-30-2002, modified for cpl6
!         Philip W Jones, LANL, modified for ESMF import/export
!
! 2005: Added ESMF import/export state routines
! 2004: Block structure added by William Lipscomb
!lipscomb - Block changes not tested yet
!
! HYCOM-CICE only version.  Alan Wallcraft NRL May 2006.
! Uses CF conventions for import/export fields
!   CF requires fluxes to be cell averages (CCSM has fluxes only "where_ice")
!   Must use (CF) standard_name and (MKS) units, 
!   but units can be modified by scale_factor and/or add_offset.
! 
!
! !INTERFACE:
!
 module ice_coupling
!
! !USES:
!
   use esmf_mod

   use ice_communicate, only: my_task, master_task
   use ice_kinds_mod
   use ice_blocks
   use ice_domain_size
   use ice_domain, only: nblocks, distrb_info
   use ice_diagnostics
   use ice_constants
   use ice_calendar
   use ice_grid
   use ice_state
   use ice_flux
   use ice_shortwave
   use ice_timers
   use ice_fileunits

   implicit none
   public  !make almost everthing public
   save

! !PUBLIC MEMBER FUNCTIONS:

   public :: CICE_CoupledInit,          &
             CICE_CoupledExtractImport, &
             CICE_CoupledFillExport,    &
             CICE_CoupledAccumulateExport


! !PUBLIC DATA MEMBERS:

   logical(kind=log_kind), public :: &
      l_coupled         ! true if coupled, false if standalone

!   character (len=char_len), public :: &
!      runtype           ! initial, continue, or branch

!
!EOP
!BOC
!

!
! --- Data types for Import/Export array pointers
      type ArrayPtrReal2D
        real(ESMF_KIND_R4), dimension(:,:), pointer :: p
      end type ArrayPtrReal2D
!       
! --- Attribute names for fields
      character(ESMF_MAXSTR), save :: &
          attNameLongName = "long_name", &
          attNameStdName  = "standard_name", &
          attNameUnits    = "units", &
          attNameSclFac   = "scale_factor", &
          attNameAddOff   = "add_offset"

!    
! --- Import Fields
      integer, parameter :: numImpFields=7
      character(ESMF_MAXSTR), save :: impFieldName(    numImpFields), &
                                      impFieldLongName(numImpFields), &
                                      impFieldStdName( numImpFields), &
                                      impFieldUnits(   numImpFields)
      real(ESMF_KIND_R4),     save :: impFieldSclFac(  numImpFields), &
                                      impFieldAddOff(  numImpFields)
!    
! --- Export Fields
      integer, parameter :: numExpFields=11
      character(ESMF_MAXSTR), save :: expFieldName(    numExpFields), &
                                      expFieldLongName(numExpFields), &
                                      expFieldStdName( numExpFields), &
                                      expFieldUnits(   numExpFields)
      real(ESMF_KIND_R4),     save :: expFieldSclFac(  numExpFields), &
                                      expFieldAddOff(  numExpFields)
!    
! --- ESMF related variables
      type(ESMF_FieldBundle), save :: expBundle, &
                                      impBundle
      type(ESMF_Field),       save :: expField(numExpFields), &
                                      impField(numImpFields)
      type(ArrayPtrReal2D),   save :: expData( numExpFields), &
                                      impData( numImpFields)
      type(ESMF_ArraySpec),   save :: arraySpec2Dr

   real (kind=dbl_kind) :: &
      timeLastCoupled        ! accumulated time since last coupling

!  real (kind=dbl_kind), &
!     dimension(nx_block, ny_block, max_blocks, numExpFields) :: &
!     exportBufferSum     ! buffer for time avg of export fields

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks) :: &
             sst_import, & !Sea Surface Temperature
             sss_import, & !Sea Surface Salinity
            uocn_import, & !Sea Surface X-Current
            vocn_import, & !Sea Surface Y-Current
             ssh_import, & !Sea Surface Height
            ssfi_import, & !Oceanic Heat Flux Available to Sea Ice
             mlt_import    !Ocean Mixed Layer Thickness

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks) :: &
            uiceT, &       !Sea Ice X-Velocity, T-grid
            viceT          !Sea Ice Y-Velocity, T-grid
!
!EOC
!
!=======================================================================

 contains

!=======================================================================
!BOP
!
! !IROUTINE: CICE_CoupledInit - initializes states and ESMF coupling
!
! !INTERFACE:
!
 subroutine CICE_CoupledInit(importState, exportState, errorCode)
!
! !DESCRIPTION:
!
!  This routine sets up everything necessary for coupling with
!  other ESMF components, including setting up the import and
!  export states for the run method and initializing the data
!  in the export state for use in other models.
!
! !REVISION HISTORY:
!
!  author: Philip W Jones, LANL
!
! HYCOM-CICE only version.  Alan Wallcraft NRL May 2006.
! Uses CF conventions for import/export fields
!   CF requires fluxes to be cell averages (CCSM has fluxes only "where_ice")
!   Must use (CF) standard_name and (MKS) units, 
!   but units can be modified by scale_factor and/or add_offset.
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:

   type (ESMF_State), intent(inout) :: &
      importState,        &! import state for CICE run method
      exportState          ! export state for CICE run method

! !OUTPUT PARAMETERS:

   integer, intent(out) :: &
      errorCode        ! error code: ESMF_Success or ESMF_Failure

!
!EOP
!BOC
!
   !--------------------------------------------------------------------
   !  local variables
   !--------------------------------------------------------------------

   integer (kind=int_kind) :: &
      i,j,k,             &! dummy loop index
      iblk,              &! dummy block index
      istat               ! error flag for allocates

   !integer (kind=int_kind), dimension(nibuff) :: &
   !   ibuff               ! integer control buffer

    integer rc,rc2

   !--------------------------------------------------------------------
   !  initialize some scalars and perform some checks
   !--------------------------------------------------------------------

   errorCode         = ESMF_SUCCESS
   timeLastCoupled   = c0

   if (max_blocks > 1) then
      write(nu_diag,*) &
         '(ice) CICE_CoupledInit: ESMF only supports 1 block/proc'
      return
   endif

!                                             
! --- Report                                  
      call ESMF_LogWrite("CICE CoupledInit routine called", &
                         ESMF_LOG_INFO, rc=rc)                             
      call ESMF_LogFlush(rc=rc)

!
!  Attributes for import fields, identical to HYCOM export fields
      impFieldAddOff(:) = 0.0 !default is no offset
      impFieldSclFac(:) = 1.0 !default is no scale factor

      impFieldName(     1) = "sst"
      impFieldLongName( 1) = "Sea Surface Temperature"
      impFieldStdName(  1) = "sea_surface_temperature"
      impFieldAddOff(   1) = +273.15 !field is in degC
      impFieldUnits(    1) = "K"
      impFieldName(     2) = "sss"
      impFieldLongName( 2) = "Sea Surface Salinity"
      impFieldStdName(  2) = "sea_surface_salinity"
      impFieldUnits(    2) = "1e-3"
      impFieldName(     3) = "ssu"
      impFieldLongName( 3) = "Sea Surface X-Current"
      impFieldStdName(  3) = "sea_water_x_velocity"
      impFieldUnits(    3) = "m s-1"
      impFieldName(     4) = "ssv"
      impFieldLongName( 4) = "Sea Surface Y-Current"
      impFieldStdName(  4) = "sea_water_y_velocity"
      impFieldUnits(    4) = "m s-1"
      impFieldName(     5) = "ssh"
      impFieldLongName( 5) = "Sea Surface Height"
      impFieldStdName(  5) = "sea_surface_height_above_sea_level"
      impFieldUnits(    5) = "m"
      impFieldName(     6) = "ssfi"
      impFieldLongName( 6) = "Oceanic Heat Flux Available to Sea Ice"
      impFieldStdName(  6) = "upward_sea_ice_basal_available_heat_flux"
      impFieldSclFac(   6) = -1.0  !field is downward
      impFieldUnits(    6) = "W m-2"
      impFieldName(     7) = "mlt"  !diagnostic
      impFieldLongName( 7) = "Ocean Mixed Layer Thickness"
      impFieldStdName(  7) = "ocean_mixed_layer_thickness"
      impFieldUnits(    7) = "m"


!                                                     
!  Attributes for export fields, identical to HYCOM import fields
      expFieldAddOff(:) = 0.0 !default is no offset
      expFieldSclFac(:) = 1.0 !default is no scale factor

      expFieldName(     1) = "sic"
      expFieldLongName( 1) = "Sea Ice Concentration"
      expFieldStdName(  1) = "sea_ice_area_fraction"
      expFieldUnits(    1) = "1"                    
      expFieldName(     2) = "sitx"
      expFieldLongName( 2) = "Sea Ice X-Stress"
      expFieldStdName(  2) = "downward_x_stress_at_sea_ice_base"
      expFieldSclFac(   2) = -1.0  !field is upward
      expFieldUnits(    2) = "Pa"
      expFieldName(     3) = "sity"
      expFieldLongName( 3) = "Sea Ice Y-Stress"
      expFieldStdName(  3) = "downward_y_stress_at_sea_ice_base"
      expFieldSclFac(   3) = -1.0  !field is upward
      expFieldUnits(    3) = "Pa"
      expFieldName(     4) = "siqs"
      expFieldLongName( 4) = "Solar Heat Flux thru Ice to Ocean"
      expFieldStdName(  4) = "downward_sea_ice_basal_solar_heat_flux"
      expFieldUnits(    4) = "W m-2"
!     CCSM exports Ice Melting Heat Flux (hocn)
      expFieldName(     5) = "sifh"
      expFieldLongName( 5) = "Ice Freezing/Melting Heat Flux"
      expFieldStdName(  5) = "upward_sea_ice_basal_heat_flux"
      expFieldSclFac(   5) = -1.0  !field is downward
      expFieldUnits(    5) = "W m-2"
      expFieldName(     6) = "sifs"
      expFieldLongName( 6) = "Ice Freezing/Melting Salt Flux"
      expFieldStdName(  6) = "downward_sea_ice_basal_salt_flux"
      expFieldUnits(    6) = "kg m-2 s-1"
      expFieldName(     7) = "sifw"
      expFieldLongName( 7) = "Ice Net Water Flux"
      expFieldStdName(  7) = "downward_sea_ice_basal_water_flux"
      expFieldUnits(    7) = "kg m-2 s-1"
      expFieldName(     8) = "sit" !diagnostic
      expFieldLongName( 8) = "Sea Ice Temperature"
      expFieldStdName(  8) = "sea_ice_temperature"
      expFieldAddOff(   8) = +273.15 !field is in degC
      expFieldUnits(    8) = "K"                  
      expFieldName(     9) = "sih" !diagnostic
      expFieldLongName( 9) = "Sea Ice Thickness"
      expFieldStdName(  9) = "sea_ice_thickness"
      expFieldUnits(    9) = "m"                
      expFieldName(    10) = "siu" !diagnostic
      expFieldLongName(10) = "Sea Ice X-Velocity"
      expFieldStdName( 10) = "sea_ice_x_velocity"
      expFieldUnits(   10) = "m s-1"             
      expFieldName(    11) = "siv" !diagnostic
      expFieldLongName(11) = "Sea Ice Y-Velocity"
      expFieldStdName( 11) = "sea_ice_y_velocity"
      expFieldUnits(   11) = "m s-1"             


!    
!  Create array specifications
      call ESMF_ArraySpecSet(arraySpec2Dr, &
           rank=2, &
           typekind=ESMF_TYPEKIND_R4, &
           rc=rc)
      if (ESMF_LogMsgFoundError(rc, &
         "Setup_ESMF: ArraySpecSet failed", rcToReturn=rc2)) &
         call ESMF_Finalize(rc=rc)

!
!  Setup export fields, bundles & state
      do i = 1,numExpFields
        expField(i)=ESMF_FieldCreate(grid=iceGrid, &
                                     arrayspec=arraySpec2Dr, &
                                     indexflag=ESMF_INDEX_GLOBAL, &
                                     staggerLoc=ESMF_STAGGERLOC_CENTER, &
                                     name=trim(expFieldName(i)), &
                                     rc=rc)
        call ESMF_FieldGet(expField(i),0,expData(i)%p,rc=rc)
        expData(i)%p(:,:) = 0.0
      enddo
!       
!  Create bundle from list of fields
      expBundle = ESMF_FieldBundleCreate(numExpFields, &
                        expField(:), name="CICE Export", &
                        rc=rc)
!    
!  Add bundle to the export state
      call ESMF_StateAdd(exportState, expBundle, rc=rc)
!     
!  Setup import fields, bundles & state
      do i = 1,numImpFields
        impField(i)=ESMF_FieldCreate(grid=iceGrid, &
                                     arrayspec=arraySpec2Dr, &
                                     indexflag=ESMF_INDEX_GLOBAL, &
                                     staggerLoc=ESMF_STAGGERLOC_CENTER, &
                                     name=trim(impFieldName(i)), &
                                     rc=rc)
        call ESMF_FieldGet(impField(i),0,impData(i)%p,rc=rc)
        impData(i)%p(:,:) = 0.0
      enddo
       sst_import(:,:,:) = 0.0 !Sea Surface Temperature
       sss_import(:,:,:) =34.0 !Sea Surface Salinity
      uocn_import(:,:,:) = 0.0 !Sea Surface X-Current
      vocn_import(:,:,:) = 0.0 !Sea Surface Y-Current
       ssh_import(:,:,:) = 0.0 !Sea Surface Height
      ssfi_import(:,:,:) = 0.0 !Oceanic Heat Flux Available to Sea Ice
       mlt_import(:,:,:) = 0.0 !Ocean Mixed Layer Thickness

!       
!  Create bundle from list of fields
      impBundle = ESMF_FieldBundleCreate(numImpFields, &
                        impField(:), name="CICE Import", &
                        rc=rc)
!    
!  Add bundle to the import state
      call ESMF_StateAdd(importState, impBundle, rc=rc)

   !--------------------------------------------------------------------
   ! fill initial export state
   !--------------------------------------------------------------------

   call CICE_CoupledAccumulateExport(errorCode)

   plabel = 'pre CICE_CoupledFillExport'
!   if(my_task.eq.mtask)call print_state(plabel,4,4,1)
   call CICE_CoupledFillExport(exportState, errorCode)

!-----------------------------------------------------------------------
!EOC
!

 end subroutine CICE_CoupledInit

!=======================================================================
!BOP
!
! !IROUTINE: CICE_CoupledExtractImport - extract fields from import state
!
! !INTERFACE:
!
 subroutine CICE_CoupledExtractImport(importState, errorCode)
!
! !DESCRIPTION:
!
!  Extracts fields from ESMF import state and stores them in
!  appropriate CICE arrays
!
! !REVISION HISTORY:
!
!  author: Philip W Jones, LANL
!
! !USES:
!
      use ice_boundary
      use ice_ocean, only: oceanmixed_ice
!ajw
      use ice_global_reductions
!
! !INPUT PARAMETERS:

   type (ESMF_State), intent(in) :: &
      importState        ! import state from which to extract fields

   real (kind=dbl_kind), parameter :: &
      min_sss_import = 2.0_dbl_kind  ! very fresh sss causes problems


! !OUTPUT PARAMETERS:

   integer, intent(out) :: &
      errorCode        ! error code: ESMF_Success or ESMF_Failure

!
!EOP
!BOC
!
   !--------------------------------------------------------------------
   ! local variables
   !--------------------------------------------------------------------

   integer (kind=int_kind) :: &
      i,j,iblk, &        ! local loop indices
      ilo,ihi,jlo,jhi, & ! beginning and end of physical domain
      ig0,jg0            ! offset to global array

   type (block) :: &
      this_block         ! block information for current block

   real    (kind=dbl_kind) :: &
         amin, amax      ! global min and max values of input array

    integer rc

!                                             
! --- Report                                  
      call ESMF_LogWrite("CICE ExtractImport routine called", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)

   !--------------------------------------------------------------------
   !  import state is defined in init routine so pointers already
   !  reference correct data
   !--------------------------------------------------------------------

   errorCode = ESMF_SUCCESS

   call ice_timer_start(8)  ! time spent coupling

   !--------------------------------------------------------------------
   ! unpack import state
   !--------------------------------------------------------------------

   !--- unpack message

   do iblk = 1, nblocks ! nblocks must be one currently
      this_block = get_block(blocks_ice(iblk),iblk)
      ilo = this_block%ilo
      ihi = this_block%ihi
      ig0 = this_block%i_glob(ilo)-ilo
      jlo = this_block%jlo
      jhi = this_block%jhi
      jg0 = this_block%j_glob(jlo)-jlo

      do j = jlo, jhi
      do i = ilo, ihi

         !--- ocn states--
          sst_import(i,j,iblk) = impData(1)%p(i+ig0,j+jg0) !SST
          sss_import(i,j,iblk) = impData(2)%p(i+ig0,j+jg0) !SSS
!ajw
          sss_import(i,j,iblk) = max( sss_import(i,j,iblk), min_sss_import )
         uocn_import(i,j,iblk) = impData(3)%p(i+ig0,j+jg0) !SS X-Current
         vocn_import(i,j,iblk) = impData(4)%p(i+ig0,j+jg0) !SS Y-Current
          ssh_import(i,j,iblk) = impData(5)%p(i+ig0,j+jg0) !SSH
         ssfi_import(i,j,iblk) = impData(6)%p(i+ig0,j+jg0) !Available Heat Flux
          mlt_import(i,j,iblk) = impData(7)%p(i+ig0,j+jg0) !Mixed Layer Thickness
      enddo ! i
      enddo ! j
   enddo ! iblk


!ajw
!        if (my_task.eq.master_task) then
!           write(6,*)" in  extract routine "
!        amin = minval(sst_import)
!        amax = maxval(sst_import)
!        write(6,*)"min/max sst_import ",amin,amax
!        amin = minval(sss_import)
!        amax = maxval(sss_import)
!        write(6,*)"min/max sss_import ",amin,amax
!        amin = minval(uocn_import)
!        amax = maxval(uocn_import)
!        write(6,*)"min/max uocn_import ",amin,amax
!        amin = minval(vocn_import)
!        amax = maxval(vocn_import)
!        write(6,*)"min/max vocn_import ",amin,amax
!       endif
!ajw
   !-----------------------------------------------------------------
   ! Interpolate ocean currents to U grid
   !-----------------------------------------------------------------

   call t2ugrid_vector(uocn_import)
   call t2ugrid_vector(vocn_import)

   call ice_HaloUpdate ( sst_import,        halo_info, &
                            field_loc_center,   field_type_scalar)
   call ice_HaloUpdate ( sss_import,        halo_info, &
                            field_loc_center,   field_type_scalar)
   call ice_HaloUpdate (uocn_import,        halo_info, &
                            field_loc_NEcorner, field_type_vector)
   call ice_HaloUpdate (vocn_import,        halo_info, &
                            field_loc_NEcorner, field_type_vector)
   call ice_HaloUpdate ( ssh_import,        halo_info, &
                            field_loc_center,   field_type_scalar)
   call ice_HaloUpdate (ssfi_import,        halo_info, &
                            field_loc_center,   field_type_scalar)
   call ice_HaloUpdate ( mlt_import,        halo_info, &
                            field_loc_center,   field_type_scalar)

   !--------------------------------------------------------------------
   ! 2-way coupling
   !--------------------------------------------------------------------


   if (.not. oceanmixed_ice) then
           sst(:,:,1) =  sst_import(:,:,1)
           sss(:,:,1) =  sss_import(:,:,1)
          uocn(:,:,1) = uocn_import(:,:,1)
          vocn(:,:,1) = vocn_import(:,:,1)
        frzmlt(:,:,1) = ssfi_import(:,:,1)
   endif

   time_forc=time !???

!ajw
         if (my_task.eq.master_task) then
            write(6,*)" in  extract routine second time "
         endif
         amin = global_minval(sst_import,distrb_info,tmask)
         amax = global_maxval(sst_import,distrb_info,tmask)
         if (my_task.eq.master_task) then
            write(6,*)"min/max sst_import ",amin,amax
         endif
         amin = global_minval(sss_import,distrb_info,tmask)
         amax = global_maxval(sss_import,distrb_info,tmask)
         if (my_task.eq.master_task) then
            write(6,*)"min/max sss_import ",amin,amax
         endif
         amin = global_minval(uocn_import,distrb_info,umask)
         amax = global_maxval(uocn_import,distrb_info,umask)
         if (my_task.eq.master_task) then
            write(6,*)"min/max uocn_import ",amin,amax
         endif
         amin = global_minval(vocn_import,distrb_info,umask)
         amax = global_maxval(vocn_import,distrb_info,umask)
         if (my_task.eq.master_task) then
            write(6,*)"min/max vocn_import ",amin,amax
         endif
!ajw

   call ice_timer_stop(8)   ! time spent coupling

!-----------------------------------------------------------------------
!EOC

 end subroutine CICE_CoupledExtractImport

!=======================================================================
!BOP
!
! !IROUTINE: CICE_CoupledFillExport - fills export state with fields
!
! !INTERFACE:
!
 subroutine CICE_CoupledFillExport(exportState, errorCode)
!
! !DESCRIPTION:
!
! Fills ESMF export state with CICE data
!
! All exported fields are grid cell averages (CF convention),
! this is different from CCSM (which applies fluxes under ice only)
!
! !REVISION HISTORY:
!
! author: Philip W Jones, LANL
!
! HYCOM-CICE only version.  Alan Wallcraft NRL May 2006.
! Uses CF conventions for import/export fields
!   CF requires fluxes to be cell averages (CCSM has fluxes only "where_ice")
!   Must use (CF) standard_name and (MKS) units, 
!   but units can be modified by scale_factor and/or add_offset.
!
! !USES
!

! !OUTPUT PARAMETERS:

   integer, intent(out) :: &
      errorCode          ! output error code (success or fail)

   type (ESMF_state), intent(out) :: &
      exportState        ! export state to be filled

!
!EOP
!BOC
!
   !--------------------------------------------------------------------
   ! local variables
   !--------------------------------------------------------------------

   integer (kind=int_kind) :: &
      i,j,iblk, &        ! local loop indices
      ilo,ihi,jlo,jhi, & ! beginning and end of physical domain
      ig0,jg0            ! offset to global array

   type (block) :: &
      this_block         ! block information for current block

    integer rc

!                                             
! --- Report                                  
      call ESMF_LogWrite("CICE FillExport routine called", &
                         ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)

   !--------------------------------------------------------------------
   ! start timer and set flags
   !--------------------------------------------------------------------

   errorCode = ESMF_Success
   call ice_timer_start(8)  ! time spent coupling

   uiceT(:,:,:) = uvel(:,:,:)
   viceT(:,:,:) = vvel(:,:,:)
   call u2tgrid_vector(uiceT)
   call u2tgrid_vector(viceT)

   plabel = 'in CICE_FILL'
!   if (my_task.eq.mtask)call print_state(plabel,4,4,1)

   do iblk = 1, nblocks ! nblocks must be one currently
      this_block = get_block(blocks_ice(iblk),iblk)
      ilo = this_block%ilo
      ihi = this_block%ihi
      ig0 = this_block%i_glob(ilo)-ilo
      jlo = this_block%jlo
      jhi = this_block%jhi
      jg0 = this_block%j_glob(jlo)-jlo

      do j = jlo, jhi
      do i = ilo, ihi

      if (tmask(i,j,iblk)) then
         expData( 1)%p(i+ig0,j+jg0) =     aice(i,j,  iblk) !Concentration
         expData( 2)%p(i+ig0,j+jg0) = strocnxT(i,j,  iblk) !X-Stress
         expData( 3)%p(i+ig0,j+jg0) = strocnyT(i,j,  iblk) !Y-Stress
         expData( 4)%p(i+ig0,j+jg0) =  fswthru(i,j,  iblk)&!Solar thru
                                         *aice(i,j,  iblk) !grid cell ave.

         if     (frzmlt(i,j,iblk).gt.c0) then
           if     (  aice(i,j,iblk).gt.c0) then
             expData( 5)%p(i+ig0,j+jg0) = frzmlt(i,j,iblk) !Freeze H.Flux
           else
             expData( 5)%p(i+ig0,j+jg0) = 0.0 !no ice
           endif !aice:else
         else
           expData( 5)%p(i+ig0,j+jg0) =  fhocn(i,j,  iblk)&!Melt HeatFlux
                                         *aice(i,j,  iblk) !grid cell ave.
         endif !frz:melt
         expData( 6)%p(i+ig0,j+jg0) =    fsalt(i,j,  iblk)&!Salt  Flux
                                         *aice(i,j,  iblk) !grid cell ave.
         expData( 7)%p(i+ig0,j+jg0) =    fresh(i,j,  iblk)&!Water Flux
                                         *aice(i,j,  iblk) !grid cell ave.
         expData( 8)%p(i+ig0,j+jg0) =     trcr(i,j,1,iblk) !Temperature
         expData( 9)%p(i+ig0,j+jg0) =     vice(i,j,  iblk) !Thickness
         expData(10)%p(i+ig0,j+jg0) =    uiceT(i,j,  iblk) !X-Velocity
         expData(11)%p(i+ig0,j+jg0) =    viceT(i,j,  iblk) !Y-Velocity
      endif !umask

      enddo ! i
      enddo ! j
   enddo ! iblk

   call ice_timer_stop(8)    ! time spent coupling

!-----------------------------------------------------------------------
!EOC

 end subroutine CICE_CoupledFillExport

!=======================================================================
!BOP
!
! !IROUTINE: CICE_CoupledAccumulateExport - accumulates time avg fields
!
! !INTERFACE:
!
 subroutine CICE_CoupledAccumulateExport(errorCode)
!
! !DESCRIPTION:
!
! Accumulates time averages of export state fields for the case when
! the coupling frequency is longer than the internal CICE time step.
!
! !REVISION HISTORY:
!
! author: Philip W Jones, LANL
!
! !USES
!
!
! !OUTPUT PARAMETERS:
!
   integer, intent(out) :: &
      errorCode              ! output error code

!
!EOP
!BOC
!
   errorCode = ESMF_Success

 end subroutine CICE_CoupledAccumulateExport

!=======================================================================

 end module ice_coupling

!=======================================================================
