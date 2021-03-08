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
! $Id: ice_calendar.F90 140 2008-07-25 20:15:53Z eclare $
!=======================================================================
!BOP
!
! !MODULE: ice_calendar - calendar routines for managing time
!
! !DESCRIPTION:
!
! Calendar routines for managing time
!
! !REVISION HISTORY:
!
! authors: Elizabeth C. Hunke, LANL
!          Tony Craig, NCAR
!
! 2006 ECH: Removed 'w' option for history; added 'h' and histfreq_n.
!           Converted to free form source (F90).
!
! !INTERFACE:
!
      module ice_calendar
!
! !USES:
!
      use ice_constants
      use ice_exit, only: abort_ice
!
!EOP
!
      implicit none
      save
!pgp
!      integer (kind=int_kind) :: &
      integer (kind=int_kind), save  :: &
         days_per_year        , & ! number of days in one year
         daymo(12)            , & ! number of days in each month
         daycal(13)               ! day number at end of month

      ! 360-day year data
!pgp
!      integer (kind=int_kind) :: &
      integer (kind=int_kind),save :: &
         daymo360(12)         , & ! number of days in each month
         daycal360(13)            ! day number at end of month
      data daymo360 /   30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30/
      data daycal360/ 0,30, 60, 90,120,150,180,210,240,270,300,330,360/

      ! 365-day year data
!pgp
!      integer (kind=int_kind) :: &
      integer (kind=int_kind),save :: &
         daymo365(12)         , & ! number of days in each month
         daycal365(13)            ! day number at end of month
      data daymo365 /   31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      data daycal365/ 0,31, 59, 90,120,151,181,212,243,273,304,334,365/
!pgp
!     leap year data
      integer (kind=int_kind), save :: &
        daymo366(12)         , & !number of days in each month
          daycal366(13)            ! day number at end of month
       data daymo366 /   31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
       data daycal366/ 0,31, 60, 91,121,152,182,213,244,274,305,335,366/

!pgp
!      integer (kind=int_kind) :: &
      integer (kind=int_kind), save :: &
         istep    , & ! local step counter for time loop
         istep0   , & ! counter, number of steps taken in previous run
         istep1   , & ! counter, number of steps at current timestep
         mday     , & ! day of the month
         hour     , & ! hour of the year
!ajw
         zulu     , & ! elapsed hours into date
         month    , & ! month number, 1 to 12
         monthp   , & ! last month
         year_init, & ! initial year
         nyr      , & ! year number
         idate    , & ! date (yyyymmdd)
         idate0   , & ! initial date (yyyymmdd)
         sec      , & ! elapsed seconds into date
         npt      , & ! total number of time steps (dt)
         ndyn_dt  , & ! reduced timestep for dynamics: ndyn_dt=dt/dyn_dt
         stop_now     , & ! if 1, end program execution
         write_restart, & ! if 1, write restart now
!ajw
         insert_now   , & ! if 1, insert ssmi and sih
         inserthr     , & ! insert input hour (0,1,2,3,4,6,12,24)
!ajw
         diagfreq     , & ! diagnostic output frequency (10 = once per 10 dt)
         dumpfreq_n   , & ! restart output frequency (10 = once per 10 d,m,y)
         histfreq_n       ! history output frequency (10 = once per 10 h,d,m,y)

!pgp
!      real (kind=dbl_kind) :: &
      real (kind=dbl_kind), save  :: &
         dt             , & ! thermodynamics timestep (s)
         dyn_dt         , & ! dynamics/transport/ridging timestep (s)
         time           , & ! total elapsed time (s)
!pgp
         time_year      , & !time of this year's start (s)
         time_forc      , & ! time of last forcing update (s)
         yday           , & ! day of the year
         tday           , & ! absolute day number
         dayyr              ! number of days per year

!pgp
!      logical (kind=log_kind) :: &
      logical (kind=log_kind), save  :: &
         new_year       , & ! new year = .true.
         new_month      , & ! new month = .true.
         new_day        , & ! new day = .true.
         new_hour       , & ! new hour = .true.
         write_ic       , & ! write initial condition now
         write_history      ! write history now

!pgp
!      character (len=1) :: &
      character (len=1), save :: &
         histfreq       , & ! history output frequency, 'y','m','d','h','1'
         dumpfreq           ! restart frequency, 'y','m','d','e','x','o','r','1','2','3','4,'5','6','7'

!=======================================================================

      contains

!=======================================================================
!BOP
!
! !IROUTINE: init_calendar - initialize calendar variables
!
! !INTERFACE:
!
      subroutine init_calendar
!
! !DESCRIPTION:
!
! Initialize calendar variables
!
! !REVISION HISTORY:
!
! authors: Elizabeth C. Hunke, LANL
!          Tony Craig, NCAR
!
! !USES:
      use ice_communicate, only: my_task, master_task
!
! !INPUT/OUTPUT PARAMETERS:
!
!EOP
!
!ajw
      real (kind=dbl_kind) :: &
         tday1                          ! gregorian calendar variables 
      integer (kind=int_kind) :: &
         iyr, nleap                 , & ! gregorian calendar variables 
!ajw - end
         k                          , &

      istep = 0         ! local timestep number
      time=istep0*dt    ! s
      yday=c0           ! absolute day number
      mday=0            ! day of the month
      month=0           ! month
      nyr=0             ! year
      idate=00000101    ! date
      sec=0             ! seconds into date
      istep1 = istep0   ! number of steps at current timestep
                        ! real (dumped) or imagined (use to set calendar)
      stop_now = 0      ! end program execution if stop_now=1
      dyn_dt = dt/real(ndyn_dt,kind=dbl_kind) ! dynamics et al timestep

      dayyr = real(days_per_year, kind=dbl_kind)
      if (days_per_year.eq.360) then
        daymo  = daymo360
        daycal = daycal360
      elseif (days_per_year.eq.365) then
        daymo  = daymo365
        daycal = daycal365
!pgp
      elseif (days_per_year.eq.366)then
        daymo = daymo366
        daycal = daycal366
        dayyr = 365.25_dbl_kind
      else
         call abort_ice('ice: year must have 360 or 365 or 366 days')
      endif

      ! determine initial date (assumes namelist year_init, istep0 unchanged)     
      sec = mod(time,secday)            ! elapsed seconds into date at
                                        ! end of dt
      tday = (time-sec)/secday + c1     ! absolute day number
!ajw
      if (days_per_year.ne.366) then
        yday = mod(tday-c1,dayyr) + c1    ! day of the year
        nyr = int((tday-c1)/dayyr) + 1    ! year number
      else !model day is calendar days since 12/31/1900
        iyr   = (tday-c2)/365.25_dbl_kind
        nleap = iyr/4
        tday1 = 365.0_dbl_kind*iyr + nleap + c2
        yday  = tday - tday1 + c1 !provisional day of the year
        if     (tday1.gt.tday)then
          iyr = iyr - 1
        elseif (yday.ge.367.0_dbl_kind) then
          iyr = iyr + 1
        elseif (yday.ge.366.0_dbl_kind .and. mod(iyr,4).ne.3)then
          iyr = iyr + 1
        endif
        if     (mod(iyr,4).ne.3) then
          daymo  = daymo365
          daycal = daycal365
        else  !leap year
          daymo  = daymo366
          daycal = daycal366
        endif
        nleap = iyr/4
        tday1 = 365.0_dbl_kind*iyr + nleap + c2
        yday  = tday - tday1 + c1	! day of the year
        nyr   = iyr + 1			! year number
      endif !days_per_year

      do k = 1, 12
        if (yday > real(daycal(k),kind=dbl_kind)) month = k
      enddo
      mday = int(yday) - daycal(month)  ! day of the month
      idate0 = (nyr+year_init-1)*10000 + month*100 + mday ! date (yyyymmdd) 
      if (my_task.eq.master_task)then
        write(6,*)"in init-calendar idate0 = ",idate0
      endif


      end subroutine init_calendar

!=======================================================================
!BOP
!
! !IROUTINE: calendar - computes date at the end of the time step
!
! !INTERFACE:
!
      subroutine calendar(ttime)
!
! !DESCRIPTION:
!
! Determine the date at the end of the time step
!
! !REVISION HISTORY:
!
! authors: Elizabeth C. Hunke, LANL
!          Tony Craig, NCAR
!
! !USES:
      use ice_fileunits
      use ice_communicate, only: my_task, master_task
!
! !INPUT/OUTPUT PARAMETERS:
!
      real (kind=dbl_kind), intent(in) :: &
         ttime                          ! time variable
!
!EOP
!
!ajw
      real (kind=dbl_kind) :: &
         tday1                          ! gregorian calendar variables 
      integer (kind=int_kind) :: &
         iyr, nleap                 , & ! gregorian calendar variables 
         npt12hr                    , & ! time steps in 12 hours
!zg
         npt3hr                    , & ! time steps in 3 hours
!ajw - end
         k                          , &
         nyrp,mdayp,hourp           , & ! previous year, day, hour
         elapsed_days               , & ! since beginning this run
         elapsed_months             , & ! since beginning this run
         elapsed_hours                  ! since beginning this run

      nyrp=nyr
      monthp=month
      mdayp=mday
      hourp=hour

      new_year=.false.
      new_month=.false.
      new_day=.false.
      new_hour=.false.
      write_history=.false.
      write_restart=0
      insert_now=0

      sec  = mod(ttime,secday)          ! elapsed seconds into date at
                                        ! end of dt
!ajw
      zulu = int(  sec/c3600)           ! elapsed hours into date
      hour = int(ttime/c3600) + c1      ! hour
      tday = (ttime-sec)/secday + c1    ! absolute day number

!pgp
      if (days_per_year.ne.366) then
        yday = mod(tday-c1,dayyr) + c1    ! day of the year
!pgp
!       moved nyr from below up here
        nyr = int((tday-c1)/dayyr) + 1    ! year number
        time_year = int((tday-c1)/dayyr)*dayyr*secday
!ajw
      else !model day is calendar days since 12/31/1900
        iyr   = (tday-c2)/365.25_dbl_kind
        nleap = iyr/4
        tday1 = 365.0_dbl_kind*iyr + nleap + c2
        yday  = tday - tday1 + c1 !provisional day of the year
        if     (tday1.gt.tday)then
          iyr = iyr - 1
        elseif (yday.ge.367.0_dbl_kind) then
          iyr = iyr + 1
        elseif (yday.ge.366.0_dbl_kind .and. mod(iyr,4).ne.3)then
          iyr = iyr + 1
        endif
        if     (mod(iyr,4).ne.3) then
          daymo  = daymo365
          daycal = daycal365
        else  !leap year
          daymo  = daymo366
          daycal = daycal366
        endif
        nleap = iyr/4
        tday1 = 365.0_dbl_kind*iyr + nleap + c2
        yday  = tday - tday1 + c1	! day of the year
        nyr   = iyr + 1			! year number
        time_year = (tday1-c1) * secday
      endif !days_per_year
      
      do k = 1, 12
        if (yday > real(daycal(k),kind=dbl_kind)) month = k
      enddo
      mday = int(yday) - daycal(month)  ! day of the month
!pgp
!      nyr = int((tday-c1)/dayyr) + 1    ! year number
      elapsed_months = (nyr - 1)*12 + month - 1
      elapsed_days   = int(tday) - 1 
!pgp      elapsed_days = int(tday)  
      elapsed_hours = int(ttime/c3600)

      idate = (nyr+year_init-1)*10000 + month*100 + mday ! date (yyyymmdd) 

      if (istep >= npt+1)  stop_now = 1
      if (nyr   /= nyrp)   new_year = .true.
      if (month /= monthp) new_month = .true.
      if (mday  /= mdayp)  new_day = .true.
      if (hour  /= hourp)  new_hour = .true.

      if (histfreq == '1') write_history=.true.

      if (istep > 1) then
        select case (histfreq)
        case ("y", "Y")
          if (new_year  .and. mod(nyr, histfreq_n)==0) &
                write_history = .true.
        case ("m", "M")
          if (new_month .and. mod(elapsed_months,histfreq_n)==0) &
                write_history = .true.
        case ("d", "D")
          if (new_day .and. mod(elapsed_days,histfreq_n)==0) &
                write_history = .true.
        case ("h", "H")
!ajw      use hour in day for histfreq_n
          if (new_hour .and. mod(zulu, histfreq_n)==0) &
                write_history = .true.
        end select

        npt12hr = nint(secday/dt)/2
        npt3hr = nint(secday/dt)/8
        select case (dumpfreq)
        case ("y", "Y")
          if (new_year  .and. mod(nyr, dumpfreq_n)==0) &
                write_restart = 1
        case ("m", "M")
          if (new_month .and. mod(elapsed_months,dumpfreq_n)==0) &
                write_restart=1
        case ("d", "D")
          if (new_day   .and. mod(elapsed_days, dumpfreq_n)==0) &
                write_restart = 1
        case ("e", "E")
          if (istep >= npt)  &
                write_restart = 1  !last time step
        case ("x", "X")
          if (istep == npt-npt12hr)  &
                write_restart = 1  !12 hours before last time step
        case ("o", "O")
          if (istep == 2*npt12hr)  &
                write_restart = 1  !24 hours into the run
        case ("3")
          if (istep == nint(( 3.d0*secday/24.d0)/dt) ) &
                write_restart = 1  ! 3 hours into the run
          if (istep == 2*npt12hr)  &
                write_restart = 1  !24 hours into the run
        case ("5")
          if (istep == nint((15.d0*secday/24.d0)/dt) ) &
                write_restart = 1  !15 hours into the run
          if (istep == 2*npt12hr)  &
                write_restart = 1  !24 hours into the run
        case ("6")
          if (istep == nint(( 6.d0*secday/24.d0)/dt) ) &
                write_restart = 1  ! 6 hours into the run
        case ("1")
!zg changed from write restart at hour 21, 24 
!zg write restart at end of run, and 3 hours from end of run
          if (istep == npt-npt3hr)  &
                write_restart = 1  !3 hours before end of run
          if (istep >= npt)  &
                write_restart = 1  !last time step
!         if (istep == nint((21.d0*secday/24.d0)/dt) ) &
!               write_restart = 1  !21 hours into the run
!          if (istep == 2*npt12hr)  &
!                write_restart = 1  !24 hours into the run
         case ("7") 
!zg write restart at end of run and 6 hours from end of run
          if (istep == npt-2*npt3hr)  &
                write_restart = 1  !6 hours before end of run
          if (istep >= npt)  &
                write_restart = 1  !last time step
        case ("4")
          if (istep == 2*npt12hr)  &
                write_restart = 1  !24 hours into the run
        case ("2")
          if (istep ==   npt12hr)  &
                write_restart = 1  !12 hours into the run
          if (istep == 2*npt12hr)  &
                write_restart = 1  !24 hours into the run
        case ("r", "R")
          if (new_day) &
                write_restart = 1  ! daily at 00Z
          if (istep == 2*npt12hr)  &
                write_restart = 1  ! 24 hours into the run
        end select
      endif
!pgp
!ajw
      if ( istep == nint((inserthr*secday/24.d0)/dt) ) then
        insert_now=1  ! inserthr hours into the run
      endif
!ajw
!      if (my_task == master_task .and. mod(istep,diagfreq) == 0 &
!                                 .and. stop_now /= 1) then
      if (my_task == master_task .and. mod(istep,diagfreq) == 0 )then
        write(nu_diag,*) ' '
        write(nu_diag,'(a7,i10,4x,a6,i10,4x,a4,i10)') &
             'istep1:', istep1, 'idate:', idate, 'sec:', sec
      endif

      end subroutine calendar

!=======================================================================

      end module ice_calendar

!=======================================================================
