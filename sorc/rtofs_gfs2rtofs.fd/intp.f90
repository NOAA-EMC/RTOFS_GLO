program intp
  !$$$ MAIN PROGRAM DOCUMENTATION BLOCK
  !
  !   MAIN PROGRAM:  Intp.f90
  !   PRGMMR: ILYA RIVIN       ORG: W/NP21      DATE: 2003-01-14    
  !                                UPDATED April 03 C.L. August 03 C.L
  !                                       September 03 B.B February 04 .c.l.. 
  !                                       July 24 Avichal Mehra
  !                                       June 1, 2006: Avichal Mehra & Ilya Rivin
  ! ABSTRACT: THIS PROGRAM INTERPOLATES GFS SURFACE FIELDS INTO HYCOM 
  !   GRID AND WRITES THE RESULTS IN HYCOM-STYLE FILES. 
  !
  ! USAGE:
  !   INPUT FILES:
  !     FTxxF001 - UNITS 11 THRU 49
  !     UNIT  5  - (STANDARD READ)
  !     UNIT  7  - FILE intp_pars.dat, COTROL RUN PARAMETRS
  !     UNIT  8  - FILE regional.grid.b, DESCRIPTOR FOR HYCOM GRID 
  !                (READ IN mod_geom.f90)
  !     UNIT  9  - FILE regional.grid.a, HYCOM GRID 
  !                (READ IN mod_geom.f90)
  !     UNIT 33  - FILE listflx.dat, LIST OF DATES AND GFS FLUS FILES TO 
  !                BE USED IN INTERPOLATIO.
  !     UNIT 59  - FILE regional.depth.a, HYCOM BATHIMETRY 
  !                (READ IN mod_geom.f90)
  !     UNIT 61  - FILE regional.mask.a, HYCOM mask
  !                (READ IN mod_geom.f90)
  !     UNIT 81  - GFS GRIBBED FLUXES FILES WITH THE NAMES FROM THE LIST 
  !                SPECIFIED IN listflx.dat.
  !     UNIT 82  - THE SAME !     
  !
  !   OUTPUT FILES:  
  !     FTxxF001 - UNITS 51 THRU 79
  !     FTxxF001 - UNIT 6 (STANDARD PRINTFILE)
  !     UNIT XX  - FILES forcing.*.a, HYCOM FORCING FILES
  !     UNIT XX  - FILES forcing.*.d, DESCRIPTIORS TO HYCOM FORCING FILES 

  use mod_hycomio1, only: mask_hycom_1,mask_hycom_2
  use mod_za,only : xcspmd,zaiost,zaiopf,zaiowr,zaiocl,idm,jdm
  use mod_hytime
  use mod_dump
  use mod_geom
  use mod_gribio
  use mod_flags

  !
  ! mrfflxs: 1 is zonal momentum flux            [N/m^2]
  !          2 is meridional momentum flux       [N/m^2]
  !          3 is air temperature at 2m          [K]
  !          4 is water vapor mixing ratio at 2m [kg/kg]
  !          5 is precipitation rate             [kg/m^2/s]
  !          6 is u-wind at 10m                  [m/s]
  !          7 is v-wind at 10m                  [m/s]
  !          8 is sensible heat flux post. up    [W/m^2] 
  !          9 is latent heat flux post. up      [W/m^2]
  !         10 is downward long wave flux        [W/m^2]
  !         11 is upward long wave flux          [W/m^2]
  !         12 is downward short wave flux       [W/m^2]
  !         13 is upward short wave flux         [W/m^2]
  !         14 is surface temperature            [K]
  !         15 is surface pressure               [Pa]
  !         16 is sea level pressure             [Pa]
  !         17 is land mask                      [land=1;sea=0]
  !
  ! flxflg=flxflg_flxs
  ! hycom:    1 is zonal momentum flux                      [N/m^2]
  !           2 is meridional momentum flux                 [N/m^2]
  !           3 is sensible heat flux positive down         [W/m^2] 
  !           4 is latent heat flux positive down           [W/m^2]
  !           5 is solar penetrative heat flux in the ocean [W/m^2] 
  !           6 is net radiative heat flux in the ocean     [W/m^2] 
  !           7 is precipitative rate                       [m/s]
  !           8 is surface pressure                         [Pa]
  !           9 is surface temperature                      [C] 
  !          10 is sea level pressure                       [Pa] 
  !     Comments:  flxflg_smpl option is used for the case flxflg =3 in hycom
  ! flxflg=flxflg_smpl
  ! hycom:    1 is zonal momentum flux                                                    [N/m^2]
  !           2 is meridional momentum flux                                               [N/m^2]
  !           3 is solar penetrative heat flux in the ocean [ net short_wave ]       [W/m^2] 
  !           4 is net heat flux in the ocean (sensible+latent+long_wave+short_wave) [W/m^2] 
  !           5 is water flux rate (precipitation - evaporation)                          [m/s]
  !           6 is wind speed at 10m                                                      [m/s] 
  !           7 is surface pressure                                                       [Pa]
  !           8 is surface temperature                                                    [C] 
  !           9 is air temperature  (read in hycom but not used)                          [C]
  !          10 is water vapor mixing ratio (read in hycom but not used)                  [kg/kg]
  !          11 is sea level pressure                                                     [Pa] 
  ! flxflg=flxflg_flds
  ! hycom:    1 is zonal momentum flux                      [N/m^2]
  !           2 is meridional momentum flux                 [N/m^2]
  !           3 is air temperature at 10m                   [C] 
  !           4 is water vapor mixing ratio at 10m          [kg/kg] 
  !           5 is precipitative rate                       [m/s]
  !           6 is wind speed at 10m                        [m/s] 
  !           7 is solar penetrative heat flux in the ocean [W/m^2] 
  !           8 is net radiative heat flux in the ocean     [W/m^2] 
  !           9 is surface pressure                         [Pa]
  !          10 is surface temperature                      [C] 
  !          11 is sea level pressure                       [Pa] 
  !
  ! i is changing from west (1) to east (nx)
  ! j is changing from north (1) to south (ny)
  !
  implicit none
  !
  namelist /intp_pars/ dbgn,flxflg,avstep,mrffreq,avg3,wslocal,global,two_clock
  integer:: flxflg,dbgn,num_month,avg3,wslocal
  logical :: global,two_clock
  !
  !dbgn      debugging =0 - no dbg; =1,2,3 - add output
  !flxflg    two admissible flags. One to be used if all fluxes are prescribed  (flxflg_flxs & flxflg_smpl)
  !          the other one if turbulent fluxes are computed in the ocean model. (flxflg_flds)
  !          See parameter values in mod_flags
  !avstep    time for averaging fields (hrs)
  !mrffreq   time interval(hrs) between GFS fields
  !avg3	   if avg3 = 1, then averaged fluxes are converted to instantaneous fields
  !wslocal   if  wslocal = 1, then wind stress are computed from wind velocities
  !
  real:: avstep,mrffreq,cs,ss,speed,cdval,cd
  !
  ! Constants:
  !
  real,parameter::      &
       t0=273.17        &
       ,t_f=-1.8        &  ! freezing temperature  
       ,evaplh=2.47e6   &  ! latent heat of evaporation (j/kg)
       ,thref =1.0e-3   &  ! reference value of specific volume (m**3/kg) 
       ,minspd=3.0	&  ! minimum value of speed for calculating cd
       ,cdmax=0.0025	&  ! max value of cd
       ,bignumber=1.e20
  integer, parameter :: nmrf=17,nextrap_max=23
  !
  integer :: nxhycom,mapflg,nyhycom,nhycom,i,j,iii,jjj,k,n,m,ntime &
       ,nxmrf,nymrf,nxmrf_prev,nymrf_prev,nxmrf2,nymrf2,na,namax &
       ,hh1,hh2,ndiff,nextrap,lua,lub,ii,edges=0,idum &
       ,lua1,lub1,open_gate,namax_climo
  integer, dimension(:,:), allocatable:: imsk_intp,imsk_hycom
  real, dimension(:), allocatable :: exhycom,eyhycom,exmrf,eymrf,htime,off_time
  real, dimension(:,:), allocatable :: exhycom2d,eyhycom2d
  real, dimension(:,:), allocatable :: qxhycom2d,qyhycom2d,anhycom2d
  real, dimension(:,:), allocatable:: mrfflx ,msk_in,msk_out,mskmrf_tmp,latent,short_wave
  real, dimension(:,:,:), allocatable :: mrfflxs,hycomflxs,workflxs
  real, dimension(:,:,:), allocatable :: hycomflxb,hycomflxp
  real, dimension(:,:), allocatable :: hycomflxv,hycomflxu
  real ::  reflon=0.,reflat=0.,gridsz=0.,pntlat=0.,chtime,ftime,dmin,dmax,mskfrac=0.99,maxspeed
  character (len=6), dimension(:),allocatable :: hyflxs1
  character (len=10), dimension (:),allocatable :: ctime
  character (len=100), dimension (:),allocatable :: mrfnames
  integer, dimension(:), allocatable :: vecflxs1,avgflxs1

  logical :: clsgrib,grid_file_exist=.false.,mrf_grid_changed
  integer, dimension(3,nmrf) :: kpds567
  integer, dimension(200) :: gds 
  integer, dimension(3) :: kpds
  !
  !
  ! Read parameters and allocate arrays
  !
  open (7,file='intp_pars.dat')
  read(7,intp_pars)
  close(7)
  write(*,intp_pars); call flush(6)
  ! check if regional.grid.[ab] exists and read mapflg. nxhycom,nyhycom will be 
  ! redefined few lines later after call xcspmd
  call rd_hycom_grid_params(nxhycom,nyhycom,mapflg,grid_file_exist)
  if(grid_file_exist) then
     write (*,*) ' ---- Files regional.grid.[ab] are found'
     call xcspmd  !input idm,jdm by use association
!!!>>> nxhycom=idm
!!!>>> nyhycom=jdm
  else
     write(*,*) 'ERROR: regional.grid.[ab] are not found'
     stop
  endif
  if (mapflg/=mapflg_mer .and. mapflg/=mapflg_ort .and. mapflg/=mapflg_tripol) then
     write (*,*) ' ---- ERROR: unknown mapflg=',mapflg
     stop
  endif

  write(*,*) ' --- FINALLY: nxhycom=',nxhycom,' nyhycom=',nyhycom,' mapflg=',mapflg
  ! initialize output 
  call zaiost
  if ( flxflg == flxflg_flxs ) then
     nhycom=10                                          ! number of HYCOM flxs
  elseif ( flxflg == flxflg_flds ) then
     nhycom=11                                         
  elseif ( flxflg == flxflg_smpl ) then
     nhycom=11
  else
     write(*,*) '=== ERROR in interpolation: wrong flag flxflg=',flxflg
     stop
  end if

  open (33,file='listflx.dat',form='formatted')
  read(33,*) ntime
  allocate (htime(ntime) &
       ,ctime(ntime) &
       ,mrfnames(ntime))
  read(33,'(a10,1x,a100)') (ctime(i),mrfnames(i),i=1,ntime)
  close(33)
  write(*,*) '# of times:',ntime
  write(*,*)'  Time     File name'
  write (*,'(a10," ",a)') (ctime(i),trim(mrfnames(i)),i=1,ntime)
  call flush(6)

  !   "HYCOM" time
  do i =1,ntime 
     call hytime(ctime(i),htime(i)) 
     write (*,*) htime(i), ctime(i) 
  end do
  namax_climo=4*30

  !allocate   offset of time marks
  allocate(off_time(1:nhycom))
  !allocate arrays on HYCOM grid
  allocate ( &
       hycomflxs(1:nxhycom,1:nyhycom,1:nhycom) &
       ,msk_out(1:nxhycom,1:nyhycom) &
       ,hyflxs1(nhycom) &
       ,vecflxs1(nhycom) &
       ,hycomflxu(1:nxhycom,1:nyhycom) &
       ,hycomflxv(1:nxhycom,1:nyhycom) &
       ,avgflxs1(nhycom) &
       ,hycomflxp(1:nxhycom,1:nyhycom,1:nhycom) &
       ,hycomflxb(1:nxhycom,1:nyhycom,1:nhycom) &
       ,exhycom2d(nxhycom+1,nyhycom+1) &
       ,eyhycom2d(nxhycom+1,nyhycom+1) &
       ,qxhycom2d(nxhycom+1,nyhycom+1) &
       ,qyhycom2d(nxhycom+1,nyhycom+1) &
       ,imsk_hycom(1:nxhycom,1:nyhycom) &
       ,anhycom2d(nxhycom,nyhycom) &
       )

  write (*,*) 'HYCOM Files are allocated' ; call flush(6)
  fluxtype: if ( flxflg == flxflg_flxs ) then
     hyflxs1=(/'tauewd','taunwd','snsibl','latent','shwflx','radflx','precip','presur','surtmp','premsl'/)
     off_time=0.0

     ! NOTE: Ilya two clock option is not ready for the global model
     if (two_clock) off_time(3:7)=-1.5/24.0  !averaged fields are off set by half of 3 hours

     vecflxs1=0
     vecflxs1(1)=1
     vecflxs1(2)=2

     avgflxs1=1
     if (wslocal==1 ) then
        avgflxs1(1:2)=0
     endif
     avgflxs1(8:9)=0
  elseif ( flxflg == flxflg_flds ) then
     hyflxs1=(/'tauewd','taunwd','airtmp','vapmix','precip','wndspd','shwflx','radflx' &
          ,'presur','surtmp','premsl'/)
     off_time=0.0

     if (two_clock) then
        off_time(7:8)=-1.5/24.0  !averaged fields are off set by half of 3 hours
        off_time(5)=-1.5/24.0    !averaged fields are off set by half of 3 hours
     endif

     vecflxs1=0
     vecflxs1(1)=1
     vecflxs1(2)=2

     avgflxs1=1
     if (wslocal==1 ) then
        avgflxs1(1:2)=0
     endif
     avgflxs1(3:4)=0
     avgflxs1(6)=0
     avgflxs1(9:10)=0
  elseif ( flxflg == flxflg_smpl ) then
     hyflxs1=(/'tauewd','taunwd','shwflx','radflx','precip','wndspd','presur','surtmp' &
          ,'airtmp','vapmix','premsl'/)
     off_time=0.0

     if (two_clock) off_time(3:5)=-1.5/24.0  !averaged fields are off set by half of 3 hours

     vecflxs1=0
     vecflxs1(1)=1
     vecflxs1(2)=2

     avgflxs1=1
     if (wslocal==1 ) then
        avgflxs1(1:2)=0
     endif
     avgflxs1(6:10)=0
  end if fluxtype

  kpds567=reshape (source=  &
       (/124,  1 , 0    &  ! UFLX        1
       ,125,   1 , 0    &  ! VFLX        2
       , 11, 105 , 2    &  ! TMP 2m      3
       , 51, 105 , 2    &  ! SPFH 2m     4
       , 59,   1 , 0    &  ! PRATE       5
       , 33, 105 ,10    &  ! UGRD 10m    6
       , 34, 105 ,10    &  ! VGRD 10m    7
       ,122,   1 , 0    &  ! SHTFL       8
       ,121,   1 , 0    &  ! LHTFL       9
       ,205,   1 , 0    &  ! DLWRF      10
       ,212,   1 , 0    &  ! ULWRF      11
       ,204,   1 , 0    &  ! DSWRF      12
       ,211,   1 , 0    &  ! USWRF      13
       , 11,   1 , 0    &  ! TMP 0m     14
       ,  1,   1 , 0    &  ! PRES       15
       ,  2, 102 , 0    &  ! PRMSL      16
       , 81,   1 , 0 /) &  ! LAND       17 always must be the last one
       ,shape = (/ 3,nmrf /) )  


  ! 
  ! Read HYCOM land/sea mask from regional.mask.a (land=0,sea=1)
  !
  print *,'--- Calculating HYCOM mask'; call flush(6)
  ! Create HYCOM mask using the regional.depth.[ab] file 
  call mask_hycom_1(imsk_hycom)

  ! Read HYCOM mask from the regional.mask.[ab] file 
  !  call mask_hycom_2(imsk_hycom)
  write(*,*) '1: imsk_hycom min, max = ',minval(imsk_hycom),maxval(imsk_hycom) ! dbgz
  write(700,'(i13)') imsk_hycom ! dbgz
  !
  ! Prepare HYCOM grid 
  !
  write(*,*) '----Prepare HYCOM grid'
  if (grid_file_exist) then 
     if(mapflg==mapflg_mer ) then
        call hycom_na_mercator(exhycom,eyhycom,dbgn)
        global=.false.
     elseif (mapflg==mapflg_tripol) then
        write(*,*) '1a: imsk_hycom min, max = ',minval(imsk_hycom),maxval(imsk_hycom) ! dbgz
        call hycom_global_tripolar(anhycom2d,qxhycom2d,qyhycom2d,dbgn)
        global=.true.
        write(*,*) '1b: imsk_hycom min, max = ',minval(imsk_hycom),maxval(imsk_hycom) ! dbgz
     elseif (mapflg==mapflg_ort) then
        call hycom_na(anhycom2d,qxhycom2d,qyhycom2d,dbgn)
        global=.false.
     endif
  else
     if(mapflg==mapflg_mer) then
        call hycom_na_mercator(gridsz,pntlat,reflon,exhycom,eyhycom,dbgn)
     else
        write(*,*) 'ERROR: no regional.grid.[ab] files for mapflg=',mapflg
        call flush(6)
        stop
     endif
  endif
  write(*,*) '2: imsk_hycom min, max = ',minval(imsk_hycom),maxval(imsk_hycom) ! dbgz
  write(701,'(i13)') imsk_hycom ! dbgz

  !
  ! For the averaging
  !
  chtime=htime(1)
  if (ntime==1) then
     namax=1
     na=1
     ! NOTE: ilya the array is not yet allocated! must be a mistake
     workflxs=0.
  else
     namax=avstep/mrffreq
     na=1
  end if
  if(namax == namax_climo) then
     write(*,*)' Climatological style records '
  else
     write(*,*)' High frequency style records '
  endif
  open_gate=1

  open (unit=700,file='maskhycom.dat',form="unformatted")
  write(700) real(imsk_hycom(:,:))
  close(700)
  !
  ! read GFS flux
  !
  print *,'--- STARTING TIME LOOP *** '; call flush(6)
  !
  ! Time loop
  !
  num_month=1
  timeloop: do m=1,ntime

     write (*,*) '<--------- '//trim(ctime(m))//' ********'
     print *, "Inside Time Loop ",m
     !
     !   Get GFS grid parameters (before  20021029.t12Z: 512x256, after: 768x384)
     call getgds(81,trim(mrfnames(m)),gds)
     nxmrf=gds(2) ; nymrf=gds(3)
     nxmrf2=nxmrf/2 ; nymrf2=nymrf/2 
     if (m==1) then 
        nxmrf_prev=nxmrf ; nymrf_prev=nymrf
        mrf_grid_changed=.true.
     else
        if (nxmrf==nxmrf_prev .and. nymrf==nymrf_prev ) then
           mrf_grid_changed=.false.
        else
           nxmrf_prev=nxmrf ; nymrf_prev=nymrf
           mrf_grid_changed=.true.
        endif
     endif
     print *, 'm,nxmrf_prev,nxmrf,nymrf_prev,nymrf,mrf_grid_changed=' &
          ,m,nxmrf_prev,nxmrf,nymrf_prev,nymrf,mrf_grid_changed
     atmgrid_changed: if (mrf_grid_changed) then
        write(*,'(''GFS grid dimensions nxmrf,nymrf,nxmrf2,nymrf2: '',4i5)') &
             nxmrf,nymrf,nxmrf2,nymrf2
        !     allocate arrays on GFS grid and GFS fluxes
        if (m>1) deallocate(mrfflx &
             ,latent &
             ,short_wave &
             ,msk_in &
             ,workflxs &
             ,mskmrf_tmp &
             ,exmrf &
             ,eymrf &
             ,mrfflxs &
             )
        allocate ( &
             mrfflx(1:nxmrf,1:nymrf) &
             ,latent(1:nxmrf,1:nymrf) &
             ,short_wave(1:nxmrf,1:nymrf) &
             ,mrfflxs(1:nxmrf,1:nymrf,1:nmrf) &
             )
        if (global) then
           allocate ( &
                msk_in(1:nxmrf+2,1:nymrf+2) &
                ,workflxs(1:nxmrf+2,1:nymrf+2,nhycom) &
                ,mskmrf_tmp(1:nxmrf+2,1:nymrf+2) &
                ,exmrf(1:nxmrf+2+edges) & 
                ,eymrf(1:nymrf+2+edges) &
                )
        else
           allocate ( &
                msk_in(1:nxmrf,1:nymrf) &
                ,workflxs(1:nxmrf,1:nymrf,1:nhycom) &
                ,mskmrf_tmp(1:nxmrf,1:nymrf) &
                ,exmrf(1:nxmrf+edges) & 
                ,eymrf(1:nymrf+edges) &
                )
        endif
        ! print *, 'nymrf,edges,nymrf+edges=',nymrf,edges,nymrf+edges !dbgzp
        !
        !     Calculate GFS grid  ( coordinates are supposed to increase )
        ! Ilya: replace later with reading from GRIB gds
        !
        call mrf_gaussian(exmrf(2:nxmrf+1+edges),eymrf(2:nymrf+1+edges),edges,dbgn)
        ! write(*,*) 'before extend: eymrf****' !dbgzp
        ! write(*,'(f13.6)') eymrf*57.2957795 !dbgzp
        ! write(*,*) 'before extend again: eymrf****' !dbgzp
        ! write(*,'(a,i5,f13.6)') ('before extend again: eymrf',i,eymrf(i)*57.2957795,i=1,size(eymrf)) !dbgzp
        if (global) call global_extend(exmrf,eymrf)
        print *, 'exmrf: min,max=',minval(exmrf)*57.2957795,maxval(exmrf)*57.2957795 !dbgzp
        print *, 'eymrf: min,max=',minval(eymrf)*57.2957795,maxval(eymrf)*57.2957795 !dbgzp
        !
        !     Pre-compute interpolation indices and weights for multiple interpolations
        !
        call horiz_intp(exmrf,eymrf,qxhycom2d,qyhycom2d,exhycom2d,eyhycom2d,idum,global=global)
        !
        !     Establish GFS mask (land=0,sea=1). 
        !
        print *,'--- Changing GFS mask'; call flush(6)
        call mask_mrf(82,trim(mrfnames(1)),kpds567(:,nmrf),mskfrac,nextrap_max,msk_in &
             ,imsk_hycom,mskmrf_tmp,nextrap,mapflg,exhycom2d,eyhycom2d,global=global)
        if(nextrap>=nextrap_max) then
           print *,'ERROR: nextrap>=nextrap_max, nextrap=',nextrap,' nextrap_max=',nextrap_max
        endif
     endif atmgrid_changed
     ! 
     !   Read GFS fluxes from GRIB
     !
     clsgrib=.false.
     do i=1,nmrf-1 
        if (i==nmrf-1) clsgrib=.true.
        kpds=kpds567(:,i)
        if( wslocal==1 .and. i==1) then  !use zonal wind
           kpds=kpds567(:,6)
        elseif( wslocal==1 .and. i==2) then !use meridional wind
           kpds=kpds567(:,7)
        endif
        call rdgrib(82,trim(mrfnames(m)),mrfflx,kpds,clsgrib)  
        mrfflxs(1:nxmrf2      ,:,i)=mrfflx(nxmrf2+1:nxmrf,:)
        mrfflxs(nxmrf2+1:nxmrf,:,i)=mrfflx(1:nxmrf2      ,:)
        print*,'GFS fluxes: i,min,max=',i,minval(mrfflxs(:,:,i)),maxval(mrfflxs(:,:,i))
     end do

     !
     if (na==namax+1) then
        workflxs=0.
        na=1
        num_month=num_month+1
     end if
     chtime=((na-1.)*chtime+htime(m))/na
     !
     !   Intermediate MRF fluxes 
     !
     latent=mrfflxs(1:nxmrf,1:nymrf,9) ; where(latent<0.)latent=0.   ! no condensation
     short_wave=mrfflxs(1:nxmrf,1:nymrf,12)-mrfflxs(1:nxmrf,1:nymrf,13)
     if ( flxflg == flxflg_flxs ) then
        workflxs(1:nxmrf,1:nymrf,1)= ((na-1.)*workflxs(1:nxmrf,1:nymrf,1) &
             -mrfflxs(1:nxmrf,1:nymrf,1))/na
        workflxs(1:nxmrf,1:nymrf,2)=((na-1.)*workflxs(1:nxmrf,1:nymrf,2) &
             -mrfflxs(1:nxmrf,1:nymrf,2))/na
        workflxs(1:nxmrf,1:nymrf,3)=((na-1.)*workflxs(1:nxmrf,1:nymrf,3) &
             -mrfflxs(1:nxmrf,1:nymrf,8))/na
        workflxs(1:nxmrf,1:nymrf,4)=((na-1.)*workflxs(1:nxmrf,1:nymrf,4) &
             -latent(1:nxmrf,1:nymrf))/na
        workflxs(1:nxmrf,1:nymrf,5)=((na-1.)*workflxs(1:nxmrf,1:nymrf,5) &
             +short_wave)/na
        workflxs(1:nxmrf,1:nymrf,6)=((na-1.)*workflxs(1:nxmrf,1:nymrf,6) &
             +mrfflxs(1:nxmrf,1:nymrf,10)-mrfflxs(1:nxmrf,1:nymrf,11) & 
             +short_wave)/na
        workflxs(1:nxmrf,1:nymrf,7)=((na-1.)*workflxs(1:nxmrf,1:nymrf,7) &
             +mrfflxs(1:nxmrf,1:nymrf,5)*thref)/na
        workflxs(1:nxmrf,1:nymrf,8)=((na-1.)*workflxs(1:nxmrf,1:nymrf,8) &
             +mrfflxs(1:nxmrf,1:nymrf,15))/na
        workflxs(1:nxmrf,1:nymrf,9)=((na-1.)*workflxs(1:nxmrf,1:nymrf,9) &
             +mrfflxs(1:nxmrf,1:nymrf,14)-t0)/na
        workflxs(1:nxmrf,1:nymrf,10)=((na-1.)*workflxs(1:nxmrf,1:nymrf,10) &
             +mrfflxs(1:nxmrf,1:nymrf,16))/na
     elseif ( flxflg == flxflg_smpl ) then
        workflxs(1:nxmrf,1:nymrf,1)=((na-1.)*workflxs(1:nxmrf,1:nymrf,1) &
             -mrfflxs(1:nxmrf,1:nymrf,1))/na
        workflxs(1:nxmrf,1:nymrf,2)=((na-1.)*workflxs(1:nxmrf,1:nymrf,2) &
             -mrfflxs(1:nxmrf,1:nymrf,2))/na
        workflxs(1:nxmrf,1:nymrf,3)=((na-1.)*workflxs(1:nxmrf,1:nymrf,3) &
             +short_wave)/na
        workflxs(1:nxmrf,1:nymrf,4)=((na-1.)*workflxs(1:nxmrf,1:nymrf,4) &
             +mrfflxs(1:nxmrf,1:nymrf,10)-mrfflxs(1:nxmrf,1:nymrf,11) & 
             +short_wave-mrfflxs(1:nxmrf,1:nymrf,8)-latent)/na
        workflxs(1:nxmrf,1:nymrf,5)=((na-1.)*workflxs(1:nxmrf,1:nymrf,5) &
             +(mrfflxs(1:nxmrf,1:nymrf,5)-latent(1:nxmrf,1:nymrf)/evaplh)*thref)/na
        workflxs(1:nxmrf,1:nymrf,6)=((na-1.)*workflxs(1:nxmrf,1:nymrf,6) &
             +sqrt(mrfflxs(1:nxmrf,1:nymrf,6)**2+mrfflxs(1:nxmrf,1:nymrf,7)**2))/na
        workflxs(1:nxmrf,1:nymrf,7)=((na-1.)*workflxs(1:nxmrf,1:nymrf,7) &
             +mrfflxs(1:nxmrf,1:nymrf,15))/na
        workflxs(1:nxmrf,1:nymrf,8)=((na-1.)*workflxs(1:nxmrf,1:nymrf,8) &
             +mrfflxs(1:nxmrf,1:nymrf,14)-t0)/na
        workflxs(1:nxmrf,1:nymrf,9)=((na-1.)*workflxs(1:nxmrf,1:nymrf,9) &
             +mrfflxs(1:nxmrf,1:nymrf,3)-t0)/na
        workflxs(1:nxmrf,1:nymrf,10)=((na-1.)*workflxs(1:nxmrf,1:nymrf,10) &
             +mrfflxs(1:nxmrf,1:nymrf,4))/na
        workflxs(1:nxmrf,1:nymrf,11)=((na-1.)*workflxs(1:nxmrf,1:nymrf,11) &
             +mrfflxs(1:nxmrf,1:nymrf,16))/na
     elseif ( flxflg == flxflg_flds ) then
        workflxs(1:nxmrf,1:nymrf,1)=((na-1.)*workflxs(1:nxmrf,1:nymrf,1) &
             -mrfflxs(1:nxmrf,1:nymrf,1))/na
        workflxs(1:nxmrf,1:nymrf,2)=((na-1.)*workflxs(1:nxmrf,1:nymrf,2) &
             -mrfflxs(1:nxmrf,1:nymrf,2))/na
        ! IMPORTANT : temperature and humidity are at 2m, not 10m
        workflxs(1:nxmrf,1:nymrf,3)=((na-1.)*workflxs(1:nxmrf,1:nymrf,3) &
             +mrfflxs(1:nxmrf,1:nymrf,3)-t0)/na
        workflxs(1:nxmrf,1:nymrf,4)=((na-1.)*workflxs(1:nxmrf,1:nymrf,4) &
             +mrfflxs(1:nxmrf,1:nymrf,4))/na
        workflxs(1:nxmrf,1:nymrf,5)=((na-1.)*workflxs(1:nxmrf,1:nymrf,5) &
             +mrfflxs(1:nxmrf,1:nymrf,5)*thref)/na
        workflxs(1:nxmrf,1:nymrf,6)=((na-1.)*workflxs(1:nxmrf,1:nymrf,6) &
             +sqrt(mrfflxs(1:nxmrf,1:nymrf,6)**2 &
             +mrfflxs(1:nxmrf,1:nymrf,7)**2))/na
        workflxs(1:nxmrf,1:nymrf,7)=((na-1.)*workflxs(1:nxmrf,1:nymrf,7) &
             +short_wave)/na
        workflxs(1:nxmrf,1:nymrf,8)=((na-1.)*workflxs(1:nxmrf,1:nymrf,8) &
             +mrfflxs(1:nxmrf,1:nymrf,10)-mrfflxs(1:nxmrf,1:nymrf,11) &
             +short_wave)/na
        workflxs(1:nxmrf,1:nymrf,9)=((na-1.)*workflxs(1:nxmrf,1:nymrf,9) &
             +mrfflxs(1:nxmrf,1:nymrf,15))/na
        workflxs(1:nxmrf,1:nymrf,10)=((na-1.)*workflxs(1:nxmrf,1:nymrf,10) &
             +mrfflxs(1:nxmrf,1:nymrf,14)-t0)/na
        workflxs(1:nxmrf,1:nymrf,11)=((na-1.)*workflxs(1:nxmrf,1:nymrf,11) &
             +mrfflxs(1:nxmrf,1:nymrf,16))/na
        ! ilya dbgz: NOTE: very rough correction for the ice.
        where (workflxs(1:nxmrf,1:nymrf,10)<= t_f) 
           workflxs(1:nxmrf,1:nymrf,3) = t_f + workflxs(1:nxmrf,1:nymrf,3) &
                - workflxs(1:nxmrf,1:nymrf,10)
           workflxs(1:nxmrf,1:nymrf,10)= t_f
        endwhere
        !dbgz
        workflxs(1:nxmrf,1:nymrf,10)=max(workflxs(1:nxmrf,1:nymrf,10),t_f)
        workflxs(1:nxmrf,1:nymrf,3)=max(workflxs(1:nxmrf,1:nymrf,3),t_f)

     end if
     if (global) call global_extend(workflxs)
     avend: if (na==namax) then
        !
        !     Loop over fluxes
        ! 
        flxloop: do i=1,nhycom
           !       Extrapolate field in MRF grid to cover all ocean in HYCOM grid
           call mask_mrf(nextrap,msk_in,workflxs(:,:,i),mskmrf_tmp,workflxs(:,:,i))
           !
           !       Interpolate 
           !(NOTE: later make calls to more efficient multiple interpolation procedures).
           !
           call horiz_intp_(workflxs(:,:,i),hycomflxp(:,:,i),    &
                exhycom2d(:,:),eyhycom2d(:,:))
           write(*,*) 'fld: i, min, max = ',i,minval(hycomflxp(:,:,i)),maxval(hycomflxp(:,:,i)) !dbgzp


           if ( (avg3 == 1) .and. (avgflxs1(i) == 1) ) then
              ! averaged fields: m=1  xp field is assumed to be for time  -3Z
              if(m.eq.1) then
                 hycomflxs(:,:,i) = hycomflxp(:,:,i)
                 hycomflxb(:,:,i) = hycomflxp(:,:,i) !xb field used for m==2
                 ! averaged fields: m=2 is  m=1  xp field is assumed to be for time  0Z
                 !                               and it is instanteneous
              elseif(m.eq.2) then
                 hycomflxs(:,:,i) = (2./3.0)*hycomflxp(:,:,i) &
                      +(1./3.)*hycomflxb(:,:,i)
                 ! averaged fields: m>2 and m-odd  xp field is assumed to be for
                 !                                three hours averaged.
                 !                       and m-even xp field is assumed to be 
                 !                                six hours averaged.
              elseif ( mod(m,2) == 0) then
                 hycomflxs(:,:,i) = 2.*hycomflxp(:,:,i) - hycomflxb(:,:,i)
              else 
                 hycomflxs(:,:,i) = hycomflxp(:,:,i)
                 hycomflxb(:,:,i) = hycomflxp(:,:,i) !xb field is used for m-even
              endif
           else
              hycomflxs(:,:,i) = hycomflxp(:,:,i)
           endif

           ! project vector given in zonal and meridional directions
           ! along x and y hycom grid directions.
           if (vecflxs1(i)==1) then
              ! save zonal 
              hycomflxu=hycomflxs(:,:,i)
           elseif (vecflxs1(i)==2) then
              ! meridional
              do jjj=1,nyhycom
                 do iii=1,nxhycom
                    maxspeed=max(hycomflxu(iii,jjj),hycomflxs(iii,jjj,i))
                    if (maxspeed>bignumber) then
                       hycomflxu(iii,jjj)=maxspeed
                       hycomflxv(iii,jjj)=maxspeed
                    else
                       cs=cos(anhycom2d(iii,jjj))
                       ss=sin(anhycom2d(iii,jjj))
                       hycomflxv(iii,jjj)= hycomflxs(iii,jjj,i)*cs-    &
                            hycomflxu(iii,jjj  )*ss    
                       hycomflxu(iii,jjj)=  hycomflxs(iii,jjj,i)*ss+    &
                            hycomflxu(iii,jjj  )*cs    
                       if (wslocal==1 .and. i==2 ) then !this is the wind stress
                          speed=max(minspd,sqrt(hycomflxu(iii,jjj)**2+hycomflxv(iii,jjj)**2))
                          cdval=min(cd(speed),cdmax)*speed
                          !here we change sign to compensate for a change in sign in the averaging loop
                          hycomflxu(iii,jjj)=-  cdval*hycomflxu(iii,jjj)
                          hycomflxv(iii,jjj)=-  cdval*hycomflxv(iii,jjj)
                       endif
                    endif
                 enddo
              enddo
           endif
           !
           !       Write air-sea fluxes in HYCOM format (.a and .b files).
           !
           !ilya May 23, 2003: msk_in instead of msk_out
           lua=10+2*(i-1) ; lub=lua+1001
           opengate: if (open_gate==1) then
              if(i==nhycom)open_gate=0
              if(mapflg==mapflg_mer) then
                 call start_abfile(lua,lub &
                      ,trim(hyflxs1(i)),nxhycom,nyhycom &
                      ,reflon=reflon,pntlat=pntlat,gridsz=gridsz)
              elseif (mapflg==mapflg_ort) then
                 call start_abfile(lua,lub &
                      ,trim(hyflxs1(i)),nxhycom,nyhycom &
                      ,gridname=' orthogonal')
              elseif (mapflg==mapflg_tripol) then
                 call start_abfile(lua,lub &
                      ,trim(hyflxs1(i)),nxhycom,nyhycom &
                      ,gridname=' tripolar')
              endif
           endif opengate
           ifclimo: if (namax.eq.namax_climo) then
              if (vecflxs1(i)==0) then
                 call write_abrecord(hycomflxs(:,:,i),imsk_hycom,.true.,lua,lub,.false. &
                      ,hyflxs1(i),mnth=num_month)
              elseif(vecflxs1(i)==2) then
                 call write_abrecord(hycomflxu(:,:),imsk_hycom,.true.,lua1,lub1,.false. &
                      ,hyflxs1(i-1),mnth=num_month)
                 call write_abrecord(hycomflxv(:,:),imsk_hycom,.true.,lua,lub,.false. &
                      ,hyflxs1(i),mnth=num_month)
              elseif(vecflxs1(i)==1) then
                 lua1=lua
                 lub1=lub
              endif
              print *,' '
           else
              ftime=htime(m)+off_time(i)
              if (vecflxs1(i)==0) then
                 write(*,*) '4: imsk_hycom min, max = ',minval(imsk_hycom),maxval(imsk_hycom) ! dbgz
                 call write_abrecord(hycomflxs(:,:,i),imsk_hycom,.true.,lua,lub,.false. &
                      ,hyflxs1(i),ftime=ftime)
              elseif (vecflxs1(i)==2) then
                 call write_abrecord(hycomflxu(:,:),imsk_hycom,.true.,lua1,lub1,.false. &
                      ,hyflxs1(i-1),ftime=ftime)
                 call write_abrecord(hycomflxv(:,:),imsk_hycom,.true.,lua,lub,.false. &
                      ,hyflxs1(i),ftime=ftime)
              elseif(vecflxs1(i)==1) then
                 lua1=lua
                 lub1=lub
              endif
              print *,' '
           endif ifclimo
           if (m==ntime) then
              if (vecflxs1(i)==0) then
                 close(lub)
                 call zaiocl(lua)
              elseif(vecflxs1(i)==2) then
                 close(lub)
                 call zaiocl(lua)
                 close(lub1)
                 call zaiocl(lua1)
              endif
           endif
        end do flxloop
        !
     end if avend
     na=na+1
     !
  end do timeloop

  !
  if (allocated(hycomflxs)) deallocate(hycomflxs)
  if (allocated(exhycom)) deallocate(exhycom)
  if (allocated(eyhycom)) deallocate(eyhycom)
  if (allocated(hyflxs1)) deallocate(hyflxs1)
  if (allocated(vecflxs1)) deallocate(vecflxs1)
  if (allocated(mrfflx)) deallocate(mrfflx)
  if (allocated(msk_in)) deallocate(msk_in)
  if (allocated(exmrf)) deallocate(exmrf)
  if (allocated(eymrf)) deallocate(eymrf)
  if (allocated(workflxs)) deallocate(workflxs)
  if (allocated(hycomflxu)) deallocate(hycomflxu)
  if (allocated(hycomflxv)) deallocate(hycomflxv)
  if (allocated(htime)) deallocate(htime)
  if (allocated(ctime)) deallocate(ctime)
  if (allocated(mrfnames)) deallocate(mrfnames)
  if (allocated(off_time)) deallocate(off_time)
  if (allocated(avgflxs1)) deallocate(avgflxs1)
  if (allocated(hycomflxp)) deallocate(hycomflxp)
  if (allocated(hycomflxb)) deallocate(hycomflxb)
  if (allocated(exhycom2d)) deallocate(exhycom2d)
  if (allocated(eyhycom2d)) deallocate(eyhycom2d)
  if (allocated(qxhycom2d)) deallocate(qxhycom2d)
  if (allocated(qyhycom2d)) deallocate(qyhycom2d)
  if (allocated(anhycom2d)) deallocate(anhycom2d)

  !
  !     Comparing masks
  !
  if (dbgn==3) then
     allocate (imsk_intp(1:nxhycom,1:nyhycom))
     ndiff=0
     where (msk_out>=mskfrac)
        imsk_intp=1
     elsewhere 
        imsk_intp=0 
     endwhere
     do j=1,nyhycom
        do i=1,nxhycom
           if(imsk_intp(i,j)==0.and.imsk_hycom(i,j)==1) then
              ndiff=ndiff+1
              write (*,'(''msk_out, imsk_intp,imsk_hycom,i,j'',e13.6,f10.3,2i2,2i5)') &
                   msk_out(i,j),imsk_intp(i,j),imsk_hycom(i,j),i,j
           end if
        end do
     end do
     write(*,*)'ndiff=',ndiff                       
     write(*,*)'msk_in:max,min=',maxval(msk_in),minval(msk_in)
     write(*,*)'msk_out:max,min=',maxval(msk_out),minval(msk_out)
     write(*,*)'real(imsk_intp):max,min=',maxval(real(imsk_intp)),minval(real(imsk_intp))
     write(*,*)'real(imsk_hycom):max,min=',maxval(real(imsk_hycom)),minval(real(imsk_hycom))
     open (unit=700,file='masks',form="unformatted")
     write(700) real(imsk_intp(:,:)),msk_out(:,:),real(imsk_hycom(:,:))
     close(700)
     !
     call dumpi(701,imsk_hycom,nxhycom,nyhycom,'mask_hycom')
     call dumpi(702,int(msk_in),nxmrf,nymrf,'mask_mrf')
     call dumpr(703,msk_out,nxhycom,nyhycom,'mask_out')
     call dumpr(704,exhycom,nxhycom+1,1,'exhycom')
     call dumpr(705,eyhycom,nyhycom+1,1,'eyhycom')
     call dumpr(706,exmrf,nxmrf+1,1,'exmrf')
     call dumpr(707,eymrf,nymrf+1,1,'eymrf')
  endif
  if (allocated(msk_out)) deallocate(msk_out)
  if (allocated(imsk_hycom)) deallocate (imsk_hycom)
  stop
  !
  ! =========================================
  !    
contains
  !
  !  =========================================
  !    
  subroutine write_abrecord(fld,msk,lmask,lua,lub,lreal4,hyflx,mnth,ftime)

    ! NOTE (ilya): move subrotine to hycomio module
    real, dimension(:,:), intent(inout) :: fld
    integer, dimension(:,:), intent(in) :: msk
    integer, intent(in) :: lua,lub
    logical, intent(in):: lmask,lreal4
    character(len=*), intent(in) :: hyflx
    real, optional, intent(in) :: ftime
    integer, optional, intent(in) :: mnth
    real :: fldmin,fldmax
    character(len=300) :: bfmt
    ! write(*,*) 'fld: min, max = ',minval(fld),maxval(fld) !dbgzp
    ! write(*,*) 'msk: min, max = ',minval(msk),maxval(msk) !dbgzp
    ! write(*,*) ' lua,lmask,lreal4= ', lua,lmask,lreal4 !dbgzp
    ! write(*,*) 'fldmin,fldmax = ',fldmin,fldmax  !dbgzp
    ! write(*,*) ' size(fld)= ',size(fld) !dbgzp
    ! write(*,*) ' size(msk)= ',size(msk) !dbgzp
    call zaiowr(fld,msk,lmask, fldmin,fldmax, lua,lreal4)
    if (present(mnth)) then
       bfmt='(A,'': month,range = '',I10,1P2E16.7)'
       write(lub,trim(bfmt))'   '//trim(hyflx),mnth,fldmin,fldmax
       write(*,trim(bfmt))'   '//trim(hyflx),mnth,fldmin,fldmax
    elseif(present(ftime)) then
       bfmt='(A,'': date,span,range = '',F10.2,'' 0 '',1P2E16.7)'
       write(lub,trim(bfmt))'   '//trim(hyflx),ftime,fldmin,fldmax
       write(*,trim(bfmt))'   '//trim(hyflx),ftime,fldmin,fldmax
    else 
       write(*,*) 'ERROR: dont know time in .b writing'
       stop
    endif
  end subroutine write_abrecord
  !
  !  =========================================
  !    
  subroutine start_abfile(lua,lub,fldname,nxhycom,nyhycom,reflon,pntlat,gridsz,gridname)
    ! NOTE (ilya): move subrotine to hycomio module
    integer, intent(in) :: lua,lub,nxhycom,nyhycom
    real, optional, intent(in) :: reflon,pntlat,gridsz
    character (len=*), intent(in) :: fldname
    character (len=*),optional, intent(in) :: gridname
    character (len=10) big_ben(3)
    character:: preambl(5)*79
    integer, dimension(8) :: date_time
    integer :: ii
    call date_and_time (big_ben(1),big_ben(2),big_ben(3),date_time)
    open (unit=lub,file='forcing.'//trim(fldname)//'.b',status='new' ,action='write')
    write(preambl(1),'(a,i4,''-'',i2,''-'',i2,2x,''['',i5,'']'',2x,i2,'':'',i2)') &
         'GDAS derived '//trim(fldname)//' created ',(date_time(ii),ii=1,6)
    do ii=2,4 
       preambl(ii) = ' ' 
    end do
    if(present(reflon) .and. present(pntlat) .and. present(gridsz) ) then
       write(preambl(5),'(a,2i5,f9.3,f9.2,f6.3)') &
            'i/jdm,reflon,pntlat,gridsz =', &
            nxhycom,nyhycom,reflon,pntlat,gridsz
    elseif (present(gridname)) then
       write(preambl(5),'(a,2i5,a)')'i/jdm =',nxhycom,nyhycom,gridname
    else
       write(*,*) 'ERROR: dont know how to write .b preambule'
       stop
    endif
    write(lub,'(A79)') preambl
    call zaiopf('forcing.'//trim(fldname)//'.a','new' , lua)
  end subroutine start_abfile
  !
  !  =========================================
  !    
end program intp
