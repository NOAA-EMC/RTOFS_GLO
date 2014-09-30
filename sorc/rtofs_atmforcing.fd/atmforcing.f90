PROGRAM atmforcing
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

  USE mod_hycomio1, ONLY: mask_hycom_1,write_abrecord,start_abfile
  USE mod_za,ONLY : xcspmd,zaiost,zaiocl,idm,jdm
  USE mod_hytime
  USE mod_dump, ONLY: dumpi,dumpr
  USE mod_geom
  USE mod_grib2io
  USE mod_flags

  !
  ! atmflxs: 1 is zonal momentum flux            [N/m^2]
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
  !         17 is land mask                      [land=1;sea=0]
  !
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
  !
  ! i is changing from west (1) to east (nx)
  ! j is changing from north (1) to south (ny)
  !
  IMPLICIT NONE
  !
  NAMELIST /intp_pars/ dbgn,avg3,wslocal,global,serial,hr3_avg
  INTEGER:: dbgn,num_month,avg3,wslocal
  LOGICAL :: global,serial,hr3_avg
  !
  !dbgn      debugging =0 - no dbg; =1,2,3 - add output
  !avg3	   if avg3 = 1, then averaged fluxes are converted to instantaneous fields
  !wslocal   if  wslocal = 1, then wind stress are computed from wind velocities
  !
  !
  ! Constants:
  !
  REAL,PARAMETER::      &
       t0=273.17        &
       ,t_f=-1.8        &  ! freezing temperature  
       ,evaplh=2.47e6   &  ! latent heat of evaporation (j/kg)
       ,thref =1.0e-3   &  ! reference value of specific volume (m**3/kg) 
       ,minspd=3.0	&  ! minimum value of speed for calculating cd
       ,cdmax=0.0025	&  ! max value of cd
       ,bignumber=1.e20   
  INTEGER, PARAMETER :: &
       natm=16          &
       ,nhycom=10       &
       ,nextrap_max=45  &
!       ,nextrap_max=23  &
       ,lp=6               ! output unit number
  !
  INTEGER :: nxhycom,mapflg,nyhycom,i,j,iii,jjj,k,n,m,ntime &
       ,nxatm,nyatm,nxatm_prev,nyatm_prev,nxatm2,nyatm2 &
       ,hh1,hh2,ndiff,nextrap,lua,lub,ii,edges=0 &
       ,lua1,lub1,iparm,jdiscno &
       ,ixmin,ixmax,jymin,jymax, nshift,jpdtno,xpts,ypts
  INTEGER, DIMENSION(:), ALLOCATABLE :: avgflxs1
  INTEGER, DIMENSION(:,:), ALLOCATABLE:: imsk_intp,imsk_hycom
  INTEGER, DIMENSION(4,natm) :: kpds567
  INTEGER, DIMENSION(200) :: gds 
  INTEGER, DIMENSION(4) :: kpds
  INTEGER, DIMENSION(natm) :: jdisc_num,jpdt_num
  REAL ::  ftime,mskfrac=0.99,maxspeed,speed,cdval,plon0
  REAL, DIMENSION(:), ALLOCATABLE :: exhycom,eyhycom,exatm,eyatm,htime,off_time
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       exhycom2d,eyhycom2d,workflx,qxhycom2d,qyhycom2d,anhycom2d,atmflx ,msk_in &
       ,msk_out,mskatm_tmp,latent,short_wave,cs,ss &
       ,hycomflxv,hycomflxu,hycomflx,hycomflxp
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: atmflxs,hycomflxb
  CHARACTER (len=6), DIMENSION(:),ALLOCATABLE :: hyflxs1
  CHARACTER (len=10), DIMENSION (:),ALLOCATABLE :: ctime
  CHARACTER (len=400), DIMENSION (:),ALLOCATABLE :: atmnames

  LOGICAL :: clsgrib,grid_file_exist=.FALSE.,atm_grid_changed
  real,dimension(5000000)      :: xgfld
! With pressue
  data jdisc_num/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2/
! Without pressure
!  data jdisc_num/0,0,0,0,0,0,0,0,0,0,0,0,0,0,2/
  !
  !
  ! Read parameters and allocate arrays
  !
  OPEN (7,file='intp_pars.dat')
  READ(7,intp_pars)
  CLOSE(7)
  open (27,file='jpdt_table.dat')
  read(27,*)(jpdt_num(i),i=1,natm)
  close(27)


  WRITE(*,intp_pars); CALL flush(lp)
  ! check if regional.grid.[ab] exists and read mapflg. nxhycom,nyhycom will be 
  ! redefined few lines later after call xcspmd
  CALL rd_hycom_grid_params(nxhycom,nyhycom,mapflg,grid_file_exist)
  IF(grid_file_exist) THEN
     WRITE (*,*) ' ---- Files regional.grid.[ab] are found'
     !  initialize SPMD processsing
     CALL xcspmd  !input idm,jdm by use association
     nxhycom=idm
     nyhycom=jdm
  ELSE
     WRITE(*,*) 'ERROR: regional.grid.[ab] are not found'
     STOP
  ENDIF
  IF (mapflg/=mapflg_tripol) THEN
     WRITE (*,*) ' ---- ERROR: unknown mapflg=',mapflg
     STOP
  ENDIF

  WRITE(*,*) ' --- FINALLY: nxhycom=',nxhycom,' nyhycom=',nyhycom,' mapflg=',mapflg
  ! initialize array i/o.
  CALL zaiost

  OPEN (33,file='listflx.dat',form='formatted')
  READ(33,*) ntime
  write(*,*) "  ntime  ",ntime
  ALLOCATE (htime(ntime) &
       ,ctime(ntime) &
       ,atmnames(ntime))
  READ(33,'(a10,1x,a400)') (ctime(i),atmnames(i),i=1,ntime)
  CLOSE(33)
  DO i=1,ntime
     iii=INDEX(atmnames(i),"<") -1
     atmnames(i)=atmnames(i)(1:iii)
  ENDDO
  WRITE(*,*) '# of times:',ntime
  WRITE(*,*)'  Time     File name'
  WRITE (*,'(a10," ",a)') (ctime(i),TRIM(atmnames(i)),i=1,ntime)
  CALL flush(lp)

  !   "HYCOM" time
  DO i =1,ntime 
     CALL hytime(ctime(i),htime(i)) 
     WRITE (*,*) htime(i), ctime(i) 
  END DO

  !allocate   offset of time marks
  ALLOCATE(off_time(1:nhycom))
  !allocate arrays on HYCOM grid
  ALLOCATE ( &
       hycomflx(1:nxhycom,1:nyhycom) &
       ,msk_out(1:nxhycom,1:nyhycom) &
       ,hyflxs1(nhycom) &
       ,hycomflxu(1:nxhycom,1:nyhycom) &
       ,hycomflxv(1:nxhycom,1:nyhycom) &
       ,avgflxs1(nhycom) &
       ,hycomflxp(1:nxhycom,1:nyhycom) &
       ,hycomflxb(1:nxhycom,1:nyhycom,1:20) &
       ,exhycom2d(nxhycom,nyhycom) &
       ,eyhycom2d(nxhycom,nyhycom) &
       ,qxhycom2d(nxhycom,nyhycom) &
       ,qyhycom2d(nxhycom,nyhycom) &
       ,imsk_hycom(1:nxhycom,1:nyhycom) &
       ,anhycom2d(nxhycom,nyhycom) &
       ,cs(nxhycom,nyhycom) &
       ,ss(nxhycom,nyhycom) &
       )

  WRITE (*,*) 'HYCOM Files are allocated' ; CALL flush(lp)
  hyflxs1=(/'tauewd','taunwd','airtmp','vapmix','precip','wndspd','shwflx','radflx' &
       ,'presur','surtmp'/)

  IF (hr3_avg) THEN
     off_time=-1.5/24.0  !averaged fields are off set by half hour
  ELSE 
    off_time=0.0
  ENDIF

  avgflxs1=1
  IF (wslocal==1 ) THEN
     avgflxs1(1:2)=0
  ENDIF
  avgflxs1(3:4)=0
  avgflxs1(6)=0
  avgflxs1(9:10)=0

  kpds567=RESHAPE (source=  &
       (/002, 017, 001, 000  &  ! UFLX        1
       ,002,  018, 001, 000  &  ! VFLX        2
       ,000,  000, 103, 002  &  ! TMP 2m      3
       ,001,  000, 103, 002  &  ! SPFH 2m     4
       ,001,  007, 001, 000  &  ! PRATE       5
       ,002,  002, 103, 010  &  ! UGRD 10m    6
       ,002,  003, 103, 010  &  ! VGRD 10m    7
       ,000,  011, 001, 000  &  ! SHTFL       8
       ,000,  010, 001, 000  &  ! LHTFL       9
       ,005,  192, 001, 000  &  ! DLWRF      10
       ,005,  193, 001, 000  &  ! ULWRF      11
       ,004,  192, 001, 000  &  ! DSWRF      12
       ,004,  193, 001, 000  &  ! USWRF      13
       ,000,  000, 001, 000  &  ! TMP 0m     14
       ,003,  000, 001, 000  &  ! PRES       15
       ,000,  000, 001, 000 /) &  ! LAND     16
!       ,003,  001, 101  &  ! PRES      16 
       ,shape = (/ 4,natm /) )  
!!!       ,  2, 102 , 0    &  ! PRMSL      16


  ! 
  !Compute HYCOM land/sea mask from regional.depth.a (land=0,sea=1)
  !
!  PRINT *,'--- Preparing HYCOM mask'; CALL flush(lp)
  CALL mask_hycom_1(imsk_hycom)

  !write(*,*) '1: imsk_hycom min, max = ',minval(imsk_hycom),maxval(imsk_hycom) ! dbgzp
  !!write(700,'(i13)') imsk_hycom ! dbgzp
  !
  ! Prepare HYCOM grid 
  !
  WRITE(*,*) '----Prepare HYCOM grid'
  IF (grid_file_exist) THEN 
     !write(*,*) '1a: imsk_hycom min, max = ',minval(imsk_hycom),maxval(imsk_hycom) ! dbgzp
     ALLOCATE(imsk_intp(1:nxhycom,1:nyhycom))
     imsk_intp=imsk_hycom
     CALL hycom_global_tripolar(imsk_intp,imsk_hycom,anhycom2d,qxhycom2d,qyhycom2d,plon0,.TRUE.)
     cs=COS(anhycom2d)
     ss=SIN(anhycom2d)
     global=.TRUE.
     !write(*,*) '1b: imsk_hycom min, max = ',minval(imsk_hycom),maxval(imsk_hycom) ! dbgzp
  ELSE
     WRITE(*,*) 'ERROR: no regional.grid.[ab] files for mapflg=',mapflg
     CALL flush(lp)
     STOP
  ENDIF
  !
  ! read GFS flux
  !
  PRINT *,'--- STARTING TIME LOOP *** '; CALL flush(lp)
  !
  ! Time loop
  !
  num_month=1
  timeloop: DO m=1,ntime

     WRITE (*,*) '<--------- '//TRIM(ctime(m))//' ********'
     PRINT *, "Inside Time Loop ",m
     !
     !   Get GFS grid parameters (before  20021029.t12Z: 512x256, after: 768x384)
!     CALL getgds(81,TRIM(atmnames(m)),gds)
     
     jdiscno=0
     call rdgrib(81,TRIM(atmnames(m)),xgfld,kpds,jpdtno,jdiscno,0,xpts,ypts)
     nxatm=xpts ; nyatm=ypts
     print *, "1st rdgrib ",nxatm,"  ypts  ",nyatm
     nxatm2=nxatm/2 ; nyatm2=nyatm/2
     IF (m==1) THEN
        nxatm_prev=nxatm ; nyatm_prev=nyatm
        atm_grid_changed=.TRUE.
     ELSE
        IF (nxatm==nxatm_prev .AND. nyatm==nyatm_prev ) THEN
           atm_grid_changed=.FALSE.
        ELSE
           nxatm_prev=nxatm ; nyatm_prev=nyatm
           atm_grid_changed=.TRUE.
        ENDIF
     ENDIF
     PRINT *, 'm,nxatm_prev,nxatm,nyatm_prev,nyatm,atm_grid_changed=' &
          ,m,nxatm_prev,nxatm,nyatm_prev,nyatm,atm_grid_changed
     atmgrid_changed: IF (atm_grid_changed) THEN
        WRITE(*,'(''GFS grid dimensions nxatm,nyatm,nxatm2,nyatm2: '',4i5)') &
             nxatm,nyatm,nxatm2,nyatm2
        !     allocate arrays on GFS grid and GFS fluxes
        IF (m>1) DEALLOCATE(atmflx &
             ,latent &
             ,short_wave &
             ,msk_in &
             ,workflx &
             ,mskatm_tmp &
             ,exatm &
             ,eyatm &
             ,atmflxs &
             )

        ALLOCATE ( &
             atmflx(1:nxatm,1:nyatm) &
             ,latent(1:nxatm,1:nyatm) &
             ,short_wave(1:nxatm,1:nyatm) &
             ,atmflxs(1:nxatm,1:nyatm,1:natm) &
             )
        IF (global) THEN
           ALLOCATE ( &
                msk_in(1:nxatm+2,1:nyatm+2) &
                ,workflx(1:nxatm+2,1:nyatm+2) &
                ,mskatm_tmp(1:nxatm+2,1:nyatm+2) &
                ,exatm(1:nxatm+2+edges) & 
                ,eyatm(1:nyatm+2+edges) &
                )
        ELSE
           ALLOCATE ( &
                msk_in(1:nxatm,1:nyatm) &
                ,workflx(1:nxatm,1:nyatm) &
                ,mskatm_tmp(1:nxatm,1:nyatm) &
                ,exatm(1:nxatm+edges) & 
                ,eyatm(1:nyatm+edges) &
                )
        ENDIF
        IF (m==1) workflx=0.
        ! print *, 'nyatm,edges,nyatm+edges=',nyatm,edges,nyatm+edges !dbgzp
        !
        !     Calculate GFS grid  ( coordinates are supposed to increase )
        ! Ilya: replace later with reading from GRIB gds
        !
        CALL atm_gaussian(exatm(2:nxatm+1+edges),eyatm(2:nyatm+1+edges),edges,mapflg_tripol,plon0,nshift,dbgn)
        PRINT *,'plon0,nshift=',plon0,nshift
        ! write(*,*) 'before extend: eyatm****' !dbgzp
        ! write(*,'(f13.6)') eyatm*57.2957795 !dbgzp
        ! write(*,*) 'before extend again: eyatm****' !dbgzp
        ! write(*,'(a,i5,f13.6)') ('before extend again: eyatm',i,eyatm(i)*57.2957795,i=1,size(eyatm)) !dbgzp
        IF (global) CALL global_extend(exatm,eyatm)
        print *, 'exatm: min,max=',minval(exatm)*57.2957795,maxval(exatm)*57.2957795 !dbgzp
        print *, 'eyatm: min,max=',minval(eyatm)*57.2957795,maxval(eyatm)*57.2957795 !dbgzp
        print *, 'size(exatm),size(eyatm)=',size(exatm),size(eyatm) !dbgzp
        print *, 'exatm: 1,nx=',exatm(1)*57.2957795,exatm(size(exatm))*57.2957795 !dbgzp
        print *, 'eyatm: 1,nx=',eyatm(1)*57.2957795,eyatm(size(eyatm))*57.2957795 !dbgzp
        PRINT *,'min(qxhycom2d), max(qxhycom2d) [in degrees] =',MINVAL(qxhycom2d)*57.2957795, MAXVAL(qxhycom2d)*57.2957795 !dbgzp 
        PRINT *,'min(qyhycom2d), max(qyhycom2d) [in degrees] =',MINVAL(qyhycom2d)*57.2957795, MAXVAL(qyhycom2d)*57.2957795 !dbgzp 
        PRINT *,'min(qxhycom2d), max(qxhycom2d) [in 57.2957795] =',MINVAL(qxhycom2d), MAXVAL(qxhycom2d) !dbgzp 
        PRINT *,'min(qyhycom2d), max(qyhycom2d) [in 57.2957795] =',MINVAL(qyhycom2d), MAXVAL(qyhycom2d) !dbgzp 
        !
        !     Pre-compute interpolation indices and weights for multiple interpolations
        !
        CALL horiz_intp(exatm,eyatm,qxhycom2d,qyhycom2d,exhycom2d,eyhycom2d,lp,global=global)
        !
        !     Establish GFS mask (land=0,sea=1). 
        !
        PRINT *,'--- Changing GFS mask'; CALL flush(lp)
        CALL mask_atm(82,TRIM(atmnames(1)),kpds567(:,natm),mskfrac,nextrap_max,msk_in &
             ,imsk_hycom,mskatm_tmp,nextrap,mapflg,exhycom2d,eyhycom2d,global=global)
        IF(nextrap>=nextrap_max) THEN
           PRINT *,'ERROR: nextrap>=nextrap_max, nextrap=',nextrap,' nextrap_max=',nextrap_max
        ENDIF
     ENDIF atmgrid_changed
     ! 
     !   Read GFS fluxes from GRIB
     !
     clsgrib=.FALSE.
     DO i=1,natm-1 
        jpdtno=jpdt_num(i)
        jdiscno=jdisc_num(i)
        IF (i==natm-1) clsgrib=.TRUE.
        kpds=kpds567(:,i)
        IF( wslocal==1 .AND. i==1) THEN  !use zonal wind
           kpds=kpds567(:,6)
           jpdtno=jpdt_num(6)
           jdiscno=jdisc_num(6)
        ELSEIF( wslocal==1 .AND. i==2) THEN !use meridional wind
           kpds=kpds567(:,7)
           jpdtno=jpdt_num(7)
           jdiscno=jdisc_num(7)
        ENDIF
! Get jpdt from the script

        CALL rdgrib(82,TRIM(atmnames(m)),xgfld,kpds,jpdtno,jdiscno,1,xpts,ypts)  
!        print *, "After 2nd rdgrib "
!        write(45,*)" kpds ",kpds,jpdtno,jdiscno
        atmflx=reshape(source=xgfld,shape=SHAPE(atmflx))
        IF (mapflg==mapflg_tripol) THEN
           DO j=1,nyatm
              atmflxs(1:nxatm-nshift,j,i) =atmflx(nshift+1:nxatm,j)
              atmflxs(nxatm-nshift+1:nxatm,j,i) = atmflx(1:nshift,j)     
           ENDDO
        ELSE
           atmflxs(:,:,i)=atmflx
        ENDIF
        !180! atmflxs(1:nxatm2      ,:,i)=atmflx(nxatm2+1:nxatm,:)
        !180! atmflxs(nxatm2+1:nxatm,:,i)=atmflx(1:nxatm2      ,:)
        PRINT*,'GFS fluxes: i,min,max=',i,MINVAL(atmflxs(:,:,i)),MAXVAL(atmflxs(:,:,i))
     END DO
     !
     !   Intermediate ATM fluxes 
     !
     latent=atmflxs(1:nxatm,1:nyatm,9) ; WHERE(latent<0.)latent=0.   ! no condensation
     short_wave=atmflxs(1:nxatm,1:nyatm,12)-atmflxs(1:nxatm,1:nyatm,13)
     IF (global) THEN
        ixmin=2
        jymin=2
        ixmax=nxatm+1
        jymax=nyatm+1
     ELSE
        ixmin=1
        jymin=1
        ixmax=nxatm
        jymax=nyatm
     END IF
     !
     !     Loop over fluxes
     ! 
     flxloop: DO i=1,nhycom
        WRITE(*,*)'INTERPOLATION FOR FLUX I= ',i,' NAME=',hyflxs1(i) &
             ,' AT TIME STEP M=',m,' TIME=',htime(m)+off_time(i) &
             ,' HTIME=',htime(m),' OFFTIME=',off_time(i)
        ! IMPORTANT : temperature and humidity are at 2m, not 10m
        wflx: SELECT CASE (i)
        CASE(1:2)
           workflx(ixmin:ixmax,jymin:jymax)=-atmflxs(1:nxatm,1:nyatm,i)
        CASE(3)
           workflx(ixmin:ixmax,jymin:jymax)=atmflxs(1:nxatm,1:nyatm,3)-t0
           !dbgz : to limit fluxes above ice
           !workflx(ixmin:ixmax,jymin:jymax)=max(workflx(ixmin:ixmax,jymin:jymax),t_f)
        CASE(4)
           workflx(ixmin:ixmax,jymin:jymax)=atmflxs(1:nxatm,1:nyatm,4)
        CASE(5)
           workflx(ixmin:ixmax,jymin:jymax)=atmflxs(1:nxatm,1:nyatm,5)*thref
        CASE(6)
           workflx(ixmin:ixmax,jymin:jymax)=SQRT(atmflxs(1:nxatm,1:nyatm,6)**2 &
                +atmflxs(1:nxatm,1:nyatm,7)**2)
        CASE(7)
           workflx(ixmin:ixmax,jymin:jymax)=short_wave
        CASE(8)
           workflx(ixmin:ixmax,jymin:jymax)=&
                atmflxs(1:nxatm,1:nyatm,10)-atmflxs(1:nxatm,1:nyatm,11)+short_wave
        CASE(9)
           workflx(ixmin:ixmax,jymin:jymax)=atmflxs(1:nxatm,1:nyatm,15)
        CASE(10)
           workflx(ixmin:ixmax,jymin:jymax)=atmflxs(1:nxatm,1:nyatm,14)-t0
           !dbgz : to limit fluxes above ice
           !workflx(ixmin:ixmax,jymin:jymax)=max(workflx(ixmin:ixmax,jymin:jymax),t_f)
        CASE default
           WRITE (*,*) ' ---- ERROR: unknown flux number i=',i
           STOP
        END SELECT wflx

        !> NOTE: KEEP AS CLUE
        !> where (workflxs(ixmin:ixmax,jymin:jymax,10)<= t_f) 
        !>    workflxs(ixmin:ixmax,jymin:jymax,3) = t_f + workflxs(ixmin:ixmax,jymin:jymax,3) &
        !>         - workflxs(ixmin:ixmax,jymin:jymax,10)
        !>    workflxs(ixmin:ixmax,jymin:jymax,10)= t_f
        !> endwhere
        IF (global) CALL global_extend(workflx)
        !       Extrapolate field in ATM grid to cover all ocean in HYCOM grid
        CALL mask_atm(nextrap,msk_in,workflx(:,:),mskatm_tmp,workflx(:,:))
        !
        !       Interpolate 
        !(NOTE: later make calls to more efficient multiple interpolation procedures).
        !
        CALL horiz_intp_(workflx(:,:),hycomflxp(:,:),exhycom2d(:,:),eyhycom2d(:,:))
        ! write(*,*) 'atmfld: m, i, min, max = ',m,i,minval(workflx(:,:)),maxval(workflx(:,:)) !dbgzp
        ! write(*,*) 'ocefld: m, i, min, max = ',m,i,minval(hycomflxp(:,:)),maxval(hycomflxp(:,:)) !dbgzp

        averaging: IF ( (avg3 == 1) .AND. (avgflxs1(i) == 1) ) THEN
           ! averaged fields: m=1  xp field is assumed to be for time  -4.5Z
           IF(m.EQ.1) THEN
              hycomflx(:,:) = hycomflxp(:,:)
              hycomflxb(:,:,i) = hycomflxp(:,:) !xb field used for m==2
           ELSEIF ( MOD(m,2) == 0) THEN
              hycomflx(:,:) = 2.*hycomflxp(:,:) - hycomflxb(:,:,i)
           ELSE 
              hycomflx(:,:) = hycomflxp(:,:)
              hycomflxb(:,:,i) = hycomflxp(:,:) !xb field is used for m-even
           ENDIF
        ELSE
           IF(m.EQ.1) THEN
              ! instantaneous fields: m=1  xp field is assumed to be for time  -3Z
              hycomflx(:,:) = hycomflxp(:,:)
              hycomflxb(:,:,i) = hycomflxp(:,:) !xb field is used for m-even
           ELSE 
              hycomflx(:,:) = 0.5*(hycomflxp(:,:)+hycomflxb(:,:,i))
              hycomflxb(:,:,i) = hycomflxp(:,:) !xb field is used for next m
           ENDIF
        ENDIF averaging
        ! write(*,*) 'ocflda: m, i, min, max = ',m,i,minval(hycomflx(:,:)),maxval(hycomflx(:,:)) !dbgzp

        ! project vector given in zonal and meridional directions
        ! along x and y hycom grid directions.
        vecflx: SELECT CASE (i)
        CASE (1)
           ! save zonal 
           hycomflxu=hycomflx(:,:)
        CASE (2)          ! meridional
           DO jjj=1,nyhycom
              DO iii=1,nxhycom
                 maxspeed=MAX(hycomflxu(iii,jjj),hycomflx(iii,jjj))
                 IF (maxspeed>bignumber) THEN
                    hycomflxu(iii,jjj)=maxspeed
                    hycomflxv(iii,jjj)=maxspeed
                 ELSE
                    hycomflxv(iii,jjj)= hycomflx(iii,jjj)*cs(iii,jjj)-    &
                         hycomflxu(iii,jjj  )*ss(iii,jjj)   
                    hycomflxu(iii,jjj)=  hycomflx(iii,jjj)*ss(iii,jjj)+    &
                         hycomflxu(iii,jjj  )*cs(iii,jjj)   
                    IF (wslocal==1 ) THEN !this is the wind stress
                       speed=MAX(minspd,SQRT(hycomflxu(iii,jjj)**2+hycomflxv(iii,jjj)**2))
                       cdval=MIN(cd(speed),cdmax)*speed
                       !here we change sign to compensate for a change in sign in the averaging loop
                       hycomflxu(iii,jjj)=-  cdval*hycomflxu(iii,jjj)
                       hycomflxv(iii,jjj)=-  cdval*hycomflxv(iii,jjj)
                    ENDIF
                 ENDIF
              ENDDO
           ENDDO
        END SELECT vecflx
        !
        !       Write air-sea fluxes in HYCOM format (.a and .b files).
        !
        !ilya May 23, 2003: msk_in instead of msk_out
        lua=10+2*(i-1) ; lub=lua+1001
        IF (m==1) THEN
           CALL start_abfile(lua,lub &
                ,TRIM(hyflxs1(i)),nxhycom,nyhycom &
                ,gridname=' tripolar')
        ENDIF
        ftime=htime(m)+off_time(i)
        SELECT CASE (i)
        CASE(1)
           lua1=lua
           lub1=lub
        CASE(2)
           CALL write_abrecord(hycomflxu(:,:),imsk_intp,.TRUE.,lua1,lub1,.FALSE. &
                ,hyflxs1(i-1),ftime=ftime)
           CALL write_abrecord(hycomflxv(:,:),imsk_intp,.TRUE.,lua,lub,.FALSE. &
                ,hyflxs1(i),ftime=ftime)
        CASE default
           CALL write_abrecord(hycomflx(:,:),imsk_intp,.TRUE.,lua,lub,.FALSE. &
                ,hyflxs1(i),ftime=ftime)
        END SELECT
        IF (m==ntime) THEN
           SELECT CASE (i)
           CASE(2)
              CLOSE(lub)
              CALL zaiocl(lua)
              CLOSE(lub1)
              CALL zaiocl(lua1)
           CASE (3:nhycom)
              CLOSE(lub)
              CALL zaiocl(lua)
           END SELECT
        ENDIF
     END DO flxloop
     !
  END DO timeloop

  !
  IF (ALLOCATED(hycomflx)) DEALLOCATE(hycomflx)
  IF (ALLOCATED(exhycom)) DEALLOCATE(exhycom)
  IF (ALLOCATED(eyhycom)) DEALLOCATE(eyhycom)
  IF (ALLOCATED(hyflxs1)) DEALLOCATE(hyflxs1)
  IF (ALLOCATED(atmflx)) DEALLOCATE(atmflx)
  IF (ALLOCATED(msk_in)) DEALLOCATE(msk_in)
  IF (ALLOCATED(exatm)) DEALLOCATE(exatm)
  IF (ALLOCATED(eyatm)) DEALLOCATE(eyatm)
  IF (ALLOCATED(workflx)) DEALLOCATE(workflx)
  IF (ALLOCATED(hycomflxu)) DEALLOCATE(hycomflxu)
  IF (ALLOCATED(hycomflxv)) DEALLOCATE(hycomflxv)
  IF (ALLOCATED(htime)) DEALLOCATE(htime)
  IF (ALLOCATED(ctime)) DEALLOCATE(ctime)
  IF (ALLOCATED(atmnames)) DEALLOCATE(atmnames)
  IF (ALLOCATED(off_time)) DEALLOCATE(off_time)
  IF (ALLOCATED(avgflxs1)) DEALLOCATE(avgflxs1)
  IF (ALLOCATED(hycomflxp)) DEALLOCATE(hycomflxp)
  IF (ALLOCATED(hycomflxb)) DEALLOCATE(hycomflxb)
  IF (ALLOCATED(exhycom2d)) DEALLOCATE(exhycom2d)
  IF (ALLOCATED(eyhycom2d)) DEALLOCATE(eyhycom2d)
  IF (ALLOCATED(qxhycom2d)) DEALLOCATE(qxhycom2d)
  IF (ALLOCATED(qyhycom2d)) DEALLOCATE(qyhycom2d)
  IF (ALLOCATED(anhycom2d)) DEALLOCATE(anhycom2d)
  IF (ALLOCATED(imsk_intp)) DEALLOCATE(imsk_intp)
  IF (ALLOCATED(cs)) DEALLOCATE(cs)
  IF (ALLOCATED(ss)) DEALLOCATE(ss)

  !
  !     Comparing masks
  !
  IF (dbgn==3) THEN
     ALLOCATE (imsk_intp(1:nxhycom,1:nyhycom))
     ndiff=0
     WHERE (msk_out>=mskfrac)
        imsk_intp=1
     ELSEWHERE 
        imsk_intp=0 
     endwhere
     DO j=1,nyhycom
        DO i=1,nxhycom
           IF(imsk_intp(i,j)==0.AND.imsk_hycom(i,j)==1) THEN
              ndiff=ndiff+1
              !write (*,'(''msk_out, imsk_intp,imsk_hycom,i,j'',e13.6,f10.3,2i2,2i5)') &
              !     msk_out(i,j),imsk_intp(i,j),imsk_hycom(i,j),i,j
           END IF
        END DO
     END DO
     WRITE(*,*)'ndiff=',ndiff ; CALL flush(lp)                      
     WRITE(*,*)'msk_in:max,min=',MAXVAL(msk_in),MINVAL(msk_in) ; CALL flush(lp)
     WRITE(*,*)'msk_out:max,min=',MAXVAL(msk_out),MINVAL(msk_out) ; CALL flush(lp)
     WRITE(*,*)'real(imsk_intp):max,min=',MAXVAL(REAL(imsk_intp)),MINVAL(REAL(imsk_intp)) ; CALL flush(lp)
     WRITE(*,*)'real(imsk_hycom):max,min=',MAXVAL(REAL(imsk_hycom)),MINVAL(REAL(imsk_hycom)) ; CALL flush(lp)
     OPEN (unit=700,file='masks',form="unformatted") ; CALL flush(lp)
     WRITE(700) REAL(imsk_intp(:,:)),msk_out(:,:),REAL(imsk_hycom(:,:)) ; CALL flush(lp)
     CLOSE(700)
     !
     CALL dumpi(701,imsk_hycom,nxhycom,nyhycom,'mask_hycom')
     CALL dumpi(702,INT(msk_in),nxatm,nyatm,'mask_atm')
     CALL dumpr(703,msk_out,nxhycom,nyhycom,'mask_out')
     CALL dumpr(704,exhycom,nxhycom+1,1,'exhycom')
     CALL dumpr(705,eyhycom,nyhycom+1,1,'eyhycom')
     CALL dumpr(706,exatm,nxatm+1,1,'exatm')
     CALL dumpr(707,eyatm,nyatm+1,1,'eyatm')
  ENDIF
  IF (ALLOCATED(msk_out)) DEALLOCATE(msk_out)
  IF (ALLOCATED(imsk_hycom)) DEALLOCATE (imsk_hycom)
  IF (ALLOCATED(imsk_intp)) DEALLOCATE(imsk_intp)
  STOP
  !
  !  =========================================
  !    
CONTAINS
  !
  !  =========================================
  !    
  REAL FUNCTION cd(wsph)
    ! borrowed from HYCOM
    IMPLICIT NONE
    REAL wsph
    cd = 0.862e-3 + 0.088e-3 * wsph - 0.00089e-3 * wsph**2
    RETURN
  END FUNCTION cd
  !
  !  =========================================
  !    
END PROGRAM atmforcing
