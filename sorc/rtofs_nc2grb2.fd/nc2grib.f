!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: RTOFS_NC2GRIB2
!   PRGMMR: STOKES           ORG: NP23        DATE: 2010-09-30
!
! ABSTRACT:  Convert regional netcdf files from Global RTOFS to GRIB2 files
!
! PROGRAM HISTORY LOG:
! 2010-09-27  Diane C. Stokes
! 2012-08-15  Bhavani Rajan
!
! USAGE:
!   INPUT FILES:
!     UNIT 20  - asc nar_name
!
!   OUTPUT FILES:
!     UNIT 50  - grib2 var_name file
!     UNIT 06  - standard output
!
!   SUBPROGRAMS CALLED:
!     LIBRARY:
!      SYSTEM    - getenv
!       W3LIB    - errexit, putgb
!          G2    - gribcreate,addlocal,addgrid,addfield,gribend
!       BACIO    - baopenw, baclose
!
!   EXIT STATES:
!     COND =   0 - Successful run
!     COND =  11 - Error calling gribcreate
!     COND =  12 - Error calling addlocal
!     COND =  13 - Error calling addgrid
!     COND =  14 - Error calling addfield
!     COND =  15 - Error calling gribend
!     COND =  ?? - Non-zero return code from baclose
!
!
! ATTRIBUTES:  (LIST MACHINES FOR WHICH CODE IS USED AND CHECKED OUT)
!
!   MACHINE:  WCOSS 
!   LANGUAGE: F90
!
c------------------------------------------------------------------------
c------------------------------------------------------------------------
! This code reads in ascii file (converted to ascii from netcdf) and writes
! the output in GRIB2 format directly
      program nc2grib
      


      use grib_mod

      integer(4), parameter::igdstmplen=19
      integer(4), parameter::ipdstmplen=15
      integer(4), parameter::idrstmplen=5

      character     ciu*2,ename*10,fname*256

c Input arrays
c     var_name     - The array or variable name
c     rsvar    - The OI random + sampling error variance
c                rsvar = e*fgvar*rfac
c     bsvar   -  final accumulated bias variance + residual
c                computed by satellite bias correction routines
c                eotbias-wt4-col.f    & eotbias-cor4-col.f

c      real(4), dimension(:,:) :: var_name,rsvar,bsvar,var_name_pack
      real, allocatable, dimension (:,:) ::  var_name,rsvar,bsvar,
     & var_name_pack
      logical(1),allocatable,dimension(:,:) ::  bmap
      character,allocatable, dimension(:) ::  cgrib

      character(len=80) title
      integer(4)    listsec0(2)
      integer(4)    listsec1(13)
      integer(4)    igds(5)
      integer(4)    igdstmpl(igdstmplen)
      integer(4)    ipdstmpl(ipdstmplen)
      integer(4)    idrstmpl(idrstmplen)
      integer       imax,jmax,ngrdpts,lcgrib,parm,p_cat
      real          lat0,lon0,lat1,lon1,dlat,dlon
      read(*,*) imax,jmax,iday,iyr,imo,fcsthr,icycle,parm,p_cat,
     *     lon0,lat0,dlat,dlon,depth
      lat1=lat0+dlat*jmax
      lon1=lon0+dlon*imax
      ngrdpts=imax*jmax
      lcgrib=ngrdpts*4

      allocate (var_name(imax,jmax),var_name_pack(imax,jmax))
      allocate (bmap(imax,jmax))
      allocate (cgrib(lcgrib))
c------------------------------------------------------------------------
c------------------------------------------------------------------------
      lun=20; lugb=50
      write(ciu,'(i2)')lugb
      ename='XLFUNIT_'//adjustl(ciu)
      call getenv (ename, fname)
      call baopenw (lugb,fname,iret)
      iprod=0  ! WANT TO CHANGE THIS TO BE INPUT OR ENV VAR

      listsec0(1)=10   ! Discipline-GRIB Master Table Number (see Code Table 0.0)
      listsec0(2)=2    ! GRIB Edition Number (currently 2)
      listsec1(1)=7    ! Id of orginating centre (Common Code Table C-1)
      listsec1(2)=0    ! Id of orginating sub-centre (local table)
      listsec1(3)=2    ! GRIB Master Tables Version Number (Code Table 1.0)
      listsec1(4)=1    ! GRIB Local Tables Version Number (Code Table 1.1)
      listsec1(5)=1    ! Significance of Reference Time (Code Table 1.2)
      listsec1(6)=iyr  ! Reference Time - Year (4 digits)
      listsec1(7)=imo! Reference Time - Month
      listsec1(8)=iday ! Reference Time - Day
      listsec1(9)=icycle ! Reference Time - Hour(cycle:0,6,12,18)
      listsec1(10)=0   ! Reference Time - Minute
      listsec1(11)=0   ! Reference Time - Second
      listsec1(12)=0     ! Production status of data (Code Table 1.3)
      listsec1(13)=1   !Type of processed data (Code Table 1.4)

! set up grid definition section
      igds(1)=0        ! Source of grid definition (see Code Table 3.0)
      igds(2)=ngrdpts  ! Number of grid points in the defined grid.
      igds(3)=0        ! Number of octets needed for each additional grid points definition.
      igds(4)=0        ! Interpretation of list for optional points definition.  (Code Table 3.11)
      igds(5)=0        ! Grid Definition Template Number (Code Table 3.1)

! set up grid definition section
      igdstmpl(1)=6    ! Shape of earth (6=Earth assumed spherical with radius = 6,371,229.0 m)
      igdstmpl(2:7)=0  ! Not needed for Shape of earth = 6
      igdstmpl(8)=imax  ! number of points along a parallel
      igdstmpl(9)=jmax  ! number of points along a meridian
      igdstmpl(10:11)=0  ! Basic angle of the initial production domain and subdivisions of this basic angle 
      igdstmpl(12) = nint(lat0*1000000) 
      igdstmpl(13) = nint(lon0*1000000)
      igdstmpl(14) = 48 
      igdstmpl(15) = nint(lat1*1000000) 
      igdstmpl(16) = nint(lon1*1000000)
      igdstmpl(17) = nint(dlat*1000000)
      igdstmpl(18) = nint(dlon*1000000)
      igdstmpl(19) = 64

      read (lun,22) var_name   
 22   format(f8.4)
 
! create msg for var_name field
      CALL gribcreate(cgrib,lcgrib,listsec0,listsec1,ierr)
      if(ierr.ne.0)then
        print *, 'Error calling gribcreate.  ierr=',ierr
        call errexit(11)
      endif


      call addgrid(cgrib,lcgrib,igds,igdstmpl,igdstmplen,0,0,ierr)
      if(ierr.ne.0)then
        print *, 'Error calling addgrid.  ierr=',ierr
        call errexit(13)
      endif

      ipdsnum = 0   !  Product Definition Template Number (see Code Table 4.0)
                    !  0:Analysis or forecast at a horizontal level or in a horizontal layer at a point in time.  See Template 4.0

      ipdstmpl(1) = p_cat   ! Parameter category (see Code table 4.1) (3:Surface Properties)
      ipdstmpl(2) = parm ! Parameter number (see Code table 4.2)  (0:Water Temperature)
      ipdstmpl(3) = 2 ! Type of generating process (see Code table 4.3) (0:Analysis)
      ipdstmpl(4) = 85 ! Background generating process identifier (defined by originating centre)
      ipdstmpl(5) = 0  ! Analysis or forecast generating process identified (see Code ON388 Table A) (44:SST Analysis)
      ipdstmpl(6) = 0 ! Hours of observational data cutoff after reference time
      ipdstmpl(7) = 0 ! Minutes of observational data cutoff after reference time
      ipdstmpl(8) = 1 ! Indicator of unit of time range (see Code table 4.4)
      ipdstmpl(9) = fcsthr ! Forecast time in units defined by octet 18
      ipdstmpl(10) = 160 ! Type of first fixed surface (see Code table 4.5)
      ipdstmpl(11) = 0 ! Scale factor of first fixed surface
      ipdstmpl(12) = depth ! Scaled value of first fixed surface
      ipdstmpl(13) = 255 ! Type of second fixed surfaced (see Code table 4.5)
      ipdstmpl(14) = 0 ! Scale factor of second fixed surface
      ipdstmpl(15) = 0 ! Scaled value of second fixed surfaces

      coordlist=0
      numcoord=0

      idrsnum = 0  ! Data Representation Template Number (see Code Table 5.0)
                   ! (Grid Point Data - Simple Packing (see Template 5.0))

      idrstmpl(1) = 0  !  reference value (R) (IEEE 32-bit floating-point value)  (will be overwritten)
      idrstmpl(2) = 0  !  Binary scale factor (E) (will be overwritten)
      idrstmpl(3) = 2  !  Decimal scale factor (D)
      idrstmpl(4) = 0  !  Number of bits used for each packed value for simple packing (will be overwritten)
      idrstmpl(5) = 0  !  Type of original field values (0:Floating Point)

      ibmap = 0   ! Bitmap indicator ( see Code Table 6.0 ) (0:bitmap applies and is included in Section 6.)
      
      where(var_name(:,:) > 99.)
        bmap(:,:) = .false.
c        var_name_pack(:,:) = 0.          
      elsewhere
        bmap(:,:) = .true.
        var_name_pack(:,:) = var_name(:,:)
      endwhere

      call addfield(cgrib,lcgrib,ipdsnum,ipdstmpl,ipdstmplen,
     1              coordlist,numcoord,idrsnum,idrstmpl,
     2              idrstmplen,var_name_pack,ngrdpts,ibmap,bmap,ierr)
      if(ierr.ne.0)then
        print *, 'Error calling addfield.  ierr=',ierr
        call errexit(14)
      endif

      call gribend(cgrib,lcgrib,lengrib,ierr)
      print*,'lengrib returned from gribend=',lengrib


      if(ierr.ne.0)then
        print *, 'Error calling gribend.  ierr=',ierr
        call errexit(15)
      endif

      call wryte(lugb,lengrib,cgrib)

      call baclose(lugb,iret)
      print*,'Return code from baclose=',iret

      call errexit(iret)
      end

