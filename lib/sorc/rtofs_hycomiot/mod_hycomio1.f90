module mod_hycomio1
use mod_hytime
use mod_za
use mod_xc
 private
 public rd_blkdata, mask_hycom_1,mask_hycom_2
!
!
contains
!
!=================================================================================
!
      subroutine rd_blkdata(lu,idm,jdm,mapflg,reflon,reflat,pntlat,grdlat)
!
!     This is procedure to read blkdat.input in HYCOM 2.0 style. 
!     Will fail for the later versions. Needs to be generalized.
!
      implicit none
      integer, intent(in) :: lu
      integer, intent(out) :: idm,jdm,mapflg
      real,intent(out) :: reflon,reflat,pntlat,grdlat
      real :: grdlon,pntlon
      integer :: i,ios
!
! --- initialize grid variables.
!
      open(unit=lu,file='blkdat.input',status='old',position='rewind',iostat=ios)
      if (ios<0) then
        print *,' ERROR: rd_blkdata : problems opening blkdat.input'
        stop 10
      endif
!
! --- skip four lines (80-characters) describing the simulation
      do i=1,4
        read(lu,*)
      enddo
!
! --- skip lines with iversn,iexpt
      read(lu,*)
      read(lu,*)
!
! --- 'mapflg' = map flag (0=mercator,1=rotated,2=uniform,3=beta-plane &
! &                           ,4=f/plane, 5-orthogonal )
      write(6,*)
      call blkini(lu,mapflg,'mapflg')

!
! --- 'idm   ' = longitudinal array size
! --- 'pntlon' = longitudinal reference grid point on pressure grid
! --- 'reflon' = longitude of reference grid point on pressure grid
! --- 'grdlon' = longitudinal grid size (degrees)
! --- 'jdm   ' = latitudinal  array size
! --- 'pntlat' = latitudinal  reference grid point on pressure grid
! --- 'reflat' = latitude of  reference grid point on pressure grid
! --- 'grdlat' = latitudinal  grid size at the equator (degrees)
      call blkini(lu,idm ,'idm   ')
      call blkinr(lu,pntlon,'pntlon','(a6," =",f10.4," ")')
      call blkinr(lu,reflon,'reflon','(a6," =",f10.4," deg E")')
      call blkinr(lu,grdlon,'grdlon','(a6," =",f10.4," degrees")')
      call blkini(lu,jdm ,'jdm   ')
      call blkinr(lu,pntlat,'pntlat','(a6," =",f10.4," ")')
      call blkinr(lu,reflat,'reflat','(a6," =",f10.4," deg N")')
      call blkinr(lu,grdlat,'grdlat','(a6," =",f10.4," degrees")')
!
      if(grdlon/=grdlat) write(*,*) 'WARNING !  rd_blkdata: grdlon.ne.grdlat' &
 &      ,' grdlon=',grdlon,' grdlat=',grdlat
     
      close(unit=lu)
      return
      end subroutine rd_blkdata
!
!=================================================================================
!
      subroutine blkinr(lu,rvar,cvar,cfmt)
      implicit none
!
!
      integer, intent(in)   :: lu
      real, intent(out)     :: rvar
      character, intent(in) :: cvar*6,cfmt*(*)
!
!     read in one real value
!
      character*6 :: cvarin
!
      read(lu,*) rvar,cvarin
      write(6,cfmt) cvarin,rvar
      call flush(6)
!
      if     (cvar.ne.cvarin) then
        write(6,*) 
        write(6,*) 'error in blkinr - input ',cvarin, &
     &                      ' but should be ',cvar
        write(6,*) 
        call flush(6)
        stop
      endif
      return
      end subroutine blkinr
!
!=================================================================================
!
      subroutine blkini(lu,ivar,cvar)
      implicit none
!
!
      integer, intent(in)     :: lu
      integer, intent(out)    :: ivar
      character*6, intent(in) :: cvar
!
!     read in one integer value
!
      character*6 :: cvarin
!
      read(lu,*) ivar,cvarin
      write(6,'(a6,'' ='',i6)') cvarin,ivar
      call flush(6)
!
      if     (cvar.ne.cvarin) then
        write(6,*) 
        write(6,*) 'error in blkini - input ',cvarin, &
     &                      ' but should be ',cvar
        write(6,*) 
        call flush(6)
        stop
      endif
      return
      end subroutine blkini
!
!=================================================================================
!
      subroutine blkinl(lu,lvar,cvar)
      implicit none
!
!
      integer, intent(in)     :: lu
      logical, intent(out)    :: lvar
      character*6, intent(in) :: cvar
!
!     read in one logical value
!     due to a SGI bug for logical I/O: read in an integer 0=F,1=T
!
      character*6 cvarin
      integer     ivar
!
      read(lu,*) ivar,cvarin
      lvar = ivar .ne. 0
      write(6,'(a6,'' ='',l6)') cvarin,lvar
      call flush(6)
!
      if     (cvar.ne.cvarin) then
        write(6,*) 
        write(6,*) 'error in blkinr - input ',cvarin, &
     &                      ' but should be ',cvar
        write(6,*) 
        call flush(6)
        stop
      endif
      return
      end subroutine blkinl
  !
  !========================================================================
  !
  subroutine mask_hycom_1(imsk)
    !
    !   Read HYCOM bathymetry and calculate HYCOM land/sea mask (land=0,sea=1)
    !
    integer, dimension(:,:), intent(out) :: imsk
    real, dimension(:,:), allocatable :: depth
    real, parameter :: huge = 2.0**100  ! 2^100, or about 1.2676506e30
    real :: dmin,dmax
    integer :: i,j,nxhycom,nyhycom
    nxhycom=size(imsk,1)
    nyhycom=size(imsk,2)
    allocate (depth(1:nxhycom,1:nyhycom))
    imsk=0
!!!    call zaiost 
    call zaiopf('regional.depth.a','old', 59)
    call zaiord(depth,imsk,.false.,dmin,dmax, 59)
    ! write (*,*) 'DEPTH is read, min,max=',dmin,dmax !dbgz
    ! write (*,*) 'DEPTH minval,maxval=',minval(depth),maxval(depth) !dbgz
    call zaiocl(59)
    imsk=0
    where (depth>0.5*huge) depth=0.
    where (depth>0) imsk=1 
    deallocate (depth)
  end subroutine mask_hycom_1
  !
  !========================================================================
  !
  subroutine mask_hycom_2(imsk)
    !
    !   Read HYCOM mask and calculate HYCOM land/sea mask (land=0,sea=1)
    !
    integer, dimension(:,:), intent(out) :: imsk
    real, dimension(:,:), allocatable :: dc
    real :: dmin,dmax
    integer :: i,j,nxhycom,nyhycom
    nxhycom=size(imsk,1)
    nyhycom=size(imsk,2)
    allocate (dc(1:nxhycom,1:nyhycom))
    imsk=0
!!!!    call zaiost 
    call zaiopf('regional.mask.a','old', 61)
    call zaiord(dc,imsk,.false.,dmin,dmax, 61)
    call zaiocl(61)
    do j=1,nyhycom
       do i=1,nxhycom
          if(dc(i,j)>0.) then
             imsk(i,j)=1  
          else 
             imsk(i,j)=0 
          endif
       end do
    end do
    deallocate (dc)
  end subroutine mask_hycom_2
!
!=================================================================================
!
end module mod_hycomio1
