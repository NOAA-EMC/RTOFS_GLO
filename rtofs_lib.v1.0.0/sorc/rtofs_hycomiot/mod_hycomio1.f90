MODULE mod_hycomio1
USE mod_hytime
USE mod_za
USE mod_xc
 PRIVATE
 PUBLIC rd_blkdata, mask_hycom_1,mask_hycom_2,write_abrecord,start_abfile
!
!
CONTAINS
!
!=================================================================================
!
      SUBROUTINE rd_blkdata(lu,idm,jdm,mapflg,reflon,reflat,pntlat,grdlat)
!
!     This is procedure to read blkdat.input in HYCOM 2.0 style. 
!     Will fail for the later versions. Needs to be generalized.
!
      IMPLICIT NONE
      INTEGER, INTENT(in) :: lu
      INTEGER, INTENT(out) :: idm,jdm,mapflg
      REAL,INTENT(out) :: reflon,reflat,pntlat,grdlat
      REAL :: grdlon,pntlon
      INTEGER :: i,ios
!
! --- initialize grid variables.
!
      OPEN(unit=lu,file='blkdat.input',status='old',position='rewind',iostat=ios)
      IF (ios<0) THEN
        PRINT *,' ERROR: rd_blkdata : problems opening blkdat.input'
        STOP 10
      ENDIF
!
! --- skip four lines (80-characters) describing the simulation
      DO i=1,4
        READ(lu,*)
      ENDDO
!
! --- skip lines with iversn,iexpt
      READ(lu,*)
      READ(lu,*)
!
! --- 'mapflg' = map flag (0=mercator,1=rotated,2=uniform,3=beta-plane &
! &                           ,4=f/plane, 5-orthogonal )
      WRITE(6,*)
      CALL blkini(lu,mapflg,'mapflg')

!
! --- 'idm   ' = longitudinal array size
! --- 'pntlon' = longitudinal reference grid point on pressure grid
! --- 'reflon' = longitude of reference grid point on pressure grid
! --- 'grdlon' = longitudinal grid size (degrees)
! --- 'jdm   ' = latitudinal  array size
! --- 'pntlat' = latitudinal  reference grid point on pressure grid
! --- 'reflat' = latitude of  reference grid point on pressure grid
! --- 'grdlat' = latitudinal  grid size at the equator (degrees)
      CALL blkini(lu,idm ,'idm   ')
      CALL blkinr(lu,pntlon,'pntlon','(a6," =",f10.4," ")')
      CALL blkinr(lu,reflon,'reflon','(a6," =",f10.4," deg E")')
      CALL blkinr(lu,grdlon,'grdlon','(a6," =",f10.4," degrees")')
      CALL blkini(lu,jdm ,'jdm   ')
      CALL blkinr(lu,pntlat,'pntlat','(a6," =",f10.4," ")')
      CALL blkinr(lu,reflat,'reflat','(a6," =",f10.4," deg N")')
      CALL blkinr(lu,grdlat,'grdlat','(a6," =",f10.4," degrees")')
!
      IF(grdlon/=grdlat) WRITE(*,*) 'WARNING !  rd_blkdata: grdlon.ne.grdlat' &
 &      ,' grdlon=',grdlon,' grdlat=',grdlat
     
      CLOSE(unit=lu)
      RETURN
      END SUBROUTINE rd_blkdata
!
!=================================================================================
!
      SUBROUTINE blkinr(lu,rvar,cvar,cfmt)
      IMPLICIT NONE
!
!
      INTEGER, INTENT(in)   :: lu
      REAL, INTENT(out)     :: rvar
      CHARACTER, INTENT(in) :: cvar*6,cfmt*(*)
!
!     read in one real value
!
      CHARACTER*6 :: cvarin
!
      READ(lu,*) rvar,cvarin
      WRITE(6,cfmt) cvarin,rvar
      CALL flush(6)
!
      IF     (cvar.NE.cvarin) THEN
        WRITE(6,*) 
        WRITE(6,*) 'error in blkinr - input ',cvarin, &
     &                      ' but should be ',cvar
        WRITE(6,*) 
        CALL flush(6)
        STOP
      ENDIF
      RETURN
      END SUBROUTINE blkinr
!
!=================================================================================
!
      SUBROUTINE blkini(lu,ivar,cvar)
      IMPLICIT NONE
!
!
      INTEGER, INTENT(in)     :: lu
      INTEGER, INTENT(out)    :: ivar
      CHARACTER*6, INTENT(in) :: cvar
!
!     read in one integer value
!
      CHARACTER*6 :: cvarin
!
      READ(lu,*) ivar,cvarin
      WRITE(6,'(a6,'' ='',i6)') cvarin,ivar
      CALL flush(6)
!
      IF     (cvar.NE.cvarin) THEN
        WRITE(6,*) 
        WRITE(6,*) 'error in blkini - input ',cvarin, &
     &                      ' but should be ',cvar
        WRITE(6,*) 
        CALL flush(6)
        STOP
      ENDIF
      RETURN
      END SUBROUTINE blkini
!
!=================================================================================
!
      SUBROUTINE blkinl(lu,lvar,cvar)
      IMPLICIT NONE
!
!
      INTEGER, INTENT(in)     :: lu
      LOGICAL, INTENT(out)    :: lvar
      CHARACTER*6, INTENT(in) :: cvar
!
!     read in one logical value
!     due to a SGI bug for logical I/O: read in an integer 0=F,1=T
!
      CHARACTER*6 cvarin
      INTEGER     ivar
!
      READ(lu,*) ivar,cvarin
      lvar = ivar .NE. 0
      WRITE(6,'(a6,'' ='',l6)') cvarin,lvar
      CALL flush(6)
!
      IF     (cvar.NE.cvarin) THEN
        WRITE(6,*) 
        WRITE(6,*) 'error in blkinr - input ',cvarin, &
     &                      ' but should be ',cvar
        WRITE(6,*) 
        CALL flush(6)
        STOP
      ENDIF
      RETURN
      END SUBROUTINE blkinl
  !
  !========================================================================
  !
  SUBROUTINE mask_hycom_1(imsk)
    !
    !   Read HYCOM bathymetry and calculate HYCOM land/sea mask (land=0,sea=1)
    !
    INTEGER, DIMENSION(:,:), INTENT(out) :: imsk
    REAL, DIMENSION(:,:), ALLOCATABLE :: depth
    REAL, PARAMETER :: huge = 2.0**100  ! 2^100, or about 1.2676506e30
    REAL :: dmin,dmax
    INTEGER :: i,j,nxhycom,nyhycom
    nxhycom=SIZE(imsk,1)
    nyhycom=SIZE(imsk,2)
    ALLOCATE (depth(1:nxhycom,1:nyhycom))
    imsk=0
!!!    call zaiost 
    CALL zaiopf('regional.depth.a','old', 59)
    CALL zaiord(depth,imsk,.FALSE.,dmin,dmax, 59)
    ! write (*,*) 'DEPTH is read, min,max=',dmin,dmax !dbgz
    ! write (*,*) 'DEPTH minval,maxval=',minval(depth),maxval(depth) !dbgz
    CALL zaiocl(59)
    imsk=0
    WHERE (depth>0.5*huge) depth=0.
    WHERE (depth>0) imsk=1 
    DEALLOCATE (depth)
  END SUBROUTINE mask_hycom_1
  !
  !========================================================================
  !
  SUBROUTINE mask_hycom_2(imsk)
    !
    !   Read HYCOM mask and calculate HYCOM land/sea mask (land=0,sea=1)
    !
    INTEGER, DIMENSION(:,:), INTENT(out) :: imsk
    REAL, DIMENSION(:,:), ALLOCATABLE :: dc
    REAL :: dmin,dmax
    INTEGER :: i,j,nxhycom,nyhycom
    nxhycom=SIZE(imsk,1)
    nyhycom=SIZE(imsk,2)
    ALLOCATE (dc(1:nxhycom,1:nyhycom))
    imsk=0
!!!!    call zaiost 
    CALL zaiopf('regional.mask.a','old', 61)
    CALL zaiord(dc,imsk,.FALSE.,dmin,dmax, 61)
    CALL zaiocl(61)
    DO j=1,nyhycom
       DO i=1,nxhycom
          IF(dc(i,j)>0.) THEN
             imsk(i,j)=1  
          ELSE 
             imsk(i,j)=0 
          ENDIF
       END DO
    END DO
    DEALLOCATE (dc)
  END SUBROUTINE mask_hycom_2
  !
  !  =========================================
  !    
  SUBROUTINE write_abrecord(fld,msk,lmask,lua,lub,lreal4,hyflx,mnth,ftime)
    !
    ! Write a record in HYCOM ab format
    !
    REAL, DIMENSION(:,:), INTENT(inout) :: fld
    INTEGER, DIMENSION(:,:), INTENT(in) :: msk
    INTEGER, INTENT(in) :: lua,lub
    LOGICAL, INTENT(in):: lmask,lreal4
    CHARACTER(len=*), INTENT(in) :: hyflx
    REAL, OPTIONAL, INTENT(in) :: ftime
    INTEGER, OPTIONAL, INTENT(in) :: mnth
    REAL :: fldmin,fldmax
    CHARACTER(len=300) :: bfmt
    CALL zaiowr(fld,msk,lmask, fldmin,fldmax, lua,lreal4)
    IF (PRESENT(mnth)) THEN
       bfmt='(A,'': month,range = '',I10,1P2E16.7)'
       WRITE(lub,TRIM(bfmt))'   '//TRIM(hyflx),mnth,fldmin,fldmax
       WRITE(*,TRIM(bfmt))'   '//TRIM(hyflx),mnth,fldmin,fldmax
    ELSEIF(PRESENT(ftime)) THEN
       bfmt='(A,'': date,span,range = '',F10.2,'' 0 '',1P2E16.7)'
       WRITE(lub,TRIM(bfmt))'   '//TRIM(hyflx),ftime,fldmin,fldmax
       WRITE(*,TRIM(bfmt))'   '//TRIM(hyflx),ftime,fldmin,fldmax
    ELSE 
       WRITE(*,*) 'ERROR: dont know time in .b writing'
       STOP
    ENDIF
  END SUBROUTINE write_abrecord
  !
  !  =========================================
  !    
  SUBROUTINE start_abfile(lua,lub,fldname,nxhycom,nyhycom,reflon,pntlat,gridsz,gridname)
    !
    ! Open files in HYCOM ab format and write a header to .b file
    !
    INTEGER, INTENT(in) :: lua,lub,nxhycom,nyhycom
    REAL, OPTIONAL, INTENT(in) :: reflon,pntlat,gridsz
    CHARACTER (len=*), INTENT(in) :: fldname
    CHARACTER (len=*),OPTIONAL, INTENT(in) :: gridname
    CHARACTER (len=10) big_ben(3)
    CHARACTER:: preambl(5)*79
    INTEGER, DIMENSION(8) :: date_time
    INTEGER :: ii
    CALL DATE_AND_TIME (big_ben(1),big_ben(2),big_ben(3),date_time)
    OPEN (unit=lub,file='forcing.'//TRIM(fldname)//'.b',status='new' ,action='write')
    WRITE(preambl(1),'(a,i4,''-'',i2,''-'',i2,2x,''['',i5,'']'',2x,i2,'':'',i2)') &
         'GDAS derived '//TRIM(fldname)//' created ',(date_time(ii),ii=1,6)
    DO ii=2,4 
       preambl(ii) = ' ' 
    END DO
    IF(PRESENT(reflon) .AND. PRESENT(pntlat) .AND. PRESENT(gridsz) ) THEN
       WRITE(preambl(5),'(a,2i5,f9.3,f9.2,f6.3)') &
            'i/jdm,reflon,pntlat,gridsz =', &
            nxhycom,nyhycom,reflon,pntlat,gridsz
    ELSEIF (PRESENT(gridname)) THEN
       WRITE(preambl(5),'(a,2i5,a)')'i/jdm =',nxhycom,nyhycom,gridname
    ELSE
       WRITE(*,*) 'ERROR: dont know how to write .b preambule'
       STOP
    ENDIF
    WRITE(lub,'(A79)') preambl
    CALL zaiopf('forcing.'//TRIM(fldname)//'.a','new' , lua)
  END SUBROUTINE start_abfile
!
!=================================================================================
!
END MODULE mod_hycomio1
