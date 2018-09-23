MODULE mod_geom
  USE mod_za,ONLY : zaiost,zaiopf,zaiord,zaiocl,idm,jdm
  USE mod_grib2io, ONLY: rdgrib
  USE mod_flags
  PRIVATE
  ! IMPLICIT NONE
  PUBLIC hycom_global_tripolar, atm_gaussian,rd_hycom_grid_params,mask_atm &
       ,horiz_intp,horiz_intp_,global_extend !!$$ ,extend_fld
  !
  REAL,PARAMETER:: radian=57.2957795
  INTERFACE atm_gaussian
     MODULE PROCEDURE atm_gaussian_1
     MODULE PROCEDURE atm_gaussian_2
  END INTERFACE
  INTERFACE hycom_global_tripolar
     MODULE PROCEDURE hycom_global_tripolar_read
     MODULE PROCEDURE hycom_global_tripolar_read_1
  END INTERFACE
  INTERFACE mask_atm
     MODULE PROCEDURE mask_atm_1
     MODULE PROCEDURE mask_atm_2
  END INTERFACE
  INTERFACE horiz_intp
     MODULE PROCEDURE horiz_intp_
     MODULE PROCEDURE horiz_intp_msk
     MODULE PROCEDURE horiz_intp_init
  END INTERFACE
  INTERFACE global_extend
     MODULE PROCEDURE global_extend_latlon_1d
     MODULE PROCEDURE global_extend_field_2d
     MODULE PROCEDURE global_extend_field_3d
     MODULE PROCEDURE global_extend_mask
  END INTERFACE
CONTAINS
  !
  !========================================================================
  !
  SUBROUTINE hycom_global_tripolar_read(anhycom,qxhycom,qyhycom,dbgn)
    INTEGER, INTENT(in) :: dbgn
    REAL, DIMENSION(:,:),INTENT(out)  :: qxhycom
    REAL, DIMENSION(:,:),INTENT(out)  :: qyhycom
    REAL, DIMENSION(:,:),INTENT(out)  :: anhycom
    ! integer, dimension(:,:),intent(inout)  :: imsk
    INTEGER :: nxhycom,nyhycom,i,j
    REAL, DIMENSION(:,:), ALLOCATABLE ::  plon,plat
    REAL, DIMENSION(:,:), ALLOCATABLE ::  qlon,qlat
    REAL, DIMENSION(:,:), ALLOCATABLE ::  tmp
    INTEGER, DIMENSION(:,:), ALLOCATABLE ::  ip1
    REAL hmina,hmaxa
    !  
    nxhycom=SIZE(qxhycom,dim=1)-1
    nyhycom=SIZE(qxhycom,dim=2)-1
    ! print *,'hycom_global_tripolar: nxhycom,nyhycom=',nxhycom,nyhycom !dbgzp
    ALLOCATE(ip1(nxhycom,nyhycom),plon(nxhycom,nyhycom),plat(nxhycom,nyhycom))
    ALLOCATE(tmp(nxhycom,nyhycom),qlon(nxhycom,nyhycom),qlat(nxhycom,nyhycom))
    plon=0.0 !dbgz
    plat=0.0 !dbgz
    qlon=0.0 !dbgz
    qlat=0.0 !dbgz
    ! print *,'size(ip1)=',size(ip1,dim=1), size(ip1,dim=2)!dbgzp
    ! print *,'size(plon)=',size(plon,dim=1),size(plon,dim=2) !dbgzp
    ! print *,'size(plat)=',size(plat,dim=1),size(plat,dim=2) !dbgzp
    ! print *,'size(qlon)=',size(qlon,dim=1),size(qlon,dim=2) !dbgzp
    ! print *,'size(qlat)=',size(qlat,dim=1), size(qlat,dim=2)!dbgzp
    ! print *,'size(anhycom)=',size(anhycom,dim=1), size(anhycom,dim=2)!dbgzp
    !
    !   read latitudes and longitudes of HYCOM mercator grid.
    !
    ip1=0.
!!!    call zaiost 
    CALL zaiopf('regional.grid.a','old', 9)
     CALL zaiord(plon, ip1,.FALSE., hmina,hmaxa, 9)
     ! write(*,*) 'reading p-longitudes max min=',hmina,hmaxa!dbgzp
     PRINT *,'first:  min(plon), max(plon)=',MINVAL(plon), MAXVAL(plon)
     CALL zaiord(plat, ip1,.FALSE., hmina,hmaxa, 9)
     ! write(*,*) 'reading p-latitudes max min=',hmina,hmaxa!dbgzp
     PRINT *,'first:  min(plat), max(plat)=',MINVAL(plat), MAXVAL(plat)
     CALL zaiord(qlon, ip1,.FALSE., hmina,hmaxa, 9)
     ! write(*,*) 'reading q-longitudes max min=',hmina,hmaxa!dbgzp
     CALL zaiord(qlat, ip1,.FALSE., hmina,hmaxa, 9)
     ! write(*,*) 'reading q-latitudes max min=',hmina,hmaxa!dbgzp
     ! skip records until  angle is read
     DO i=1,5
       CALL zaiord(tmp, ip1,.FALSE., hmina,hmaxa, 9)
    ENDDO
    WRITE(*,*) 'reading angles max min=',hmina,hmaxa
    PRINT *,'min(anhycom), max(anhycom)=',MINVAL(tmp), MAXVAL(tmp)!dbgz
    WRITE(*,*) 'reading angle between x-axis and East max min=',hmina,hmaxa
    CALL zaiocl(9)
    !
    ! set ang to radians
    anhycom(:,:)= tmp(:,:)*ACOS(-1.0)/180.0
    ! print *,'before: min(plon), max(plon)=',minval(plon), maxval(plon)!dbgzp
    ! print *,'before: min(plat), max(plat)=',minval(plat), maxval(plat)!dbgzp
    ! print *,'before: min(qlon), max(qlon)=',minval(qlon), maxval(qlon)!dbgzp
    ! print *,'before: min(qlat), max(qlat)=',minval(qlat), maxval(qlat)!dbgzp
    ! range of longitude in the range of [-180,180]
    !180! plon=plon-360.
    !180! qlon=qlon-360.
    !180! where(plon<-180.0)plon=360.0+plon
    !180! where(qlon<-180.0)qlon=360.0+qlon
    ! print *,'after:  min(plon), max(plon)=',minval(plon), maxval(plon)!dbgzp
    ! print *,'after:  min(plat), max(plat)=',minval(plat), maxval(plat)!dbgzp
    ! print *,'after:  min(qlon), max(qlon)=',minval(qlon), maxval(qlon)!dbgzp
   !  print *,'after:  min(qlat), max(qlat)=',minval(qlat), maxval(qlat)!dbgzp
    !
    !   Calculate grid edges 
    ! 
    qyhycom(1:nxhycom,1:nyhycom)=qlat/radian
    qyhycom(nxhycom+1,2:nyhycom+1)=plat(nxhycom,1:nyhycom)/radian
    qyhycom(2:nxhycom+1,nyhycom+1)=plat(1:nxhycom,nyhycom)/radian
    !   corners
    !   not really needed
    qyhycom(1,nyhycom+1)=plat(1,nyhycom)/radian
    qyhycom(nxhycom+1,1)=plat(nxhycom,1)/radian

    qxhycom(1:nxhycom,1:nyhycom)=qlon/radian
    qxhycom(nxhycom+1,2:nyhycom+1)=plon(nxhycom,1:nyhycom)/radian
    qxhycom(2:nxhycom+1,nyhycom+1)=plon(1:nxhycom,nyhycom)/radian
    !   corners
    !   not really needed
    qxhycom(1,nyhycom+1)=plon(1,nyhycom)/radian
    qxhycom(nxhycom+1,1)=plon(nxhycom,1)/radian
    !
    PRINT *,'min(qxhycom), max(qxhycom) [in degrees] =',MINVAL(qxhycom)*radian, MAXVAL(qxhycom)*radian
    PRINT *,'min(qyhycom), max(qyhycom) [in degrees] =',MINVAL(qyhycom)*radian, MAXVAL(qyhycom)*radian
    IF (dbgn>=2) THEN
       WRITE (*,'("plat ==========")')
       WRITE (*,'(8F10.4)') (plat(1,i),i=1,nyhycom)
       WRITE (*,'("plon ==========")')
       WRITE (*,'(1X,I5,F10.4)') (i,plon(i,1),i=1,nxhycom)
       WRITE (*,'("qyhycom ==========")')
       WRITE (*,'(1X,I5,2F10.4)') (i,qyhycom(1,i),qyhycom(1,i)*radian,i=1,nyhycom+1,10)
       WRITE (*,'("qxhycom ==========")')
       WRITE (*,'(1X,I5,2F10.4)') (i,qxhycom(i,1),qxhycom(i,1)*radian,i=1,nxhycom+1,10)
    ENDIF
    DEALLOCATE(ip1,plon,plat,qlon,qlat)
    !
  END SUBROUTINE hycom_global_tripolar_read
  !
  !========================================================================
  !
  SUBROUTINE hycom_global_tripolar_read_1(maskin,maskout,an,qx,qy,plon0,dbgn)
    IMPLICIT NONE
    LOGICAL, INTENT(in) :: dbgn
    INTEGER, DIMENSION(:,:),INTENT(in)  :: maskin
    INTEGER, DIMENSION(:,:),INTENT(out)  :: maskout
    REAL, DIMENSION(:,:),INTENT(out)  :: qx
    REAL, DIMENSION(:,:),INTENT(out)  :: qy
    REAL, DIMENSION(:,:),INTENT(out)  :: an
    REAL,INTENT(out)  :: plon0
    REAL, DIMENSION(:,:),ALLOCATABLE  :: plon,plat,qlon,qlat
    INTEGER, PARAMETER :: nskip=5,lu=9
    INTEGER :: nx,ny,i,j
    INTEGER, DIMENSION(:,:), ALLOCATABLE ::  ip1
    REAL hmina,hmaxa
    !  
    nx=SIZE(qx,dim=1)
    ny=SIZE(qy,dim=2)
    maskout=maskin
    ! if (dbgn) print *,'hycom_global_tripolar_1: nx,ny=',nx,ny !dbgzp
    ALLOCATE(ip1(nx,ny),plon(nx,ny),plat(nx,ny),qlon(nx,ny),qlat(nx,ny))
    !
    !   read latitudes and longitudes of HYCOM mercator grid.
    !
    ip1=0.
!!!    call zaiost 
    CALL zaiopf('regional.grid.a','old', lu)
    CALL zaiord(plon, ip1,.FALSE., hmina,hmaxa, lu)
    if (dbgn) write(*,*) 'reading p-longitudes max min=',hmina,hmaxa!dbgzp
    PRINT *,'first:  min(plon), max(plon)=',MINVAL(plon), MAXVAL(plon)
    CALL zaiord(plat, ip1,.FALSE., hmina,hmaxa, lu)
    if (dbgn) write(*,*) 'reading p-latitudes max min=',hmina,hmaxa!dbgzp
    PRINT *,'first:  min(plat), max(plat)=',MINVAL(plat), MAXVAL(plat)
    CALL zaiord(qlon, ip1,.FALSE., hmina,hmaxa, 9)
    write(*,*) 'reading q-longitudes max min=',hmina,hmaxa!dbgzp
    CALL zaiord(qlat, ip1,.FALSE., hmina,hmaxa, 9)
    write(*,*) 'reading q-latitudes max min=',hmina,hmaxa!dbgzp
    ! skip records until  angle is read
    DO i=1, nskip
       CALL zaiord(an, ip1,.FALSE., hmina,hmaxa, lu)
    ENDDO
    DEALLOCATE(ip1)
    IF (dbgn) WRITE(*,*) 'reading angles max min=',hmina,hmaxa
    WRITE(*,*) 'reading angle between x-axis and East max min=',hmina,hmaxa
    CALL zaiocl(lu)
    !
    ! set ang to radians
    an(:,:)= an(:,:)*ACOS(-1.0)/180.0
    ! range of longitude in the range of [-180,180]
    !180! plon=plon-360.
    !180! where(plon<-180.0)plon=360.0+plon
    !180! qlon=qlon-360.
    !180! where(qlon<-180.0)qlon=360.0+qlon
    !
    plon0=MINVAL(plon(:,:))
    !
    ! Shift mask accordingly
    ! Set grid edges
    qy(1:nx,1:ny)=qlat/radian
    qx(1:nx,1:ny)=qlon/radian
!!$    !
!!$    !   Calculate grid edges 
!!$    ! 
!!$    qy(1:nx,1:ny)=qlat/radian
!!$    qy(nx+1,2:ny+1)=plat(nx,1:ny)/radian
!!$    qy(2:nx+1,ny+1)=plat(1:nx,ny)/radian
!!$    !   corners
!!$    !   not really needed
!!$    qy(1,ny+1)=plat(1,ny)/radian
!!$    qy(nx+1,1)=plat(nx,1)/radian
!!$
!!$    qx(1:nx,1:ny)=qlon/radian
!!$    qx(nx+1,2:ny+1)=plon(nx,1:ny)/radian
!!$    qx(2:nx+1,ny+1)=plon(1:nx,ny)/radian
    !
    PRINT *,'min(qx), max(qx) [in degrees] =',MINVAL(qx)*radian, MAXVAL(qx)*radian
    PRINT *,'min(qy), max(qy) [in degrees] =',MINVAL(qy)*radian, MAXVAL(qy)*radian
    PRINT *,'min(qx), max(qx) [in radian] =',MINVAL(qx), MAXVAL(qx)
    PRINT *,'min(qy), max(qy) [in radian] =',MINVAL(qy), MAXVAL(qy)
    IF (dbgn) THEN
       WRITE (*,'("plat ==========")')
       WRITE (*,'(8F10.4)') (plat(1,i),i=1,ny)
       WRITE (*,'("plon ==========")')
       WRITE (*,'(1X,I5,F10.4)') (i,plon(i,1),i=1,nx)
    ENDIF
    DEALLOCATE(plon,plat,qlon,qlat)
    !
  END SUBROUTINE hycom_global_tripolar_read_1
  !
  !========================================================================
  !
  SUBROUTINE atm_gaussian_1(exatm,eyatm,edges,dbgn)
    REAL,PARAMETER:: acon=180./3.14159265
    INTEGER, INTENT(in) :: dbgn,edges
    REAL, DIMENSION(:),INTENT(out) :: exatm
    REAL, DIMENSION(:),INTENT(out) :: eyatm
    REAL, DIMENSION(:), ALLOCATABLE :: xatm,yatm,cosc,gwt,sinc,colat,wos2
    INTEGER :: nxatm,nyatm,nxatm2,nyatm2,i,nx,ny
    REAL :: dxatm
    !
    nxatm=SIZE(exatm)
    nyatm=SIZE(eyatm)
    IF(edges.EQ.1) THEN
       nxatm=nxatm-1
       nyatm=nyatm-1
    ENDIF
    !
    nxatm2=nxatm/2 ; nyatm2=nyatm/2 ; dxatm=360./nxatm
    !
    !   calculate latitudes and longitudes of ATM gaussian grid.
    !
    ALLOCATE (xatm(nxatm),yatm(nyatm) &
         &    ,cosc(1:nyatm),gwt(1:nyatm),sinc(1:nyatm),colat(1:nyatm),wos2(1:nyatm))
    CALL lggaus(nyatm,cosc,gwt,sinc,colat,wos2)
    yatm=(/(SIGN(1,nyatm2-i)*ACOS(sinc(i))*acon,i=1,nyatm)/)
    xatm=(/((i-1.)*dxatm,i=1,nxatm)/)
    !
    !   Calculate ATM grid points used in interpolation
    !   edges=1 at edges; otherwise at center
    !
    !   step 1. reset origin to nxatm2
    exatm=xatm/radian
    !180! exatm(1:nxatm2)=(xatm(nxatm2+1:nxatm)-360.0)/radian
    !180! exatm(nxatm2+1:nxatm)=xatm(1:nxatm2)/radian
    !   step 2  edges
    IF(edges.EQ.1) THEN
       exatm(1:nxatm)=exatm(1:nxatm)-0.5*dxatm/radian
       exatm(nxatm+1)=exatm(nxatm)+dxatm/radian
       !
       eyatm(1)=(yatm(1)-0.5*(yatm(2)-yatm(1)))/radian
       eyatm(2:nyatm)=0.5*(yatm(1:nyatm-1)+yatm(2:nyatm))/radian
       eyatm(nyatm+1)=(yatm(nyatm)+0.5*(yatm(nyatm)-yatm(nyatm-1)))/radian
    ELSE
       eyatm=yatm/radian
    ENDIF
    !
    IF (dbgn>=2) THEN
       ny=nyatm
       nx=nxatm
       IF(edges.EQ.1) THEN
          ny=ny+1
          nx=nx+1
       ENDIF
       WRITE (*,'("yatm ==========")')
       WRITE (*,'(1X,I5,F10.4)') (i,yatm(nyatm+1-i),i=1,nyatm)
       WRITE (*,'("xatm ==========")')
       WRITE (*,'(1X,I5,F10.4)') (i,xatm(i),i=1,nxatm)
       WRITE (*,'("eyatm ==========")')
       WRITE (*,'(1X,I5,2F10.4)') (i,eyatm(i),eyatm(i)*radian,i=1,ny)
       WRITE (*,'("exatm ==========")')
       WRITE (*,'(1X,I5,2F10.4)') (i,exatm(i),exatm(i)*radian,i=1,nx)
    END IF
    !
  END SUBROUTINE atm_gaussian_1
  !
  !========================================================================
  !
  SUBROUTINE atm_gaussian_2(exatm,eyatm,edges,mapflg,plon0,nshift,dbgn)
    REAL,PARAMETER:: acon=180./3.14159265
    INTEGER, INTENT(in) :: dbgn,edges,mapflg
    INTEGER, INTENT(out) :: nshift
    REAL, INTENT(in) :: plon0 
    REAL, DIMENSION(:),INTENT(out) :: exatm
    REAL, DIMENSION(:),INTENT(out) :: eyatm
    REAL, DIMENSION(:), ALLOCATABLE :: xatm,yatm,cosc,gwt,sinc,colat,wos2
    INTEGER :: nxatm,nyatm,nxatm2,nyatm2,i,nx,ny
    REAL :: dxatm
    !
    nxatm=SIZE(exatm)
    nyatm=SIZE(eyatm)
    IF(edges.EQ.1) THEN
       nxatm=nxatm-1
       nyatm=nyatm-1
    ENDIF
    !
    nxatm2=nxatm/2 ; nyatm2=nyatm/2 ; dxatm=360./nxatm
    !
    !   calculate latitudes and longitudes of ATM gaussian grid.
    !
    ALLOCATE (xatm(nxatm),yatm(nyatm) &
         &    ,cosc(1:nyatm),gwt(1:nyatm),sinc(1:nyatm),colat(1:nyatm),wos2(1:nyatm))
    CALL lggaus(nyatm,cosc,gwt,sinc,colat,wos2)
    yatm=(/(SIGN(1,nyatm2-i)*ACOS(sinc(i))*acon,i=1,nyatm)/)
    xatm=(/((i-1.)*dxatm,i=1,nxatm)/)
    IF (mapflg==mapflg_tripol) THEN
       DO i=1,nxatm-1
          IF (xatm(i)>=plon0 .AND. plon0<xatm(i+1)) THEN
             nshift=i-1
             EXIT
          ENDIF
       END DO
       xatm=xatm+plon0
    ENDIF
    !
    !   Calculate ATM grid points used in interpolation
    !   edges=1 at edges; otherwise at center
    !
    !   step 1. reset origin to nxatm2
    exatm=xatm/radian
    !180! exatm(1:nxatm2)=(xatm(nxatm2+1:nxatm)-360.0)/radian
    !180! exatm(nxatm2+1:nxatm)=xatm(1:nxatm2)/radian
    !   step 2  edges
    IF(edges.EQ.1) THEN
       exatm(1:nxatm)=exatm(1:nxatm)-0.5*dxatm/radian
       exatm(nxatm+1)=exatm(nxatm)+dxatm/radian
       !
       eyatm(1)=(yatm(1)-0.5*(yatm(2)-yatm(1)))/radian
       eyatm(2:nyatm)=0.5*(yatm(1:nyatm-1)+yatm(2:nyatm))/radian
       eyatm(nyatm+1)=(yatm(nyatm)+0.5*(yatm(nyatm)-yatm(nyatm-1)))/radian
    ELSE
       eyatm=yatm/radian
    ENDIF
    !
    IF (dbgn>=2) THEN
       ny=nyatm
       nx=nxatm
       IF(edges.EQ.1) THEN
          ny=ny+1
          nx=nx+1
       ENDIF
       PRINT *,'nshift,plon0=',nshift,plon0
       WRITE (*,'("yatm ==========")')
       WRITE (*,'(1X,I5,F10.4)') (i,yatm(nyatm+1-i),i=1,nyatm)
       WRITE (*,'("xatm ==========")')
       WRITE (*,'(1X,I5,F10.4)') (i,xatm(i),i=1,nxatm)
       WRITE (*,'("eyatm ==========")')
       WRITE (*,'(1X,I5,2F10.4)') (i,eyatm(i),eyatm(i)*radian,i=1,ny)
       WRITE (*,'("exatm ==========")')
       WRITE (*,'(1X,I5,2F10.4)') (i,exatm(i),exatm(i)*radian,i=1,nx)
    END IF
    !
  END SUBROUTINE atm_gaussian_2
  !
  !========================================================================
  !
  SUBROUTINE lggaus( nlat, cosc, gwt, sinc, colat, wos2 )
    !   ********** warning *************
    !   this routine may not converge using 32-bit arithmetic
    !
    !   routines from amy solomon, 28 jan 1991.
    !
    !    lggaus finds the gaussian latitudes by finding the roots of the
    !    ordinary legendre polynomial of degree nlat using newton's
    !    iteration method.
    !
    !    on entry:
    !       nlat - the number of latitudes (degree of the polynomial)
    !  
    !    on exit: for each gaussian latitude
    !       cosc   - cos(colatitude) or sin(latitude)
    !       gwt    - the gaussian weights
    !       sinc   - sin(colatitude) or cos(latitude)
    !       colat  - the colatitudes in radians
    !       wos2   - gaussian weight over sin**2(colatitude)
    !    also
    !       xlim   -convergence criterion for iteration of cos latitude
    !
    !
    !-----------------------------------------------------------------------
    !
    REAL:: cosc(nlat),gwt(nlat),sinc(nlat),colat(nlat),wos2(nlat) & 
         &   ,fi,fi1,a,b,c,d,g,gm,gp,gt,delta
    !irivin: to speed up things
    REAL, PARAMETER :: pi = 3.141592653589793,xlim  = 1.0e-5
    !real, parameter :: pi = 3.141592653589793,xlim  = 1.0e-7
    INTEGER :: nlat,nzero,nother,i,j
    !
    !      -the number of zeros between pole and equator
    nzero = nlat/2
    nother= nlat-nzero+1
    !
    !      -set first guess for cos(colat)      
    DO i=1,nzero
       cosc(i) = SIN( (i-0.5)*pi/nlat + pi*0.5 )
    ENDDO
    !
    !      -constants for determining the derivative of the polynomial
    fi  = nlat
    fi1 = fi+1.0
    a   = fi*fi1 / SQRT(4.0*fi1*fi1-1.0)
    b   = fi1*fi / SQRT(4.0*fi*fi-1.0)
    !
    !    -loop over latitudes, iterating the search for each root
    DO i=1,nzero
       j=0
       !
       !         -determine the value of the ordinary legendre polynomial for
       !         -the current guess root
30     CALL lgord( g, cosc(i), nlat )
       !
       !         -determine the derivative of the polynomial at this point
       CALL lgord( gm, cosc(i), nlat-1 )
       CALL lgord( gp, cosc(i), nlat+1 )
       gt = (cosc(i)*cosc(i)-1.0) / (a*gp-b*gm)
       !
       !         -update the estimate of the root
       delta   = g*gt
       cosc(i) = cosc(i) - delta
       !
       !         -if convergence criterion has not been met, keep trying
       j = j+1
       IF( ABS(delta).GT.xlim ) go to 30
       !     print*,' lat no.',i,j,' iterations'
       !
       !         -determine the gaussian weights
       c      = 2.0 *( 1.0-cosc(i)*cosc(i) )
       CALL lgord( d, cosc(i), nlat-1 )
       d      = d*d*fi*fi
       gwt(i) = c *( fi-0.5 ) / d
    ENDDO
    !
    !      -determine the colatitudes and sin(colat) and weights over sin**2
    colat(1:nzero)= ACOS(cosc(1:nzero))
    sinc(1:nzero) = SIN(colat(1:nzero))
    wos2(1:nzero) = gwt(1:nzero) /( sinc(1:nzero)*sinc(1:nzero) )
    !
    !      -if nlat is odd, set values at the equator
    IF( MOD(nlat,2) .NE. 0 ) THEN
       i       = nzero+1
       cosc(i) = 0.0
       c       = 2.0
       CALL lgord( d, cosc(i), nlat-1 )
       d       = d*d*fi*fi
       gwt(i)  = c *( fi-0.5 ) / d
       colat(i)= pi*0.5
       sinc(i) = 1.0
       wos2(i) = gwt(i)
    END IF
    !
    !      -determine the southern hemisphere values by symmetry
    DO i=nlat-nzero+1,nlat
       cosc(i) =-cosc(nlat+1-i)
       gwt(i)  = gwt(nlat+1-i)
       colat(i)= pi-colat(nlat+1-i)
       sinc(i) = sinc(nlat+1-i)
       wos2(i) = wos2(nlat+1-i)
    END DO
    RETURN
  END SUBROUTINE lggaus
  !
  !========================================================================
  !
  SUBROUTINE lgord( f, cosc, n )
    ! 
    !    lgord calculates the value of an ordinary legendre polynomial at a
    !    latitude.
    !
    !    on entry:
    !       cosc - cos(colatitude)
    !       n      - the degree of the polynomial
    !
    !    on exit:
    !       f      - the value of the legendre polynomial of degree n at
    !                latitude asin(cosc)
    !
    !------------------------------------------------------------------------
    REAL :: f,cosc,colat,c1,fn,ang,s1,c4,a,b,fk
    INTEGER :: n,k
    !  
    !      -determine the colatitude
    colat = ACOS(cosc)
    !
    c1 = SQRT(2.0)
    DO k=1,n
       c1 = c1 * SQRT( 1.0 - 1.0/(4*k*k) )
    END DO
    !
    fn = n
    ang= fn * colat
    s1 = 0.0
    c4 = 1.0
    a  =-1.0
    b  = 0.0
    DO k=0,n,2
       IF (k.EQ.n) c4 = 0.5 * c4
       s1 = s1 + c4 * COS(ang)
       a  = a + 2.0
       b  = b + 1.0
       fk = k
       ang= colat * (fn-fk-2.0)
       c4 = ( a * (fn-b+1.0) / ( b * (fn+fn-a) ) ) * c4
    END DO
    f = s1 * c1
    !
    RETURN
  END SUBROUTINE lgord
  !
  !========================================================================
  !
  SUBROUTINE ismus(mskout)
    REAL, DIMENSION(:,:) :: mskout
    INTEGER :: i,j,m,n,k,num,mm,nn,iu=18
    REAL :: a,b
    CHARACTER*30 as,bs,cs
    LOGICAL ex
    n=SIZE(mskout,2)
    m=SIZE(mskout,1)
    WRITE(as,*)m
    WRITE(bs,*)n
    cs='ismus_msk'//TRIM(ADJUSTL(as))//'x'//TRIM(ADJUSTL(bs))//'.dat'
    INQUIRE(file=TRIM(cs),exist=ex)
    IF(.NOT.ex) THEN
       PRINT *,'ismus: warning: mask for the ismus correction is not found' &
            &          ,'(looked for the file '//TRIM(cs)//')'

       STOP
    ENDIF
    PRINT *,'ismus: mask for the ismus correction is '//cs
    OPEN(iu,file=TRIM(cs),form="formatted")
    READ(iu,*)a,b
    num=INT(a)
    IF (num>=0) THEN
       READ(iu,*)a,b
       mm=INT(a)
       nn=INT(b)
       IF(mm.NE.m .OR. nn.NE.n) THEN
          PRINT *,' ismus: Error in ismus'
          PRINT *,'ismus: m,n,mm,nn =',m,n,mm,nn
          STOP
       ENDIF
       DO k=1,num
          READ(iu,*)a,b
          i=INT(a)
          j=INT(b)
          IF(i.LT.1.OR.i.GT.m.OR.j.LT.1.OR.j.GT.n) THEN
             PRINT *,'ismus:  Error in ismus '
             PRINT *,'ismus: i,j,k=',i,j,k
             STOP
          ENDIF
          mskout(i,j)=0.  ! ATM convention: 1-land, 0-ocean
          PRINT *,'ismus:  ATM mask is corrected for i,j=',i,j
       ENDDO
    ELSE
       PRINT *,' ismus: Error in ismus'
       STOP
    ENDIF
    CLOSE(iu)
  END SUBROUTINE ismus
  !
  !========================================================================
  !
  SUBROUTINE mask_atm_1(lu,name,kpds,mskfrac,nextrap_max,msk &
,imsk_hycom,msk_tmp,nextrap,mapflg,exhycom2d,eyhycom2d,global)
    !
    !   Establish ATM mask (land=0,sea=1)
    !
    ! INPUT:
    ! lu          is init number for atmospheric GRIB file with land/sea mask
    ! name        is name of the atmospheric GRIB file with land/sea mask
    ! mskfrac     is a number which ....
    ! nextrap_max is maximum number of inerations in mask extrapolation procedure
    ! kpds        are pds(5,6,7) numbers for LAND in atmospheric GRIB file.
    ! imsk_hycom  is land/sea mask on HYCOM grid
    ! mapflag     is the flag defining the HYCOM grid type.
    ! exhycom2d
    ! eyhycom2d
    ! global      (optional)
    !
    ! OUTPUT:
    ! msk
    ! msk_tmp
    ! nextrap is an actual number of iteration used in mask extrapolation procedure.
    !
    CHARACTER*(*), INTENT(in) :: name
    INTEGER, INTENT(in) :: lu,nextrap_max
    REAL, INTENT(in) :: mskfrac
    INTEGER, DIMENSION(:,:), INTENT(in) :: imsk_hycom
    INTEGER, DIMENSION(:), INTENT(in)  :: kpds
    REAL, DIMENSION(:,:), INTENT(out) :: msk,msk_tmp
    REAL, DIMENSION(SIZE(msk,1),SIZE(msk,2)) :: flx
    REAL, DIMENSION(SIZE(imsk_hycom,1),SIZE(imsk_hycom,2)) :: msk_out
    REAL, DIMENSION(:,:) :: exhycom2d,eyhycom2d
    INTEGER, INTENT (out) :: nextrap
    INTEGER, INTENT (in) :: mapflg
    LOGICAL, OPTIONAL, INTENT (in) :: global
    INTEGER :: i,j,n,nxatm,nyatm,nxhycom,nyhycom,nxatm2,nyatm2
    CHARACTER*30 as,bs,cs
    LOGICAL ::   ex,glo=.FALSE.
    INTEGER :: idum
    !
    IF (PRESENT(global)) glo=global
    nxatm=SIZE(msk,1)
    nyatm=SIZE(msk,2)
    nxhycom=SIZE(imsk_hycom,1)
    nyhycom=SIZE(imsk_hycom,2)
    WRITE(*,*)SIZE(exhycom2d,dim=1),SIZE(exhycom2d,dim=2),' exhycom2d size'
    WRITE(*,*)SIZE(eyhycom2d,dim=1),SIZE(eyhycom2d,dim=2),' eyhycom2d size'
    nxatm2=nxatm/2 ; nyatm2=nyatm/2 
!    CALL rdgrib(lu,TRIM(name),xgfld,jpdtno,npts)  ! LAND
    !dbgz IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    flx=1 ! all is sea
    !
    !   Fix ithsmus problem.
    !
    ! ilya; ismus correction is ready for this grid only
    IF (mapflg==mapflg_ort)  CALL ismus(flx)
    !
    !   Shift mask following shift of longitude grid
    !
    !180! msk_tmp(1:nxatm2      ,:)=flx(nxatm2+1:nxatm,:)
    !180! msk_tmp(nxatm2+1:nxatm,:)=flx(1:nxatm2      ,:)
    msk=ABS(1.-msk_tmp(:,:))                          ! land =0 ; sea =1
    IF (glo) THEN
       CALL global_extend(msk)
       CALL global_extend(msk_tmp)
    ENDIF
    !
    !   If required, determine number of iterations needed to extrapolate field in ATM 
    !   grid to cover ocean region 
    !
    msk_tmp=msk 
    iterx: DO n=1,nextrap_max
       CALL extend_fld(msk_tmp,flx(:,:),msk_tmp,flx(:,:))
       CALL horiz_intp(exhycom2d,eyhycom2d,msk_tmp,msk_out,idum)
       loopj: DO j=1,nyhycom
          loopi: DO i=1,nxhycom
             IF(imsk_hycom(i,j)==1 .AND. msk_out(i,j)<mskfrac) THEN
                WRITE(*,*)'iterations on land/sea mask: i,j,msk_out,imsk_hycom' &
                     ,i,j,msk_out(i,j),imsk_hycom(i,j)
                EXIT loopj 
             END IF
          END DO loopi
       END DO loopj
       IF (j>nyhycom .AND.  i>nxhycom) EXIT iterx
    END DO iterx
    nextrap=n+1
    WRITE(*,*)'+++++ # of interations for land/sea mask extrapolation is nextrap=',nextrap
  END SUBROUTINE mask_atm_1
  !
  !========================================================================
  !
  SUBROUTINE mask_atm_2(nextrap,mskin,flxin,mskout,flxout)
    !
    !       Extrapolate field in ATM grid to cover all ocean in HYCOM grid
    !
    INTEGER, INTENT(in) :: nextrap
    REAL, DIMENSION(:,:), INTENT(in) :: mskin,flxin
    REAL, DIMENSION(:,:), INTENT(out) :: flxout,mskout
    INTEGER :: n
    mskout=mskin
    flxout=flxin
    DO n=1,nextrap
       CALL extend_fld(mskout,flxout,mskout,flxout)
    END DO
  END SUBROUTINE mask_atm_2
  !
  !========================================================================
  !
  SUBROUTINE rd_hycom_grid_params(idm,jdm,mapflg,ex)
    !
    !   read HYCOM grid parameters 
    !
    INTEGER, INTENT(out) :: idm,jdm,mapflg
    LOGICAL, INTENT(out)  :: ex
    CHARACTER*80 :: cline
    idm=-1 ; jdm=-1; mapflg=-1
    INQUIRE(file='regional.grid.a',exist=ex)
    IF(.NOT.ex) RETURN
    INQUIRE(file='regional.grid.b',exist=ex)
    IF(.NOT.ex) RETURN
    OPEN(8, file='regional.grid.b',status='old' ,action='read')
    READ(8,'(a)') cline ; READ (cline,*) idm
    READ(8,'(a)') cline ; READ (cline,*) jdm
    READ(8,'(a)') cline ; READ (cline,*) mapflg
    IF (mapflg /= mapflg_mer .AND. mapflg /= mapflg_ort .AND.  mapflg /= mapflg_tripol ) THEN 
       PRINT *, 'ERROR: Wrong map flag in regional.grid.b, mapflg=',mapflg
       STOP
    ENDIF
    PRINT *,'HYCOM GRID parameters from regional.grid.b: mapflg,idm,jdm=', mapflg,idm,jdm
    CLOSE(8)    
  END SUBROUTINE rd_hycom_grid_params
  !
  !========================================================================
  !
  SUBROUTINE extend_fld(msk_in,fld_in,msk_out,fld_out)
    !
    ! Extrapolate field fld_in under the mask msk_in.
    ! The zero order extrapolated field fld_out, is valid for the extended mask 
    ! msk_out.
    !
    ! Convention: invalid field value if mask has value 0, otherwise is valid.
    !
    ! msk_in     input real mxn array mask
    ! fld_in     input real mxn array    field
    ! msk_out    output real mxn array extended mask
    ! fld_out    output real mxn array    extended field
    !
    REAL, DIMENSION(:,:), INTENT(in) ::  msk_in
    REAL, DIMENSION(:,:), INTENT(out) :: msk_out
    REAL, DIMENSION(:,:), INTENT(in)    ::  fld_in
    REAL, DIMENSION(:,:), INTENT(out)    :: fld_out
    INTEGER :: i,j,i1,j1,m,n,imsk_in(SIZE(msk_in,1),SIZE(msk_in,2))
    REAL :: val, count, corner

    m=SIZE(msk_in,1)
    n=SIZE(msk_in,2)
    corner=1.0/SQRT(2.0)
    !
    ! -- copy field and mask
    !
    fld_out=fld_in
    msk_out=msk_in
    imsk_in=INT(msk_in)
    !
    ! -- update under the msk_in using values of fld_in at eight
    ! -- neighbor points if they are valid.
    ! -- if the update is possible, set msk_out to valid and fld_out
    ! -- to the average of the valid fld_in values.
    !
    DO j=1,n
       DO i=1,m
          IF( imsk_in(i,j).EQ.0 ) THEN
             val=0.0
             count=0.0
             IF(i.GT.1 ) THEN
                i1=i-1
                j1=j
                IF ( imsk_in(i1,j1).EQ.1) THEN
                   val=val+fld_in(i1,j1)
                   count=count+1.0
                ENDIF
             ENDIF
             !
             IF(i.LT.m ) THEN
                i1=i+1
                j1=j
                IF ( imsk_in(i1,j1).EQ.1) THEN
                   val=val+fld_in(i1,j1)
                   count=count+1.0
                ENDIF
             ENDIF
             IF(j.GT.1 ) THEN
                i1=i
                j1=j-1
                IF ( imsk_in(i1,j1).EQ.1) THEN
                   val=val+fld_in(i1,j1)
                   count=count+1.0
                ENDIF
             ENDIF
             !
             IF(j.LT.n ) THEN
                i1=i
                j1=j+1
                IF ( imsk_in(i1,j1).EQ.1) THEN
                   val=val+fld_in(i1,j1)
                   count=count+1.0
                ENDIF
             ENDIF
             !
             IF(i.GT.1.AND.j.LT.n ) THEN
                i1=i-1
                j1=j+1
                IF ( imsk_in(i1,j1).EQ.1) THEN
                   val=val+fld_in(i1,j1)*corner
                   count=count+corner
                ENDIF
             ENDIF
             !
             IF(i.LT.m.AND.j.GT.1 ) THEN
                i1=i+1
                j1=j-1
                IF ( imsk_in(i1,j1).EQ.1) THEN
                   val=val+fld_in(i1,j1)*corner
                   count=count+corner
                ENDIF
             ENDIF
             IF(j.GT.1.AND.i.GT.1 ) THEN
                i1=i-1
                j1=j-1
                IF ( imsk_in(i1,j1).EQ.1) THEN
                   val=val+fld_in(i1,j1)*corner
                   count=count+corner
                ENDIF
             ENDIF
             !
             IF(j.LT.n.AND.i.LT.m ) THEN
                i1=i+1
                j1=j+1
                IF ( imsk_in(i1,j1).EQ.1) THEN
                   val=val+fld_in(i1,j1)*corner
                   count=count+corner
                ENDIF
             ENDIF
             IF(count.GT.0.0) THEN
                fld_out(i,j)=val/count
                msk_out(i,j)=1.
             ENDIF
          ENDIF
       ENDDO
    ENDDO

    RETURN
  END SUBROUTINE extend_fld
  !
  !========================================================================
  !
  SUBROUTINE horiz_intp_init(exatm,eyatm,qx,qy,ex,ey,iu,global)
    IMPLICIT NONE
    REAL, DIMENSION(:,:),INTENT(in) ::  qx,qy
    REAL, DIMENSION(:),INTENT(in) ::  exatm
    REAL, DIMENSION(0:),INTENT(in) ::  eyatm
    REAL, DIMENSION(:,:),INTENT(out) :: ex,ey
    INTEGER, INTENT(in):: iu
    LOGICAL, OPTIONAL, INTENT(in) :: global
    INTEGER :: i,j,idm,jdm
    INTEGER :: id,jd,ip,jp,ic,ipmax,jpmax,ipmin,jpmin
    REAL :: x,y
    LOGICAL :: glo=.FALSE.
    !
    IF (PRESENT(global)) glo=global
    id=SIZE(exatm)
    jd=SIZE(eyatm)
    idm=SIZE(qx,dim=1)
    jdm=SIZE(qy,dim=2)
    !
    ! --- check that ocean grid is properly covered by atm grid
    !
    ic=0
    IF( MINVAL(exatm) > MINVAL(qx) ) THEN
       ic = ic + 1
       WRITE(iu,*)'min of exatm ', MINVAL(exatm),' >  min of lon target grid ',MINVAL(qx)
    ENDIF
    IF( MINVAL(eyatm)  > MINVAL(qy) ) THEN
       ic = ic + 1
       WRITE(iu,*)'min of eyatm ', MINVAL(eyatm),' >  min of lat target grid ',MINVAL(qy)
    ENDIF
    IF( MAXVAL(exatm) < MAXVAL(qx) ) THEN
       ic = ic + 1
       WRITE(iu,*)'max of exatm ', MAXVAL(exatm),' <  max of lon target grid ',MAXVAL(qx)
    ENDIF
    IF( MAXVAL(eyatm) < MAXVAL(qy) ) THEN
       ic = ic + 1
       WRITE(iu,*)'max of eyatm ', MAXVAL(eyatm),' >  max of lat target grid ',MAXVAL(qy)
    ENDIF
    IF(ic > 0) THEN
       WRITE(iu,*)' eatm grid does not contain target grid.'
       CALL flush(iu)
       STOP
    ENDIF
    !
    IF (glo) THEN
       ipmin=1
       jpmin=1
       ipmax=id+1
       jpmax=jd+1
    ELSE
       ipmin=1
       jpmin=1
       ipmax=id
       jpmax=jd
    ENDIF
    DO j=1,jdm
       DO i=1,idm
          ! exatm is in  increasing order
          x=qx(i,j)
          ip=ipmin
          DO WHILE(x > exatm(ip) .AND. ip < ipmax )
             ip=ip+1
          ENDDO
          ex(i,j)=ip-1.0+(x-exatm(ip-1))/(exatm(ip)-exatm(ip-1))
          ! eyatm is in decreasing order
          y=qy(i,j)
          ip=jpmin
          DO WHILE(y < eyatm(ip) .AND. ip < jpmax)
             ip=ip+1
          ENDDO
          ey(i,j)=ip-1.0+(y-eyatm(ip-1))/(eyatm(ip)-eyatm(ip-1))
       ENDDO
    ENDDO

    write(*,*) 'horiz_intp_init: idm,jdm=', idm,jdm !dbgzp
    write(*,*) 'horiz_intp_init: qx min, max=',minval(qx),maxval(qx) !dbgzp
    write(*,*) 'horiz_intp_init: qy min, max=',minval(qy),maxval(qy) !dbgzp
    write(*,*) 'horiz_intp_init: exatm min, max=',minval(exatm),maxval(exatm) !dbgzp
    write(*,*) 'horiz_intp_init: eyatm min, max=',minval(eyatm),maxval(eyatm) !dbgzp
    write(*,*) 'horiz_intp_init: ex min, max=',minval(ex),maxval(ex) !dbgzp
    write(*,*) 'horiz_intp_init: ey min, max=',minval(ey),maxval(ey) !dbgzp

    RETURN
  END SUBROUTINE horiz_intp_init
  !
  !========================================================================
  !
  SUBROUTINE horiz_intp_old(win,wout,xout,yout)
    IMPLICIT NONE
    REAL, DIMENSION(:,:),INTENT(in) ::  win, xout,yout
    REAL, DIMENSION(:,:),INTENT(out) :: wout
    INTEGER i,j,idmp1,jdmp1,k
    INTEGER ii,jj
    REAL x,y,z1,z2,z3,z4
    !
    idmp1=SIZE(wout,dim=1)
    jdmp1=SIZE(wout,dim=2)

    DO j=1,jdmp1-1
       DO i=1,idmp1-1
          x=xout(i,j)
          y=yout(i,j)
          ii=INT(x)
          x=x-ii
          jj=INT(y)
          y=y-jj
          z1=(win(ii,jj)*(1.0-x)+win(ii+1,jj)*x)*(1.-y)     &
               + (win(ii,jj+1)*(1.0-x)+win(ii+1,jj+1)*x)*y
          !
          x=xout(i+1,j)
          y=yout(i+1,j)
          ii=INT(x)
          x=x-ii
          jj=INT(y)
          y=y-jj
          z2=(win(ii,jj)*(1.0-x)+win(ii+1,jj)*x)*(1.-y)     &
               +(win(ii,jj+1)*(1.0-x)+win(ii+1,jj+1)*x)*y
          !
          x=xout(i+1,j+1)
          y=yout(i+1,j+1)
          ii=INT(x)
          x=x-ii
          jj=INT(y)
          y=y-jj
          z3=(win(ii,jj)*(1.0-x)+win(ii+1,jj)*x)*(1.-y)     &
               +(win(ii,jj+1)*(1.0-x)+win(ii+1,jj+1)*x)*y
          !
          x=xout(i,j+1)
          y=yout(i,j+1)
          ii=INT(x)
          x=x-ii
          jj=INT(y)
          y=y-jj
          z4=(win(ii,jj)*(1.0-x)+win(ii+1,jj)*x)*(1.-y)     &
               +(win(ii,jj+1)*(1.0-x)+win(ii+1,jj+1)*x)*y
          !
          wout(i,j)=0.25*(z1+z2+z3+z4)
       ENDDO
    ENDDO
    RETURN
  END SUBROUTINE  horiz_intp_old
  !
  !========================================================================
  !
  SUBROUTINE horiz_intp_(win,wout,xout,yout)
    IMPLICIT NONE
    REAL, DIMENSION(:,:),INTENT(in) ::  win, xout,yout
    REAL, DIMENSION(:,:),INTENT(out) :: wout
    INTEGER :: i,j,idmp1,jdmp1
    REAL,DIMENSION(4) :: z
    !
    idmp1=SIZE(wout,dim=1)
    jdmp1=SIZE(wout,dim=2)
    DO j=1,jdmp1-1
       DO i=1,idmp1-1
          CALL edge_interp( win ,xout(i,j)     ,yout(i,j)      ,z(1) )
          CALL edge_interp( win ,xout(i+1,j)   ,yout(i+1,j)    ,z(2) )
          CALL edge_interp( win ,xout(i+1,j+1) ,yout(i+1,j+1)  ,z(3) )
          CALL edge_interp( win ,xout(i,j+1)   ,yout(i,j+1)    ,z(4) )
          wout(i,j)=0.25*SUM(z)
       ENDDO
    ENDDO
    wout(idmp1,:)=wout(1,:)
    wout(:,jdmp1)=wout(:,1)
    RETURN
  END SUBROUTINE  horiz_intp_
  !
  !========================================================================
  !
  SUBROUTINE  edge_interp(win,xin,yin,z)
    REAL, DIMENSION(:,:),INTENT(in) ::  win
    REAL, INTENT(in):: xin,yin
    REAL, INTENT(out):: z
    REAL :: x,y
    INTEGER :: ii,jj
    !
    ii=INT(xin)
    x=xin-ii
    jj=INT(yin)
    y=yin-jj
    z=(win(ii,jj)*(1.0-x)+win(ii+1,jj)*x)*(1.-y)     &
         +(win(ii,jj+1)*(1.0-x)+win(ii+1,jj+1)*x)*y
  END SUBROUTINE  edge_interp
  !
  !========================================================================
  !
  SUBROUTINE  horiz_intp_msk(xout,yout,mskin,mskout,iu)
    IMPLICIT NONE
    REAL, DIMENSION(:,:),INTENT(in) ::  xout,yout,mskin
    REAL, DIMENSION(:,:),INTENT(out) :: mskout
    INTEGER, INTENT(in) :: iu
    INTEGER i,j,idmp1,jdmp1
    INTEGER ii,jj
    REAL x,y
    !
    idmp1=SIZE(xout,dim=1)
    jdmp1=SIZE(xout,dim=2)
    DO j=1,jdmp1-1
       DO i=1,idmp1-1
          mskout(i,j)=0.0
          x=xout(i,j)
          y=yout(i,j)
          ii=INT(x)
          jj=INT(y)
          IF(mskin(ii,jj).EQ.1.0) THEN
             mskout(i,j)=mskout(i,j)+0.25
          ENDIF
          x=xout(i+1,j)
          y=yout(i+1,j)
          ii=INT(x)
          jj=INT(y)
          !
          IF(mskin(ii,jj).EQ.1.0) THEN
             mskout(i,j)=mskout(i,j)+0.25
          ENDIF
          !
          x=xout(i+1,j+1)
          y=yout(i+1,j+1)
          ii=INT(x)
          jj=INT(y)
          IF(mskin(ii,jj).EQ.1.0) THEN
             mskout(i,j)=mskout(i,j)+0.25
          ENDIF
          !
          x=xout(i,j+1)
          y=yout(i,j+1)
          ii=INT(x)
          jj=INT(y)
          IF(mskin(ii,jj).EQ.1.0) THEN
             mskout(i,j)=mskout(i,j)+0.25
          ENDIF
          !
       ENDDO
    ENDDO
    RETURN
  END SUBROUTINE  horiz_intp_msk
  !
  !========================================================================
  !
  SUBROUTINE global_extend_latlon_1d(x,y)
    REAL, DIMENSION(:), INTENT(inout):: x,y
    INTEGER :: nx,ny
    REAL :: dx
    ! integer :: i ! dbgzp
    nx=SIZE(x)
    ny=SIZE(y)
    dx=x(3)-x(2)
    x(1)=x(2)-dx
    x(nx)=x(nx-1)+dx
    y(1)=90.0/radian ! dbgz
    y(ny)=-90.0/radian  !dbgz
    ! print *, 'global_extend: ny=',ny !dbgzp
    ! print *, 'global_extend: x: min,max=',minval(x)*radian,maxval(x)*radian !dbgzp
    ! print *, 'global_extend: y: min,max=',minval(y)*radian,maxval(y)*radian !dbgzp
    ! write(*,'(a,i5,f13.4)') ('global_extend: y: ',i,y(i)*radian,i=1,ny) ! dbgzp
  END SUBROUTINE global_extend_latlon_1d
  !
  !========================================================================
  !
  SUBROUTINE global_extend_mask(msk)
    INTEGER, DIMENSION(:,:), INTENT(inout):: msk
    INTEGER :: nx,ny
    nx=SIZE(msk,dim=1)
    ny=SIZE(msk,dim=2)
    msk(1,:)=msk(nx-1,:)
    msk(nx,:)=msk(2,:)
    msk(:,1)=0 ! dbgz Ocean at Nothern Pole
    msk(:,ny)=1 ! dbgz Land at Southern Pole 
  END SUBROUTINE global_extend_mask
  !
  !========================================================================
  !
  SUBROUTINE global_extend_field_2d(fld)
    REAL, DIMENSION(:,:), INTENT(inout):: fld
    INTEGER :: nx,ny
    nx=SIZE(fld,dim=1)
    ny=SIZE(fld,dim=2)
    fld(1,:)=fld(nx-1,:)
    fld(nx,:)=fld(2,:)
    fld(:,1)=fld (:,2) !! sum(fld(2:nx-1,2))/nx !dbgz
    fld(:,ny)=fld(:,ny-1) ! sum(fld(2:nx-1,ny-1))/nx !dbgz
  END SUBROUTINE global_extend_field_2d
  !
  !========================================================================
  !
  SUBROUTINE global_extend_field_3d(fld)
    REAL, DIMENSION(:,:,:), INTENT(inout):: fld
    INTEGER :: nx,ny,nk,i
    nx=SIZE(fld,dim=1)
    ny=SIZE(fld,dim=2)
    nk=SIZE(fld,dim=3)
    DO i=1,nk
      CALL global_extend_field_2d(fld(:,:,nk))
    ENDDO
  END SUBROUTINE global_extend_field_3d
  !
  !========================================================================
  !
END MODULE mod_geom





