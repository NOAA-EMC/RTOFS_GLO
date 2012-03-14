MODULE mod_gribio
 PRIVATE
 PUBLIC rdgrib,getgds
 LOGICAL, DIMENSION(200),SAVE :: opn=.TRUE. 
CONTAINS
!
!========================================================================
!
  SUBROUTINE rdgrib(lugb,grbfile,fld,pds,cls)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: lugb,pds(3)
    CHARACTER, INTENT(in) ::  grbfile*(*)
    LOGICAL, INTENT(in) :: cls
    REAL, DIMENSION(:,:),INTENT(out) :: fld
    REAL, DIMENSION(SIZE(fld)) :: fld1
    INTEGER :: jpds(25),jgds(22),igrd(5,3)
    INTEGER :: kpds(25),kgds(22),ierr,lugi,n,ndata,kskp
    LOGICAL lbms(SIZE(fld))
!
    CALL opngrib(lugb,grbfile)
!
    lugi=lugb+1
    jpds(1:25) = -1     
    jgds(1:22) = -1
    jpds(5) = pds(1)
    jpds(6) = pds(2)
    jpds(7) = pds(3)
    n=-1
    CALL getgb(lugb,lugi,SIZE(fld1),n,jpds,jgds,ndata,kskp,kpds,kgds,lbms,fld1,ierr)
    fld=RESHAPE(source=fld1,shape=SHAPE(fld))

    IF(ierr.NE.0) THEN
      PRINT *,' mod_gribio: error in GETGB for ',ierr,jpds
       STOP
    ENDIF
    PRINT *,'mod_gribio: jpds(5:7),fldmin,fldmax=',jpds(5:7),MINVAL(fld),MAXVAL(fld)
!
   IF (cls) THEN
     CALL clsgrib(lugb)
   ENDIF
  END SUBROUTINE rdgrib
!
!===========================================================================================
!
  SUBROUTINE opngrib(lugb,grbfile)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: lugb
    CHARACTER, INTENT(in) ::  grbfile*(*)
    INTEGER :: lugi,ierr
    CHARACTER (len=200) :: idxfile 
    LOGICAL, SAVE :: opned
! 
    lugi=lugb+1
    idxfile=TRIM(grbfile)//'.idx'
    IF (opn(lugb)) THEN 
      INQUIRE(unit=lugb,opened=opned)
      IF(opned) THEN
        PRINT *,' mod_gribio: error: unit lugb=',lugb,' is in use'
        STOP
      ENDIF
      CALL baopenr(lugb,TRIM(grbfile),ierr)         
      IF(ierr.NE.0) THEN
        PRINT *,'mod_gribio: error opening file ',grbfile
        STOP
      ELSE
        WRITE(*,*) 'mod_gribio: opening GRIB file '//TRIM(grbfile)//" as unit ",lugb
      ENDIF
      INQUIRE(unit=lugi,opened=opned)
      IF(opned) THEN
        PRINT *,' mod_gribio: error: unit lugi=',lugi,' is in use'
        STOP
      ENDIF
      CALL baopenr(lugi,TRIM(idxfile),ierr)
      IF(ierr.NE.0) THEN
        PRINT *,'error opening file ',idxfile
! NOTE: later instead of stopping set idxunit=0 and continue make index file 
        STOP
      ELSE
        WRITE(*,*) 'mod_gribio: opening index file '//TRIM(idxfile)//" as unit ",lugi
      ENDIF
      opn(lugb)=.FALSE.
    ENDIF
!
  END SUBROUTINE opngrib
!
!===========================================================================================
!
  SUBROUTINE clsgrib(lugb)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: lugb
    INTEGER :: lugi,ierr
! 
    lugi=lugb+1
    CALL baclose(lugb,ierr)
    IF(ierr.NE.0) THEN
      PRINT *,'mod_gribio: error closing unit ',lugb
      STOP
    ENDIF
    PRINT *,'unit ', lugb,' closed'
    CALL baclose(lugi,ierr)
    IF(ierr.NE.0) THEN
      PRINT *,'mod_gribio: error closing unit ',lugi
      STOP
    ENDIF
    PRINT *,'unit ', lugi,' closed'
    opn(lugb)=.TRUE.
!
  END SUBROUTINE clsgrib
!
!===========================================================================================
!
  SUBROUTINE getgds(lugb,grbfile,kgds)
    IMPLICIT NONE
    INTEGER, PARAMETER :: ln=200,mbuf=256*1024
    INTEGER, INTENT(in) :: lugb
    CHARACTER, INTENT(in) ::  grbfile*(*)
    INTEGER, DIMENSION(ln), INTENT(out) :: kgds
    INTEGER, DIMENSION(ln) :: jpds=-1,jgds=-1,jens=-1,kpds,kens
    INTEGER :: jr=0,nnum,nlen,mnum=0,kr,lskip,lgrib,ierr,lugi
    CHARACTER*1 cbuf(mbuf)
    CALL opngrib(lugb,grbfile)
!
    lugi=lugb+1
!
    CALL getgi(lugi,mnum,mbuf,cbuf,nlen,nnum,ierr)
    IF(ierr.NE.0) THEN
      PRINT *,'mod_gribio: problems in subroutine getgi, ierr=',ierr
      STOP
    ENDIF
!
    CALL getgb1s(cbuf,nlen,nnum,jr,jpds,jgds,jens, kr,kpds,kgds,kens,lskip,lgrib,ierr)
    IF(ierr.NE.0) THEN
      PRINT *,'mod_gribio: problems in subroutine getgb1s, ierr=',ierr
      STOP
    ENDIF
!
   CALL clsgrib(lugb)
!    
  END SUBROUTINE getgds
!
!===========================================================================================
!
END MODULE mod_gribio
