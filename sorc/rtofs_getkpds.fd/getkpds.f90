PROGRAM getkgds
!$$$ MAIN PROGRAM DOCUMENTATION BLOCK
!
!   MAIN PROGRAM:  getkpds.f90
!   PRGMMR: ILYA RIVIN       ORG: W/NP21      DATE: 2006-06-01    
!                                UPDATED 
! ABSTRACT: THIS PROGRAM GETS KPDS VALUES FOR A GRIB FILE 
!
! PROGRAM HISTORY LOG:
!   06-06-01  RIVIN
!
! USAGE:
!   INPUT FILES:
!     FTxxF001 - UNITS 11 THRU 49
!     UNIT  5  - (STANDARD READ)
!     UNIT 46  - GRIB FILE
!     UNIT 47  - GRIB INDEX FILE
!
!   OUTPUT FILES:  
!     FTxxF001 - UNITS 51 THRU 79
!     FTxxF001 - UNIT 6 (STANDARD PRINTFILE)
!     UNIT 77  - kpds.dat
  !
  IMPLICIT NONE
  INTEGER,PARAMETER :: lugb=46,mbuf=256*1024,ln=200,lugi=lugb+1
  INTEGER :: ierr,mnum=0,nnum,nlen,jr=0,kr,lskip,lgrib,i,nout
  INTEGER, DIMENSION(ln) :: gds,pds,kpds,kgds,jpds=-1,jgds=-1,jens=-1,kens
  CHARACTER(len=500) :: grbfile,idxfile
  CHARACTER*1 cbuf(mbuf)
  EXTERNAL getgi,getgb1s
  !
  ! write(*,'(/a/)') '*************** START program getkgds ********************'
  CALL getenv('GRBFILE',value=grbfile)
  CALL getenv('IDXFILE',value=idxfile)
  !
  CALL baopenr(lugb,TRIM(ADJUSTL(grbfile)),ierr)
  IF(ierr.NE.0) THEN
     PRINT *,'error opening file ',TRIM(ADJUSTL(grbfile)),' ierr=',ierr
     STOP
  ENDIF
  CALL baopenr(lugi,TRIM(ADJUSTL(idxfile)),ierr)
  IF(ierr.NE.0) THEN
     PRINT *,'error opening file ',TRIM(ADJUSTL(idxfile)),' ierr=',ierr
     STOP
  ENDIF
  !
  CALL getgi(lugi,mnum,mbuf,cbuf,nlen,nnum,ierr)
  IF(ierr.NE.0) THEN
     PRINT *, 'problems getgi: ierr=',ierr
     STOP
  ENDIF
  !
  CALL getgb1s(cbuf,nlen,nnum,jr,jpds,jgds,jens, kr,kpds,kgds,kens,lskip,lgrib,ierr)
  IF(ierr.NE.0) THEN
     PRINT *,'problems in getgb1s, ierr=',ierr
     STOP
  ENDIF
  CALL baclose(lugb,ierr)
  IF(ierr.NE.0) THEN
     PRINT *,'error closing unit ',lugb
     STOP
  ENDIF
  CALL baclose(lugi,ierr)
  IF(ierr.NE.0) THEN
     PRINT *,'error closing unit ',lugi
     STOP
  ENDIF
  nout=SIZE(kgds,1)
  WRITE(*,*)(kgds(i),i=1,nout)
  OPEN(unit=77,file='kpds.dat',form='formatted',status='new',action='write',IOSTAT=ierr)
  IF (ierr/=0) STOP 'cant opent kpds.dat'
  WRITE(77,*,IOSTAT=ierr)(kgds(i),i=1,nout)  
  IF (ierr/=0) STOP 'cant write kpds.dat'
  ! write(*,'(/a/)') '*************** FINISH program getkgds ********************'
END PROGRAM getkgds
