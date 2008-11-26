program getkgds
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
  implicit none
  integer,parameter :: lugb=46,mbuf=256*1024,ln=200,lugi=lugb+1
  integer :: ierr,mnum=0,nnum,nlen,jr=0,kr,lskip,lgrib,i,nout
  integer, dimension(ln) :: gds,pds,kpds,kgds,jpds=-1,jgds=-1,jens=-1,kens
  character(len=500) :: grbfile,idxfile
  character*1 cbuf(mbuf)
  external getgi,getgb1s
  !
  ! write(*,'(/a/)') '*************** START program getkgds ********************'
  CALL getenv('GRBFILE',value=grbfile)
  CALL getenv('IDXFILE',value=idxfile)
  !
  call baopenr(lugb,trim(adjustl(grbfile)),ierr)
  if(ierr.ne.0) then
     print *,'error opening file ',trim(adjustl(grbfile)),' ierr=',ierr
     stop
  endif
  call baopenr(lugi,trim(adjustl(idxfile)),ierr)
  if(ierr.ne.0) then
     print *,'error opening file ',trim(adjustl(idxfile)),' ierr=',ierr
     stop
  endif
  !
  call getgi(lugi,mnum,mbuf,cbuf,nlen,nnum,ierr)
  if(ierr.ne.0) then
     print *, 'problems getgi: ierr=',ierr
     stop
  endif
  !
  call getgb1s(cbuf,nlen,nnum,jr,jpds,jgds,jens, kr,kpds,kgds,kens,lskip,lgrib,ierr)
  if(ierr.ne.0) then
     print *,'problems in getgb1s, ierr=',ierr
     stop
  endif
  call baclose(lugb,ierr)
  if(ierr.ne.0) then
     print *,'error closing unit ',lugb
     stop
  endif
  call baclose(lugi,ierr)
  if(ierr.ne.0) then
     print *,'error closing unit ',lugi
     stop
  endif
  nout=size(kgds,1)
  write(*,*)(kgds(i),i=1,nout)
  open(unit=77,file='kpds.dat',form='formatted',status='new',action='write',IOSTAT=ierr)
  if (ierr/=0) stop 'cant opent kpds.dat'
  write(77,*,IOSTAT=ierr)(kgds(i),i=1,nout)  
  if (ierr/=0) stop 'cant write kpds.dat'
  ! write(*,'(/a/)') '*************** FINISH program getkgds ********************'
end program getkgds
