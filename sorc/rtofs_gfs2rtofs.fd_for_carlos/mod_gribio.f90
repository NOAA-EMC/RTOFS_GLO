module mod_gribio
 private
 public rdgrib,getgds
 logical, dimension(200),save :: opn=.true. 
contains
!
!========================================================================
!
  subroutine rdgrib(lugb,grbfile,fld,pds,cls)
    implicit none
    integer, intent(in) :: lugb,pds(3)
    character, intent(in) ::  grbfile*(*)
    logical, intent(in) :: cls
    real, dimension(:,:),intent(out) :: fld
    real, dimension(size(fld)) :: fld1
    integer :: jpds(25),jgds(22),igrd(5,3)
    integer :: kpds(25),kgds(22),ierr,lugi,n,ndata,kskp
    logical lbms(size(fld))
!
    call opngrib(lugb,grbfile)
!
    lugi=lugb+1
    jpds(1:25) = -1     
    jgds(1:22) = -1
    jpds(5) = pds(1)
    jpds(6) = pds(2)
    jpds(7) = pds(3)
    n=-1
    call getgb(lugb,lugi,size(fld1),n,jpds,jgds,ndata,kskp,kpds,kgds,lbms,fld1,ierr)
    fld=reshape(source=fld1,shape=shape(fld))

    if(ierr.ne.0) then
      print *,' mod_gribio: error in GETGB for ',ierr,jpds
       stop
    endif
    print *,'mod_gribio: jpds(5:7),fldmin,fldmax=',jpds(5:7),minval(fld),maxval(fld)
!
   if (cls) then
     call clsgrib(lugb)
   endif
  end subroutine rdgrib
!
!===========================================================================================
!
  subroutine opngrib(lugb,grbfile)
    implicit none
    integer, intent(in) :: lugb
    character, intent(in) ::  grbfile*(*)
    integer :: lugi,ierr
    character (len=200) :: idxfile 
    logical, save :: opned
! 
    lugi=lugb+1
    idxfile=trim(grbfile)//'.idx'
    if (opn(lugb)) then 
      inquire(unit=lugb,opened=opned)
      if(opned) then
        print *,' mod_gribio: error: unit lugb=',lugb,' is in use'
        stop
      endif
      call baopenr(lugb,trim(grbfile),ierr)         
      if(ierr.ne.0) then
        print *,'mod_gribio: error opening file ',grbfile
        stop
      else
        write(*,*) 'mod_gribio: opening GRIB file '//trim(grbfile)//" as unit ",lugb
      endif
      inquire(unit=lugi,opened=opned)
      if(opned) then
        print *,' mod_gribio: error: unit lugi=',lugi,' is in use'
        stop
      endif
      call baopenr(lugi,trim(idxfile),ierr)
      if(ierr.ne.0) then
        print *,'error opening file ',idxfile
! NOTE: later instead of stopping set idxunit=0 and continue make index file 
        stop
      else
        write(*,*) 'mod_gribio: opening index file '//trim(idxfile)//" as unit ",lugi
      endif
      opn(lugb)=.false.
    endif
!
  end subroutine opngrib
!
!===========================================================================================
!
  subroutine clsgrib(lugb)
    implicit none
    integer, intent(in) :: lugb
    integer :: lugi,ierr
! 
    lugi=lugb+1
    call baclose(lugb,ierr)
    if(ierr.ne.0) then
      print *,'mod_gribio: error closing unit ',lugb
      stop
    endif
    print *,'unit ', lugb,' closed'
    call baclose(lugi,ierr)
    if(ierr.ne.0) then
      print *,'mod_gribio: error closing unit ',lugi
      stop
    endif
    print *,'unit ', lugi,' closed'
    opn(lugb)=.true.
!
  end subroutine clsgrib
!
!===========================================================================================
!
  subroutine getgds(lugb,grbfile,kgds)
    implicit none
    integer, parameter :: ln=200,mbuf=256*1024
    integer, intent(in) :: lugb
    character, intent(in) ::  grbfile*(*)
    integer, dimension(ln), intent(out) :: kgds
    integer, dimension(ln) :: jpds=-1,jgds=-1,jens=-1,kpds,kens
    integer :: jr=0,nnum,nlen,mnum=0,kr,lskip,lgrib,ierr,lugi
    character*1 cbuf(mbuf)
    call opngrib(lugb,grbfile)
!
    lugi=lugb+1
!
    call getgi(lugi,mnum,mbuf,cbuf,nlen,nnum,ierr)
    if(ierr.ne.0) then
      print *,'mod_gribio: problems in subroutine getgi, ierr=',ierr
      stop
    endif
!
    call getgb1s(cbuf,nlen,nnum,jr,jpds,jgds,jens, kr,kpds,kgds,kens,lskip,lgrib,ierr)
    if(ierr.ne.0) then
      print *,'mod_gribio: problems in subroutine getgb1s, ierr=',ierr
      stop
    endif
!
   call clsgrib(lugb)
!    
  end subroutine getgds
!
!===========================================================================================
!
end module mod_gribio
