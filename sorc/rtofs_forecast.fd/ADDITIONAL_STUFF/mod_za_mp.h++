
#if defined(NOMPIR8) /* LAM does not support mpi_real[48] */
# define MTYPE4 mpi_real
#else /* most MPI's allow mpi_real[48] */
# define MTYPE4 mpi_real4
#endif

c
c-----------------------------------------------------------------------
c
c     machine dependent I/O routines.
c     MPI-2 I/O version, with I/O from first processor in each row.
c     contained in module mod_za.
c
c     author:  Alan J. Wallcraft,  NRL.
c
c-----------------------------------------------------------------------
c
      subroutine zagetc(cline,ios, iunit)
      implicit none
c
      character*80, intent(out)   :: cline
      integer,      intent(out)   :: ios
      integer,      intent(in)    :: iunit
c
c**********
c*
c  1) machine specific routine for reading one text line from a file.
c
c  2) The read is performed on the first processor only.
c*
c**********
c
      integer        iline,ibuf
      common/czgetc/ iline(81,0:1),ibuf
      save  /czgetc/
c
      integer i
c
c --- I/O from first processor only
c
      ibuf = mod(ibuf+1,2)
c
      if     (mnproc.eq.1) then
        read(iunit,'(a)',iostat=ios) cline
        do i= 1,80
          iline(i,ibuf) = ichar(cline(i:i))
        enddo
        iline(81,ibuf) = ios
      endif
c
c     broadcast to all other processors
c
      call xcgetc(iline(:,ibuf))
      do i= 1,80
        cline(i:i) = char(iline(i,ibuf))
      enddo
      ios = iline(81,ibuf)  ! iostat value
      return
      end subroutine zagetc

      subroutine zaiopn(cstat, iaunit)
      implicit none
c
      integer,       intent(in)    :: iaunit
      character*(*), intent(in)    :: cstat
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) machine specific routine for opening a file for array i/o.
c
c     must call zaiost before first call to zaiopn.
c     see also 'zaiope' and 'zaiopf'.
c
c  2) the filename is taken from the environment variable FORxxxA,
c       where xxx = iaunit, with default fort.xxxa.
c
c     array i/o is mpi-2 i/o.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     cstat indicates the file type, it can be 'scratch', 'old', or
c      'new'.
c     all i/o to iaunit must be performed by zaiord and zaiowr.
c     the file should be closed using zaiocl.
c*
c**********
c
c --- spval  = data void marker, 2^100 or about 1.2676506e30
c --- n2drec = size of output 2-d array, multiple of 4096
      real*4     spval
      parameter (spval=2.0**100)
      integer    n2drec
      parameter (n2drec=((itdm*jtdm+4095)/4096)*4096)
c
      include 'mpif.h'
      integer        mpierr,mpireq,mpistat
      common/xcmpii/ mpierr,mpireq(4),
     &               mpistat(mpi_status_size,4*iqr)
      save  /xcmpii/
c
      real*4         w
      common/czioxw/ w(n2drec)
      save  /czioxw/
c
      integer(kind=mpi_offset_kind) disp
      integer                       iamode,iahint
      character                     cfile*256,cenv*7
#if defined(TIMER)
c
      call xctmr0(16)
#endif
c
c     test file state.
c
      if     (iarec(iaunit).ne.-1) then
        write(lp,9000) iaunit
        call xcstop('(zaiopn)')
               stop '(zaiopn)'
      endif
c
      iarec(iaunit) = 0
c
c --- I/O from first processor in each row.
c
      if     (mproc.eq.mp_1st) then
*
*     if     (mnproc.eq.1) then
*     write(lp,*) 'zaiopn - iaunit = ',iaunit
*     call flush(lp)
*     endif
c
c     get filename.
c
      write(cenv,1000) iaunit
      cfile = ' '
      call getenv(cenv,cfile)
      if     (cfile.eq.' ') then
        write(cfile,1100) iaunit
      endif
c
c     open file.
c
      if     (cstat.eq.'OLD' .or.
     &        cstat.eq.'old'     ) then
        iamode = MPI_MODE_RDONLY +
     &           MPI_MODE_UNIQUE_OPEN
        iahint = file_info_zaiord  !see zaiost
      elseif (cstat.eq.'NEW' .or.
     &        cstat.eq.'new'     ) then
        iamode = MPI_MODE_WRONLY +
     &           MPI_MODE_CREATE +
     &           MPI_MODE_EXCL   +
     &           MPI_MODE_UNIQUE_OPEN
        iahint = file_info_zaiowr  !see zaiost
      else !scratch file
        iamode = MPI_MODE_RDWR            +
     &           MPI_MODE_DELETE_ON_CLOSE +
     &           MPI_MODE_UNIQUE_OPEN
        iahint = file_info_zaiowr  !see zaiost
      endif
      call mpi_file_open(group_1st_in_row,
     &                   trim(cfile),
     &                   iamode,
     &                   iahint,
     &                   iahand(iaunit),
     &                   mpierr)
      if     (mpierr.ne.0) then
        write(lp,9100) iaunit
        write(lp,*) 'mpi_file_open - mpierr = ',mpierr
        call flush(lp)
        call xchalt('(zaiopn)')
               stop '(zaiopn)'
      endif
      disp = 0
      call mpi_file_set_view(iahand(iaunit),
     &                       disp,
     &                       MTYPE4,
     &                       MTYPE4,
     &                       "native",  !"external32" may not be available.
     &                       iahint,
     &                       mpierr)
      if     (mpierr.ne.0) then
        write(lp,9100) iaunit
        write(lp,*) 'mpi_file_set_view - mpierr = ',mpierr
        call flush(lp)
        call xchalt('(zaiopn)')
               stop '(zaiopn)'
      endif
c
      endif  ! I/O from first processor in each row
      call xcsync(no_flush)
#if defined(TIMER)
c
      call xctmr1(16)
#endif
      return
c
 1000 format('FOR',i3.3,'A')
 1100 format('fort.',i3.3,'a')
 9000 format(/ /10x,'error in zaiopn -  array I/O unit ',
     &   i3,' is not marked as available.'/ /)
 9100 format(/ /10x,'error in zaiopn -  can''t open unit ',i3,
     &   ', for array I/O.'/ /)
      end subroutine zaiopn

      subroutine zaiope(cenv,cstat, iaunit)
      implicit none
c
      integer,       intent(in)    :: iaunit
      character*(*), intent(in)    :: cenv,cstat
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) machine specific routine for opening a file for array i/o.
c
c     must call zaiost before first call to zaiope.
c     see also 'zaiopn' and 'zaiopf'.
c
c  2) the filename is taken from environment variable 'cenv'.
c
c     array i/o is mpi-2 i/o.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     cstat indicates the file type, it can be 'scratch', 'old', or
c      'new'.
c     all i/o to iaunit must be performed by zaiord and zaiowr.
c      arrays passed to these routines must conform to 'h'.
c     the file should be closed using zaiocl.
c*
c**********
c
c --- spval  = data void marker, 2^100 or about 1.2676506e30
c --- n2drec = size of output 2-d array, multiple of 4096
      real*4     spval
      parameter (spval=2.0**100)
      integer    n2drec
      parameter (n2drec=((itdm*jtdm+4095)/4096)*4096)
c
      include 'mpif.h'
      integer        mpierr,mpireq,mpistat
      common/xcmpii/ mpierr,mpireq(4),
     &               mpistat(mpi_status_size,4*iqr)
      save  /xcmpii/
c
      real*4         w
      common/czioxw/ w(n2drec)
      save  /czioxw/
c
      integer(kind=mpi_offset_kind) disp
      integer                       iamode,iahint
      character                     cfile*256
c
#if defined(TIMER)
c
      call xctmr0(16)
#endif
c
c     test file state.
c
      if     (iarec(iaunit).ne.-1) then
        write(lp,9000) iaunit
        call xcstop('(zaiope)')
               stop '(zaiope)'
      endif
c
      iarec(iaunit) = 0
c
c --- I/O from first processor in each row.
c
      if     (mproc.eq.mp_1st) then
*
*     if     (mnproc.eq.1) then
*     write(lp,*) 'zaiope - iaunit = ',iaunit
*     call flush(lp)
*     endif
c
c     get filename.
c
      cfile = ' '
      call getenv(cenv,cfile)
      if     (cfile.eq.' ') then
        write(lp,9300) cenv(1:len_trim(cenv))
        write(lp,*) 'iaunit = ',iaunit
        call flush(lp)
        call xchalt('(zaiope)')
               stop '(zaiope)'
      endif
c
c     open file.
c
      if     (cstat.eq.'OLD' .or.
     &        cstat.eq.'old'     ) then
        iamode = MPI_MODE_RDONLY +
     &           MPI_MODE_UNIQUE_OPEN
        iahint = file_info_zaiord  !see zaiost
      elseif (cstat.eq.'NEW' .or.
     &        cstat.eq.'new'     ) then
        iamode = MPI_MODE_WRONLY +
     &           MPI_MODE_CREATE +
     &           MPI_MODE_EXCL   +
     &           MPI_MODE_UNIQUE_OPEN
        iahint = file_info_zaiowr  !see zaiost
      else !scratch file
        iamode = MPI_MODE_RDWR            +
     &           MPI_MODE_DELETE_ON_CLOSE +
     &           MPI_MODE_UNIQUE_OPEN
        iahint = file_info_zaiowr  !see zaiost
      endif
      call mpi_file_open(group_1st_in_row,
     &                   trim(cfile),
     &                   iamode,
     &                   iahint,
     &                   iahand(iaunit),
     &                   mpierr)
      if     (mpierr.ne.0) then
        write(lp,9100) iaunit,trim(cfile)
        write(lp,*) 'mpi_file_open - mpierr = ',mpierr
        write(lp,*) 'cenv = ',trim(cenv)
        call flush(lp)
        call xchalt('(zaiope)')
               stop '(zaiope)'
      endif    
      disp = 0 
      call mpi_file_set_view(iahand(iaunit),
     &                       disp,
     &                       MTYPE4,
     &                       MTYPE4,
     &                       "native",  !"external32" may not be available.
     &                       iahint,
     &                       mpierr)
      if     (mpierr.ne.0) then
        write(lp,9100) iaunit,trim(cfile)
        write(lp,*) 'mpi_file_set_view - mpierr = ',mpierr
        write(lp,*) 'cenv = ',trim(cenv)
        call flush(lp)
        call xchalt('(zaiope)')
               stop '(zaiope)'
      endif    
c
      endif  ! I/O from first processor in each row
      call xcsync(no_flush)
#if defined(TIMER)
c
      call xctmr1(16)
#endif
      return
c
 9000 format(/ /10x,'error in zaiope -  array I/O unit ',
     &   i3,' is not marked as available.'/ /)
 9100 format(/ /10x,'error in zaiope -  can''t open unit ',i3,
     &   ', for array I/O.' /
     &   10x,'cfile = ',a/ /)
 9300 format(/ /10x,'error in zaiope -  environment variable ',a,
     &   ' not defined'/ /)
      end subroutine zaiope

      subroutine zaiopf(cfile,cstat, iaunit)
      implicit none
c
      integer,       intent(in)    :: iaunit
      character*(*), intent(in)    :: cfile,cstat
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) machine specific routine for opening a file for array i/o.
c
c     must call zaiost before first call to zaiopf.
c     see also 'zaiopn' and 'zaiope'.
c
c  2) the filename is taken from 'cfile'.
c
c     array i/o is mpi-2 i/o.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     cstat indicates the file type, it can be 'scratch', 'old', or
c      'new'.
c     all i/o to iaunit must be performed by zaiord and zaiowr.
c      arrays passed to these routines must conform to 'h'.
c     the file should be closed using zaiocl.
c*
c**********
c
c --- spval  = data void marker, 2^100 or about 1.2676506e30
c --- n2drec = size of output 2-d array, multiple of 4096
      real*4     spval
      parameter (spval=2.0**100)
      integer    n2drec
      parameter (n2drec=((itdm*jtdm+4095)/4096)*4096)
c
      include 'mpif.h'
      integer        mpierr,mpireq,mpistat
      common/xcmpii/ mpierr,mpireq(4),
     &               mpistat(mpi_status_size,4*iqr)
      save  /xcmpii/
c
      real*4         w
      common/czioxw/ w(n2drec)
      save  /czioxw/
c
      integer(kind=mpi_offset_kind) disp
      integer                       iamode,iahint
#if defined(TIMER)
c
      call xctmr0(16)
#endif
c
c     test file state.
c
      if     (iarec(iaunit).ne.-1) then
        write(lp,9000) iaunit
        call xcstop('(zaiopf)')
               stop '(zaiopf)'
      endif
c
      iarec(iaunit) = 0
c
c --- I/O from first processor in each row.
c
      if     (mproc.eq.mp_1st) then
*
*     if     (mnproc.eq.1) then
*     write(lp,*) 'zaiopf - iaunit = ',iaunit
*     call flush(lp)
*     endif
c
c     open file.
c
      if     (cstat.eq.'OLD' .or.
     &        cstat.eq.'old'     ) then
        iamode = MPI_MODE_RDONLY +
     &           MPI_MODE_UNIQUE_OPEN
        iahint = file_info_zaiord  !see zaiost
      elseif (cstat.eq.'NEW' .or.
     &        cstat.eq.'new'     ) then
        iamode = MPI_MODE_WRONLY +
     &           MPI_MODE_CREATE +
     &           MPI_MODE_EXCL   +
     &           MPI_MODE_UNIQUE_OPEN
        iahint = file_info_zaiowr  !see zaiost
      else !scratch file
        iamode = MPI_MODE_RDWR            +
     &           MPI_MODE_DELETE_ON_CLOSE +
     &           MPI_MODE_UNIQUE_OPEN
        iahint = file_info_zaiowr  !see zaiost
      endif
      call mpi_file_open(group_1st_in_row,
     &                   trim(cfile),
     &                   iamode,
     &                   iahint,
     &                   iahand(iaunit),
     &                   mpierr)
      if     (mpierr.ne.0) then
        write(lp,9100) iaunit,trim(cfile)
        write(lp,*) 'mpi_file_open - mpierr = ',mpierr
        call flush(lp)
        call xchalt('(zaiopf)')
               stop '(zaiopf)'
      endif    
      disp = 0 
      call mpi_file_set_view(iahand(iaunit),
     &                       disp,
     &                       MTYPE4,
     &                       MTYPE4,
     &                       "native",  !"external32" may not be available.
     &                       iahint,
     &                       mpierr)       
      if     (mpierr.ne.0) then            
        write(lp,9100) iaunit,trim(cfile)
        write(lp,*) 'mpi_file_set_view - mpierr = ',mpierr
        call flush(lp)                                    
        call xchalt('(zaiopf)')                           
               stop '(zaiopf)' 
      endif                    
      if     (mnproc.eq.1) then
        call zaio_hints(iahand(iaunit))
      endif
c
      endif  ! I/O from first processor in each row
      call xcsync(no_flush)
#if defined(TIMER)
c
      call xctmr1(16)
#endif
      return
c
 9000 format(/ /10x,'error in zaiopf -  array I/O unit ',
     &   i3,' is not marked as available.'/ /)
 9100 format(/ /10x,'error in zaiopf -  can''t open unit ',i3,
     &   ', for array I/O.' /
     &   10x,'cfile = ',a/ /)
      end subroutine zaiopf

      subroutine zaio_hints(file_handle)
      implicit none
c
      integer, intent(in) :: file_handle
c
c**********
c*
c  1) prints current hints.
c*
c**********
c
      logical       flag
      integer       hints,i,mpierr,nkeys
      character*256 key,value
c
      call MPI_File_get_info(file_handle, hints, mpierr)
      call MPI_Info_get_nkeys(hints, nkeys, mpierr)
c
      write(6,"(/a)") 'zaio_hints:'
      do i= 0,nkeys-1
        call MPI_Info_get_nthkey(hints, i, key, mpierr)
        call MPI_Info_get(hints, key, 256, value, flag, mpierr)
        write(6,"(4a)")
     &    '    key=',trim(key),
     &    '  value=',trim(value)
      enddo
      write(6,*)
      return
      end subroutine zaio_hints

      subroutine zaiopi(lopen, iaunit)
      implicit none
c
      logical, intent(out)   :: lopen
      integer, intent(in)    :: iaunit
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) is an array i/o unit open?
c
c  2) must call zaiost before first call to zaiopi.
c*
c**********
c
      lopen = iarec(iaunit).ne.-1
      return
      end subroutine zaiopi

      subroutine zaiost
      implicit none
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) machine specific routine for initializing array i/o.
c
c  2) see also zaiopn, zaiord, zaiowr, and zaiocl.
c*
c**********
c
      include 'mpif.h'
      integer        mpierr,mpireq,mpistat
      common/xcmpii/ mpierr,mpireq(4),
     &               mpistat(mpi_status_size,4*iqr)
      save  /xcmpii/
c
      integer       i
      character*256 value
c
      do i= 1,999
        iarec(i) = -1
      enddo
c
c     mpi-2 i/o group.
c
      if     (mproc.eq.mp_1st) then
        i = 1
        call mpi_comm_split(mpi_comm_world, i,0,
     &                      group_1st_in_row, mpierr)
      else
        i = 0
        call mpi_comm_split(mpi_comm_world, i,0,
     &                      group_1st_in_row, mpierr)
        call mpi_comm_free( group_1st_in_row, mpierr)
      endif
c
c     mpi-2 hints for array i/o
c
      call mpi_info_create(file_info_zaiord,
     &                     mpierr)
      call mpi_info_create(file_info_zaiowr,
     &                     mpierr)
#if defined(AIX)
      call mpi_info_set(file_info_zaiord,
     &                  'IBM_largeblock_io',  !read on calling task
     &                  'true',
     &                  mpierr)
      write(value,'(i10)') (((itdm*jtdm+4095)/4096)*4096)*4
      call mpi_info_set(file_info_zaiowr,
     &                  'IBM_io_buffer_size',
     &                  trim(value),          !write from a single task
     &                  mpierr)
#else
      call mpi_info_free(file_info_zaiord,  !set to mpi_info_null
     &                   mpierr)
      call mpi_info_free(file_info_zaiowr,  !set to mpi_info_null
     &                   mpierr)
#endif
c
      if     (mnproc.eq.1) then
        write(lp,'(/a/)') 
     &    'zaiost - Array I/O is MPI-2 I/O from one task per row'
      endif
c
#if defined(TIMER)
c
c     initialize timers.
c
      call xctmrn(16,'zaio**')
      call xctmrn(17,'zaiord')
      call xctmrn(18,'zaiowr')
#endif
      return
      end subroutine zaiost

      subroutine zaiocl(iaunit)
      implicit none
c
      integer, intent(in)    :: iaunit
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) machine specific routine for array i/o file closing.
c
c     must call zaiopn for this array unit before calling zaiocl.
c
c  2) array i/o is mpi-2 i/o.
c*
c**********
c
      include 'mpif.h'
      integer        mpierr,mpireq,mpistat
      common/xcmpii/ mpierr,mpireq(4),
     &               mpistat(mpi_status_size,4*iqr)
      save  /xcmpii/
#if defined(TIMER)
c
      call xctmr0(16)
#endif
c
      if     (iarec(iaunit).lt.0) then
        write(lp,9000) iaunit
        call xcstop('(zaiocl)')
               stop '(zaiocl)'
      endif
c
      iarec(iaunit) = -1
c
c --- I/O from first processor in each row
c
      if     (mproc.eq.mp_1st) then
*
*     if     (mnproc.eq.1) then
*     write(lp,*) 'zaiocl - iaunit = ',iaunit
*     call flush(lp)
*     endif
c
      call mpi_file_close(iahand(iaunit),mpierr)
c
      endif  ! I/O from first processor in each row
      call xcsync(no_flush)
#if defined(TIMER)
c
      call xctmr1(16)
#endif
      return
c
 9000 format(/ /10x,'error in zaiocl -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
      end subroutine zaiocl

      subroutine zaiofl(iaunit)
      implicit none
c
      integer, intent(in)    :: iaunit
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) machine specific routine for array i/o buffer flushing.
c
c     must call zaiopn for this array unit before calling zaiocl.
c
c  2) array i/o is mpi-2 i/o.
c*
c**********
c
      include 'mpif.h'
      integer        mpierr,mpireq,mpistat
      common/xcmpii/ mpierr,mpireq(4),
     &               mpistat(mpi_status_size,4*iqr)
      save  /xcmpii/
#if defined(TIMER)
c
      call xctmr0(16)
#endif
c
      if     (iarec(iaunit).lt.0) then
        write(lp,9000) iaunit
        call xcstop('(zaiofl)')
               stop '(zaiofl)'
      endif
c
c --- I/O from first processor in each row
c
      if     (mproc.eq.mp_1st) then
        call mpi_file_sync(iahand(iaunit),mpierr)
      endif  ! I/O from first processor in each row
      call xcsync(no_flush)
#if defined(TIMER)
c
      call xctmr1(16)
#endif
      return
c
 9000 format(/ /10x,'error in zaiofl -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
      end subroutine zaiofl

      subroutine zaioiq(iaunit, irec)
      implicit none
c
      integer, intent(in)    :: iaunit
      integer, intent(out)   :: irec
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) machine specific routine for array i/o inquiry.
c
c  2) returns the number of records processed, or -1 for a closed file.
c*
c**********
c
      irec = iarec(iaunit)
      return
      end subroutine zaioiq

      subroutine zaiorw(iaunit)
      implicit none
c
      integer, intent(in)    :: iaunit
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) machine specific routine for array i/o file rewinding.
c
c     must call zaiopn for this array unit before calling zaiocl.
c
c  2) array i/o is mpi-2 i/o.
c*
c**********
#if defined(TIMER)
c
      call xctmr0(16)
#endif
c
      if     (iarec(iaunit).lt.0) then
        write(lp,9000) iaunit
        call xcstop('(zaiorw)')
               stop '(zaiorw)'
      endif
c
      iarec(iaunit) = 0
*
*     if     (mnproc.eq.1) then
*     write(lp,*) 'zaiorw - iaunit,rec = ',iaunit,iarec(iaunit)
*     call flush(lp)
*     endif
      call xcsync(no_flush)
#if defined(TIMER)
c
      call xctmr1(16)
#endif
      return
c
 9000 format(/ /10x,'error in zaiorw -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
      end subroutine zaiorw

      subroutine zaiord3(h, l, mask,lmask, hmin,hmax,  iaunit)
      implicit none
c
      logical, intent(in)    :: lmask
      integer, intent(in)    :: l,iaunit
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in)    :: mask
#if defined(REAL4)
      real*4,  intent(out)   :: hmin(l),hmax(l)
      real*4,  dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,l),
     &         intent(out)   :: h
#else
      real,    intent(out)   :: hmin(l),hmax(l)
      real,    dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,l),
     &         intent(out)   :: h
#endif
c
c**********
c*
c  1) machine specific routine for 3-d array reading.
c
c     must call zaiopn for this array unit before calling zaiord.
c
c  2) array i/o is mpi-2 i/o.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     the array, 'h',  must conform to that passed in the associated
c      call to zaiopn.
c
c  4) hmin,hmax are returned as the minimum and maximum value in the 
c     array, ignoring array elements set to 2.0**100.  
c     if lmask==.true. the range is calculated only where mask.ne.0,
c     with all other values unchanged in h on exit.  It is then an
c     error if mask.ne.0 anywhere the input is 2.0**100.
c*
c**********
c
c     this version just calls zaiord l times.
c
      integer k
c
      do k= 1,l
        call zaiord(h(1-nbdy,1-nbdy,k), mask,lmask,
     &              hmin(k),hmax(k), iaunit)
      enddo
c
      return
      end subroutine zaiord3

      subroutine zaiord(h, mask,lmask, hmin,hmax,  iaunit)
      implicit none
c
      logical, intent(in)    :: lmask
      integer, intent(in)    :: iaunit
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in)    :: mask
#if defined(REAL4)
      real*4,  intent(out)   :: hmin,hmax
      real*4,  dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(out)   :: h
#else
      real,    intent(out)   :: hmin,hmax
      real,    dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(out)   :: h
#endif
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) machine specific routine for array reading.
c
c     must call zaiopn for this array unit before calling zaiord.
c
c  2) array i/o is mpi-2 i/o.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     the array, 'h',  must conform to that passed in the associated
c      call to zaiopn.
c
c  4) hmin,hmax are returned as the minimum and maximum value in the 
c     array, ignoring array elements set to 2.0**100.  
c     if lmask==.true. the range is calculated only where mask.ne.0,
c     with all other values unchanged in h on exit.  It is then an
c     error if mask.ne.0 anywhere the input is 2.0**100.
c*
c**********
c
c --- spval  = data void marker, 2^100 or about 1.2676506e30
c --- n2drec = size of output 2-d array, multiple of 4096
      real*4     spval
      parameter (spval=2.0**100)
      integer    n2drec
      parameter (n2drec=((itdm*jtdm+4095)/4096)*4096)
c
      include 'mpif.h'
      integer        mpierr,mpireq,mpistat
      common/xcmpii/ mpierr,mpireq(4),
     &               mpistat(mpi_status_size,4*iqr)
      save  /xcmpii/
c
      real*4         w
      common/czioxw/ w(n2drec)
      save  /czioxw/
c
      real*4         atmp,amsk,tmsk,htmp
      common/czioxr/ atmp(itdm,jtdm),amsk(itdm,jtdm),
     &               tmsk(idm*jdm),htmp(idm*jdm)
      save  /czioxr/
c
      integer(kind=mpi_offset_kind) disp
      integer                        i,j
      real*4                         wmin,wmax
      real                           rmin(1),rmax(1),vsave
#if defined(TIMER)
c
      call xctmr0(17)
#endif
c
      if     (iarec(iaunit).lt.0) then
        write(lp,9000) iaunit
        call xcstop('(zaiord)')
               stop '(zaiord)'
      endif
c
      iarec(iaunit) = iarec(iaunit) + 1
      rmin(1) =  spval 
      rmax(1) = -spval 
      if     (lmask) then
        do j= 1,jj
          do i= 1,ii
            tmsk(i+(j-1)*ii) = mask(i,j)
            htmp(i+(j-1)*ii) =    h(i,j)
          enddo !i
        enddo !j
        vsave = vland
        vland = 0.0
        call xcaget4(amsk,tmsk, -1)
        vland = spval
        call xcaget4(atmp,htmp, -1)  ! to preserve masked values
        vland = vsave
      endif
c
c --- I/O from first processor in each row
c
      if     (mproc.eq.mp_1st) then
*
*     if     (mnproc.eq.1) then
*     write(lp,*) 'zaiord - iaunit,rec = ',iaunit,iarec(iaunit)
*     write(lp,*) 'zaiord - mask.1,1    = ',amsk(1,1)
*     write(lp,*) 'zaiord - h.1,1       = ',atmp(1,1)
*     call flush(lp)
*     endif
c
      disp = n2drec
      disp = (iarec(iaunit)-1)*disp + itdm*j0
      call mpi_file_read_at(iahand(iaunit),
     &                      disp,
     &                      w(1+j0*itdm),
     &                      itdm*jj,
     &                      MTYPE4,
     &                      mpistat(1,1),
     &                      mpierr)
      if     (mpierr.ne.0) then
        write(lp,9100) iarec(iaunit),iaunit
        write(lp,*) 'mpierr = ',mpierr
        call flush(lp)
        call xchalt('(zaiord)')
               stop '(zaiord)'
      endif
#if defined(ENDIAN_IO)
      call zaio_endian(w(1+j0*itdm),itdm*jj) !swap to big-endian
                                             !"external32" often not available
#endif
      wmin =  spval 
      wmax = -spval 
      if     (lmask) then
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&              REDUCTION(MIN:wmin) REDUCTION(MAX:wmax)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= j0,j0+jj
          do i= 1,itdm
            if     (amsk(i,j).ne.0.0) then
              atmp(i,j) = w(i+(j-1)*itdm)
              wmin = min( wmin, w(i+(j-1)*itdm) )
              wmax = max( wmax, w(i+(j-1)*itdm) )
            endif
          enddo !i
        enddo !j
        if     (wmax.eq.spval) then
          write(lp,9200) iarec(iaunit),iaunit
          call flush(lp)
          call xchalt('(zaiord)')
                 stop '(zaiord)'
        endif
      else
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&              REDUCTION(MIN:wmin) REDUCTION(MAX:wmax)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= j0+1,j0+jj
          do i= 1,itdm
            atmp(i,j) = w(i+(j-1)*itdm)
            if     (w(i+(j-1)*itdm).ne.spval) then
              wmin = min( wmin, w(i+(j-1)*itdm) )
              wmax = max( wmax, w(i+(j-1)*itdm) )
            endif
          enddo !i
        enddo !j
      endif !lmask:else
      rmin(1) = wmin
      rmax(1) = wmax
c
      endif  ! I/O from first processor in each row
c
      call xcsync(no_flush)
      call xcaput4(atmp,htmp, -1)
      do j= 1,jj
        do i= 1,ii
          h(i,j) = htmp(i+(j-1)*ii)
        enddo !i
      enddo !j
      timer_on = .false. !don't time min max
        call xcminr(rmin)
        call xcmaxr(rmax)
      timer_on = .true.
      hmin = rmin(1)
      hmax = rmax(1)
#if defined(TIMER)
c
      call xctmr1(17)
#endif
      return
c
 9000 format(/ /10x,'error in zaiord -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
 9100 format(/ /10x,'error in zaiord -  can''t read record',
     &   i4,' on array I/O unit ',i3,'.'/ /)
 9200 format(/ /10x,'error in zaiord -  record',
     &   i4,' on array I/O unit ',i3,
     &   ' has 2.0**100 outside masked region.'/ /)
      end subroutine zaiord

      subroutine zaiosk(iaunit)
      implicit none
c
      integer, intent(in)    :: iaunit
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) machine specific routine for skipping an array read.
c
c     must call zaiopn for this array unit before calling zaiosk.
c
c  2) array i/o is mpi-2 i/o.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     the array, 'h',  must conform to that passed in the associated
c      call to zaiopn.
c*
c**********
#if defined(TIMER)
c
      call xctmr0(16)
#endif
c
      if     (iarec(iaunit).lt.0) then
        write(lp,9000) iaunit
        call xcstop('(zaiosk)')
               stop '(zaiosk)'
      endif
c
      iarec(iaunit) = iarec(iaunit) + 1
c
*     if     (mnproc.eq.1) then
*     write(lp,*) 'zaiosk - iaunit,rec = ',iaunit,iarec(iaunit)
*     call flush(lp)
*     endif
      call xcsync(no_flush)
#if defined(TIMER)
c
      call xctmr1(16)
#endif
      return
c
 9000 format(/ /10x,'error in zaiosk -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
      end subroutine zaiosk

      subroutine zaiowr3(h, l, mask,lmask, hmin,hmax, iaunit, lreal4)
      implicit none
c
      logical, intent(in)    :: lmask,lreal4
      integer, intent(in)    :: l,iaunit
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in)    :: mask
#if defined(REAL4)
      real*4,  intent(out)   :: hmin(l),hmax(l)
      real*4,  dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,l),
     &         intent(inout) :: h
#else
      real,    intent(out)   :: hmin(l),hmax(l)
      real,    dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,l),
     &         intent(inout) :: h
#endif
c
c**********
c*
c  1) machine specific routine for 3-d array writing.
c
c     must call zaiopn for this array unit before calling zaiord.
c
c  2) array i/o is mpi-2 i/o.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     the array, 'h',  must conform to that passed in the associated
c      call to zaiopn.
c
c  4) hmin,hmax are returned as the minimum and maximum value in the array.
c     if lmask==.true. the range is only where mask.ne.0, with all other
c     values output as 2.0**100.
c
c  5) If lreal4==.true. then h is overwritten on exit with real*4 version
c     of the same array.  This is typically used for reproducability on
c     restart.
c*
c**********
c
c     this version just calls zaiowr l times.
c
      integer k
c
      do k= 1,l
        call zaiowr(h(1-nbdy,1-nbdy,k), mask,lmask,
     &              hmin(k),hmax(k), iaunit, lreal4)
      enddo
      return
      end subroutine zaiowr3

      subroutine zaiowr(h, mask,lmask, hmin,hmax,  iaunit, lreal4)
      implicit none
c
      logical, intent(in)    :: lmask,lreal4
      integer, intent(in)    :: iaunit
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in)    :: mask
#if defined(REAL4)
      real*4,  intent(out)   :: hmin,hmax
      real*4,  dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(inout) :: h
#else
      real,    intent(out)   :: hmin,hmax
      real,    dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(inout) :: h
#endif
c
      integer        iarec,iahand
      common/czioxx/ iarec(999),iahand(999)
      save  /czioxx/
c
c**********
c*
c  1) machine specific routine for array writing.
c
c     must call zaiopn for this array unit before calling zaiord.
c
c  2) array i/o is mpi-2 i/o.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     the array, 'h',  must conform to that passed in the associated
c      call to zaiopn.
c
c  4) hmin,hmax are returned as the minimum and maximum value in the array.
c     if lmask==.true. the range is only where mask.ne.0, with all other
c     values output as 2.0**100.
c
c  5) If lreal4==.true. then h is overwritten on exit with real*4 version
c     of the same array.  This is typically used for reproducability on
c     restart.
c*
c**********
c
c --- spval  = data void marker, 2^100 or about 1.2676506e30
c --- n2drec = size of output 2-d array, multiple of 4096
      real*4     spval
      parameter (spval=2.0**100)
      integer    n2drec
      parameter (n2drec=((itdm*jtdm+4095)/4096)*4096)
c
      include 'mpif.h'
      integer        mpierr,mpireq,mpistat
      common/xcmpii/ mpierr,mpireq(4),
     &               mpistat(mpi_status_size,4*iqr)
      save  /xcmpii/
c
      real*4         w
      common/czioxw/ w(n2drec)
      save  /czioxw/
c
      real*4         atmp,amsk,tmsk,htmp
      common/czioxr/ atmp(itdm,jtdm),amsk(itdm,jtdm),
     &               tmsk(idm*jdm),htmp(idm*jdm)
      save  /czioxr/
c
      integer(kind=mpi_offset_kind) disp
      integer                       i,j,lrec
      real*4                        wmin,wmax
      real                          rmin(1),rmax(1),vsave
#if defined(TIMER)
c
      call xctmr0(18)
#endif
c
      if     (iarec(iaunit).lt.0) then
        write(lp,9000) iaunit
        call xcstop('(zaiowr)')
               stop '(zaiowr)'
      endif
c
      iarec(iaunit) = iarec(iaunit) + 1
      rmin(1) =  spval
      rmax(1) = -spval
c
      if     (lmask) then
        do j= 1,jj
          do i= 1,ii
            tmsk(i+(j-1)*ii) = mask(i,j)
            htmp(i+(j-1)*ii) =    h(i,j)
          enddo !i
        enddo !j
        vsave = vland
        vland = 0.0
        call xcaget4(amsk,tmsk, -1)
        vland = spval
        call xcaget4(atmp,htmp, -1)
        vland = vsave
      else
        do j= 1,jj
          do i= 1,ii
            htmp(i+(j-1)*ii) =    h(i,j)
          enddo !i
        enddo !j
        vsave = vland
        vland = spval
        call xcaget4(atmp,htmp, -1)
        vland = vsave
      endif
c
c --- I/O from first processor in each row.
c
      if     (mproc.eq.mp_1st) then
c
      wmin =  spval
      wmax = -spval
      if     (lmask) then
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&              REDUCTION(MIN:wmin) REDUCTION(MAX:wmax)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= j0+1,j0+jj
          do i= 1,itdm
            if     (amsk(i,j).ne.0.0) then
              w(i+(j-1)*itdm) = atmp(i,j)
              wmin = min( wmin, w(i+(j-1)*itdm) )
              wmax = max( wmax, w(i+(j-1)*itdm) )
            else
              w(i+(j-1)*itdm) = spval
            endif
          enddo !i
        enddo !j
      else
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&              REDUCTION(MIN:wmin) REDUCTION(MAX:wmax)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= j0+1,j0+jj
          do i= 1,itdm
            w(i+(j-1)*itdm) = atmp(i,j)
            if     (w(i+(j-1)*itdm).ne.spval) then
              wmin = min( wmin, w(i+(j-1)*itdm) )
              wmax = max( wmax, w(i+(j-1)*itdm) )
            endif
          enddo !i
        enddo !j
      endif !lmask:else
      if     (nproc.eq.jpr) then
        do i= itdm*jtdm+1,n2drec
          w(i) = spval
        enddo
        lrec = n2drec - itdm*j0
      else
        lrec = itdm*jj
      endif
      rmin(1) = wmin
      rmax(1) = wmax
c
      disp = n2drec
      disp = (iarec(iaunit)-1)*disp + itdm*j0
#if defined(ENDIAN_IO)
      call zaio_endian(w(1+itdm*j0),lrec) !swap to big-endian
                                          !"external32" often not available
#endif
*     call mpi_file_write_at(iahand(iaunit),
      call mpi_file_write_at_all(iahand(iaunit),
     &                       disp,
     &                       w(1+itdm*j0),
     &                       lrec,
     &                       MTYPE4,
     &                       mpistat(1,1),
     &                       mpierr)
      if     (mpierr.ne.0) then
        write(lp,9100) iarec(iaunit),iaunit
        write(lp,*) 'mpierr = ',mpierr
        call flush(lp)
        call xchalt('(zaiowr)')
               stop '(zaiowr)'
      endif
c
      endif  ! I/O from first processor in each row
      call xcsync(no_flush)
      if     (lreal4) then
#if defined(REAL4)
! ---   h is already real*4
#else
        call xcaput4(atmp,htmp, -1)  ! h is not real*4, so update it
        do j= 1,jj
          do i= 1,ii
            h(i,j) = htmp(i+(j-1)*ii)
          enddo !i
        enddo !j
#endif
      endif
      timer_on = .false. !don't time min max
        call xcminr(rmin)
        call xcmaxr(rmax)
      timer_on = .true.
      hmin = rmin(1)
      hmax = rmax(1)
#if defined(TIMER)
c
      call xctmr1(18)
#endif
      return
c
 9000 format(/ /10x,'error in zaiowr -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
 9100 format(/ /10x,'error in zaiowr -  can''t write record',
     &   i4,' on array I/O unit ',i3,'.'/ /)
      end subroutine zaiowr
