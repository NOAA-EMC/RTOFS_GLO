      module mod_restart
      implicit none

      real, save, allocatable, dimension (:,:)   :: 
     & pbot,   ! bottom pressure at t=0
     & psikk,  ! montg.pot.     in bottom layer
     & thkk    ! virt.pot.dens. in bottom layer

      contains

      subroutine restart_in(flnmrsi, n)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character*120 flnmrsi
      integer       n
c
c     read in one timestep from a restart file on unit 11.
c     no ice and no tracers
c
      character cline*80
c
      allocate(  pbot(ii,jj) )
      allocate( psikk(ii,jj) )
      allocate(  thkk(ii,jj) )
c
      open (unit=11,file=flnmrsi(1:len_trim(flnmrsi)-2)//'.b',
     &      status='old',action='read',form='formatted')
      call zaiopf(flnmrsi(1:len_trim(flnmrsi)-2)//'.a','old', 11)
      read( 11,'(a)') cline
      if     (mnproc.eq.1) then
      write(lp,'(a)') cline(1:len_trim(cline))
      endif !1st tile
      if     (cline(1:9).ne.'RESTART:') then
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)') 'error - input file is not a HYCOM restart'
        endif !1st tile
        call xcstop('(restart_in)')
               stop '(restart_in)'
      endif
      read( 11,'(a)') cline
      if     (mnproc.eq.1) then
      write(lp,'(a)') cline(1:len_trim(cline))
      call flush(lp)
      endif !1st tile
c
      if     (n.eq.1) then !1st time step
        call restart_in3d(u,     kk,2,2, ip, 'u       ')
        call restart_sk3d(       kk,         'u       ')
        call restart_in3d(v,     kk,2,2, ip, 'v       ')
        call restart_sk3d(       kk,         'v       ')
        call restart_in3d(dp,    kk,1,1, ip, 'dp      ')
        call restart_sk3d(       kk,         'dp      ')
        call restart_in3d(temp,  kk,2,2, ip, 'temp    ')
        call restart_sk3d(       kk,         'temp    ')
        call restart_in3d(saln,  kk,2,2, ip, 'saln    ')
        call restart_sk3d(       kk,         'saln    ')
        call restart_in3d(th3d,  kk,2,2, ip, 'th3d    ')
        call restart_sk3d(       kk,         'th3d    ')
c
        call restart_in3d(ubaro, 1, 1,1, ip, 'ubavg   ')
        call restart_sk3d(       1,          'ubavg   ')
        call restart_sk3d(       1,          'ubavg   ')
        call restart_in3d(vbaro, 1, 1,1, ip, 'vbavg   ')
        call restart_sk3d(       1,          'vbavg   ')
        call restart_sk3d(       1,          'vbavg   ')
        call restart_in3d(pbaro, 1, 1,1, ip, 'pbavg   ')
        call restart_sk3d(       1,          'pbavg   ')
        call restart_sk3d(       1,          'pbavg   ')
c
        call restart_in3d(pbot,  1, 1,1, ip, 'pbot    ')
        call restart_in3d(psikk, 1, 1,1, ip, 'psikk   ')
        call restart_in3d(thkk,  1, 1,1, ip, 'thkk    ')
c
        call restart_in3d(dpmixl,1, 1,1, ip, 'dpmixl  ')
        call restart_sk3d(       1,          'dpmixl  ')
      else !2nd time step
        call restart_sk3d(       kk,         'u       ')
        call restart_in3d(u,     kk,2,2, ip, 'u       ')
        call restart_sk3d(       kk,         'v       ')
        call restart_in3d(v,     kk,2,2, ip, 'v       ')
        call restart_sk3d(       kk,         'dp      ')
        call restart_in3d(dp,    kk,1,1, ip, 'dp      ')
        call restart_sk3d(       kk,         'temp    ')
        call restart_in3d(temp,  kk,2,2, ip, 'temp    ')
        call restart_sk3d(       kk,         'saln    ')
        call restart_in3d(saln,  kk,2,2, ip, 'saln    ')
        call restart_sk3d(       kk,         'th3d    ')
        call restart_in3d(th3d,  kk,2,2, ip, 'th3d    ')
c
        call restart_sk3d(       1,          'ubavg   ')
        call restart_in3d(ubaro, 1, 1,1, ip, 'ubavg   ')
        call restart_sk3d(       1,          'ubavg   ')
        call restart_sk3d(       1,          'vbavg   ')
        call restart_in3d(vbaro, 1, 1,1, ip, 'vbavg   ')
        call restart_sk3d(       1,          'vbavg   ')
        call restart_sk3d(       1,          'pbavg   ')
        call restart_in3d(pbaro, 1, 1,1, ip, 'pbavg   ')
        call restart_sk3d(       1,          'pbavg   ')
c
        call restart_in3d(pbot,  1, 1,1, ip, 'pbot    ')
        call restart_in3d(psikk, 1, 1,1, ip, 'psikk   ')
        call restart_in3d(thkk,  1, 1,1, ip, 'thkk    ')
c
        call restart_sk3d(       1,          'dpmixl  ')
        call restart_in3d(dpmixl,1, 1,1, ip, 'dpmixl  ')
      endif
c
      close (unit=11)
      call zaiocl(11)
c
      call getdepth(dpbl) !dpbl is workspace
      return
      end subroutine restart_in

      subroutine restart_in_pbot(flnmrsi)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character*120 flnmrsi
c
c     read in a small part of a a restart file on unit 11.
c
      character cline*80
c
      allocate(  pbot(ii,jj) )
      allocate( psikk(ii,jj) )
      allocate(  thkk(ii,jj) )
c
      open (unit=11,file=flnmrsi(1:len_trim(flnmrsi)-2)//'.b',
     &      status='old',action='read',form='formatted')
      call zaiopf(flnmrsi(1:len_trim(flnmrsi)-2)//'.a','old', 11)
      read( 11,'(a)') cline
      if     (mnproc.eq.1) then
      write(lp,'(a)') cline(1:len_trim(cline))
      endif !1st tile
      if     (cline(1:9).ne.'RESTART:') then
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)') 'error - input file is not a HYCOM restart'
        endif !1st tile
        call xcstop('(restart_in)')
               stop '(restart_in)'
      endif
      read( 11,'(a)') cline
      if     (mnproc.eq.1) then
      write(lp,'(a)') cline(1:len_trim(cline))
      call flush(lp)
      endif !1st tile
c
      call restart_sk3d(2*kk, 'u       ')
      call restart_sk3d(2*kk, 'v       ')
      call restart_sk3d(2*kk, 'dp      ')
      call restart_sk3d(2*kk, 'temp    ')
      call restart_sk3d(2*kk, 'saln    ')
      call restart_sk3d(2*kk, 'th3d    ')
c
      call restart_sk3d(3,    'ubavg   ')
      call restart_sk3d(3,    'vbavg   ')
      call restart_sk3d(3,    'pbavg   ')
c
      call restart_in3d(pbot,  1,1,1, ip, 'pbot    ')
      call restart_in3d(psikk, 1,1,1, ip, 'psikk   ')
      call restart_in3d(thkk,  1,1,1, ip, 'thkk    ')
c
      close (unit=11)
      call zaiocl(11)
      return
      end subroutine restart_in_pbot

      subroutine restart_in3d(field3d,l, k1,ki, mask, cfield)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      integer   l,k1,ki
      real,    dimension (ii,jj,*) ::
     & field3d
      integer, dimension (ii,jj) ::
     & mask
      character cfield*8
c
c --- read a single restart 3-d array field from unit 11.
c --- file input is 1:l, field3d output is k1:k1+ki*(l-1):ki.
c
      integer   i,layer,level,k,kout
      real      hmina,hminb,hmaxa,hmaxb
      character cline*80
c
      if     (mnproc.eq.1) then
      write(lp,'(a,i3,2x,a)') 'restart_in3d - l,cfield = ',l,cfield
      call flush(lp)
      endif !1st tile
c
      kout = k1
      do k= 1,l
        read ( 11,'(a)')  cline
        if     (mnproc.eq.1) then
        write (lp,'(a)')  cline(1:len_trim(cline))
        endif !1st tile
        if     (cline(1:8).ne.cfield) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,a /)') cline(1:len_trim(cline)),
     &           'error in restart_in3d - expected ',cfield
          endif !1st tile
          call xcstop('(restart_in3d)')
                 stop '(restart_in3d)'
        endif
        i = index(cline,'=')
        read (cline(i+1:),*) layer,level,hminb,hmaxb
        call zaiord(field3d(1,1,kout),
     &              mask,.false., hmina,hmaxa, 11)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,3i3 / a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      'iunit,k,l = ',11,k,l,
     &      cline,
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          endif !1st tile
          call xcstop('(restart_in3d)')
                 stop '(restart_in3d)'
        endif
*       write(6,*) 'kout, field3d = ',kout,field3d(20,20,kout)
        kout = kout + ki
      enddo !k
c
      return
      end subroutine restart_in3d

      subroutine restart_sk3d(l, cfield)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      integer   l
      character cfield*8
c
c --- skip a single restart 3-d array field from unit 11.
c
      integer   k
      character cline*80
c
*     if     (mnproc.eq.1) then
*     write(lp,'(a,i3,2x,a)') 'restart_sk3d - l,cfield = ',l,cfield
*     call flush(lp)
*     endif !1st tile
c
      do k= 1,l
        call zaiosk(11)
        read ( 11,'(a)')  cline
*       if     (mnproc.eq.1) then
*       write (lp,'(a)')  cline(1:len_trim(cline))
*       endif !1st tile
        if     (cline(1:8).ne.cfield) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,a /)') cline(1:len_trim(cline)),
     &           'error in restart_sk3d - expected ',cfield
          endif !1st tile
          call xcstop('(restart_in3d)')
                 stop '(restart_in3d)'
        endif
      enddo
c
      return
      end subroutine restart_sk3d

      subroutine restart_out(flnmrso, nstepx, dtimex,
     &                       iexpt,iversn,yrflag, icegln,trcout)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character*120 flnmrso
      logical       icegln,trcout
      integer       nstepx,iexpt,iversn,yrflag
      real*8        dtimex
c
c     write out in a restart file on unit 12
c
      logical   lopen
      integer   i,j,k,l
      real      xmin(2*kk),xmax(2*kk)
      character cline*80
c
      call zaiopf(flnmrso(1:len_trim(flnmrso)-2)//'.a','new', 12)
      if     (mnproc.eq.1) then
        open (unit=12,file=flnmrso(1:len_trim(flnmrso)-2)//'.b',
     &        status='new',action='write',form='formatted')
        write(lp,'(a)') ' creating a new restart file'
      endif !1st tile
c
      if     (mnproc.eq.1) then
      write(12,'(a,3i6)') 'RESTART: iexpt,iversn,yrflag = ',
     &                              iexpt,iversn,yrflag
      write(cline,*)                nstepx,dtimex
      write(12,'(a,a)')   'RESTART: nstep,dtime = ',
     &                              cline(1:len_trim(cline))
      endif !1st tile
c
      call zaiowr3(u,      2*kk, iu,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 0,1
        do k= 1,kk
          write(12,4100) 'u       ',k,l+1,xmin(k+l*kk),xmax(k+l*kk)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(v,      2*kk, iv,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 0,1
        do k= 1,kk
          write(12,4100) 'v       ',k,l+1,xmin(k+l*kk),xmax(k+l*kk)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(dp,       kk, ip,.false., xmin,xmax, 12, .true.)
      call zaiowr3(dp,       kk, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 0,1
        do k= 1,kk
          write(12,4100) 'dp      ',k,l+1,xmin(k     ),xmax(k     )
        enddo
      enddo
      endif !1st tile
      call zaiowr3(temp,   2*kk, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 0,1
        do k= 1,kk
          write(12,4100) 'temp    ',k,l+1,xmin(k+l*kk),xmax(k+l*kk)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(saln,   2*kk, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 0,1
        do k= 1,kk
          write(12,4100) 'saln    ',k,l+1,xmin(k+l*kk),xmax(k+l*kk)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(th3d,   2*kk, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 0,1
        do k= 1,kk
          write(12,4100) 'th3d    ',k,l+1,xmin(k+l*kk),xmax(k+l*kk)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(ubaro,      1, iu,.false., xmin,xmax, 12, .true.)
      call zaiowr3(ubaro,      1, iu,.false., xmin,xmax, 12, .true.)
      call zaiowr3(ubaro,      1, iu,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,3
        do k= 0,0
          write(12,4100) 'ubavg   ',k,l,  xmin(1),xmax(1)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(vbaro,      1, iv,.false., xmin,xmax, 12, .true.)
      call zaiowr3(vbaro,      1, iv,.false., xmin,xmax, 12, .true.)
      call zaiowr3(vbaro,      1, iv,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,3
        do k= 0,0
          write(12,4100) 'vbavg   ',k,l,  xmin(1),xmax(1)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(pbaro,      1, ip,.false., xmin,xmax, 12, .true.)
      call zaiowr3(pbaro,      1, ip,.false., xmin,xmax, 12, .true.)
      call zaiowr3(pbaro,      1, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,3
        do k= 0,0
          write(12,4100) 'pbavg   ',k,l,  xmin(1),xmax(1)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(pbot,       1, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,1
        do k= 0,0
          write(12,4100) 'pbot    ',k,l,  xmin(l),xmax(l)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(psikk,      1, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,1
        do k= 0,0
          write(12,4100) 'psikk   ',k,l,  xmin(l),xmax(l)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(thkk,       1, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,1
        do k= 0,0
          write(12,4100) 'thkk    ',k,l,  xmin(l),xmax(l)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(dpmixl,      1, ip,.false., xmin,xmax, 12, .true.)
      call zaiowr3(dpmixl,      1, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,2
        do k= 0,0
          write(12,4100) 'dpmixl  ',k,l,  xmin(1),xmax(1)
        enddo
      enddo
      endif !1st tile
      if (icegln) then
        call zaiowr3(temice,     1, ip,.false., xmin,xmax, 12, .true.)
        if     (mnproc.eq.1) then
        do l= 1,1
          do k= 0,0
            write(12,4100) 'temice  ',k,l,  xmin(l),xmax(l)
          enddo
        enddo
        endif !1st tile
        call zaiowr3(covice,     1, ip,.false., xmin,xmax, 12, .true.)
        if     (mnproc.eq.1) then
        do l= 1,1
          do k= 0,0
            write(12,4100) 'covice  ',k,l,  xmin(l),xmax(l)
          enddo
        enddo
        endif !1st tile
        call zaiowr3(thkice,     1, ip,.false., xmin,xmax, 12, .true.)
        if     (mnproc.eq.1) then
        do l= 1,1
          do k= 0,0
            write(12,4100) 'thkice  ',k,l,  xmin(l),xmax(l)
          enddo
        enddo
        endif !1st tile
      endif
      if (trcout) then
        call zaiowr3(tracer,   kk, ip,.false., xmin,xmax, 12, .true.)
        call zaiowr3(tracer,   kk, ip,.false., xmin,xmax, 12, .true.)
        if     (mnproc.eq.1) then
        do l= 0,1
          do k= 1,kk
            write(12,4100) 'tracer  ',k,l+1,xmin(k     ),xmax(k     )
          enddo
        enddo
        endif !1st tile
      endif
      call zaiofl(12)
      if     (mnproc.eq.1) then
      write(lp,'(a,f11.2)') ' restart created at model day',dtimex
      endif !1st tile
      call xcsync(flush_lp)
c
      return
 4100 format(a,': layer,tlevel,range = ',i3,i3,2x,1p2e16.7)
      end subroutine restart_out

      end module mod_restart
