c #
c DISTRIBUTION STATEMENT B: Distribution authorized to U.S. Government
c agencies based upon the reasons of possible Premature Distribution
c and the possibility of containing Software Documentation as listed
c on Table 1 of DoD Instruction 5230.24, Distribution Statements on
c Technical Documents, of 23 August 2012. Other requests for this
c document shall be made to Dr. Ruth H. Preller, Superintendent,
c Oceanography Division, U.S. Naval Research Laboratory, DEPARTMENT
c OF THE NAVY, John C. Stennis Space Center, MS 39529-5004; (228)
c 688-4670 (voice); ruth.preller@nrlssc.navy.mil (e-mail).
c #
      subroutine datefor(wday, iyr,mon,idy,ihr)
      implicit none
      integer iyr,mon,idy,ihr
      real*8  wday
c
c**********
c*
c  1) convert date into 'model day', for yrflag=3 only.
c
c  2) the 'model day' is the number of days since 001/1901 (which is 
c      model day 1.0).
c     for example:
c      a) iyr=1901,mon=1,idy=1, represents 0000z hrs on 01/01/1901
c         so wday would be 1.0.
c      a) iyr=1901,mon=1,idy=2, represents 0000z hrs on 02/01/1901
c         so wday would be 2.0.
c     year must be no less than 1901.0, and no greater than 2099.0.
c     note that year 2000 is a leap year (but 1900 and 2100 are not).
c*
c**********
c
      integer nleap
c
      integer month(13)
      data    month / 0,  31,  59,  90, 120, 151, 181,
     &                   212, 243, 273, 304, 334, 365 /
c
c     find the right year.
c
      nleap = (iyr-1901)/4
      wday  = 365.0d0*(iyr-1901) + nleap + month(mon) + idy + ihr/24.0d0
      if     (mod(iyr,4).eq.0 .and. mon.gt.2) then
        wday  = wday + 1.0d0
      endif
      return
c     end of datefor
      end
c
c
      subroutine forday(dtime,yrflag, iyear,iday,ihour)
      use mod_cb_arrays
      implicit none
c
      real*8  dtime
      integer yrflag, iyear,iday,ihour
c
c --- converts model day to "calendar" date (year,ordinal-day,hour).
c
      real*8  dtim1,day
      integer iyr,nleap
c
      if     (yrflag.eq.0) then
c ---   360 days per model year, starting Jan 16
        iyear =  int((dtime+15.001d0)/360.d0) + 1
        iday  =  mod( dtime+15.001d0 ,360.d0) + 1
        ihour = (mod( dtime+15.001d0 ,360.d0) + 1.d0 - iday)*24.d0
c
      elseif (yrflag.eq.1) then
c ---   366 days per model year, starting Jan 16
        iyear =  int((dtime+15.001d0)/366.d0) + 1
        iday  =  mod( dtime+15.001d0 ,366.d0) + 1
        ihour = (mod( dtime+15.001d0 ,366.d0) + 1.d0 - iday)*24.d0
c
      elseif (yrflag.eq.2) then
c ---   366 days per model year, starting Jan 01
        iyear =  int((dtime+ 0.001d0)/366.d0) + 1
        iday  =  mod( dtime+ 0.001d0 ,366.d0) + 1
        ihour = (mod( dtime+ 0.001d0 ,366.d0) + 1.d0 - iday)*24.d0
c
      elseif (yrflag.eq.3) then
c ---   model day is calendar days since 01/01/1901
        iyr   = (dtime-1.d0)/365.25d0
        nleap = iyr/4
        dtim1 = 365.d0*iyr + nleap + 1.d0
        day   = dtime - dtim1 + 1.d0
        if     (dtim1.gt.dtime) then
          iyr = iyr - 1
        elseif (day.ge.367.d0) then
          iyr = iyr + 1
        elseif (day.ge.366.d0 .and. mod(iyr,4).ne.3) then
          iyr = iyr + 1
        endif
        nleap = iyr/4
        dtim1 = 365.d0*iyr + nleap + 1.d0
c
        iyear =  1901 + iyr
        iday  =  dtime - dtim1 + 1.001d0
        ihour = (dtime - dtim1 + 1.001d0 - iday)*24.d0
c
      else
        if     (mnproc.eq.1) then
        write(lp,*)
        write(lp,*) 'error in forday - unsupported yrflag value'
        write(lp,*)
        endif !1st tile
*       call xcstop('(forday)')
               stop '(forday)'
      endif
      return
      end
c
c
      subroutine fordate(dtime,yrflag, iyear,month,iday,ihour)
      implicit none
c
      double precision dtime
      integer          yrflag, iyear,month,iday,ihour
c
c --- converts model day to "calendar" date (year,month,day,hour).
c
      integer          jday,k,m
c
      integer month0(13,3)
      data month0 / 1,  31,  61,  91, 121, 151, 181,
     +                 211, 241, 271, 301, 331, 361,
     +              1,  32,  60,  91, 121, 152, 182,
     +                 213, 244, 274, 305, 335, 366,
     +              1,  32,  61,  92, 122, 153, 183,
     +                 214, 245, 275, 306, 336, 367 /
c
      call forday(dtime,yrflag, iyear,jday,ihour)
c
      if (yrflag.eq.3) then
        if     (mod(iyear,4).eq.0) then
          k = 3
        else
          k = 2
        endif
      elseif (yrflag.eq.0) then
        k = 1
      else
        k = 3
      endif
      do m= 1,12
        if     (jday.ge.month0(m,  k) .and.
     +          jday.lt.month0(m+1,k)      ) then
          month = m
          iday  = jday - month0(m,k) + 1
        endif
      enddo
      return
      end
c
c
      subroutine rdnest(dtime)
      use mod_cb_arrays
      implicit none
c
      real*8    dtime
c
c --- 3-d nesting archive input processing
c
c --- filenames  ./nest/arch[vm].????_???_??.[ab]
c ---            ./nest/rmu.[ab]
c
c --- I/O and array I/O unit 915 is used for rmun[pv] only (not reserved).
c --- I/O and array I/O unit 920 is reserved for the entire run.
c
c --- all input fields must be defined at all grid points
c
      logical   larchm,lmonth
      save      larchm,lmonth
      integer   iarch
      save      iarch
      real*8    dnestf,dnesti,dtimei,dtime0,dtime1
      save      dnestf,dnesti,dtimei,dtime0,dtime1
c
      integer   iyr,mon,idy,ihr
      integer   i,ios,j,k
      character preambl(5)*79,cline*80
      real*8    wdaymn(-1:2)
c
c --- wn0 negative on first call only.
      if     (wn0.lt.-1.0) then
c
c ---   initialize nesting fields
c
        if     (mnproc.eq.1) then
        write (lp,*) ' now initializing 3-d nesting fields ...'
        endif !1st tile
*       call xcsync(flush_lp)
c
*       call zaiopf('nest/rmu.a', 'old', 915)
*       if     (mnproc.eq.1) then  ! .b file from 1st tile only
*       open (unit=uoff+915,file='nest/rmu.b',
*    &        status='old',action='read')
*       endif !1st tile
*       call zagetc(cline,ios, uoff+915)  !1st line of the header on all tiles
*       if     (mnproc.eq.1) then  ! .b file from 1st tile only
*       rewind uoff+915
*       read  (uoff+915,'(a79)') preambl
*       endif !1st tile
*       call preambl_print(preambl)
*       if     (cline.eq.'Relaxation Masks') then  !two masks
*         call rdmonth(rmunp, 915)
*         call rdmonth(rmunv, 915)
*         call xctilr(rmunp,1,1, nbdy,nbdy, halo_ps)
*         call xctilr(rmunv,1,1, nbdy,nbdy, halo_ps)
*       else !one mask
*         call rdmonth(rmunp, 915)
*         call xctilr(rmunp,1,1, nbdy,nbdy, halo_ps)
*         rmunv(:,:) = rmunp(:,:)
*       endif !1 or 2 masks
*       if     (mnproc.eq.1) then  ! .b file from 1st tile only
*       close (unit=uoff+915)
*       endif !1st tile
*       call zaiocl(915)
c
        dnestf = abs(nestfq)
        if     (dnestf.lt.1.0) then
          dnestf = (baclin/86400.0d0)*
     &             max(1,nint((86400.0d0*dnestf)/baclin))
        endif
        lmonth = abs(nestfq+30.5).lt.0.1  !monthly mean archives
        if     (lmonth) then
          call fordate(dtime,yrflag, iyr,mon,idy,ihr)
          if     (mon.eq.1) then
            call datefor(wdaymn(-1), iyr-1,   12,1,0)
            call datefor(wdaymn( 0), iyr,      1,1,0)
            call datefor(wdaymn( 1), iyr,      2,1,0)
            call datefor(wdaymn( 2), iyr,      3,1,0)
          elseif (mon.eq.11) then
            call datefor(wdaymn(-1), iyr,     10,1,0)
            call datefor(wdaymn( 0), iyr,     11,1,0)
            call datefor(wdaymn( 1), iyr,     12,1,0)
            call datefor(wdaymn( 2), iyr+1,    1,1,0)
          elseif (mon.eq.12) then
            call datefor(wdaymn(-1), iyr,     11,1,0)
            call datefor(wdaymn( 0), iyr,     12,1,0)
            call datefor(wdaymn( 1), iyr+1,    1,1,0)
            call datefor(wdaymn( 2), iyr+1,    2,1,0)
          else
            call datefor(wdaymn(-1), iyr,  mon-1,1,0)
            call datefor(wdaymn( 0), iyr,  mon,  1,0)
            call datefor(wdaymn( 1), iyr,  mon+1,1,0)
            call datefor(wdaymn( 2), iyr,  mon+2,1,0)
          endif
          if     (dtime.lt.0.5d0*(wdaymn(0)+wdaymn(1))) then
            dtime0 = 0.5d0*(wdaymn(-1)+wdaymn( 0))
            dtime1 = 0.5d0*(wdaymn( 0)+wdaymn( 1))
          else
            dtime0 = 0.5d0*(wdaymn( 0)+wdaymn( 1))
            dtime1 = 0.5d0*(wdaymn( 1)+wdaymn( 2))
          endif
          ln0    = 1
          call rdnest_in(dtime0,larchm,lmonth,1)
          ln1    = 2
          call rdnest_in(dtime1,larchm,lmonth,2)
        else
          larchm = nestfq.lt.0.0            !mean archives spaning -nestfq days
          if     (larchm) then
            dnesti = 0.5d0*dnestf
          else
            dnesti = 0.0
          endif
          dtimei = int((dtime-dnesti)/dnestf)*dnestf + dnesti
c
          dtime0 = dtimei
          ln0    = 1
          call rdnest_in(dtime0,larchm,lmonth,1)
c
          iarch  = 1
          dtime1 = dtimei + dnestf
          ln1    = 2
          call rdnest_in(dtime1,larchm,lmonth,2)
        endif
c
        if     (mnproc.eq.1) then
        write (lp,*)
        write (lp,*) ' dtime,dtime0,dtime1 = ',dtime,dtime0,dtime1
        write (lp,*)
        write (lp,*) ' ...finished initializing 3-d nesting fields'
        endif !1st tile
*       call xcsync(flush_lp)
      endif  ! initialization
c
      if     (dtime.gt.dtime1) then
c
c ---   get the next set of fields.
*       do k= 1,kk
*         do j= 1,jj
*           do i= 1,ii
*             tnest(i,j,k,1) = tnest(i,j,k,2)
*             snest(i,j,k,1) = snest(i,j,k,2)
*             pnest(i,j,k,1) = pnest(i,j,k,2)
*             unest(i,j,k,1) = unest(i,j,k,2)
*             vnest(i,j,k,1) = vnest(i,j,k,2)
*           enddo
*         enddo
*       enddo
        if     (lmonth) then
          dtime0 = dtime1
          call fordate(dtime1,yrflag, iyr,mon,idy,ihr)
          if     (mon.eq.11) then
            call datefor(wdaymn( 1), iyr,     12,1,0)
            call datefor(wdaymn( 2), iyr+1,    1,1,0)
          elseif (mon.eq.12) then
            call datefor(wdaymn( 1), iyr+1,    1,1,0)
            call datefor(wdaymn( 2), iyr+1,    2,1,0)
          else
            call datefor(wdaymn( 1), iyr,  mon+1,1,0)
            call datefor(wdaymn( 2), iyr,  mon+2,1,0)
          endif
          dtime1 = 0.5d0*(wdaymn( 1)+wdaymn( 2))
          call rdnest_in(dtime1,larchm,lmonth,2)
        else
          dtime0 = dtime1
          iarch  = iarch + 1
          dtime1 = dtimei + dnestf*iarch
          call rdnest_in(dtime1,larchm,lmonth,2)
        endif
c
*           if     (mnproc.eq.1) then
*           write(lp,*) ' exit rdnest_in - ',dtime,dtime0,dtime1
*           endif !1st tile
*           call xcsync(flush_lp)
      endif  ! next set of fields.
c
c --- linear interpolation in time.
      wn0 = (dtime1-dtime)/(dtime1-dtime0)
      wn1 = 1.0 - wn0
*           if     (mnproc.eq.1) then
*           write(lp,*) 'rdnest - dtime,wn0,wn1 = ',dtime,wn0,wn1
*           endif !1st tile
*           call xcsync(flush_lp)
      return
      end
c
c
      subroutine rdnest_in(dtime,larchm,lmonth,lslot)
      use mod_cb_arrays
      implicit none
c
      real*8    dtime
      integer   lslot
      logical   larchm,lmonth
c
c --- input 3-d nesting fields from archive on model day dtime.
c --- filenames  nest/arch[vm].????_???_??.[ab]
c --- I/O and array I/O unit 920 is reserved for the entire run.
c
      logical    ldebug_rdnest
      parameter (ldebug_rdnest=.false.)
c
      character flnm*22, cline*80, cvarin*6, cfield*8
      integer   i,idmtst,ios,j,jdmtst,k,layer
      integer   iyear,iday,ihour
      logical   meanar,nodens
c
      call forday(dtime, yrflag, iyear,iday,ihour)
c
      if     (larchm) then
        write(flnm,'("nest/archm.",i4.4,"_",i3.3,"_",i2.2)')
     &                             iyear,iday,ihour
      else
        write(flnm,'("nest/archv.",i4.4,"_",i3.3,"_",i2.2)')
     &                             iyear,iday,ihour
      endif
c
      if     (mnproc.eq.1) then
      write (lp,"(a,a,a,f12.5,a)") 'rdnest_in: ',flnm," (",dtime,")"
      endif !1st tile
*     call xcsync(flush_lp)
*
*     call zaiopf(flnm//'.a','old', 920)
*     if     (mnproc.eq.1) then  ! .b file from 1st tile only
*       open (unit=uoff+920,file=flnm//'.b',form='formatted',
*    &        status='old',action='read')
c
*       read(uoff+920,'(a)') cline
*       read(uoff+920,'(a)') cline
*       read(uoff+920,'(a)') cline
*       read(uoff+920,'(a)') cline
*
*       read(uoff+920,'(a)') cline
*       read(uoff+920,'(a)') cline
*       read(uoff+920,'(a)') cline
*     endif !1st tile
c
*     call zagetc(cline,ios, uoff+920)
*     read(cline,*) idmtst,cvarin
*     if     (mnproc.eq.1) then
*     write(lp,*) cvarin,' = ',idmtst
*     endif !1st tile
*     if (cvarin.ne.'idm   ') then
*       if     (mnproc.eq.1) then
*       write(lp,*)
*       write(lp,*) 'error in rdnest_in - input ',cvarin,
*    &                        ' but should be idm   '
*       write(lp,*)
*       endif !1st tile
*       call xcstop('(rdnest_in)')
*              stop '(rdnest_in)'
*     endif
*     call zagetc(cline,ios, uoff+920)
*     read(cline,*) jdmtst,cvarin
*     if     (mnproc.eq.1) then
*     write(lp,*) cvarin,' = ',jdmtst
*     endif !1st tile
*     if (cvarin.ne.'jdm   ') then
*       if     (mnproc.eq.1) then
*       write(lp,*)
*       write(lp,*) 'error in rdnest_in - input ',cvarin,
*    &                        ' but should be jdm   '
*       write(lp,*)
*       endif !1st tile
*       call xcstop('(rdnest_in)')
*              stop '(rdnest_in)'
*     endif
c
*     if (idmtst.ne.itdm .or. jdmtst.ne.jtdm) then
*       if     (mnproc.eq.1) then
*       write(lp,*)
*       write(lp,*) 'error in rdnest_in - input idm,jdm',
*    &                        ' not consistent with parameters'
*       write(lp,*) 'idm,jdm = ',itdm,  jtdm,  '  (dimensions.h)'
*       write(lp,*) 'idm,jdm = ',idmtst,jdmtst,'  (input)'
*       write(lp,*)
*       endif !1st tile
*       call xcstop('(rdnest_in)')
*              stop '(rdnest_in)'
*     endif
c
*     if     (mnproc.eq.1) then  ! .b file from 1st tile only
*       read (uoff+920,*)
*     endif
c
c --- skip surface fields.
c
*     call rd_archive(util1, cfield,layer, 920)  ! montg1 (discarded)
*     if     (cfield.ne.'montg1  ') then
*       if     (mnproc.eq.1) then
*       write(lp,'(/ a / a,a /)') cfield,
*    &         'error in rdnest_in - expected ','montg1  '
*       endif !1st tile
*       call xcstop('(rdbaro_in)')
*              stop '(rdbaro_in)'
*     endif
*     nodens = layer.ne.0  !new or original archive type
*     if     (mnproc.eq.1) then  ! .b file from 1st tile only
*       read (uoff+920,*)
*     endif
*     call zaiosk(920)                  !srfhgt
*     call zagetc(cline,ios, uoff+920)  !steric or surflx
*     call zaiosk(920)
*     if     (cline(1:8).eq.'steric  ') then  !surflx
*       if     (mnproc.eq.1) then  ! .b file from 1st tile only
*         read (uoff+920,*)
*       endif
*       call zaiosk(920)
*     endif
*     if     (nodens) then
*       do i= 1,3 !salflx,dpbl,dpmixl
*         if     (mnproc.eq.1) then  ! .b file from 1st tile only
*           read (uoff+920,*)
*         endif
*         call zaiosk(920)
*       enddo
*     else
*       do i= 1,8 !salflx,dpbl,dpmixl,tmix,smix,thmix,umix,vmix
*         if     (mnproc.eq.1) then  ! .b file from 1st tile only
*           read (uoff+920,*)
*         endif
*         call zaiosk(920)
*       enddo
*     endif !nodens:else
*     call zagetc(cline,ios, uoff+920)  !kemix or covice or u_btrop
*     meanar = cline(1:8).eq.'kemix   '
*     if     (meanar) then
*       call zaiosk(920)  !skip kemix
*       call rd_archive(util1, cfield,layer, 920) !covice or u_btrop
*       if     (cfield.eq.'covice  ') then
*         if     (mnproc.eq.1) then  ! .b file from 1st tile only
*           read (uoff+920,*)
*         endif
*         call zaiosk(920) !skip thkice
*         if     (mnproc.eq.1) then  ! .b file from 1st tile only
*           read (uoff+920,*)
*         endif
*         call zaiosk(920) !skip temice
*         call rd_archive(util1, cfield,layer, 920) !u_btrop
*       endif
*       call rd_archive(util2, cfield,layer, 920) !v_btrop
*       if     (mnproc.eq.1) then  ! .b file from 1st tile only
*         read (uoff+920,*)
*       endif
*       call zaiosk(920)  !skip kebtrop
*     else !standard archive file
*       if     (cline(1:8).eq.'covice  ') then
*         call zaiosk(920) !skip covice
*         if     (mnproc.eq.1) then  ! .b file from 1st tile only
*           read (uoff+920,*)
*         endif
*         call zaiosk(920) !skip thkice
*         if     (mnproc.eq.1) then  ! .b file from 1st tile only
*           read (uoff+920,*)
*         endif
*         call zaiosk(920) !skip temice
*         if     (mnproc.eq.1) then  ! .b file from 1st tile only
*           read (uoff+920,*)
*         endif
*       endif
*       call zaiosk(920)  !skip u_btrop
*       if     (mnproc.eq.1) then  ! .b file from 1st tile only
*         read (uoff+920,*)
*       endif
*       call zaiosk(920)  !skip v_btrop
*     endif !meanar:else
c
c --- 3-d fields.
c
*     do k=1,kk
*       call rd_archive(unest(1-nbdy,1-nbdy,k,lslot), cfield,layer, 920)
*       if     (cfield.ne.'u-vel.  ') then
*         if     (mnproc.eq.1) then
*         write(lp,'(/ a / a,a /)') cfield,
*    &           'error in rdnest_in - expected ','u-vel.  '
*         endif !1st tile
*         call xcstop('(rdnest_in)')
*                stop '(rdnest_in)'
*       endif
*       call rd_archive(vnest(1-nbdy,1-nbdy,k,lslot), cfield,layer, 920)
*       if     (cfield.ne.'v-vel.  ') then
*         if     (mnproc.eq.1) then
*         write(lp,'(/ a / a,a /)') cfield,
*    &           'error in rdnest_in - expected ','v-vel.  '
*         endif !1st tile
*         call xcstop('(rdnest_in)')
*                stop '(rdnest_in)'
*       endif
*       if     (meanar) then
*         if     (mnproc.eq.1) then  ! .b file from 1st tile only
*           read (uoff+920,*)
*         endif
*         call zaiosk(920)  !skip k.e.
*       endif
*       if     (k.ne.kk) then
*         call rd_archive(pnest(1-nbdy,1-nbdy,k+1,lslot),
*    &                    cfield,layer, 920)
*         if     (cfield.ne.'thknss  ') then
*           if     (mnproc.eq.1) then
*           write(lp,'(/ a / a,a /)') cfield,
*    &             'error in rdnest_in - expected ','thknss  '
*           endif !1st tile
*           call xcstop('(rdnest_in)')
*                  stop '(rdnest_in)'
*         endif
*       else
*         if     (mnproc.eq.1) then  ! .b file from 1st tile only
*           read (uoff+920,*)
*         endif
*         call zaiosk(920)
*       endif
*       call rd_archive(tnest(1-nbdy,1-nbdy,k,lslot), cfield,layer, 920)
*       if     (cfield.ne.'temp    ') then
*         if     (mnproc.eq.1) then
*         write(lp,'(/ a / a,a /)') cfield,
*    &           'error in rdnest_in - expected ','temp    '
*         endif !1st tile
*         call xcstop('(rdnest_in)')
*                stop '(rdnest_in)'
*       endif
*       call rd_archive(snest(1-nbdy,1-nbdy,k,lslot), cfield,layer, 920)
*       if     (cfield.ne.'salin   ') then
*         if     (mnproc.eq.1) then
*         write(lp,'(/ a / a,a /)') cfield,
*    &           'error in rdnest_in - expected ','salin   '
*         endif !1st tile
*         call xcstop('(rdnest_in)')
*                stop '(rdnest_in)'
*       endif
*       if     (.not. nodens) then
*         if     (mnproc.eq.1) then  ! .b file from 1st tile only
*           read (uoff+920,*)
*         endif
*         call zaiosk(920)  !skip density
*       endif !.not.nodens
*     enddo
*
*     if     (mnproc.eq.1) then  ! .b file from 1st tile only
*     close( unit=uoff+920)
*     endif
*     call zaiocl(920)

*     if     (meanar) then
*       call xctilr(pnest(1-nbdy,1-nbdy,1,lslot),1,kk, 1,1, halo_ps)
*     endif
c
!$OMP PARALLEL DO PRIVATE(j,i,k)
!$OMP&         SCHEDULE(STATIC,jblk)
*     do j=1,jj
*       if     (meanar) then  !mean archive
c         for thin layers, take baroclinic velocity from above
c         otherwise, convert from total to baroclinic velocity
*         do k= 1,kk
*           do i=1,ii
*             if     (iu(i,j).eq.1) then
*               if     (min(pnest(i,  j,k,lslot),
*    &                      pnest(i-1,j,k,lslot) ).lt.tencm) then
*                 unest(i,j,k,lslot) = unest(i,j,max(1,k-1),lslot)
*               else
*                 unest(i,j,k,lslot) = unest(i,j,k,lslot) - util1(i,j)
*               endif !thin layer:else
*             endif !iu
*             if     (iv(i,j).eq.1) then
*               if     (min(pnest(i,j,  k,lslot),
*    &                      pnest(i,j-1,k,lslot) ).lt.tencm) then
*                 vnest(i,j,k,lslot) = vnest(i,j,max(1,k-1),lslot)
*               else
*                 vnest(i,j,k,lslot) = vnest(i,j,k,lslot) - util2(i,j)
*               endif !thin layer:else
*             endif !iv
*           enddo !i
*         enddo !k
*       endif !meanar
*       convert from layer thickness to interface depth (pressure)
*       do i=1,ii
*         pnest(i,j,1,lslot) = 0.0
*         do k= 3,kk
*           pnest(i,j,k,lslot) = pnest(i,j,k,  lslot) +
*    &                           pnest(i,j,k-1,lslot)
*         enddo !k
*       enddo !i
*     enddo !j
c
*     if     (ldebug_rdnest .and. ittest.ne.-1 .and. jttest.ne.-1) then
*       call xcsync(flush_lp)
*       if     (i0.lt.ittest .and. i0+ii.ge.ittest .and.
*    &          j0.lt.jttest .and. j0+jj.ge.jttest      ) then
*103      format(i8,i5,i4,1x,a,a/
*    &           (i8,5x,i4,1x,a,a,2f7.3,2f7.3,f8.4,f9.3,f9.2))
*         write(lp,103)
*    &       nstep,itest+i0,jtest+j0,'rdnest',
*    &       ':   utot   vtot   temp   saln    dens    thkns     dpth',
*    &      (nstep,k,                'rdnest',':',
*    &       unest(itest,jtest,k,lslot)+ubnest(itest,jtest,lslot),
*    &       vnest(itest,jtest,k,lslot)+vbnest(itest,jtest,lslot),
*    &       tnest(itest,jtest,k,lslot),
*    &       snest(itest,jtest,k,lslot),
*    &       0.0,
*    &       (pnest(itest,jtest,k+1,lslot)-
*    &        pnest(itest,jtest,k,  lslot) )*qonem,
*    &       pnest(itest,jtest,k+1,lslot)*qonem,
*    &       k=1,kk-1),
*    &      (nstep,k,                'rdnest',':',
*    &       unest(itest,jtest,k,lslot)+ubnest(itest,jtest,lslot),
*    &       vnest(itest,jtest,k,lslot)+vbnest(itest,jtest,lslot),
*    &       tnest(itest,jtest,k,lslot),
*    &       snest(itest,jtest,k,lslot),
*    &       0.0,
*    &       depths(i,j)-pnest(itest,jtest,k,lslot)*qonem,
*    &       depths(i,j),
*    &       k=kk,kk)
*       endif
*       call xcsync(flush_lp)
*     endif
c
      return
      end
