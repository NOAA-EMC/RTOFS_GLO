      program conv_archv
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
c --- switch an archive file's density between sig0 and sig2.
c
      character label*81,text*18,flnm_i*240,flnm_o*240
      logical initl,trcout,icegln
c
      integer          artype,iexpt,iversn,yrflag,kpalet,mxlflg
      integer          i,ibad,j,k,kkbot,kkin,kkout,kktop,l,newsig
      real             sigma(99),thbase,dpoij,dpuij,dpvij
      double precision time3(3),time,year
c
      real, allocatable :: dpo(:,:,:),dpu(:,:),dpv(:,:)
c
      data trcout/.false./  ! must be .false. (no tracer remapping)
      data initl /.true. /
c
      REAL*4  R4
      REAL*8  R8
      REAL*8  R,T,S
      REAL*8  SIG
      REAL*8  C1,C2,C3,C4,C5,C6,C7
c
c --- auxiliary statement for real*4 to real*8 conversion
      R8(R4)=R4
c
c --- sigma-theta as a function of temp (deg c) and salinity (mil)
c --- (polynomial fit that is cubic in T and linear in S)
      SIG(T,S)=(C1+C3*S+T*(C2+C5*S+T*(C4+C7*S+C6*T)))
c
      call xcspmd
      call zaiost
      lp=6
c
c --- 'flnm_i' = name of original archive file
c --- 'flnm_o' = name of target   archive file
c --- 'iexpt ' = experiment number x10  (000=from archive file)
c --- 'yrflag' = days in year flag (0=360J16,1=366J16,2=366J01,3=actual)
c --- 'idm   ' = longitudinal array size
c --- 'jdm   ' = latitudinal  array size
c --- 'kdm'    = original number of layers
c --- 'newsig' = new sigma (0 or 2)
c
      read (*,'(a)') flnm_i
      write (lp,'(2a)') ' input file: ',trim(flnm_i)
      call flush(lp)
      read (*,'(a)') flnm_o
      write (lp,'(2a)') 'output file: ',trim(flnm_o)
      call flush(lp)
      call blkini(iexpt, 'iexpt ')
      call blkini(yrflag,'yrflag')
      call blkini(ii,    'idm   ')
      call blkini(jj,    'jdm   ')
      if     (ii.ne.idm .or. jj.ne.jdm) then
        write(lp,*)
        write(lp,*) 'error - wrong idm or jdm (should be:',
     &                                         idm,jdm,')'
        write(lp,*)
        call flush(lp)
        stop
      endif
      iorign = 1
      jorign = 1
c
      call blkini(kkin,  'kdm   ')
      kkout = kkin
c
      call blkini(newsig,'newsig')
      if     (newsig.eq.0) then
c ---   coefficients for sigma-0 (based on Brydon & Sun fit)
        C1=-1.36471E-01
        C2= 4.68181E-02
        C3= 8.07004E-01
        C4=-7.45353E-03
        C5=-2.94418E-03
        C6= 3.43570E-05
        C7= 3.48658E-05
      else
c ---   coefficients for sigma-2 (based on Brydon & Sun fit)
        C1= 9.77093E+00
        C2=-2.26493E-02
        C3= 7.89879E-01
        C4=-6.43205E-03
        C5=-2.62983E-03
        C6= 2.75835E-05
        C7= 3.15235E-05
      endif
c
c --- 'thbase' = reference density
c
      call blkinr(thbase,
     &           'thbase','("blkinr: ",a6," =",f11.4," sig")')
c
c --- new layer densities (sigma units)
c
      write(lp,*)
      do k=1,kkout
        call blkinr(sigma(k),
     &              'sigma ','("blkinr: ",a6," =",f11.4," sig")')
c
        if     (k.gt.1) then
          if      (sigma(k).le.sigma(k-1)) then
            write(lp,'(/ a,i3 /)')
     .        'error - sigma is not stabally stratified'
            call flush(lp)
            stop
          endif
        endif
      enddo
c
c --- array allocation
c
      kk    = 0
      kkmax = max(kkin,kkout)
      call plot_alloc
c
      dpthfil = 'regional.depth'
c
      do j=1,jj
        do i=1,ii
          p(i,j,1)=0.
        enddo
      enddo
c
c --- read the archive file, from "*.[ab]".
c
      kk = kkin
      call getdatb(flnm_i,time3,artype,initl,icegln,trcout,
     &             iexpt,iversn,yrflag,kkin)       ! hycom input
      time = time3(3)
      if     (artype.gt.2) then
        write(lp,*)
        write(lp,*) 'error - only artype==1 and artype==2 allowed'
        write(lp,*)
        call flush(lp)
        stop
      endif
c
c --- land masks.
c
      call bigrid(depths)
c
c --- check that bathymetry is consistent with this archive.
c
      ibad = 0
      do j= 1,jj
        do i= 1,ii
          if     (ip(i,j).eq.1) then
            if     (srfht(i,j).gt.2.0**99) then
              ibad = ibad + 1   ! topo sea, srfht land
            endif
          else
            if     (srfht(i,j).lt.2.0**99) then
              ibad = ibad + 1   ! topo land, srfht sea
            endif
          endif
        enddo
      enddo
      if     (ibad.ne.0) then
        write(lp,*)
        write(lp,*) 'error - wrong bathymetry for this archive file'
        write(lp,*) 'number of mismatches = ',ibad
        write(lp,*)
        call flush(lp)
        stop
      endif
c
c     recalculate layers.
c
      do k= 1,kkout
        write(lp,'(a,i3)') 'a) updating layer = ',k
        call flush(lp)
        do j=1,jj
          do i=1,ii
            th3d(i,j,k) = sig(r8(temp(i,j,k)),
     &                        r8(saln(i,j,k))) - thbase
          enddo !i
        enddo !j
      enddo !k
c
      theta(1:kkout) = sigma(1:kkout)
c
c --- write the archive file, in "*.[AB]".
c
      l = len_trim(flnm_o)
      if     (flnm_o(l-1:l).eq.'.a' .or. flnm_o(l-1:l).eq.'.b') then
        flnm_o(l-1:l) = '  '  ! to prevent putdat from using '*.[AB]'
      endif
      kk = kkout
      call putdat(flnm_o,artype,time3,icegln,trcout,
     &            iexpt,iversn,yrflag,kkout, thbase)
      end
