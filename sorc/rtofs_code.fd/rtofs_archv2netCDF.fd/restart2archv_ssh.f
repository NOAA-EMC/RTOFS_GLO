      program restart2archv
      use mod_plot     ! HYCOM plot array interface
      use mod_restart  ! HYCOM restart suplement to mod_plot
      use mod_za       ! HYCOM array I/O interface
c
      implicit none
c
c --- hycom restart to hycom archive file.
c
      real           amn,amx
      common/conrng/ amn,amx
c
      character*240    flnmarch,flnmrsi
c
      integer          artype,iexpt,iversn,yrflag,iceflg,kapref,n,
     &                 i,j,k,l,kapi
      real             sigma(99),thstar(99,2),thref,pref,oneta
      real             skap,montg1,montg2
      real             hmina,hmaxa
      real             kappaf    !real function
      double precision time3(3)
      real*8           time,year
c
      real, allocatable :: tbaric(:,:)
c
      real, parameter :: flag = 2.0**100
c
c --- 'lhycom' -- hycom (vs micom) input file
c --- 'trcout' -- tracer input
c --- 'icegln' -- ice    input
      logical   icegln
      logical   lhycom,trcout
      data      lhycom/.true. /,
     &          trcout/.false./
c
      call xcspmd
      call zaiost
      lp=6
c
c --- read model data
c ---   'flnmrsi'  = name of  input restart file
c ---   'flnmarch' = name of output archive file
c ---   'iexpt '   = experiment number x10  (000=from archive file)
c ---   'yrflag'   = days in year flag (0=360J16,1=366J16,2=366J01,3=actual)
c ---   'iceflg'   = ice model flag (0=none(default),1=energy loan model)
c ---   'idm   '   = longitudinal array size
c ---   'jdm   '   = latitudinal  array size
c ---   'kapref'   = thermobaric reference state (-1 to 3, optional, default 0)
c ---   'kdm   '   = number of layers
c ---   'n     '   = extract restart time slot number (1 or 2)
        read (*,'(a)')    flnmrsi 
        write (lp,'(2a)') ' input restart file: ',
     &                    flnmrsi(1:len_trim(flnmrsi))
        call flush(lp)
        read (*,'(a)')    flnmarch
        write (lp,'(2a)') 'output archive file: ',
     &                    flnmarch(1:len_trim(flnmarch))
        call flush(lp)
        call blkini(iexpt, 'iexpt ')
        call blkini(yrflag,'yrflag')
        call blkini2(i,j,  'iceflg','idm   ')  !read iceflg or idm
        if (j.eq.1) then
          iceflg = i
          call blkini(ii,    'idm   ')
        else
          ii     = i
          iceflg = 0  !no ice by default
        endif
        call blkini(jj,    'jdm   ')
        call blkini2(i,j,  'kapref','kdm   ')  !read kapref or kdm
        if (j.eq.1) then
          if     (i.lt.0) then
            kapref = i  !-1
            kapnum = 2  !declared in mod_restart
          else
            kapref = i  !0 to 3
            kapnum = 1  !declared in mod_restart
          endif
          call blkini(kk,  'kdm   ')
        else
          kk     = i
          kapref = 0  !no reference state
          kapnum = 1  !declared in mod_restart
        endif
        call blkini(n,     'n     ')
        if     (ii.ne.idm .or. jj.ne.jdm) then
          write(lp,*)
          write(lp,*) 'error - wrong idm or jdm (should be:',
     &                                           idm,jdm,')'
          write(lp,*)
          call flush(lp)
          stop
        endif
        iorign = 1
        jorign = 1
c
c ---   'thbase' = reference density (sigma units)
        call blkinr(thbase,
     &             'thbase','("blkinr: ",a6," =",f11.4," sig")')
c
        thref = 1.0e-3
        if     (thbase.le.30.0) then
          pref =    0.0   !assume sigma0
        else
          pref = 2000.e4  !assume sigma2
        endif
c                                                             
c ---   layer densities (sigma units)                       
c                                      
        write(lp,*)                      
        do k=1,kk
          call blkinr(sigma(k),
     &                'sigma ','("blkinr: ",a6," =",f11.4," sig")')
c                                                                
          if     (k.gt.1) then                                     
            if      (sigma(k).le.sigma(k-1)) then
            write(lp,'(/ a,i3 /)')             
     .          'error - sigma is not stabally stratified'
              call flush(lp)                              
              stop                                        
            endif           
          endif    !k.gt.1
        enddo !k  
        write(lp,*)
        call flush(lp)
c
c --- array allocation
c
      call plot_alloc
c
      dpthfil = 'regional.depth'
c
c --- read the input restart file
c
      icegln = iceflg.ne.0
      call restart_in(flnmrsi,icegln,n,time)
      time3(1)=time
      time3(2)=time
      time3(3)=time
c
      if     (yrflag.eq.0) then
        year  = 360.0d0
      elseif (yrflag.lt.3) then
        year  = 366.0d0
      else
        year  = 365.25d0
      endif
c
c --- define grid scale
c
      call bigrid(depths)
c
c --- tbaric?
c
      if     (kapref.lt.0) then
        write (lp,'(/a)') ' input from tbaric.a:'
        call flush(lp)
        allocate( tbaric(idm,jdm) )
        call zaiopf('tbaric.a','old', 9)
        call zaiord(tbaric,ip,.false., hmina,hmaxa, 9)
        call zaiocl(9)
        write (lp,'(a,2f8.2/)') 'min,max =',hmina,hmaxa
        do j= 1,jj
          do i= 1,ii
            if     (ip(i,j).eq.0) then
              tbaric(i,j) = 1.0 !land
            endif
          enddo
        enddo
      endif
c
c --- write the archive file.
c
      ctitle(1) = 'restart converted to  archive'
      ctitle(2) = ' '
      ctitle(3) = ' '
      ctitle(4) = ' '
c
      artype      = 1
      theta(1:kk) = sigma(1:kk)
c
      do j= 1,jj
        do i= 1,ii
          if     (ip(i,j).eq.1) then
c
c ---       calculate montg and srfht, allowing for thermobaricity
c
            p(i,j,1) = 0.0
            do k= 1,kk
              p(i,j,k+1) = p(i,j,k)+dp(i,j,k)
              if     (kapref.eq.0) then
                thstar(k,1) = th3d(i,j,k)
              elseif (kapref.gt.0) then
                thstar(k,1) = th3d(i,j,k)
     &                         + kappaf(temp(i,j,k),
     &                                  saln(i,j,k),
     &                                     p(i,j,k),
     &                                  pref,
     &                                  kapref)
              else
c
c               kapi is the 2nd reference state (1st is always 2)
c               skap is the scale factor (0.0-1.0) for the 1st reference state
c
                if     (max(tbaric(    i,          j),
     &                      tbaric(max(i-1, 1),    j),
     &                      tbaric(min(i+1,ii),    j),
     &                      tbaric(    i,      max(j-1, 1)),
     &                      tbaric(    i,      min(j+1,jj)) )
     &                  .gt.2.0) then
                  kapi = 3.0
                  skap = 3.0 - tbaric(i,j)
                else
                  kapi = 1.0
                  skap = tbaric(i,j) - 1.0
                endif
                thstar(k,1) = th3d(i,j,k)
     &                         + kappaf(temp(i,j,k),
     &                                  saln(i,j,k),
     &                                     p(i,j,k),
     &                                  pref,
     &                                  2)
                thstar(k,2) = th3d(i,j,k)
     &                         + kappaf(temp(i,j,k),
     &                                  saln(i,j,k),
     &                                     p(i,j,k),
     &                                  pref,
     &                                  kapi)
              endif !kapref
            enddo !k
            oneta  = 1.0 + pbaro(i,j)/p(i,j,kk+1)
            montg1 = psikk(i,j,1)+
     &          ( p(i,j,kk+1)*(thkk(i,j,1)-thstar(kk,1))
     &            -pbaro(i,j)*(thstar(kk,1)+thbase) )*thref**2
            do k=kk-1,1,-1
              montg1=montg1+p(i,j,k+1)*oneta
     &                    *(thstar(k+1,1)-thstar(k,1))*thref**2
            enddo !k
            if     (kapref.ge.0) then
              montg(i,j) = montg1
            else
              montg2 = psikk(i,j,2)+
     &            ( p(i,j,kk+1)*(thkk(i,j,2)-thstar(kk,2))
     &              -pbaro(i,j)*(thstar(kk,2)+thbase) )*thref**2
              do k=kk-1,1,-1
                montg2=montg2+p(i,j,k+1)*oneta
     &                      *(thstar(k+1,2)-thstar(k,2))*thref**2
              enddo !k
              montg(i,j) = skap*montg1 + (1.0-skap)*montg2
            endif
            srfht(i,j) = montg(i,j) + thref*pbaro(i,j)
          endif !ip.eq.1
c
          surflx(i,j) = 0.0
          salflx(i,j) = 0.0
c
          dpbl(  i,j) = dpmixl(i,j)
          tmix(  i,j) = temp(i,j,1)
          smix(  i,j) = saln(i,j,1)
          thmix( i,j) = th3d(i,j,1)
          umix(  i,j) =    u(i,j,1)
          vmix(  i,j) =    v(i,j,1)
        enddo !i
      enddo !j
      l = len_trim(flnmarch)
      if     (flnmarch(l-1:l).eq.'.a' .or. flnmarch(l-1:l).eq.'.b') then
        flnmarch(l-1:l) = '  '  ! to prevent putdat from using '*.[AB]'
      endif
*     write(lp,*) 'nstep,time3 =',nstep,time3(:)
      call putdat(flnmarch,artype,time3,icegln,trcout,
     &            iexpt,iversn,yrflag,kk, thbase)
c
      stop '(normal)'
      end

      real function kappaf(tf,sf,prsf, pref,kkff)
      implicit none
c
      integer kkff
      real    tf,sf,prsf,pref
c
c --- wrapper for hycom kappaf (stmt_fns.h)
c
      real    kappaf1
      integer kkf
      real    r,s,t,prs
c
c --- coefficients for kappa^(theta)
c --- new values (w.r.t. t-toff,s-soff,prs) from Shan Sun, Sep.2004
c --- 1=Arctic/Antarctic; 2=Atlantic; 3=Mediterranean
      real, parameter, dimension(3) ::
     &  toff = (/  0.0,             3.0,            13.0 /)
     & ,soff = (/ 34.5,            35.0,            38.5 /)
     & ,qttt = (/ -3.03869354E-05, -3.03869352E-05, -3.03869353E-05 /)
     & ,qtt  = (/  4.56625601E-03,  4.29277358E-03,  3.38116552E-03 /)
     & ,qt   = (/ -2.88801209E-01, -2.61828868E-01, -1.81335007E-01 /)
     & ,qs   = (/ -1.08670290E-01, -1.05131061E-01, -9.33336309E-02 /)
     & ,qst  = (/  7.90503772E-04,  7.71096940E-04,  1.07270585E-03 /)
     & ,qpt  = (/  1.07813750E-09,  1.00638435E-09,  7.57239852E-10 /)
     & ,qpst = (/  1.41541548E-11,  1.48598578E-11,  3.89226107E-12 /)
     & ,qptt = (/ -1.31383708E-11, -1.31383707E-11, -1.31383708E-11 /)
c
c --- thermobaric compressibility coefficient (integral from prs to pref)
c ---     Sun et.al. (1999) JPO 29 pp 2719-2729.
c --- kappaf1 used internally to simplify offsetting T and S,
c --- always invoke via kappaf.
c --- offset limits based on stability estimates from:
c ---     Hallberg (2005) Ocean Modelling 8 pp 279-300.
c --- t: potential temperature; s: psu; prs: pressure; kkf: ref.state
c ---     example: kappaf(4.5,34.5,1.e7,1) =  0.11411243
c ---     example: kappaf(4.5,34.5,1.e7,2) =  0.03091669
c ---     example: kappaf(4.5,34.5,1.e7,3) = -0.06423524
      kappaf1(t,s,prs,kkf)=(1.e-11/1.0e-3)*(prs-pref)*
     &  ( s*( qs(kkf)+t* qst(kkf) ) +
     &    t*( qt(kkf)+t*(qtt(kkf)+t*qttt(kkf))+
     &        0.5*(prs+pref)*(qpt(kkf)+s*qpst(kkf)+t*qptt(kkf)) ) )
      kappaf = kappaf1(max(-1.5,         tf-toff(kkff) ),
     &                 max(-4.0,min(2.0, sf-soff(kkff))),
     &                 prsf,kkff)
      return
      end
