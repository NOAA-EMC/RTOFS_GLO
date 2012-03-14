      program ncoda_archv
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
c --- remap a HYCOM 2.0 archive file to multiple NCODA analysises.
c
c --- The NCODA analysis is on z-levels, but the output archive
c --- has the same number of layers as the input and remains in
c --- hybrid space.  It can either leave the input layer interfaces
c --- "as is" (and remap T&S) or move them based on NCODA's lyrprs.
c
      real, parameter :: flag = 2.0**100
c
      character label*81,text*18,flnm_i*240,flnm_o*240,
     &          flnm_t*240,flnm_s*240,flnm_p*240,flnm_m*240,
     &          flnm_u*240,flnm_v*240,flnm_z*240
      logical   initl,trcout,icegln,ljext,larctic
c
      integer          artype,iexpt,iversn,yrflag
      integer          i,ia,ib,ibad,ih,j,ja,jb,jh,
     &                 k,k2,kkin,kkout,kmld,l,newtop
      integer          intflg,isoflg,itest,jtest
      integer          idmn,idmp,i1stn,jdmn,j1stn,
     &                 kncoda,ncoda_cycle
      integer          nhybrd,nsigma
      real             dp00,dp00x,dp00f,dp00i,ds00,ds00x,ds00f,dp0ij,
     &                 dp0k(99),dp0kf,dpm,dpms,isotop,
     &                 ds0k(99),ds0kf,dsm,dsms,dssk(99),dp0cum(99+1)
      real             u1(99),v1(99),t1(99),s1(99),r1(99),p1(0:99),
     &                 uz(99),vz(99),tz(99),sz(99),rz(99),pz(0:99),
     &                 zz(99),zi(0:99),rl(99)
      real             sigma(99),thk(99),thbase,
     &                 depthu,depthv,onem,qonem,thref,q,
     &                 mldij,qk,pk,pnk,
     &                 vzero,uzero,sarctic
      real             hmina,hmaxa
      double precision time3(3),time,year,mass_h,mass_n
c
      real,    allocatable :: pout(:,:,:)
      real,    allocatable :: tncoda(:,:,:),sncoda(:,:,:),pncoda(:,:,:)
      real,    allocatable :: uinc(:,:,:),vinc(:,:,:)
      real,    allocatable :: uncoda(:,:,:),vncoda(:,:,:)
      real,    allocatable :: mncoda(:,:)
      real,    allocatable :: pij(:),mldlay(:,:),work(:,:)
      integer, allocatable :: incoda(:,:)
c
      data trcout/.false./  ! must be .false. (no tracer remapping)
      data initl /.true. /
c
      REAL*8     AZERO,AHALF,ATHIRD,A1P5
      PARAMETER (AZERO  =0.D0)
      PARAMETER (AHALF  =1.D0/2.D0)
      PARAMETER (ATHIRD =1.D0/3.D0)
      PARAMETER (A1P5   =3.0D0/2.D0)
c
c --- sub-coefficients for locally referenced sigma
c --- a fit towards Jackett & McDougall (1995)
      REAL*8, PARAMETER, DIMENSION(7) ::
     &  ALPHAP = (/ -0.1364705627213484   , 0.04681812123458564,
     &               0.80700383913187     ,-0.007453530323180844,
     &              -0.002944183249153631 , 0.00003435702568990446,
     &               0.0000348657661057688 /)
     & ,BETAP  = (/  0.05064226654169138  ,-0.0003571087848996894,
     &              -0.0000876148051892879, 5.252431910751829e-6,
     &               1.579762259448864e-6 ,-3.466867400295792e-8,
     &              -1.687643078774232e-8 /)
     & ,GAMMAP = (/ -5.526396144304812e-6 , 4.885838128243163e-8,
     &               9.96026931578033e-9  ,-7.251389796582352e-10,
     &              -3.987360250058777e-11, 4.006307891935698e-12,
     &               8.26367520608008e-13 /)
c
      REAL*4  R4
      REAL*8  R8
      REAL*8  R,T,S,PRS
      REAL*8  SIG,SIGLOC
      REAL*8  C1P,C2P,C3P,C4P,C5P,C6P,C7P
      REAL*8  C1,C2,C3,C4,C5,C6,C7
c
      REAL*8  SOFSIG,TOFSIG
      REAL*8  A0,A1,A2,CUBR,CUBQ,CUBAN,CUBRL,CUBIM
c
c --- auxiliary statement for real*4 to real*8 conversion
      R8(R4)=R4
c
c --- auxiliary statements for finding root of 3rd degree polynomial
      A0(S)=(C1+C3*S)/C6
      A1(S)=(C2+C5*S)/C6
      A2(S)=(C4+C7*S)/C6
      CUBQ(  S)=ATHIRD*A1(S)-(ATHIRD*A2(S))**2
      CUBR(R,S)=ATHIRD*(AHALF*A1(S)*A2(S)-A1P5*(A0(S)-R/C6))
     &            -(ATHIRD*A2(S))**3
c --- if q**3+r**2>0, water is too dense to yield real root at given
c --- salinitiy. setting q**3+r**2=0 in that case is equivalent to
c --- lowering sigma until a double real root is obtained.
      CUBAN(R,S)=ATHIRD*ATAN2(SQRT(MAX(AZERO,
     &            -(CUBQ(S)**3+CUBR(R,S)**2))),CUBR(R,S))
      CUBRL(R,S)=SQRT(-CUBQ(S))*COS(CUBAN(R,S))
      CUBIM(R,S)=SQRT(-CUBQ(S))*SIN(CUBAN(R,S))
c
c --- temp (deg c) as a function of sigma and salinity (mil)
      TOFSIG(R,S)=-CUBRL(R,S)+SQRT(3.D0)*CUBIM(R,S)-ATHIRD*A2(S)
c
c --- salinity (mil) as a function of sigma and temperature (deg c)
      SOFSIG(R,T)=(R-C1-T*(C2+T*(C4+C6*T)))/(C3+T*(C5+C7*T))
c
c --- sigma-theta as a function of temp (deg c) and salinity (mil)
c --- (polynomial fit that is cubic in T and linear in S)
      SIG(T,S)=(C1+C3*S+T*(C2+C5*S+T*(C4+C7*S+C6*T)))
c
c --- locally referenced sigma, a fit towards Jackett & McDougall (1995)
c --- t: potential temperature; s: psu; prs: pressure
      C1P(PRS)=ALPHAP(1)+1.D-5*PRS*(BETAP(1)+1.D-5*PRS*GAMMAP(1))
      C2P(PRS)=ALPHAP(2)+1.D-5*PRS*(BETAP(2)+1.D-5*PRS*GAMMAP(2))
      C3P(PRS)=ALPHAP(3)+1.D-5*PRS*(BETAP(3)+1.D-5*PRS*GAMMAP(3))
      C4P(PRS)=ALPHAP(4)+1.D-5*PRS*(BETAP(4)+1.D-5*PRS*GAMMAP(4))
      C5P(PRS)=ALPHAP(5)+1.D-5*PRS*(BETAP(5)+1.D-5*PRS*GAMMAP(5))
      C6P(PRS)=ALPHAP(6)+1.D-5*PRS*(BETAP(6)+1.D-5*PRS*GAMMAP(6))
      C7P(PRS)=ALPHAP(7)+1.D-5*PRS*(BETAP(7)+1.D-5*PRS*GAMMAP(7))
      SIGLOC(T,S,PRS)=C1P(PRS)+C3P(PRS)*S+
     &       T*(C2P(PRS)+C5P(PRS)*S+T*(C4P(PRS)+C7P(PRS)*S+C6P(PRS)*T))
c
      call xcspmd
      call zaiost
      lp=6
c
      thref = 1.0e-3
      onem  = 9806.0   ! g/thref
      qonem = 1.0/onem
c
c --- 'flnm_i' = name of original  archive file
c --- 'flnm_o' = name of target    archive file
c --- 'intflg' = vertical interpolation flag (0=T&S, 1=th&S, 2=th&T)
c --- 'isoflg' = Preserve isopycnal layer flag (0=n(def),1=y,2=y&layT,3=y&isoT)
c --- 'iexpt ' = experiment number x10  (000=from archive file)
c --- 'yrflag' = days in year flag (0=360J16,1=366J16,2=366J01,3=actual)
c --- 'idm   ' = longitudinal array size
c --- 'jdm   ' = latitudinal  array size
c --- 'itest ' = longitudinal test point (optional, default 0)
c --- 'jtest ' = latitudinal  test point (optional, default 0)
c --- 'kdm   ' = number of layers
c
      read (*,'(a)') flnm_i
      write (lp,'(2a)') ' input file: ',trim(flnm_i)
      call flush(lp)
      read (*,'(a)') flnm_o
      write (lp,'(2a)') 'output file: ',trim(flnm_o)
      write(lp,*)
      call flush(lp)
      call blkini(intflg,'intflg')
      call blkini(isoflg,'isoflg')
      call blkini(iexpt ,'iexpt ')
***
      call blkini(yrflag,'yrflag')
      call blkini(ii,    'idm   ')
      call blkini(jj,    'jdm   ')
      call blkini2(i,j,  'itest ','kdm   ')  !read itest or kdm
      if (j.eq.1) then
        itest  = i
        call blkini(jtest, 'jtest ')
        call blkini(kkin,  'kdm   ')
      else
        itest  = 0
        jtest  = 0
        kkin   = i
      endif
      kkout = kkin
      if     (ii.ne.idm .or. jj.ne.jdm) then
        write(lp,*)
        write(lp,*) 'error - wrong idm or jdm (should be:',
     &                                         idm,jdm,')'
        write(lp,*)
        call flush(lp)
        stop
      endif
      if     (isoflg.ge.2 .and. intflg.eq.1) then
        write(lp,*)
        write(lp,*) 'error - isoflg==2,3 needs new T (intflg=0 or 2)'
        write(lp,*)
        call flush(lp)
        stop
      endif
      iorign = 1
      jorign = 1
c
c --- 'nhybrd' = number of hybrid levels (0=all isopycnal)
c --- 'nsigma' = number of sigma  levels (nhybrd-nsigma z-levels)
c --- 'dp00'   = deep    z-level spacing minimum thickness (m)
c --- 'dp00x'  = deep    z-level spacing maximum thickness (m)
c --- 'dp00f'  = deep    z-level spacing stretching factor (1.0=const.z)
c --- 'ds00'   = shallow z-level spacing minimum thickness (m)
c --- 'ds00x'  = shallow z-level spacing maximum thickness (m)
c --- 'ds00f'  = shallow z-level spacing stretching factor (1.0=const.z)
c --- 'dp00i'  = deep iso-pycnal spacing minimum thickness (m)
c --- 'isotop' = shallowest depth for isopycnal layers     (m)
c
c --- the above specifies a vertical coord. that is isopycnal or:
c ---     z in    deep water, based on dp00,dp00x,dp00f
c ---     z in shallow water, based on ds00,ds00x,ds00f and nsigma
c ---     sigma between them, based on ds00,ds00x,ds00f and nsigma
c --- for z-only set nsigma=0 (and ds00,ds00x,ds00f=dp00,dp00x,dp00f)
c --- for sigma-z (shallow-deep) use a very small ds00
c ---  (pure sigma-z also has ds00f=dp00f and ds00x=dp00x*ds00/dp00)
c --- for z-sigma (shallow-deep) use a very large dp00 (not recommended)
c --- for sigma-only set nsigma=kdm, dp00 large, and ds00 small
c
c --- away from the surface, the minimum layer thickness is dp00i.
c
      call blkini(nhybrd,'nhybrd')
      call blkini(nsigma,'nsigma')
      call blkinr(dp00,  'dp00  ','(a6," =",f10.4," m")')
      call blkinr(dp00x, 'dp00x ','(a6," =",f10.4," m")')
      call blkinr(dp00f, 'dp00f ','(a6," =",f10.4," ")')
      call blkinr(ds00,  'ds00  ','(a6," =",f10.4," m")')
      call blkinr(ds00x, 'ds00x ','(a6," =",f10.4," m")')
      call blkinr(ds00f, 'ds00f ','(a6," =",f10.4," ")')
      call blkinr(dp00i, 'dp00i ','(a6," =",f10.4," m")')
      call blkinr(isotop,'isotop','(a6," =",f10.4," m")')
c
      if     (nhybrd.gt.kkout) then
        write(lp,'(/ a,i3 /)')
     &    'error - maximum nhybrd is kdmnew =',kkout
        call flush(lp)
      endif
      if     (nsigma.gt.nhybrd) then
        write(lp,'(/ a,i3 /)')
     &    'error - maximum nsigma is nhybrd =',nhybrd
        call flush(lp)
      endif
      if (dp00f.lt.1.0) then
        write(lp,'(/ a /)')
     &    'error - must have dp00f>=1.0'
        call flush(lp)
      endif
      if (dp00f.eq.1.0 .and. dp00.ne.dp00x) then
        write(lp,'(/ a /)')
     &    'error - must have dp00x==dp00 for dp00f==1.0'
        call flush(lp)
      endif
      if (dp00.gt.dp00x) then
        write(lp,'(/ a /)')
     &    'error - dp00x must be at least dp00'
        call flush(lp)
      endif
      if (ds00.gt.dp00 .or. ds00x.gt.dp00x .or. ds00f.gt.dp00f) then
        write(lp,'(/ a /)')
     &    'error - must have ds00,ds00x,ds00f <= dp00,dp00x,dp00f'
        call flush(lp)
      endif
      if (ds00.le.0.0) then
        write(lp,'(/ a /)')
     &    'error - must have ds00>0.0'
        call flush(lp)
      endif
      if (ds00f.lt.1.0) then
        write(lp,'(/ a /)')
     &    'error - must have ds00f>=1.0'
        call flush(lp)
      endif
      if (ds00f.eq.1.0 .and. ds00.ne.ds00x) then
        write(lp,'(/ a /)')
     &    'error - must have ds00x==ds00 for ds00f==1.0'
        call flush(lp)
      endif
       if (ds00.gt.ds00x) then
        write(lp,'(/ a /)')
     &    'error - ds00x must be at least ds00'
        call flush(lp)
      endif
c
c --- 'thbase' = new reference density (sigma units)
c
      call blkinr(thbase,
     &           'thbase','("blkinr: ",a6," =",f11.4," sig")')
c
c --- target layer densities (sigma units)
c
      write(lp,*)
      do k=1,kkout
        call blkinr(sigma(k),
     &              'sigma ','("blkinr: ",a6," =",f11.4," sig")')
        thk(k) = sigma(k) - thbase
c
        if     (k.gt.1) then
          if      (sigma(k).le.sigma(k-1)) then
            write(lp,'(/ a /)')
     &        'error - sigma is not stabally stratified'
            call flush(lp)
            stop
          endif
        endif
      enddo
c
      IF     (THBASE.lt.30.0) THEN
c ---   coefficients for sigma-0 (based on Brydon & Sun fit)
        C1=-1.36471E-01
        C2= 4.68181E-02
        C3= 8.07004E-01
        C4=-7.45353E-03
        C5=-2.94418E-03
        C6= 3.43570E-05
        C7= 3.48658E-05
      ELSE
c ---   coefficients for sigma-2 (based on Brydon & Sun fit)
        C1= 9.77093E+00
        C2=-2.26493E-02
        C3= 7.89879E-01
        C4=-6.43205E-03
        C5=-2.62983E-03
        C6= 2.75835E-05
        C7= 3.15235E-05
      ENDIF
c
c --- logorithmic k-dependence of dp0 (deep z's)
      dp00i  =dp00i *onem
      isotop =isotop*onem
c
      dp00   =dp00 *onem
      dp00x  =dp00x*onem
      dp0k(1)=dp00
      dpm    =dp0k(1)
      dpms   =dpm
      write(lp,*)
      write(lp,135) 1,dp0k(1)*qonem,dpm*qonem,dpms*qonem
      call flush(lp)
 135  format('dp0k(',i2,') =',f7.2,' m',
     &          '    thkns =',f7.2,' m',
     &          '    depth =',f8.2,' m')
c
      dp0kf=1.0
      do k=2,kkout
        dp0kf=dp0kf*dp00f
        if     (k.le.nhybrd) then
          dp0k(k)=min(dp00*dp0kf,dp00x)
        else
          dp0k(k)=0.0
        endif
        dpm  = dp0k(k)
        dpms = dpms + dpm
        write(lp,135) k,dp0k(k)*qonem,dpm*qonem,dpms*qonem
        call flush(lp)
      enddo
c
c --- logorithmic k-dependence of ds0 (shallow z-s)
      ds00   =ds00 *onem
      ds00x  =ds00x*onem
      ds0k(1)=ds00
      dsm    =ds0k(1)
      dsms   =dsm
      write(lp,*)
      write(lp,130) 1,ds0k(1)*qonem,dsm*qonem,dsms*qonem
 130  format('ds0k(',i2,') =',f7.2,' m',
     &          '    thkns =',f7.2,' m',
     &          '    depth =',f8.2,' m')
      call flush(lp)
c
      ds0kf=1.0
      do k=2,nsigma
        ds0kf=ds0kf*ds00f
        ds0k(k)=min(ds00*ds0kf,ds00x)
        dsm  = ds0k(k)
        dsms = dsms + dsm
        write(lp,130) k,ds0k(k)*qonem,dsm*qonem,dsms*qonem
        call flush(lp)
      enddo
      write(lp,*)
c
c --- sigma-depth scale factors
      do k=1,nsigma
        dssk(k)=ds0k(k)/dsms  ! fraction of depths in sigma layer k
      enddo
      do k= nsigma+1,kkout
        ds0k(k)=dp0k(k)
        dssk(k)=0.0           ! these layers are zero in sigma mode
      enddo
c
c --- array allocation
c
      kk    = 0
      kkmax = max(kkin,kkout)
      call plot_alloc
c
      allocate( incoda(idm,jdm) )
      allocate(   pout(idm,jdm,kkout+1) )
c
      dpthfil = 'regional.depth'
c
      do jh=1,jj
        do ih=1,ii
          p(ih,jh,1)=0.
        enddo
      enddo
c
c --- read the archive file, from "*.[ab]".
c
      kk = kkin
      call getdatb(flnm_i,time3,artype,initl,icegln,trcout,
     &             iexpt,iversn,yrflag,kkin)       ! hycom input
      time = time3(3)
      if     (artype.eq.3) then
        write(lp,*)
        write(lp,*) 'error - cannot remap std.dev. archive'
        write(lp,*)
        call flush(lp)
        stop
      endif
c
c --- land masks.
c
      call bigrid(depths)
c
      do jh= 1,jj
        do ih= 1,ii
          depths(ih,jh) = depths(ih,jh)*onem
        enddo
      enddo
c
c --- check that bathymetry is consistent with this archive.
c
      ibad = 0
      do jh= 1,jj
        do ih= 1,ii
          if     (ip(ih,jh).eq.1) then
            if     (srfht(ih,jh).gt.2.0**99) then
              ibad = ibad + 1   ! topo sea, srfht land
            endif
          else
            if     (srfht(ih,jh).lt.2.0**99) then
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
c --- primary NCODA loop
c
      do ncoda_cycle= 1,999 !exit on flnm_t=="NONE"
c
c --- 'flnm_t' = name of ncoda temperature file, or "NONE" to exit
c --- 'flnm_s' = name of ncoda salinity    file
c --- 'flnm_u' = name of ncoda u-vel. inc. file, or "NONE"
c --- 'flnm_v' = name of ncoda v-vel. inc. file, or "NONE"
c --- 'flnm_p' = name of ncoda dens offset file, or "NONE"
c --- 'flnm_m' = name of ncoda subreg mask file, or "NONE"
c --- 'flnm_z' = name of ncoda cell interface depths (text file)
c --- 'i1stn ' = i-origin of ncoda subregion
c --- 'j1stn ' = j-origin of ncoda subregion
c --- 'idmn  ' = i-extent of ncoda subregion (<=idm; 0 implies idm)
c --- 'jdmn  ' = j-extent of ncoda subregion (<=jdm; 0 implies jdm)
c --- 'kncoda' = number   of ncoda levels
c
      write(lp,*)
      write(lp,*) 'NCODA Cycle number',ncoda_cycle
      write(lp,*)
      call flush(lp)
      read (*,'(a)') flnm_t
      write (lp,'(2a)') 'Tncoda file: ',trim(flnm_t)
      if     (flnm_t.eq."NONE") then
        write(lp,*)
        write(lp,*) '***** EXIT NCODA Cycle *****'
        write(lp,*)
        call flush(lp)
        exit !ncoda_cycle
      endif
      read (*,'(a)') flnm_s
      write (lp,'(2a)') 'Sncoda file: ',trim(flnm_s)
      read (*,'(a)') flnm_u
      write (lp,'(2a)') 'Uncoda file: ',trim(flnm_u)
      read (*,'(a)') flnm_v
      write (lp,'(2a)') 'Vncoda file: ',trim(flnm_v)
      read (*,'(a)') flnm_p
      write (lp,'(2a)') 'Pncoda file: ',trim(flnm_p)
      read (*,'(a)') flnm_m
      write (lp,'(2a)') 'Mncoda file: ',trim(flnm_m)
      read (*,'(a)') flnm_z
      write (lp,'(2a)') 'Zncoda file: ',trim(flnm_z)
      write(lp,*)
      call flush(lp)
      call blkini(i1stn ,'i1stn ')
      call blkini(j1stn ,'j1stn ')
      call blkini(idmn  ,'idmn  ')
      call blkini(jdmn  ,'jdmn  ')
      call blkini(kncoda,'kncoda')
      ljext = (j1stn + jdmn - 1) .gt. jdm  !assume an involved arctic patch
      if     (ljext) then
        write(lp,*)
        write (lp,'(a)') 'subregion extends across arctic boundary'
        write(lp,*)
        call flush(lp)
      endif
      idmp = min( idmn, idm)  !idmn will be idm+1 in 360-degree cases.
c
c --- read kncoda sets of ncoda fields.
c
      write(lp,*) 'open  ',trim(flnm_z)
      call flush(lp)
      call zhopnc(9, flnm_z, 'FORMATTED', 'OLD', 0)
      do k= 0,kncoda
        read(9,*) zi(k)
        zi(k) = onem*zi(k)
      enddo !k
      close(unit=9)
      write(lp,*) 'close ',trim(flnm_z)
      call flush(lp)
      zz(1) = 0.0
      do k= 2,kncoda
        zz(k) = 0.5*(zi(k-1)+zi(k))
      enddo !k
c
c --- deallocate these arrays at end of ncoda_cycle loop
      allocate( tncoda(1:idmn,1:jdmn,kncoda),
     &          sncoda(1:idmn,1:jdmn,kncoda),
     &          pncoda(1:idmn,1:jdmn,kncoda),
     &          mncoda(1:idmn,1:jdmn) )
c
      write(lp,*) 'open  ',trim(flnm_t)
      call flush(lp)
      call zhopnc(9, flnm_t, 'UNFORMATTED', 'OLD', -idmn*jdmn)
      do k= 1,kncoda
        read(unit=9,rec=k) tncoda(:,:,k)
      enddo !k
      close(unit=9)
      write(lp,*) 'close ',trim(flnm_t)
      call flush(lp)
c
      write(lp,*) 'open  ',trim(flnm_s)
      call flush(lp)
      call zhopnc(9, flnm_s, 'UNFORMATTED', 'OLD', -idmn*jdmn)
      do k= 1,kncoda
        read(unit=9,rec=k) sncoda(:,:,k)
      enddo !k
      close(unit=9)
      write(lp,*) 'close ',trim(flnm_s)
      call flush(lp)
c
      if     (flnm_u.ne."NONE") then
c ---   deallocate these arrays at end of ncoda_cycle loop
        allocate( uncoda(1:idmn,1:jdmn,kncoda),
     &            vncoda(1:idmn,1:jdmn,kncoda) )
        allocate(   uinc(1:idmn,1:jdmn,kkout),
     &              vinc(1:idmn,1:jdmn,kkout) )
c
        write(lp,*) 'open  ',trim(flnm_u)
        call flush(lp)
        call zhopnc(9, flnm_u, 'UNFORMATTED', 'OLD', -idmn*jdmn)
        do k= 1,kncoda
          read(unit=9,rec=k) uncoda(:,:,k)
        enddo !k
        close(unit=9)
        write(lp,*) 'close ',trim(flnm_u)
        call flush(lp)
c
        write(lp,*) 'open  ',trim(flnm_v)
        call flush(lp)
        call zhopnc(9, flnm_v, 'UNFORMATTED', 'OLD', -idmn*jdmn)
        do k= 1,kncoda
          read(unit=9,rec=k) vncoda(:,:,k)
        enddo !k
        close(unit=9)
        write(lp,*) 'close ',trim(flnm_v)
        call flush(lp)
      endif !u&vncoda
c
      if     (flnm_p.ne."NONE") then
        write(lp,*) 'open  ',trim(flnm_p)
        call flush(lp)
        call zhopnc(9, flnm_p, 'UNFORMATTED', 'OLD', -idmn*jdmn)
        do k= 1,kncoda
          read(unit=9,rec=k) pncoda(:,:,k)
          if     (k.eq.1) then
            do j= 1,jdmn
              do i= 1,idmn
                pncoda(i,j,k) = max(0.0, pncoda(i,j,k)*1.0198*onem )
              enddo !i
            enddo !j
          elseif (k.lt.kncoda) then
            do j= 1,jdmn
              do i= 1,idmn
                pncoda(i,j,k) =          pncoda(i,j,k)*1.0198*onem  
              enddo !i
            enddo !j
          else !k.eq.ncoda
            do j= 1,jdmn
              do i= 1,idmn
                pncoda(i,j,k) =     0.0
              enddo !i
            enddo !j
          endif
        enddo !k
        close(unit=9)
        write(lp,*) 'close ',trim(flnm_p)
        call flush(lp)
      endif !pncoda
c
      if     (flnm_m.ne."NONE") then
        write(lp,*) 'open  ',trim(flnm_m)
        call flush(lp)
        call zhopnc(9, flnm_m, 'UNFORMATTED', 'OLD', -idmn*jdmn)
        read( unit=9,rec=1) mncoda(:,:)
        close(unit=9)
        write(lp,*) 'close ',trim(flnm_m)
        call flush(lp)
        incoda(:,:) = 0  !land outside ncoda subregion
        do j= 1,jdmn
          jh = j+j1stn-1
          larctic = ljext .and. jh.ge.jdm
          if     (larctic) then
            jh = jdm-1-(jh-jdm)  !p-grid
          endif
          do i= 1,idmp
            ih = mod(i+i1stn-2+idm,idm)+1
            if     (larctic) then
              ih = idm-mod(ih-1,idm)  !p-grid
            endif
            if     (mncoda(i,j).lt.2.0**99) then
              incoda(ih,jh) = 1 !ncoda sea point
            else
              incoda(ih,jh) = 0 !ncoda land point
            endif
          enddo
        enddo
      else
        incoda(:,:) = 0  !land outside ncoda subregion
        do j= 1,jdmn
          jh = j+j1stn-1
          larctic = ljext .and. jh.ge.jdm
          if     (larctic) then
            jh = jdm-1-(jh-jdm)  !p-grid
          endif
          do i= 1,idmp
            ih = mod(i+i1stn-2+idm,idm)+1
            if     (larctic) then
              ih = idm-mod(ih-1,idm)  !p-grid
            endif
            incoda(ih,jh) = ip(ih,jh)
          enddo
        enddo
      endif !mncoda:else
c
c --- mixed-layer depth in layer space (1 < mldlay < kk+1).
c --- must be at least as deep as the fixed vertical coordinate layers.
c --- only calculate this once.
c
      if     ((flnm_p.ne."NONE" .or. isoflg.ne.0) .and.
     &        .not. allocated(mldlay)                  ) then
        allocate( pij(kkin+1), mldlay(idm,jdm), work(idm,jdm) )
c
        do jh= 1,jdm
          do ih= 1,idm
            if     (ip(ih,jh).eq.1) then
              mldlay(ih,jh) = kkin  !usually modified below
              mldij     = max( dpmixl(ih,jh), isotop )
              dp0cum(1) = 0.0
              pij(   1) = 0.0
              do k= 1,kkin
c ---           q is dp0k(k) when in surface fixed coordinates
c ---           q is dp00i   when much deeper than surface fixed coordinates
                if     (dp0k(k).le.dp00i .or. k.eq.1) then
                  q =      dp0k(k)
                else
                  q = max( dp00i,
     &                     dp0k(k) * dp0k(k)/
     &                               max( dp0k(k),
     &                                    pij(k)-dp0cum(k) ) )
                endif
                dp0ij=min(q,max(ds0k(k),dssk(k)*depths(ih,jh)))
                dp0cum(k+1) = dp0cum(k) + dp0ij
                pij(   k+1) = pij(   k) + dp(ih,jh,k)
                if     (dp(ih,jh,k).lt.1.5*dp0ij) then  !fixed coordinates
                  mldij = max( mldij, pij(k+1) )
                endif
                if     (mldij.lt.pij(k+1)) then
                  if     (isotop.ge.pij(k)) then  !always fixed zone
                    mldlay(ih,jh) = k + 1
                  else
                    mldlay(ih,jh) = k + (mldij - pij(k)) / dp(ih,jh,k)
                  endif
                  exit
                endif
              enddo !k
              if     (ih.eq.itest .and. jh.eq.jtest) then
                write(6,*) 'mld      = ',dpmixl(ih,jh)*qonem  !meters
                write(6,*) 'mldlay   = ',mldlay(ih,jh)        !layers
              endif
            endif !ip
          enddo !i
        enddo !j
c
c ---   smooth three times
c
        call psmoo(mldlay, work)
        call psmoo(mldlay, work)
        call psmoo(mldlay, work)
        if     (min(itest,jtest).gt.0) then
          write(6,*) 'mldlaysm = ',mldlay(itest,jtest)  !layers
        endif
c
        deallocate( pij, work)  !keep mldlay
      endif !mldlay, if needed
c
c --- form exisiting and target interface depths.
c
      do jh= 1,jdm
        do ih= 1,idm
          if     (ip(ih,jh).eq.1) then
*           write(lp,'(a,2i4)') 'p - ih,jh = ',ih,jh
*           call flush(lp)
            p(ih,jh,1) = 0.0
            do k= 1,kkin-1
              p(ih,jh,k+1) = min(p(ih,jh,k) + dp(ih,jh,k),
     &                           depths(ih,jh))
            enddo !k
            p(ih,jh,kkin+1) = depths(ih,jh)
c
c ---       default is that interfaces remain the same
c
            do k= 1,kkout+1 !kkout=kkin
              pout(ih,jh,k) = p(ih,jh,k)
            enddo !k
          endif !ip
        enddo !ih
      enddo !jh
c
      if     (flnm_p.ne."NONE") then
        do j= 1,jdmn
          jh = j+j1stn-1
          larctic = ljext .and. jh.ge.jdm
          if     (larctic) then
            jh = jdm-1-(jh-jdm)  !p-grid
          endif
          do i= 1,idmp
            ih = mod(i+i1stn-2+idm,idm)+1
            if     (larctic) then
              ih = idm-mod(ih-1,idm)  !p-grid
            endif
            if     (incoda(ih,jh).eq.1) then
c
c ---         move interfaces based on pncoda
c
              kmld = int(mldlay(ih,jh))
              dp0cum(1)=0.0
              pout(ih,jh,1) = 0.0
              do k= 2,kkout !kkout=kkin
c ---           q is dp0k(k-1) when in surface fixed coordinates
c ---           q is dp00i     when much deeper than surface fixed coordinates
                if     (dp0k(k-1).le.dp00i) then
                  q =      dp0k(k-1)
                else
                  q = max( dp00i,
     &                     dp0k(k-1) * dp0k(k-1)/
     &                               max( dp0k( k-1),
     &                                    p(ih,jh,k-1)-dp0cum(k-1) ) )
                endif
                dp0ij=min(q,max(ds0k(k-1),dssk(k-1)*depths(ih,jh)))
                dp0cum(k)=dp0cum(k-1)+dp0ij
                pk = p(ih,jh,k) !no motion
                if    (k-1.ge.kmld) then
c ---             below the mixed layer and isotop
                  do l=2,kncoda
                    if     (zz(l).gt.pk) then
                      qk  = (zz(l)-pk)/(zz(l)-zz(l-1))
                      pnk = (1.0-qk)*pncoda(i,j,l)  +
     &                           qk *pncoda(i,j,l-1)
*                     if     (k-1.eq.kmld) then
* ---                   scale by fraction of layer deeper than the mixed-layer
*                       pk  = pk + pnk*(kmld+1.0-mldlay(ih,jh))
*                     else
                        pk  = pk + pnk
*                     endif
                      if     (ih.eq.itest .and. jh.eq.jtest) then
                        write(lp,*) 'k,l,zz = ',
     &                               k,l,zz(l-1)*qonem,zz(l)*qonem
                        write(lp,*) 'k,l,qk = ',
     &                               k,l,qk
                        write(lp,*) 'k,l,pn = ',
     &                               k,l,pncoda(i,j,l-1)*qonem,
     &                                   pncoda(i,j,l)  *qonem
                        write(lp,*) 'k,l,pk = ',
     &                               k,l,p(ih,jh,k)*qonem,pk*qonem
                        call flush(lp)
                      endif
                      exit
                    endif
                  enddo !l
                elseif (ih.eq.itest .and. jh.eq.jtest) then
                  write(lp,*) 'k,th3d,target = ',
     &                         k,th3d(ih,jh,k),thk(k)
                  call flush(lp)
                endif !below mixed layer
                pout(ih,jh,k) = min( max( pk,
     &                                    pout(ih,jh,k-1)+dp0ij ),
     &                               depths(ih,jh) )
                if     (ih.eq.itest .and. jh.eq.jtest) then
                  write(lp,*) 'k,l,po = ',
     &                         k,0,p(ih,jh,k)*qonem,
     &                             pout(ih,jh,k)*qonem
                  call flush(lp)
                endif
              enddo !k
              pout(ih,jh,kkout+1)=depths(ih,jh)
            endif !incoda
          enddo !i
        enddo !j
        if     (ljext) then  !arctic patch
          do i= 1,idm
            ih = idm-mod(i-1,idm)
            do k= 1,kkout+1
              pout(i,jdm,k) = pout(ih,jdm-1,k)
            enddo !k
          enddo !i
        endif !ljext
      endif !pncoda
c
c     remap layers.
c
      p1(0) = 0.0
      pz(0) = 0.0
      do j= 1,jdmn
        jh = j+j1stn-1
        larctic = ljext .and. jh.ge.jdm
        if     (larctic) then
          jh = jdm-1-(jh-jdm)  !p-grid
        endif
        do i= 1,idmp
          ih = mod(i+i1stn-2+idm,idm)+1
          if     (larctic) then
            ih = idm-mod(ih-1,idm)  !p-grid
          endif
          if     (incoda(ih,jh).eq.1) then
            do k= 1,kncoda
              if     (zz(k).le.depths(ih,jh)) then
                t1(k) = tncoda(i,j,k)
                s1(k) = sncoda(i,j,k)
                if     (min(t1(k),s1(k)).lt.-900.0) then
                  write(lp,*)
                  write(lp,*) 'error - NCODA too shallow'
                  write(lp,*) '        i,j,k,zz,depth = ',
     &                                 ih,jh,k,zz(k)*qonem,
     &                                 depths(ih,jh)*qonem
                  write(lp,*)
                  call flush(lp)
                  stop
                endif
                r1(k) =    SIG(R8(t1(k)),R8(s1(k))) - thbase
                rl(k) = SIGLOC(R8(t1(k)),R8(s1(k)),R8(zz(k)))
                if     (rl(k).lt.rl(max(k-1,1))) then  !unstable (k>1)
                  t1(k) = t1(k-1)
                  s1(k) = s1(k-1)
                  r1(k) = r1(k-1)
                endif !unstable to neutral, or more neutral
                if     (flnm_u.ne."NONE") then
                  u1(k) = uncoda(i,j,k)
                  v1(k) = vncoda(i,j,k)
                endif !uvncoda
              else  !inherit from above
                t1(k) = t1(k-1)
                s1(k) = s1(k-1)
                r1(k) = r1(k-1)
                if     (flnm_u.ne."NONE") then
                  u1(k) = 0.0
                  v1(k) = 0.0
                endif !uvncoda
              endif
            enddo
            do k= 1,kkout
              pz(k) = pout(ih,jh,k+1)
              tz(k) = temp(ih,jh,k)  !used below zi range
              sz(k) = saln(ih,jh,k)  !used below zi range
              rz(k) = th3d(ih,jh,k)  !used below zi range
              uz(k) = 0.0            !used below zi range, zero increment
              vz(k) = 0.0            !used below zi range, zero increment
            enddo
            if     (intflg.eq.0) then  !T&S
              call remap_plm_1(t1,zi,kncoda,
     &                         tz,pz,kkout)
              call remap_plm_1(s1,zi,kncoda,
     &                         sz,pz,kkout)
              if     (ih.eq.itest .and. jh.eq.jtest) then
                write(lp,'(a,2i5)') 'remap s1 - i,j =',ih,jh
                call flush(lp)
                call remap_plm_1_debug(s1,zi,kncoda,
     &                                 sz,pz,kkout)
              endif
            elseif (intflg.eq.1) then  !th&S
              call remap_plm_1(r1,zi,kncoda,
     &                         rz,pz,kkout)
              call remap_plm_1(s1,zi,kncoda,
     &                         sz,pz,kkout)
              if     (ih.eq.itest .and. jh.eq.jtest) then
                write(lp,'(a,2i5)') 'remap s1 - i,j =',ih,jh
                call flush(lp)
                call remap_plm_1_debug(s1,zi,kncoda,
     &                                 sz,pz,kkout)
              endif
            else !th&T
              call remap_plm_1(r1,zi,kncoda,
     &                         rz,pz,kkout)
              call remap_plm_1(t1,zi,kncoda,
     &                         tz,pz,kkout)
              if     (ih.eq.itest .and. jh.eq.jtest) then
                write(lp,'(a,2i5)') 'remap t1 - i,j =',ih,jh
                call flush(lp)
                call remap_plm_1_debug(t1,zi,kncoda,
     &                                 tz,pz,kkout)
              endif
            endif !intflg
            if     (flnm_u.ne."NONE") then
              call remap_plm_1(      u1,zi,kncoda,
     &                               uz,pz,kkout)
              call remap_plm_1(      v1,zi,kncoda,
     &                               vz,pz,kkout)
            endif !uvncoda
            if     (isoflg.ne.0) then
              kmld = int(mldlay(ih,jh))
            else
              kmld = kkout+1  !turned off
            endif
            if     (isoflg.eq.3 .and. kmld.lt.kkout) then
              rz(kmld+1) = th3d(ih,jh,kmld+1)
              do k= kmld+2,kkout
                rz(k) = max(th3d(ih,jh,k),rz(k-1))
              enddo !k
c             sample tz at rz isopycnal depths
              if     (ih.eq.itest .and. jh.eq.jtest) then
                write(lp,'(a,2i5)') 'remap tz - i,j =',ih,jh
                call flush(lp)
                call remap_isopyc_debug(t1,r1,kncoda,
     &                            tz(kmld+1),rz(kmld+1),kkout-kmld)
              else
                call remap_isopyc(t1,r1,kncoda,
     &                            tz(kmld+1),rz(kmld+1),kkout-kmld)
              endif
            endif !isoflg==3
            mass_h = 0.d0
            mass_n = 0.d0
            do k= 1,kkout
                  if     (ih.eq.itest .and. jh.eq.jtest) then
                    write(lp,'(a,i3,3f12.6)')
     &                           'k,OLD:t,s,th =',
     &                                      k,
     &                           temp(ih,jh,k),
     &                           saln(ih,jh,k),
     &                           th3d(ih,jh,k)
                    call flush(lp)
                  endif
c
              mass_h = mass_h + dp(ih,jh,k)*th3d(ih,jh,k)
c
              dp(ih,jh,  k  ) = pz(k) - pz(k-1)
              if     (k.gt.kmld+1) then !use existing layer values
                if     (isoflg.lt.2) then
*                 temp(ih,jh,k) = temp(ih,jh,k)
*                 th3d(ih,jh,k) = th3d(ih,jh,k)
*                 saln(ih,jh,k) = saln(ih,jh,k)
                else
                  temp(ih,jh,k) = tz(k)
*                 th3d(ih,jh,k) = th3d(ih,jh,k)
                  saln(ih,jh,k) = SOFSIG(R8(th3d(ih,jh,k)+thbase),
     &                                     R8(tz(k)))
                endif
              elseif (intflg.eq.0) then  !T&S
                saln(ih,jh,k) = sz(k)
                temp(ih,jh,k) = tz(k)
                th3d(ih,jh,k) = SIG(R8(tz(k)),R8(sz(k))) - thbase
              elseif (intflg.eq.1) then  !th&S
                saln(ih,jh,k) = sz(k)
                th3d(ih,jh,k) = rz(k)
                temp(ih,jh,k) = TOFSIG(R8(rz(k)+thbase),R8(sz(k)))
              else !th&T
                temp(ih,jh,k) = tz(k)
                th3d(ih,jh,k) = rz(k)
                saln(ih,jh,k) = SOFSIG(R8(rz(k)+thbase),R8(tz(k)))
              endif !intflg
              mass_n = mass_n + dp(ih,jh,k)*th3d(ih,jh,k)
c
              if     (flnm_u.ne."NONE") then
                uinc(i,j,k) = uz(k)
                vinc(i,j,k) = vz(k)
              endif !uvncoda
c
                  if     (ih.eq.itest .and. jh.eq.jtest) then
                    write(lp,'(a,i3,3f12.6)')
     &                           'k,NEW:t,s,th =',
     &                                      k,
     &                           temp(ih,jh,k),
     &                           saln(ih,jh,k),
     &                           th3d(ih,jh,k)
                    call flush(lp)
                  endif
            enddo !k
            srfht(ih,jh) = srfht(ih,jh) + (mass_h - mass_n)*thref**2
            montg(ih,jh) = montg(ih,jh) + (mass_h - mass_n)*thref**2
            if     (artype.eq.2) then
              do k= 1,kkin
                p1(k) =    p(ih,jh,k+1)
                t1(k) =   ke(ih,jh,k)  !artype==2
              enddo
              do k= 1,kkout
                pz(k) = pout(ih,jh,k+1)
              enddo
              call remap_plm_1(t1,p1,kkin,
     &                         tz,pz,kkout)
              do k= 1,kkout
                  ke(ih,jh,k) = tz(k)  !artype==2
              enddo
            endif  !artype==2
          endif  !ip
        enddo !i
      enddo !j
      if     (ljext) then  !update p-grid arctic halo
        do i= 1,idm
          ih = idm-mod(i-1,idm)
          do k= 1,kkout
              dp(i,jdm,k) =   dp(ih,jdm-1,k)
            saln(i,jdm,k) = saln(ih,jdm-1,k)
            temp(i,jdm,k) = temp(ih,jdm-1,k)
            th3d(i,jdm,k) = th3d(ih,jdm-1,k)
            if     (artype.eq.2) then
              ke(i,jdm,k) = ke(ih,jdm-1,k)
            endif  !artype==2
          enddo !k
          srfht(i,jdm) = srfht(ih,jdm-1)
          montg(i,jdm) = montg(ih,jdm-1)
        enddo !i
      endif !ljext
c
      do j= 1,jdmn
        jh = j+j1stn-1
        sarctic = 1.0  !default
        larctic = ljext .and. jh.ge.jdm
        if     (larctic) then
          jh = jdm-1-(jh-jdm)  !u-grid, flip sign of velocity
          sarctic = -1.0
        endif
        ja = max(jh-1,1)
        do i= 1,idmp
          if     (i.eq.1) then
            if     (idmp.eq.idm) then
              ib = idmp  !NCODA is periodic
            else
              ib = 1
            endif
          else
            ib = i-1
          endif
          ih = mod(i+i1stn-2+idm,idm)+1
          if     (larctic) then
            ih = mod(idm-(ih-1),idm)+1  !u-grid
          endif
          ia = mod(ih-2+idm,idm)+1  !assume periodic
          if     (iu(ih, jh).eq.1) then
            if     (incoda(ih,jh).eq.1 .and.
     &              incoda(ia,jh).eq.1      ) then
              if     (flnm_p.ne."NONE") then
                depthu = min(depths(ih,jh),depths(ia,jh))
                do k= 1,kkin
                  p1(k) = min(depthu,0.5*(p(ih,jh,k+1)+p(ia,jh,k+1)))
                  u1(k) = u(ih,jh,k)
                enddo
                do k= 1,kkout
                  pz(k) = min(depthu,0.5*(pout(ih,jh,k+1)+
     &                                    pout(ia,jh,k+1)))
                enddo
                call remap_plm_1(u1,p1,kkin,
     &                           uz,pz,kkout)
                do k= 1,kkout
                  u(ih,jh,k) = uz(k)
                enddo
              endif !pncoda
              if     (flnm_u.ne."NONE") then
                depthu = min(depths(ih,jh),depths(ia,jh))
                uzero  = 0.0
                do k= 1,kkout
                  pz(k) = min(depthu,0.5*(pout(ih,jh,k+1)+
     &                                    pout(ia,jh,k+1)))
                  uz(k) = 0.5*(uinc(i,j,k)+uinc(ib,j,k))
                  u(ih,jh,k) = u(ih,jh,k) + uz(k)
                  uzero = uzero + u(ih,jh,k)*(pz(k)-pz(k-1))
                enddo
                if     (uzero.ne.0.0) then
                  uzero = uzero / depthu  !this must be moved from u to ubaro
                  ubaro(ih,jh) = ubaro(ih,jh) + uzero
                  do k= 1,kkout
                    u(ih,jh,k) = u(ih,jh,k) - uzero
                  enddo !k
                endif !uzero
              endif !uncoda
            endif !incoda
          endif !iu
        enddo !i
      enddo !j
      if     (ljext) then  !update u-grid arctic halo
        do i= 1,idm
          ih = mod(idm-(i-1),idm)+1
          do k= 1,kkout
            u(i,jdm,k) = -u(ih,jdm-1,k)
          enddo !k
        enddo !i
      endif !ljext
c
      do j= 1,jdmn
        jb = max(j-1, 1)
        jh = j+j1stn-1
        sarctic = 1.0  !default
        larctic = ljext .and. jh.gt.jdm
        if     (larctic) then
          jh = jdm-(jh-jdm)  !v-grid, flip sign of velocity
          sarctic = -1.0
        endif
        ja = max(jh-1,1)
        do i= 1,idmp
          ih = mod(i+i1stn-2+idm,idm)+1
          if     (larctic) then
            ih = idm-mod(ih-1,idm)  !v-grid
          endif
          ia = mod(ih-2+idm,idm)+1  !assume periodic
          if     (iv(ih,jh).eq.1) then
            if     (incoda(ih,jh).eq.1 .and.
     &              incoda(ih,ja).eq.1      ) then
              if     (flnm_p.ne."NONE") then
                depthv = min(depths(ih,jh),depths(ih,ja))
                do k= 1,kkin
                  p1(k) = min(depthv,0.5*(p(ih,jh,k+1)+p(ih,ja,k+1)))
                  v1(k) = sarctic*v(ih,jh,k)
                enddo
                do k= 1,kkout
                  pz(k) = min(depthv,0.5*(pout(ih,jh,k+1)+
     &                                    pout(ih,ja,k+1)))
                enddo
                call remap_plm_1(v1,p1,kkin,
     &                           vz,pz,kkout)
                do k= 1,kkout
                  v(ih,jh,k) = sarctic*vz(k)
                enddo
              endif !pncoda
              if     (flnm_v.ne."NONE") then
                depthv = min(depths(ih,jh),depths(ih,ja))
                vzero  = 0.0
                do k= 1,kkout
                  pz(k) = min(depthv,0.5*(pout(ih,jh,k+1)+
     &                                    pout(ih,ja,k+1)))
                  vz(k) = 0.5*(vinc(i,j,k)+vinc(i,jb,k))
                  v(ih,jh,k) = v(ih,jh,k) + sarctic*vz(k)
                  vzero = vzero + v(ih,jh,k)*(pz(k)-pz(k-1))
                enddo
                if     (vzero.ne.0.0) then
                  vzero = vzero / depthv  !this must be moved from v to vbaro
                  vbaro(ih,jh) = vbaro(ih,jh) + vzero
                  do k= 1,kkout
                    v(ih,jh,k) = v(ih,jh,k) - vzero
                  enddo !k
                endif !vzero
              endif !vncoda
            endif !incoda
          endif !iv
        enddo !i
      enddo !j
      if     (ljext) then  !update v-grid arctic halo
        if     (i1stn.le.idm/2) then
c ---     assume that i1stn:i1stn+idmp-1 is within 1:idm/2
          do i= idm/2+1,idm
            ih = idm-mod(i-1,idm)
            do k= 1,kkout
              v(i,jdm,k) = -v(ih,jdm,k)
            enddo !k
          enddo !i
        else
c ---     assume that i1stn:i1stn+idmp-1 is within idm/2+1:idm
          do i= 1,idm/2
            ih = idm-mod(i-1,idm)
            do k= 1,kkout
              v(i,jdm,k) = -v(ih,jdm,k)
            enddo !k
          enddo !i
        endif
      endif !ljext
c
c --- end of ncoda_cycle loop: deallocate ncoda arrays
      deallocate( tncoda, sncoda, pncoda, mncoda )
      if     (flnm_u.ne."NONE") then
        deallocate( uncoda, vncoda, uinc, vinc )
      endif !flnm_u
c
      enddo !ncoda_cycle
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

      subroutine remap_plm_1(t, p, kk,
     &                       tz,pz,kz)
      implicit none
c
      integer kk,kz
      real    t( kk),p( kk+1),
     &        tz(kz),pz(kz+1)
c
c**********
c*
c  1) remap from one set of vertical cells to another.
c     method: piecewise linear across each input cell
c             the output is the average of the interpolation
c             profile across each output cell.
c
c  2) input arguments:
c       t     - scalar field in p-layer space
c       p     - layer interface depths (non-negative m)
c                 p(   1) is the surface
c                 p(kk+1) is the bathymetry
c       kk    - dimension of t  (number of  input layers)
c       pz    - target interface depths (non-negative m)
c                 pz(k+1) >= pz(k)
c       kz    - dimension of tz (number of output layers)
c
c  3) output arguments:
c       tz    - scalar field in pz-layer space
c
c  4) must have:
c           0 = p(1) <= pz(l) <= pz(l+1)
c           0        <= pz(k) <= pz(k+1)
c      output layer  spaning p(kk+1) uses   input tz below p(kk+1)
c      output layers below   p(kk+1) return input tz unchanged
c
c  5) Tim Campbell, Mississippi State University, October 2002.
c*
c**********
c
      real,parameter :: thin=9806.0e-6  !minimum layer thickness
c
      integer k,l,lf
      real    q,qc,zb,zc,zt,tzk
      real    ts(kk),pt(kk+1)
c
c --- compute PLM slopes for input layers
      do k=1,kk
        pt(k)=max(p(k+1)-p(k),thin)
      enddo
      call plm1(pt, t, ts, kk)
c --- compute output layer averages
      lf=1
      zb=pz(1)
      do k= 1,kz
        zt = zb
        zb = pz(k+1)
*       WRITE(6,*) 'k,zt,zb = ',k,zt,zb
        if     (zb-zt.lt.thin) then
c
c ---     thin layer, values taken from layer above
c
          tz(k) = tz(k-1)
        elseif (zt.ge.p(kk+1)) then
c
c ---     "bottomed" layer, input is unchanged
c
*         tz(k) = tz(k)
        else
c
c         form layer averages.
c
          if     (p(lf).gt.zt) then
            WRITE(6,*) 'bad lf = ',lf
            stop
          endif
          tzk = 0.0
          do l= lf,kk
            if     (p(l).gt.zb) then
*             WRITE(6,*) 'l,lf= ',l,lf,l-1
              lf = l-1
              exit
            elseif (p(l).ge.zt .and. p(l+1).le.zb) then
c
c             the input layer is completely inside the output layer
c
              q   = max(p(l+1)-p(l),thin)/(zb-zt)
              tzk = tzk + q*t(l)
*             WRITE(6,'(a,i5,2f12.7)') 'L,q,t  = ',
*    &                                  l,q,t(l)
            else
c
c             the input layer is partially inside the output layer
c             average of linear profile is its center value
c
              q   = max( min(p(l+1),zb)-max(p(l),zt), thin )/(zb-zt)
              zc  = 0.5*(min(p(l+1),zb)+max(p(l),zt))
              qc  = (zc-p(l))/pt(l) - 0.5
              tzk = tzk + q*(t(l) + qc*ts(l))
*             WRITE(6,'(a,i5,2f12.7)') 'L,q,t* = ',
*    &                                  l,q,(t(l)+qc*ts(l))
            endif
          enddo !l
          if     (zb-p(kk+1).ge.thin) then
c
c             the output layer is partially below the input
c
            q   = (zb-p(kk+1))/(zb-zt)
            tzk = tzk + q*tz(k)
*           WRITE(6,'(a,i5,2f12.7)') 'K,q,tz = ',
*    &                                k,q,tz(k)
          endif
          tz(k) = tzk
        endif
      enddo !k
      return
      end subroutine remap_plm_1

      subroutine remap_plm_1_debug(t, p, kk,
     &                             tz,pz,kz)
      implicit none
c
      integer kk,kz
      real    t( kk),p( kk+1),
     &        tz(kz),pz(kz+1)
c
c**********
c*
c  1) remap from one set of vertical cells to another.
c     method: piecewise linear across each input cell
c             the output is the average of the interpolation
c             profile across each output cell.
c
c  2) input arguments:
c       t     - scalar field in p-layer space
c       p     - layer interface depths (non-negative m)
c                 p(   1) is the surface
c                 p(kk+1) is the bathymetry
c       kk    - dimension of t  (number of  input layers)
c       pz    - target interface depths (non-negative m)
c                 pz(k+1) >= pz(k)
c       kz    - dimension of tz (number of output layers)
c
c  3) output arguments:
c       tz    - scalar field in pz-layer space
c
c  4) must have:
c           0 = p(1) <= pz(l) <= pz(l+1)
c           0        <= pz(k) <= pz(k+1)
c      output layer  spaning p(kk+1) uses   input tz below p(kk+1)
c      output layers below   p(kk+1) return input tz unchanged
c
c  5) Tim Campbell, Mississippi State University, October 2002.
c*
c**********
c
      real,parameter :: thin=9806.0e-6  !minimum layer thickness
c
      integer k,l,lf
      real    q,qc,zb,zc,zt,tzk
      real    ts(kk),pt(kk+1)
c
c --- compute PLM slopes for input layers
      do k=1,kk
        pt(k)=max(p(k+1)-p(k),thin)
      enddo
      call plm1(pt, t, ts, kk)
c --- compute output layer averages
      lf=1
      zb=pz(1)
      do k= 1,kz
        zt = zb
        zb = pz(k+1)
        WRITE(6,*) 'k,zt,zb = ',k,zt,zb
        if     (zb-zt.lt.thin) then
c
c ---     thin layer, values taken from layer above
c
          tz(k) = tz(k-1)
        elseif (zt.ge.p(kk+1)) then
c
c ---     "bottomed" layer, input is unchanged
c
*         tz(k) = tz(k)
        else
c
c         form layer averages.
c
          if     (p(lf).gt.zt) then
            WRITE(6,*) 'bad lf = ',lf
            stop
          endif
          tzk = 0.0
          do l= lf,kk
            if     (p(l).gt.zb) then
              WRITE(6,*) 'l,lf= ',l,lf,l-1
              lf = l-1
              exit
            elseif (p(l).ge.zt .and. p(l+1).le.zb) then
c
c             the input layer is completely inside the output layer
c
              q   = max(p(l+1)-p(l),thin)/(zb-zt)
              tzk = tzk + q*t(l)
              WRITE(6,'(a,i5,2f12.7)') 'L,q,t  = ',
     &                                  l,q,t(l)
            else
c
c             the input layer is partially inside the output layer
c             average of linear profile is its center value
c
              q   = max( min(p(l+1),zb)-max(p(l),zt), thin )/(zb-zt)
              zc  = 0.5*(min(p(l+1),zb)+max(p(l),zt))
              qc  = (zc-p(l))/pt(l) - 0.5
              tzk = tzk + q*(t(l) + qc*ts(l))
              WRITE(6,'(a,i5,2f12.7)') 'L,q,t* = ',
     &                                  l,q,(t(l)+qc*ts(l))
            endif
          enddo !l
          if     (zb-p(kk+1).ge.thin) then
c
c             the output layer is partially below the input
c
            q   = (zb-p(kk+1))/(zb-zt)
            tzk = tzk + q*tz(k)
            WRITE(6,'(a,i5,2f12.7)') 'K,q,tz = ',
     &                                k,q,tz(k)
          endif
          tz(k) = tzk
        endif
      enddo !k
      return
      end subroutine remap_plm_1_debug

      subroutine plm1(pt, t, ts,kk)
      implicit none
c
      integer kk
      real     t(kk),pt(kk),ts(kk)
c
c**********
c*
c  1) generate a monotonic PLM interpolation of a layered field
c
c  2) input arguments:
c       pt    - layer interface thicknesses (non-zero)
c       t     - scalar field in layer space
c       kk    - dimension of a  (number of layers)
c
c  3) output arguments:
c       ts    - scalar field slopes for PLM interpolation
c
c  4) except at data voids, must have:
c           p(   1) == zero (surface)
c           p( l+1) >= p(:,:,l)
c           p(kk+1) == bathymetry
c
c  5) Tim Campbell, Mississippi State University, September 2002.
c*
c**********
c
      integer l
      real    ql(kk),qc(kk),qr(kk)
c
      !compute grid spacing ratios for slope computations
      ql(1)=0.0
      qc(1)=0.0
      qr(1)=0.0
      do l=2,kk-1
        ql(l)=2.0*pt(l)/(pt(l-1)+pt(l))
        qc(l)=2.0*pt(l)/(pt(l-1)+2.0*pt(l)+pt(l+1))
        qr(l)=2.0*pt(l)/(pt(l)+pt(l+1))
      enddo
      ql(kk)=0.0
      qc(kk)=0.0
      qr(kk)=0.0
      !compute normalized layer slopes
      call slope(ql,qc,qr,t,ts,kk)
      return
      end subroutine plm1

      subroutine slope(rl,rc,rr,a,s,n)
      implicit none
c
      integer,intent(in)  :: n
      real,   intent(in)  :: rl(n),rc(n),rr(n),a(n)
      real,   intent(out) :: s(n)
c
c**********
c*
c  1) generate slopes for monotonic piecewise linear distribution
c
c  2) input arguments:
c       rl   - left grid spacing ratio
c       rc   - center grid spacing ratio
c       rr   - right grid spacing ratio
c       a    - scalar field zone averages
c       n    - number of zones
c
c  3) output arguments:
c       s    - zone slopes
c
c  4) Tim Campbell, Mississippi State University, September 2002.
c*
c**********
c
      integer,parameter :: ic=2, im=1, imax=100
      real,parameter :: fracmin=1e-6, dfac=0.5
c
      integer i,j
      real    sl,sc,sr
      real    dnp,dnn,dl,dr,ds,frac
c
c Compute zone slopes
c Campbell Eq(15) -- nonuniform grid
c
      s(1)=0.0
      do j=2,n-1
        sl=rl(j)*(a(j)-a(j-1))
        sr=rr(j)*(a(j+1)-a(j))
        if (sl*sr.gt.0.) then
          s(j)=sign(min(abs(sl),abs(sr)),sl)
        else
          s(j)=0.0
        endif
      enddo
      s(n)=0.0
c
c Minimize discontinuities between zones
c Apply single pass discontinuity minimization: Campbell Eq(19)
c
      do j=2,n-1
        if(s(j).ne.0.0) then
          dl=-0.5*(s(j)+s(j-1))+a(j)-a(j-1)
          dr=-0.5*(s(j+1)+s(j))+a(j+1)-a(j)
          ds=sign(min(abs(dl),abs(dr)),dl)
          s(j)=s(j)+2.0*ds
        endif
      enddo
      return
      end subroutine slope

      subroutine remap_isopyc(t, r, kk,
     &                        tz,rz,kz)
      implicit none
c
      integer kk,kz
      real    t( kk),r( kk),
     &        tz(kz),rz(kz)
c
c**********
c*
c  1) remap from one set of vertical cells to another.
c     method: sample at isopycnal depths
c
c  2) input arguments:
c       t     - scalar  field in p-layer space
c       r     - density field in p-layer space
c       kk    - dimension of t  (number of  input layers)
c       rz    - target densities
c                 rz(k+1) >= rz(k)
c       kz    - dimension of tz (number of output layers)
c
c  3) output arguments:
c       tz    - scalar field in density space
c
c  4) except at data voids, must have:
c           r(1) <= r(l) <= r( l+1)
c      a target density lighter than r(1)  returns tz unchanged
c      a target density heavier than r(kk) returns tz unchanged
c
c  5) Alan Wallcraft, NRL, April 2005.
c*
c**********
c
      real,parameter :: rtiny=0.001  !minimum density change
c
      integer k,l,lf
      real    q
c
      lf = 1
      do k= 1,kz
        if     (r( 1).le.rz(k) .and.
     &          r(kk).ge.rz(k)      ) then
          if     (r(lf).eq.rz(k)) then
            tz(k) = t(l)
          else
            do l= lf,kk
              if     (r(l).ge.rz(k)) then
                q     = (r(l)-rz(k))/max(r(l)-r(l-1),rtiny)
                tz(k) = t(l) - q*(t(l)-t(l-1))
                lf = l
                exit !l
              endif !found target density
            enddo !l
          endif !r(lf)==rz(k)
        endif !rz(k) in range
      enddo !k
      return
      end subroutine remap_isopyc

      subroutine remap_isopyc_debug(t, r, kk,
     &                              tz,rz,kz)
      implicit none
c
      integer kk,kz
      real    t( kk),r( kk),
     &        tz(kz),rz(kz)
c
c**********
c*
c  1) remap from one set of vertical cells to another.
c     method: sample at isopycnal depths
c
c  2) input arguments:
c       t     - scalar  field in p-layer space
c       r     - density field in p-layer space
c       kk    - dimension of t  (number of  input layers)
c       rz    - target densities
c                 rz(k+1) >= rz(k)
c       kz    - dimension of tz (number of output layers)
c
c  3) output arguments:
c       tz    - scalar field in density space
c
c  4) except at data voids, must have:
c           r(1) <= r(l) <= r( l+1)
c      a target density lighter than r(1)  returns tz unchanged
c      a target density heavier than r(kk) returns tz unchanged
c
c  5) Alan Wallcraft, NRL, April 2005.
c*
c**********
c
      real,parameter :: rtiny=0.001  !minimum density change
c
      integer k,l,lf
      real    q
c
      lf = 1
      do k= 1,kz
        if     (r( 1).le.rz(k) .and.
     &          r(kk).ge.rz(k)      ) then
          if     (r(lf).eq.rz(k)) then
            tz(k) = t(lf)
            WRITE(6,*) 'k,lf,rz,t,tz = ',k,lf,rz(k),r(l),tz(k),t(lf)
          else
            WRITE(6,*) 'k,lf= ',k,lf
            do l= lf,kk
              if     (r(l).ge.rz(k)) then
                q     = (r(l)-rz(k))/max(r(l)-r(l-1),rtiny)
                tz(k) = t(l) - q*(t(l)-t(l-1))
                WRITE(6,*) 'k,lm,rz,t,tz = ',k,l-1,rz(k),r(l-1),
     &                                             tz(k),t(l-1),q
                WRITE(6,*) 'k,l, rz,t,tz = ',k,l,  rz(k),r(l),
     &                                             tz(k),t(l)
                lf = l
                exit !l
              endif !found target density
            enddo !l
          endif !r(lf)==rz(k)
        endif !rz(k) in range
      enddo !k
      return
      end subroutine remap_isopyc_debug
