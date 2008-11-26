      subroutine hybgen(m,n)
      use mod_xc    ! HYCOM communication interface
      use mod_pipe  ! HYCOM debugging interface
c
c --- hycom version 1.0
      implicit none
c
      include 'common_blocks.h'
c
      integer m,n
c
c --- ---------------------
c --- hybrid grid generator
c --- ---------------------
c
      logical, parameter :: lpipe_hybgen=.false.  !for debugging
c
      integer   i,j,k,l
      character text*12
c
 103  format (i9,2i5,a/(33x,i3,2f8.3,f8.3,f9.3,f9.2))
cdiag if (itest.gt.0 .and. jtest.gt.0) then
cdiag   write (lp,103) nstep,itest+i0,jtest+j0,
cdiag.  '  entering hybgen:  temp    saln    dens     thkns     dpth',
cdiag.  (k,temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag.  th3d(itest,jtest,k,n)+thbase,dp(itest,jtest,k,n)*qonem,
cdiag.  p(itest,jtest,k+1)*qonem,k=1,kk)
cdiag endif
c
      call xctilr(dpmixl( 1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_ps)
c
      margin = 0  ! no horizontal derivatives
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        call hybgenaj(m,n, j)
      enddo
!$OMP END PARALLEL DO
c
c --- vertical momentum flux across moving interfaces (the s-dot term in the
c --- momentum equation) - required to locally conserve momentum when hybgen
c --- moves vertical coordinates first, store old interface pressures in
c --- -pu-, -pv-
c
!$OMP PARALLEL DO PRIVATE(j,l,i,k)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do l=1,isu(j)
          do i=max(1-margin,ifu(j,l)),min(ii+margin,ilu(j,l))
            pu(i,j,1)=0.0
            pu(i,j,2)=dpu(i,j,1,n)
          enddo
          do k=2,kk
            do i=max(1-margin,ifu(j,l)),min(ii+margin,ilu(j,l))
              pu(i,j,k+1)=pu(i,j,k)+dpu(i,j,k,n)
            enddo
          enddo
        enddo
        do l=1,isv(j)
          do i=max(1-margin,ifv(j,l)),min(ii+margin,ilv(j,l))
            pv(i,j,1)=0.0
            pv(i,j,2)=dpv(i,j,1,n)
          enddo
          do k=2,kk
            do i=max(1-margin,ifv(j,l)),min(ii+margin,ilv(j,l))
              pv(i,j,k+1)=pv(i,j,k)+dpv(i,j,k,n)
            enddo
          enddo
        enddo
      enddo
!$OMP END PARALLEL DO
c
c --- update layer thickness at -u,v- points.
      call dpudpv(dpu(1-nbdy,1-nbdy,1,n),
     &            dpv(1-nbdy,1-nbdy,1,n),
     &            p,depthu,depthv, margin)  ! p's halo extended by dpudpv
c
      if     (lpipe .and. lpipe_hybgen) then
c ---   compare two model runs.
        do k= 1,kk+1
          write (text,'(a9,i3)') 'p      k=',k
          call pipe_compare(p( 1-nbdy,1-nbdy,k),ip,text)
          write (text,'(a9,i3)') 'pu     k=',k
          call pipe_compare(pu(1-nbdy,1-nbdy,k),iu,text)
          write (text,'(a9,i3)') 'pv     k=',k
          call pipe_compare(pv(1-nbdy,1-nbdy,k),iv,text)
        enddo
        do k= 1,kk
          write (text,'(a9,i3)') 'dp     k=',k
          call pipe_compare(dp( 1-nbdy,1-nbdy,k,n),ip,text)
          write (text,'(a9,i3)') 'dpu    k=',k
          call pipe_compare(dpu(1-nbdy,1-nbdy,k,n),iu,text)
          write (text,'(a9,i3)') 'dpv    k=',k
          call pipe_compare(dpv(1-nbdy,1-nbdy,k,n),iv,text)
        enddo
      endif
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        call hybgenbj(m,n, j)
      enddo
!$OMP END PARALLEL DO
c
      return
      end
      subroutine hybgenaj(m,n,j )
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      include 'common_blocks.h'
c
      integer m,n, j
c
c --- --------------------------------------------
c --- hybrid grid generator, single j-row (part A).
c --- --------------------------------------------
c
      logical, parameter :: lunmix=.true.     !unmix a too light deepest layer
      logical, parameter :: lpcm=.false.      !PCM, instead of PLM, remapping
      logical, parameter :: lconserve=.false. !explicitly conserve each column
c
      double precision tsum,ssum,thsum,trsum(mxtrcr),psum,
     &                 q2sum,q2lsum,rpsum
      double precision asum(  mxtrcr+5,3)
      real             offset(mxtrcr+5)
      real ttem(kdm+1,2),tsal(kdm+1,2),tthe(kdm+1,2),
     &     tq2( kdm+1,2),tq2l(kdm+1,2),ttrc(kdm+1,2,mxtrcr),
     &     dprs(kdm+1),pres(kdm+2),
     &     qhrlx( kdm+1), !relaxation coefficient, from qhybrlx
     &     dp0ij( kdm),   !minimum layer thickness
     &     dp0cum(kdm+1)  !minimum interface depth
      real pwidth,p_hat,p_hat0,p_hat2,p_hat3,hybrlx,
     &     delt,deltm,dels,delsm,thnew,q,qtr,qts,thkbop,
     &     qbot,qcen,qtop,zbot,zbox,zcen,ztop,zthk,dpthin
      integer i,k,k1,ka,kbot,kbox,ktop,ksubl,kp,kq,ktr,l,fixlay
ccc   real colint,colins,colout,colous
cdiag      character*12 cinfo
c
      double precision, parameter :: dsmll=1.0d-8
      double precision, parameter ::   zp5=0.5    !for sign function
c
c --- c u s h i o n   function (from Bleck & Benjamin, 1992):
c --- if delp >= qqmx*dp0 >>  dp0, -cushn- returns -delp-
c --- if delp <= qqmn*dp0 << -dp0, -cushn- returns  -dp0-
c
      real       qqmn,qqmx,cusha,cushb
      parameter (qqmn=-4.0, qqmx=2.0)  ! shifted range
*     parameter (qqmn=-2.0, qqmx=4.0)  ! traditional range
*     parameter (qqmn=-4.0, qqmx=6.0)  ! somewhat wider range
      parameter (cusha=qqmn**2*(qqmx-1.0)/(qqmx-qqmn)**2)
      parameter (cushb=1.0/qqmn)
c
      real qq,cushn,delp,dp0
      include 'stmt_fns.h'
      qq(   delp,dp0)=max(qqmn, min(qqmx, delp/dp0))
      cushn(delp,dp0)=dp0*
     &                (1.0+cusha*(1.0-cushb*qq(delp,dp0))**2)*
     &                max(1.0, delp/(dp0*qqmx))
c
      dpthin = 0.001*onemm
      thkbop = thkbot*onem
      hybrlx = 1.0/qhybrlx
c
      do 1 l=1,isp(j)
c
      do 2 i=max(1-margin,ifp(j,l)),min(ii+margin,ilp(j,l))
c
      dp0cum(1)=0.0
      qhrlx( 1)=1.0 !no relaxation in top layer
      dp0ij( 1)=min( dp0k(1), max( ds0k(1), dssk(1)*depths(i,j) ) )
      dp0cum(2)=dp0cum(1)+dp0ij(1)
      qhrlx( 2)=1.0 !no relaxation in top layer
      p(i,j, 2)=p(i,j,1)+dp(i,j,1,n)
      do k=2,kk
c ---   q is dp0k(k) when in surface fixed coordinates
c ---   q is dp00i   when much deeper than surface fixed coordinates
        if     (dp0k(k).le.dp00i) then
          q  =      dp0k(k)
          qts=      0.0     !0 at dp0k
        else
          q  = max( dp00i,
     &              dp0k(k) * dp0k(k)/
     &                        max( dp0k( k),
     &                             p(i,j,k)-dp0cum(k) ) )
          qts= 1.0 - (q-dp00i)/(dp0k(k)-dp00i)  !0 at dp0k, 1 at dp00i
        endif
        qhrlx( k+1)=1.0/(1.0 + qts*(hybrlx-1.0))  !1 at  dp0k, qhybrlx at dp00i
        dp0ij( k)  =min( q,max( ds0k(k), dssk(k)*depths(i,j) ) )
        dp0cum(k+1)=dp0cum(k)+dp0ij(k)
        p(i,j, k+1)=p(i,j,k)+dp(i,j,k,n)
      enddo
c
c --- identify the always-fixed coordinate layers
      fixlay = 1  !layer 1 always fixed
      do k= 2,nhybrd
        if     (dp0cum(k).ge.topiso(i,j)) then
          exit  !layers k to nhybrd can be isopycnal
        endif
        qhrlx(k+1)=1.0  !no relaxation in fixed layers
        fixlay = fixlay+1
      enddo !k
c
cdiag if (itest.gt.0 .and. jtest.gt.0) then
cdiag   write (lp,'(a/(i6,1x,2f8.3,2f9.3,f9.3))')
cdiag.  'hybgen:   thkns  minthk     dpth  mindpth   hybrlx',
cdiag.  (k,dp(itest,jtest,k,n)*qonem,   dp0ij(k)*qonem,
cdiag.      p(itest,jtest,k+1)*qonem,dp0cum(k+1)*qonem,
cdiag.      1.0/qhrlx(k+1),
cdiag.   k=1,kk)
cdiag endif
c
c --- mark the layer containing the mixed layer base by -ksubl- if the
c --- kraus-turner mixed layer model is selected. otherwise set ksubl to -1.
      if     (mxlkta) then
        ksubl=-1
        do k=1,kk
          if (p(i,j,k  ).lt.dpmixl(i,j,n) .and. 
     &        p(i,j,k+1).ge.dpmixl(i,j,n)+onemm) then
            ksubl=k
          endif
        enddo
        klist(i,j)=ksubl
      else
        ksubl=-1
      endif
c
c --- identify the deepest layer kp with significant thickness (> dpthin)
c
      kp = 2  !minimum allowed value
      do k=kk,3,-1
        if (p(i,j,k+1)-p(i,j,k).ge.dpthin) then
          kp=k
          exit
        endif
      enddo
c
c --- massless or near-massless (thickness < dpthin) layers
c
      do k=kp+1,kk
        if (k.le.nhybrd) then
c ---     fill thin and massless layers on sea floor with fluid from above
          th3d(i,j,k,n)=th3d(i,j,k-1,n)
          saln(i,j,k,n)=saln(i,j,k-1,n)
          temp(i,j,k,n)=temp(i,j,k-1,n)
        elseif (th3d(i,j,k,n).ne.theta(i,j,k)) then
          if (hybflg.ne.2) then
c ---       fill with saln from above
            th3d(i,j,k,n)=max(theta(i,j,k), th3d(i,j,k-1,n))
            saln(i,j,k,n)=saln(i,j,k-1,n)
            temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,saln(i,j,k,n))
            saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,temp(i,j,k,n))
          else
c ---       fill with temp from above
            th3d(i,j,k,n)=max(theta(i,j,k), th3d(i,j,k-1,n))
            temp(i,j,k,n)=temp(i,j,k-1,n)
            saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,temp(i,j,k,n))
          endif
        endif
        do ktr= 1,ntracr
          tracer(i,j,k,n,ktr)=tracer(i,j,k-1,n,ktr)
        enddo
        if (mxlmy) then
          q2 (i,j,k,n)=q2( i,j,k-1,n)
          q2l(i,j,k,n)=q2l(i,j,k-1,n)
        endif
      enddo !k
c
      k=kp
c
      if     (lunmix .and. !usually .true.
     &        theta(i,j,k)-epsil.gt.th3d(i,j,k,n) .and.
     &        theta(i,j,k-1)    .lt.th3d(i,j,k,n) .and.
     &        ( th3d(i,j,k,n)- th3d(i,j,k-1,n)).gt.
     &        (theta(i,j,k)  -theta(i,j,k-1)  )*0.001  ) then
c
c ---   water in the deepest inflated layer with significant thickness
c ---   (kp) is too light
c ---
c ---   split layer into 2 sublayers, one near the desired density
c ---   and one exactly matching the T&S properties of layer k-1.
c ---   To prevent "runaway" T or S, the result satisfies either
c ---     abs(T.k - T.k-1) <= abs(T.k-2 - T.k-1) or
c ---     abs(S.k - S.k-1) <= abs(S.k-2 - S.k-1) 
c ---   It is also limited to a 50% change in layer thickness.
c
        delsm=abs(saln(i,j,k-2,n)-saln(i,j,k-1,n))
        dels =abs(saln(i,j,k-1,n)-saln(i,j,k,  n))
        deltm=abs(temp(i,j,k-2,n)-temp(i,j,k-1,n))
        delt =abs(temp(i,j,k-1,n)-temp(i,j,k,  n))
c ---   sanity check on deltm and delsm
        q=min(temp(i,j,k-2,n),temp(i,j,k-1,n),temp(i,j,k,n))
        if     (q.gt. 6.0) then
          deltm=min( deltm,  6.0*(theta(i,j,k)-theta(i,j,k-1)) )
        elseif (q.gt. 0.0) then
          deltm=min( deltm, 10.0*(theta(i,j,k)-theta(i,j,k-1)) )
        else  !(q.le. 0.0)
          deltm=min( deltm, 25.0*(theta(i,j,k)-theta(i,j,k-1)) )
        endif
        delsm=min( delsm, 1.3*(theta(i,j,k)-theta(i,j,k-1)) )
        qts=0.0
        if     (delt.gt.epsil) then
          qts=max(qts, (min(deltm, 2.0*delt)-delt)/delt)  ! qts<=1.0
        endif
        if     (dels.gt.epsil) then
          qts=max(qts, (min(delsm, 2.0*dels)-dels)/dels)  ! qts<=1.0
        endif
        q=(theta(i,j,k)-th3d(i,j,k,  n))/
     &    (theta(i,j,k)-th3d(i,j,k-1,n))
        q=min(q,qts/(1.0+qts))  ! upper sublayer <= 50% of total
        q=qhrlx(k)*q
c ---   qhrlx is relaxation coefficient (inverse baroclinic time steps)
        p_hat=q*(p(i,j,k+1)-p(i,j,k))
        p(i,j,k)=p(i,j,k)+p_hat
        if     (hybflg.eq.0) then  !T&S
          temp(i,j,k,n)=temp(i,j,k,n)+(q/(1.0-q))*(temp(i,j,k,n)  -
     &                                             temp(i,j,k-1,n) )
          saln(i,j,k,n)=saln(i,j,k,n)+(q/(1.0-q))*(saln(i,j,k,n)  -
     &                                             saln(i,j,k-1,n) )
          th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
        elseif (hybflg.eq.1) then  !th&S
          th3d(i,j,k,n)=th3d(i,j,k,n)+(q/(1.0-q))*(th3d(i,j,k,n)  -
     &                                             th3d(i,j,k-1,n) )
          saln(i,j,k,n)=saln(i,j,k,n)+(q/(1.0-q))*(saln(i,j,k,n)  -
     &                                             saln(i,j,k-1,n) )
          temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,saln(i,j,k,n))
        elseif (hybflg.eq.2) then  !th&T
          th3d(i,j,k,n)=th3d(i,j,k,n)+(q/(1.0-q))*(th3d(i,j,k,n)  -
     &                                             th3d(i,j,k-1,n) )
          temp(i,j,k,n)=temp(i,j,k,n)+(q/(1.0-q))*(temp(i,j,k,n)  -
     &                                             temp(i,j,k-1,n) )
          saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,temp(i,j,k,n))
        endif
        if     (ntracr.gt.0 .and. p_hat.ne.0.0) then
          qtr=p_hat/(p(i,j,k)-p(i,j,k-1))  !ok because <1.0 and >0.0
          do ktr= 1,ntracr
            if     (trcflg(ktr).eq.2) then !temperature tracer
              tracer(i,j,k,n,ktr)=tracer(i,j,k,n,ktr)+
     &                           (q/(1.0-q))*(tracer(i,j,k,  n,ktr)-
     &                                        tracer(i,j,k-1,n,ktr))
            else !standard tracer - not split into two sub-layers
              tracer(i,j,k-1,n,ktr)=tracer(i,j,k-1,n,ktr)+
     &                                   qtr*(tracer(i,j,k,  n,ktr)-
     &                                        tracer(i,j,k-1,n,ktr))
cdiag              if (i.eq.itest .and. j.eq.jtest) then
cdiag                write(lp,'(a,i4,i3,5e12.3)')
cdiag     &            'hybgen, 10(+):',
cdiag     &            k,ktr,p_hat,p(i,j,k),p(i,j,k-1),
cdiag     &            qtr,tracer(i,j,k-1,n,ktr)
cdiag                call flush(lp)
cdiag              endif !debug
            endif !trcflg
          enddo !ktr
        endif !tracers
        if (mxlmy) then
          qtr=p_hat/(p(i,j,k)-p(i,j,k-1))  !ok because <1.0 and >0.0
          q2( i,j,k-1,n)=q2( i,j,k-1,n)+
     &                     qtr*(q2( i,j,k,n)-q2( i,j,k-1,n))
          q2l(i,j,k-1,n)=q2l(i,j,k-1,n)+
     &                     qtr*(q2l(i,j,k,n)-q2l(i,j,k-1,n))
        endif
cdiag        if (i.eq.itest .and. j.eq.jtest) then
cdiag          write(lp,'(a,i3,f6.3,5f8.3)')
cdiag     &      'hybgen, 10(+):',
cdiag     &      k,q,temp(i,j,k,n),saln(i,j,k,n),
cdiag     &          th3d(i,j,k,n)+thbase,theta(i,j,k)+thbase
cdiag          call flush(lp)
cdiag        endif !debug
cdiag        if (i.eq.itest .and. j.eq.jtest) then
cdiag          write(lp,'(a,i3,f6.3,5f8.3)')
cdiag     &      'hybgen, 10(-):',
cdiag     &      k,0.0,temp(i,j,k,n),saln(i,j,k,n),
cdiag     &          th3d(i,j,k,n)+thbase,theta(i,j,k)+thbase
cdiag          call flush(lp)
cdiag        endif !debug
      endif !too light
c
c --- store one-dimensional arrays of -temp-, -saln-, and -p- for the 'old'
c --- vertical grid before attempting to restore isopycnic conditions
      pres(1)=p(i,j,1)
      kp=0
      do k=1,kk
        k1=k+kp
        if (k.ne.ksubl) then
          tthe(k1,1)=th3d(i,j,k,n)
          ttem(k1,1)=temp(i,j,k,n)
          tsal(k1,1)=saln(i,j,k,n)
          do ktr= 1,ntracr
            ttrc(k1,1,ktr)=tracer(i,j,k,n,ktr)
          enddo
          if (mxlmy) then
            tq2( k1,1)=q2( i,j,k,n)
            tq2l(k1,1)=q2l(i,j,k,n)
          endif
          pres(k1+1)=p(i,j,k+1)
          dprs(k1)  =pres(k1+1)-pres(k1)
        else				!  k = ksubl
c ---     expand layer into two sublayers, above and below mixed layer base
          tthe(k,1)=tthe(k-1,1)
          ttem(k,1)=ttem(k-1,1)
          tsal(k,1)=tsal(k-1,1)
          pres(k+1)=dpmixl(i,j,n)
          dprs(k)  =pres(k+1)-pres(k)
          q=(p(i,j,k)-dpmixl(i,j,n))/(p(i,j,k+1)-dpmixl(i,j,n))
          tthe(k+1,1)=th3d(i,j,k,n)+q*(tthe(k,1)-th3d(i,j,k,n))
          ttem(k+1,1)=temp(i,j,k,n)+q*(ttem(k,1)-temp(i,j,k,n))
          tsal(k+1,1)=saln(i,j,k,n)+q*(tsal(k,1)-saln(i,j,k,n))
          pres(k+2)=p(i,j,k+1)
          dprs(k+1)=pres(k+2)-pres(k+1)
          do ktr= 1,ntracr
            ttrc(k  ,1,ktr)=ttrc(k-1,1,ktr)
            ttrc(k+1,1,ktr)=tracer(i,j,k,n,ktr)+
     &                      q*(ttrc(k,1,ktr)-tracer(i,j,k,n,ktr))
          enddo
          kp=1
        endif !k.ne.ksubl:else
      enddo !k
c
      if     (lpcm) then  !usually .false.
c
c ---   PCM (zero slope, recovers original hybgen behaviour).
c
        do k= 1,kk+kp
          tthe(k,2)=0.0
          ttem(k,2)=0.0
          tsal(k,2)=0.0
          do ktr= 1,ntracr
            ttrc(k,2,ktr)=0.0
          enddo
          if (mxlmy) then
            tq2( k,2)=0.0
            tq2l(k,2)=0.0
          endif
        enddo
      else
c
c ---   PLM (non-zero slope, but no new extrema)
c ---   layer value is (:,1)-0.5*(:,2) at top    interface,
c ---              and (:,1)+0.5*(:,2) at bottom interface.
c ---   still use PCM for isopycnal layers, because we don't
c ---   want detrainment to change the density of these layers.
c
c ---   monotonized central-difference limiter (van Leer, 1977,
c ---   JCP 23 pp 276-299).  For a discussion of PLM limiters, see
c ---   Finite Volume Methods for Hyperbolic Problems by R.J. Leveque.
c
        do k= 1,kk+kp
          if     (k.eq.1 .or. k.eq.kk+kp .or.
     &            dprs(k).le.dpthin      .or.
     &            dprs(k).gt.dp0kp(k)        ) then
c ---       top, bottom, thin and isopycnal layers have zero slope.
            tthe(k,2)=0.0
            ttem(k,2)=0.0
            tsal(k,2)=0.0
            do ktr= 1,ntracr
              ttrc(k,2,ktr)=0.0
            enddo
            if (mxlmy) then
              tq2( k,2)=0.0
              tq2l(k,2)=0.0
            endif
          else
c ---       interior non-isopycnal layer
c ---       use qcen in place of 0.5 to allow for non-uniform grid
            qcen = dprs(k)/(dprs(k)+0.5*(dprs(k-1)+dprs(k+1)))
c
            ztop = 2.0*(tthe(k,  1)-tthe(k-1,1))
            zbot = 2.0*(tthe(k+1,1)-tthe(k,  1))
            zcen =qcen*(tthe(k+1,1)-tthe(k-1,1))
            if     (ztop*zbot.gt.0.0) then
              tthe(k,2)=sign(min(abs(zcen),abs(zbot),abs(ztop)),zbot)
            else
              tthe(k,2)=0.0  !local extrema, so no slope
            endif
c
            ztop = 2.0*(ttem(k,  1)-ttem(k-1,1))
            zbot = 2.0*(ttem(k+1,1)-ttem(k,  1))
            zcen =qcen*(ttem(k+1,1)-ttem(k-1,1))
            if     (ztop*zbot.gt.0.0) then
              ttem(k,2)=sign(min(abs(zcen),abs(zbot),abs(ztop)),zbot)
            else
              ttem(k,2)=0.0  !local extrema, so no slope
            endif
c
            ztop = 2.0*(tsal(k,  1)-tsal(k-1,1))
            zbot = 2.0*(tsal(k+1,1)-tsal(k,  1))
            zcen =qcen*(tsal(k+1,1)-tsal(k-1,1))
            if     (ztop*zbot.gt.0.0) then
              tsal(k,2)=sign(min(abs(zcen),abs(zbot),abs(ztop)),zbot)
            else
              tsal(k,2)=0.0  !local extrema, so no slope
            endif
c
            do ktr= 1,ntracr
              ztop = 2.0*(ttrc(k,  1,ktr)-ttrc(k-1,1,ktr))
              zbot = 2.0*(ttrc(k+1,1,ktr)-ttrc(k,  1,ktr))
              zcen =qcen*(ttrc(k+1,1,ktr)-ttrc(k-1,1,ktr))
              if     (ztop*zbot.gt.0.0) then
                ttrc(k,2,ktr)=sign(min(abs(zcen),abs(zbot),abs(ztop)),
     &                             zbot)
              else
                ttrc(k,2,ktr)=0.0  !local extrema, so no slope
              endif
            enddo
c
            if (mxlmy) then
              ztop = 2.0*(tq2( k,  1)-tq2( k-1,1))
              zbot = 2.0*(tq2( k+1,1)-tq2( k,  1))
              zcen =qcen*(tq2( k+1,1)-tq2( k-1,1))
              if     (ztop*zbot.gt.0.0) then
                tq2( k,2)=sign(min(abs(zcen),abs(zbot),abs(ztop)),zbot)
              else
                tq2( k,2)=0.0  !local extrema, so no slope
              endif
              ztop = 2.0*(tq2l(k,  1)-tq2l(k-1,1))
              zbot = 2.0*(tq2l(k+1,1)-tq2l(k,  1))
              zcen =qcen*(tq2l(k+1,1)-tq2l(k-1,1))
              if     (ztop*zbot.gt.0.0) then
                tq2l(k,2)=sign(min(abs(zcen),abs(zbot),abs(ztop)),zbot)
              else
                tq2l(k,2)=0.0  !local extrema, so no slope
              endif
            endif !mxlmy
          endif !top/bottom/thin/isopycnal or PLM layer
        enddo !k
      endif !PCM:PLM
c
ccc   colint=temp(i,j,1,n)*(p(i,j,2)-p(i,j,1))
ccc   colins=saln(i,j,1,n)*(p(i,j,2)-p(i,j,1))
c
c --- try to restore isopycnic conditions by moving layer interfaces
c
      do 88 k=2,nhybrd
cdiag            if (i.eq.itest .and. j.eq.jtest) then
cdiag              write(cinfo,'(a9,i2.2,1x)') '  do 88 k=',k
cdiag 109          format (i9,2i5,a,a/(i9,8x,a,a,i3,f9.2,f8.2,f9.2,f8.2))
cdiag              write(lp,109) nstep,itest+i0,jtest+j0,
cdiag     .         cinfo,':    othkns  odpth    nthkns  ndpth',
cdiag     .        (nstep,cinfo,':',k1,
cdiag     .         (pres(k1+1)-
cdiag     .          pres(k1)   )*qonem,
cdiag     .          pres(k1+1)  *qonem,
cdiag     .         (p(itest,jtest,k1+1)-
cdiag     .          p(itest,jtest,k1)   )*qonem,
cdiag     .          p(itest,jtest,k1+1)  *qonem,k1=1,kk)
cdiag              call flush(lp)
cdiag            endif !debug
c
ccc   colint=colint+temp(i,j,k,n)*(p(i,j,k+1)-p(i,j,k))
ccc   colins=colins+saln(i,j,k,n)*(p(i,j,k+1)-p(i,j,k))
c
c --- maintain constant thickness in layers 1 to fixlay
      if (k.le.fixlay+1) then
        p_hat=-999.0*dp0k(k-1)
        go to 9
      end if
c
c --- is density noticeably different from isopycnic reference value?
      if (abs(th3d(i,j,k,n)-theta(i,j,k)).lt.epsil) go to 8
c
      if (th3d(i,j,k,n).le.theta(i,j,k)) go to 7	!  layer too light
c
c --- water in layer k is too dense. try to dilute with water from layer k-1
c
c --- if layer k-1 is too light, perform this adjustment every other pair
c --- of time steps beneath the z-coordinate domain. otherwise, layers
c --- k-1 and k may not be able to reach their target densities
      if (mod(nstep-1,4).le.1 .and.
     &    p(i,j,k).gt.dp0cum(k)+onem .and.
     &    th3d(i,j,k-1,n).lt.theta(i,j,k-1)) go to 8
c
cdiag      if (i.eq.itest .and. j.eq.jtest) then
cdiag        write(lp,'(a,f8.4)') 'hybgen, too dense:',
cdiag     &                       th3d(i,j,k,n)-theta(i,j,k)
cdiag        call flush(lp)
cdiag      endif !debug
c
      if     ((theta(i,j,k)-th3d(i,j,k-1,n)).le.epsil) then
        p_hat=min(p(i,j,k-1),
     &            p(i,j,k)-999.0*dp0k(k-1))  ! take entire layer k-1
      else
        q=(theta(i,j,k)-th3d(i,j,k,n))/(theta(i,j,k)-th3d(i,j,k-1,n))  ! -ve
        p_hat=p(i,j,k)+q*(p(i,j,k+1)-p(i,j,k))
      end if
c
c --- maintain minimum layer thickess of layer k-1.
c
 9    continue
      p_hat0=p_hat
      p_hat=max(p(i,j,k-1)+dp0ij(k-1),
     &      min(p(i,j,k+1)-dp0ij(k),
     &          p(i,j,k-1)+cushn(p_hat0-p(i,j,k-1),dp0ij(k-1)) ))
cdiag      if (i.eq.itest .and. j.eq.jtest) then
cdiag        write(lp,'(a,3f8.2)') 'hybgen, 9: ',
cdiag     &   (p_hat0-p(i,j,k-1))*qonem,
cdiag     &   cushn(p_hat0-p(i,j,k-1),dp0ij(k-1))*qonem,p_hat*qonem
cdiag        call flush(lp)
cdiag      endif !debug
c
c --- if isopycnic conditions cannot be achieved because of a blocking
c --- layer in the interior ocean, move interface k-1 (and k-2 if
c --- necessary) upward
c
      if     (k.le.2) then
c ---   do nothing.
      else if (p_hat.ge.p(i,j,k) .and. 
     &         p(i,j,k-1).gt.dp0cum(k-1)+onem .and.
     &         (p(i,j,kk+1)-p(i,j,k-1).lt.thkbop .or.
     &          p(i,j,k-1) -p(i,j,k-2).gt.qqmx*dp0ij(k-2))) then  ! k.gt.2
        p_hat2=p(i,j,k-2)+cushn(p(i,j,k-1)-p_hat+p_hat0-p(i,j,k-2),
     &                          dp0ij(k-2))
        if (p_hat2.lt.p(i,j,k-1)-onemm) then
          p(i,j,k-1)=(1.0-qhrlx(k-1))*p(i,j,k-1) + 
     &                    qhrlx(k-1) *max(p_hat2,2.0*p(i,j,k-1)-p_hat)
cdiag          if (i.eq.itest .and. j.eq.jtest) then
cdiag            write(lp,'(a,i3.2,f8.2)') 'hybgen,  1blocking :',
cdiag     .                              k-1,p(i,j,k-1)*qonem
cdiag            call flush(lp)
cdiag          endif !debug
          p_hat=p(i,j,k-1)+cushn(p_hat0-p(i,j,k-1),dp0ij(k-1))
        else if (k.le.3) then
c ---     do nothing.
        else if (p(i,j,k-2).gt.dp0cum(k-2)+onem .and.
     &           (p(i,j,kk+1)-p(i,j,k-2).lt.thkbop .or.
     &            p(i,j,k-2) -p(i,j,k-3).gt.qqmx*dp0ij(k-3))) then  ! k.gt.3
          p_hat3=p(i,j,k-3)+cushn(p(i,j,k-2)-p_hat+p_hat0-p(i,j,k-3),
     &                            dp0ij(k-3))
          if (p_hat3.lt.p(i,j,k-2)-onemm) then
            p(i,j,k-2)=(1.0-qhrlx(k-2))*p(i,j,k-2) +
     &                 qhrlx(k-2)*max(p_hat3,2.0*p(i,j,k-2)-p(i,j,k-1))
cdiag            if (i.eq.itest .and. j.eq.jtest) then
cdiag              write(lp,'(a,i3.2,f8.2)') 'hybgen,  2blocking :',
cdiag     .                                k-2,p(i,j,k-2)*qonem
cdiag              call flush(lp)
cdiag            endif !debug
            p_hat2=p(i,j,k-2)+cushn(p(i,j,k-1)-p_hat+p_hat0-p(i,j,k-2),
     &                              dp0ij(k-2))
            if (p_hat2.lt.p(i,j,k-1)-onemm) then
              p(i,j,k-1)=(1.0-qhrlx(k-1))*p(i,j,k-1) +
     &                    qhrlx(k-1) *max(p_hat2,2.0*p(i,j,k-1)-p_hat)
cdiag              if (i.eq.itest .and. j.eq.jtest) then
cdiag                write(lp,'(a,i3.2,f8.2)') 'hybgen,  3blocking :',
cdiag     .                                  k-1,p(i,j,k-1)*qonem
cdiag                call flush(lp)
cdiag              endif !debug
              p_hat=p(i,j,k-1)+cushn(p_hat0-p(i,j,k-1),dp0ij(k-1))
            end if
          end if
        end if
      end if
c
      if (p_hat.le.p(i,j,k)) then
c
c --- upper intfc moves up. entrain layer k-1 water into layer k
c
        q = p_hat - p(i,j,k)
        p(i,j,k)=max(pres(k-1),min(pres(k+1),p(i,j,k)+qhrlx(k)*q))
c ---   qhrlx is relaxation coefficient (inverse baroclinic time steps)
cdiag        if (i.eq.itest .and. j.eq.jtest) then
cdiag          write(lp,'(a,i3.2,28.2)') 'hybgen, entrain(k) :',
cdiag     .                            k,p_hat*qonem,p(i,j,k)*qonem
cdiag          call flush(lp)
cdiag        endif !debug
c
      else				!  p_hat > p(i,j,k)
c
c --- move upper interface down and entrain layer k water into layer k-1
c --- if maintenance of minimum thickness forces the interface below the
c --- bottom, then all interfaces below k must reside at the bottom
c
        p_hat0 = p_hat
        q      = p_hat - p(i,j,k)
        p_hat  = max(pres(k-1),min(pres(k+1),p(i,j,k)+qhrlx(k)*q))
c ---   qhrlx is relaxation coefficient (inverse baroclinic time steps)
        if (p_hat.gt.p(i,j,kk+1)) then
          do ka=k,kk
            p(i,j,k)=p(i,j,kk+1)
          enddo
        else
          p_hat=min(p_hat,p(i,j,k+1))
          p(i,j,k)=p_hat
        endif
cdiag        if (i.eq.itest .and. j.eq.jtest) then
cdiag          write(lp,'(a,i3.2,2f8.2)') 'hybgen, entrain(k-):',
cdiag     .                            k,p_hat0*qonem,p(i,j,k)*qonem
cdiag          call flush(lp)
cdiag        endif !debug
c
c --- do we need to inflate layer k by lowering  l o w e r  interface?
c
        if (k.lt.kk) then
          if (p(i,j,k+2).lt.p(i,j,kk+1)) then
            p_hat=p(i,j,k+1)
            go to 6
          endif
        endif
      endif
      go to 8
c
c --- water in layer k is too light. try to dilute with water from layer k+1
c
 7    continue
c
c --- is layer k touching the sea floor?
      if (p(i,j,k+1).eq.p(i,j,kk+1)) go to 8
c
c --- are we below any KT mixed layer?
      if (mxlkta .and. k.le.klist(i,j)) go to 8
c
c --- if layer k+1 is too dense, perform this adjustment every other pair
c --- of time steps beneath the z-coordinate domain. otherwise, layers
c --- k and k+1 may not be able to reach their target densities
      if (mod(nstep-1,4).ge.2 .and.
     &    p(i,j,k+1).gt.dp0cum(k+1)+onem   .and.
     &    th3d(i,j,k+1,n).gt.theta(i,j,k+1)     ) go to 8
c
cdiag      if (i.eq.itest .and. j.eq.jtest) then
cdiag        write(lp,'(a,f8.4)') 'hybgen, too light:',
cdiag     &                       theta(i,j,k)-th3d(i,j,k,n)
cdiag        call flush(lp)
cdiag      endif !debug
c
      if     ((th3d(i,j,k+1,n)-theta(i,j,k)).le.epsil) then
        p_hat=max(p(i,j,k+2),
     &            p(i,j,k+1)+999.0*dp0k(k))  ! take entire layer k+1
      else
        q=(th3d(i,j,k,n)-theta(i,j,k))/(th3d(i,j,k+1,n)-theta(i,j,k))  ! -ve
        p_hat=p(i,j,k+1)+q*(p(i,j,k)-p(i,j,k+1))
      endif
c
c --- if layer k+1 does not touch the bottom and does not contain the KT
c --- mixed layer base, then maintain minimum thicknesses of layers
c --- k and k+1 to the greatest extent possible, but permit layers to
c --- collapse to zero thickness at the bottom
 6    continue
      if (mxlkta .and. k.eq.klist(i,j)) go to 8
      if     (p(i,j,k+2).lt.p(i,j,kk+1)) then
        if     (p(i,j,kk+1)-p(i,j,k).gt.dp0ij(k)+dp0ij(k+1)) then
          p_hat=p(i,j,k+2)-cushn(p(i,j,k+2)-p_hat,dp0ij(k+1))
        endif
        p_hat=p(i,j,k)  +  max(p_hat    -p(i,j,k),dp0ij(k))
        p_hat=min(p_hat,
     &            max(0.5*(p(i,j,k+1)+p(i,j,k+2)),
     &                     p(i,j,k+2)-dp0ij(k+1)))
      else
        p_hat=min(p(i,j,k+2),p_hat)
      endif !p.k+2<p.kk+1
c
      if (p_hat.gt.p(i,j,k+1)) then
c
c ---   entrain layer k+1 water into layer k. 
        p(i,j,k+1)=(1.0-qhrlx(k+1))*p(i,j,k+1) + qhrlx(k+1)*p_hat
cdiag      if (i.eq.itest .and. j.eq.jtest) then
cdiag        write(lp,'(a,i3.2,f8.2)') 'hybgen, entrain(k+):',
cdiag   .                            k+1,p(i,j,k+1)*qonem
cdiag        call flush(lp)
cdiag      endif !debug
      endif !entrain
 8    continue
c
c --- if layer above is too thin, move interface down.
c --- dp0ij(k-1) is "fixed coordinate" layer thickness.
      p_hat0=p(i,j,k-1)+dp0ij(k-1)
      if (p_hat0.gt.p(i,j,k)   .and.
     &    p_hat0.lt.p(i,j,kk+1)     ) then
        p(i,j,k)=p_hat0
      endif !p_hat0
c
c --- enforce interface order (is this necessary?), usually inexpensive.
      do ka= k+1,kk
        if     (p(i,j,ka).ge.p(i,j,k)) then
          exit  ! usually get here quickly
        else
          p(i,j,ka) = p(i,j,k)
        endif
      enddo !ka
 88   continue !k
c
ccc   write (lp,'(a,1p,2e16.8,e9.1)') 'temp. column integral:',
ccc  .  colint,colout,(colout-colint)/colint
ccc   write (lp,'(a,1p,2e16.8,e9.1)') 'saln. column integral:',
ccc  .  colins,colous,(colous-colins)/colins
c
c --- remap scalar field profiles from the 'old' vertical
c --- grid onto the 'new' vertical grid, using PLM
c
      if     (lconserve) then  !usually .false.
        do ktr=1,ntracr+5
          asum(ktr,1) = 0.d0
          asum(ktr,2) = 0.d0
          asum(ktr,3) = 0.d0
        enddo !ktr
      endif
c
      zbot=0.0
      kbot=1
      do k=1,kk
        ztop=zbot  !top is bottom of previous layer
        ktop=kbot
        if     (ztop.ge.pres(ktop+1)) then
          ktop=ktop+1
        endif
cdiag       if (ktop.eq.kbot .and. i.eq.itest .and. j.eq.jtest) then
cdiag         write(lp,'(a,2i3,2f23.16)')
cdiag&          'k,ktop,dp =',k,ktop,
cdiag&          ztop*qonem,(pres(ktop+1)-ztop)*qonem
cdiag       endif !debug
        zbot=p(i,j,k+1)
        zthk=zbot-ztop
        dp(i,j,k,n)=zthk
cdiag       if (i.eq.itest .and. j.eq.jtest) then
cdiag         write(lp,'(a,2i3,3f23.16)')
cdiag&          'k,z =',k,k,ztop*qonem,zbot*qonem,zthk*qonem
cdiag       endif !debug
        if     (zthk.gt.dpthin .and. ztop.lt.p(i,j,kk+1)) then
c ---     normal layer
          kbot=ktop
          do while (pres(kbot+1).lt.zbot.and.kbot.lt.kk+kp)
            kbot=kbot+1
          enddo
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write(lp,'(a,2i3,2f23.16)')
cdiag&            'k,p =',ktop,kbot,
cdiag&            pres(ktop)*qonem,pres(kbot+1)*qonem
cdiag         endif !debug
c
c ---     include thin adjacent layers in sum
          zbox=zbot
          do k1= k+1,kk
            if     (p(i,j,k1+1)-p(i,j,k1).gt.dpthin) then
              exit !thick layer
            else
              zbox=p(i,j,k1+1)  !include thin adjacent layers
              if     (zbox.eq.p(i,j,kk+1)) then
                exit !at bottom
              endif
            endif
          enddo
          zthk=zbox-ztop
c
          kbox=ktop
          do while (pres(kbox+1).lt.zbox.and.kbox.lt.kk+kp)
            kbox=kbox+1
          enddo
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write(lp,'(a,2i3,2f23.16)')
cdiag&            'k,px=',ktop,kbox,
cdiag&            pres(ktop)*qonem,pres(kbox+1)*qonem
cdiag         endif !debug
          if     (ktop.eq.kbox) then
c ---       single layer
            if     (p(i,j,k)  .ne.pres(kbox)   .or.
     &              p(i,j,k+1).ne.pres(kbox+1)     ) then
c ---         part of a single layer
              zcen = 0.5*(ztop+zbox)
              if     (dprs(kbox).gt.dpthin) then
                q = 0.5 - (pres(kbox+1)-zcen)/dprs(kbox)
              else
                q = 0.0
              endif
              if     (hybflg.eq.0) then  !T&S
                temp(i,j,k,n)=ttem(kbox,1)+q*ttem(kbox,2)
                saln(i,j,k,n)=tsal(kbox,1)+q*tsal(kbox,2)
                th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
              elseif (hybflg.eq.1) then  !th&S
                th3d(i,j,k,n)=tthe(kbox,1)+q*tthe(kbox,2)
                saln(i,j,k,n)=tsal(kbox,1)+q*tsal(kbox,2)
                temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,
     &                               saln(i,j,k,n))
              elseif (hybflg.eq.2) then  !th&T
                th3d(i,j,k,n)=tthe(kbox,1)+q*tthe(kbox,2)
                temp(i,j,k,n)=ttem(kbox,1)+q*ttem(kbox,2)
                saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,
     &                               temp(i,j,k,n))
              endif
              do ktr= 1,ntracr
                tracer(i,j,k,n,ktr)=ttrc(kbox,1,ktr)+q*ttrc(kbox,2,ktr)
              enddo
              if (mxlmy) then
                q2( i,j,k,n)=tq2( kbox,1)+q*tq2( kbox,2)
                q2l(i,j,k,n)=tq2l(kbox,1)+q*tq2l(kbox,2)
              endif
            else
c ---         all of a single layer
              temp(i,j,k,n)=ttem(kbox,1)
              saln(i,j,k,n)=tsal(kbox,1)
              th3d(i,j,k,n)=tthe(kbox,1)
              do ktr= 1,ntracr
                tracer(i,j,k,n,ktr)=ttrc(kbox,1,ktr)
              enddo
              if (mxlmy) then
                q2( i,j,k,n)=tq2( kbox,1)
                q2l(i,j,k,n)=tq2l(kbox,1)
              endif
              q = 0.0 !for debugging only
            endif !part:all of single layer
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write(lp,'(a,i3,3f23.16)')
cdiag&            'k,q =',kbox,q,ttrc(kbox,1,1),tracer(i,j,k,n,1)
*    &            'k,q =',kbox,q,tthe(kbox,1),th3d(i,j,k,n)
cdiag         endif !debug
          else
c ---       multiple layers.
            if     (ktop.le.k .and. kbox.ge.k) then
              ka = k
            elseif (kbox-ktop.ge.3) then
              ka = (kbox+ktop)/2
            elseif (dprs(ktop).ge.dprs(kbox)) then
              ka = ktop
            else
              ka = kbox
            endif !choose ka
c ---       calculate as perturbation from layer ka (reduces roundoff)
            offset(1)=ttem(ka,1)
            offset(2)=tsal(ka,1)
            offset(3)=tthe(ka,1)
            if (mxlmy) then
              offset(4)=tq2( ka,1)
              offset(5)=tq2l(ka,1)
            endif
            do ktr= 1,ntracr
              offset(ktr+5)=ttrc(ka,1,ktr)
            enddo
c
            qtop = pres(ktop+1)-ztop !partial layer thickness
            zcen = 0.5*(ztop+pres(ktop+1))
            if     (dprs(ktop).gt.dpthin) then
              q = 0.5 - (pres(ktop+1)-zcen)/dprs(ktop)
            else
              q = 0.0
            endif
            tsum =((ttem(ktop,1)+q*ttem(ktop,2))-offset(1))*qtop
            ssum =((tsal(ktop,1)+q*tsal(ktop,2))-offset(2))*qtop
            thsum=((tthe(ktop,1)+q*tthe(ktop,2))-offset(3))*qtop
            do ktr= 1,ntracr
              trsum(ktr)=((  ttrc(ktop,1,ktr)+
     &                     q*ttrc(ktop,2,ktr) )-offset(ktr+5))*qtop
            enddo
            if (mxlmy) then
              q2sum =((tq2( ktop,1)+q*tq2( ktop,2))-offset(4))*qtop
              q2lsum=((tq2l(ktop,1)+q*tq2l(ktop,2))-offset(5))*qtop
            endif
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write(lp,'(a,i3,3f23.16)')
cdiag&            'k,f =',ktop,qtop/max(dprs(ktop),dpthin),
cdiag&                         qtop/zthk,trsum(1)/zthk
*    &            'k,f =',ktop,qtop/zthk,ttrc(ktop,1,1),trsum(1)/zthk
*    &            'k,f =',ktop,qtop/zthk,tthe(ktop,1),thsum/zthk
cdiag         endif !debug
c
            do k1= ktop+1,kbox-1
              tsum =tsum +(ttem(k1,1)-offset(1))*dprs(k1)
              ssum =ssum +(tsal(k1,1)-offset(2))*dprs(k1)
              thsum=thsum+(tthe(k1,1)-offset(3))*dprs(k1)
              do ktr= 1,ntracr
                trsum(ktr)=trsum(ktr)+
     &                       (ttrc(k1,1,ktr)-offset(ktr+5))*dprs(k1)
              enddo
              if (mxlmy) then
                q2sum =q2sum +(tq2( k1,1)-offset(4))*dprs(k1)
                q2lsum=q2lsum+(tq2l(k1,1)-offset(5))*dprs(k1)
              endif
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write(lp,'(a,i3,3f23.16)')
cdiag&            'k,f =',k1,1.0,dprs(k1)/zthk,trsum(1)/zthk
*    &            'k,f =',k1,dprs(k1)/zthk,ttrc(k1,1,1),trsum(1)/zthk
*    &            'k,f =',k1,dprs(k1)/zthk,tthe(k1,1),thsum/zthk
cdiag         endif !debug
            enddo !k1
c
            qbot = zbox-pres(kbox) !partial layer thickness
            zcen = 0.5*(pres(kbox)+zbox)
            if     (dprs(kbox).gt.dpthin) then
              q = 0.5 - (pres(kbox+1)-zcen)/dprs(kbox)
            else
              q = 0.0
            endif
            tsum =tsum +((ttem(kbox,1)+q*ttem(kbox,2))-offset(1))*qbot
            ssum =ssum +((tsal(kbox,1)+q*tsal(kbox,2))-offset(2))*qbot
            thsum=thsum+((tthe(kbox,1)+q*tthe(kbox,2))-offset(3))*qbot
            do ktr= 1,ntracr
              trsum(ktr)=trsum(ktr)+
     &                     ((  ttrc(kbox,1,ktr)+
     &                       q*ttrc(kbox,2,ktr) )-offset(ktr+5))*qbot
            enddo
            if (mxlmy) then
              q2sum =q2sum +
     &                 ((tq2( kbox,1)+q*tq2( kbox,2))-offset(4))*qbot
              q2lsum=q2lsum+
     &                 ((tq2l(kbox,1)+q*tq2l(kbox,2))-offset(5))*qbot
            endif
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write(lp,'(a,i3,3f23.16)')
cdiag&            'k,f =',kbox,qbot/max(dprs(kbox),dpthin),
cdiag&                         qbot/zthk,trsum(1)/zthk
*    &            'k,f =',kbox,qbot/zthk,ttrc(kbox,1,1),trsum(1)/zthk
*    &            'k,f =',kbox,qbot/zthk,tthe(kbox,1),thsum/zthk
cdiag         endif !debug
c
            rpsum=1.0d0/zthk
            if     (hybflg.eq.0) then  !T&S
              temp(i,j,k,n)=offset(1)+tsum*rpsum
              saln(i,j,k,n)=offset(2)+ssum*rpsum
              th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
            elseif (hybflg.eq.1) then  !th&S
              th3d(i,j,k,n)=offset(3)+thsum*rpsum
              saln(i,j,k,n)=offset(2)+ ssum*rpsum
              temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,saln(i,j,k,n))
            elseif (hybflg.eq.2) then  !th&T
              th3d(i,j,k,n)=offset(3)+thsum*rpsum
              temp(i,j,k,n)=offset(1)+ tsum*rpsum
              saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,temp(i,j,k,n))
            endif
            do ktr= 1,ntracr
              tracer(i,j,k,n,ktr)=offset(ktr+5)+trsum(ktr)*rpsum
              if     (trcflg(ktr).ne.2) then !not a temperature tracer
c ---           correct for round-off below zero
                tracer(i,j,k,n,ktr)=max(tracer(i,j,k,n,ktr),0.0)
              endif
            enddo
            if (mxlmy) then
              q2( i,j,k,n)=max(dsmll,offset(4)+q2sum *rpsum)
              q2l(i,j,k,n)=max(dsmll,offset(5)+q2lsum*rpsum)
            endif
          endif !single or multiple layers
        else
c ---     thin or bottomed layer
cdiag       if (i.eq.itest .and. j.eq.jtest) then
cdiag         write(lp,'(a,i3)')
cdiag&          'thin k =',k
cdiag       endif !debug
          if (k.le.nhybrd) then
c ---       fill with fluid from above
            th3d(i,j,k,n)=th3d(i,j,k-1,n)
            saln(i,j,k,n)=saln(i,j,k-1,n)
            temp(i,j,k,n)=temp(i,j,k-1,n)
          elseif (hybflg.ne.2) then
c ---       fill with saln from above
            th3d(i,j,k,n)=theta(i,j,k)
            saln(i,j,k,n)=saln(i,j,k-1,n)
            temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,saln(i,j,k,n))
            saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,temp(i,j,k,n))
          else
c ---       fill with temp from above
            th3d(i,j,k,n)=theta(i,j,k)
            temp(i,j,k,n)=temp(i,j,k-1,n)
            saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,temp(i,j,k,n))
          endif
          do ktr= 1,ntracr
            tracer(i,j,k,n,ktr)=tracer(i,j,k-1,n,ktr)
          enddo
          if (mxlmy) then
            q2( i,j,k,n)=q2( i,j,k-1,n)
            q2l(i,j,k,n)=q2l(i,j,k-1,n)
          endif
        endif !normal:thin layer
c
        if     (lconserve) then  !usually .false.
          asum(1,1) = asum(1,1) + ttem(    k,1)*dprs(k)
          asum(1,2) = asum(1,2) + temp(i,j,k,n)*zthk
          asum(2,1) = asum(2,1) + tsal(    k,1)*dprs(k)
          asum(2,2) = asum(2,2) + saln(i,j,k,n)*zthk
          asum(3,1) = asum(3,1) + tthe(    k,1)*dprs(k)
          asum(3,2) = asum(3,2) + th3d(i,j,k,n)*zthk
          if (mxlmy) then
            asum(4,1) = asum(4,1) + tq2(     k,1)*dprs(k)
            asum(4,2) = asum(4,2) +  q2( i,j,k,n)*zthk
            asum(5,1) = asum(5,1) + tq2l(    k,1)*dprs(k)
            asum(5,2) = asum(5,2) +  q2l(i,j,k,n)*zthk
          endif
          do ktr= 1,ntracr
            asum(ktr+5,1) = asum(ktr+5,1) + ttrc(      k,1,ktr)*dprs(k)
            asum(ktr+5,2) = asum(ktr+5,2) + tracer(i,j,k,n,ktr)*zthk
          enddo
        endif !lconserve
c
      enddo !k
c
cdiag if (i.eq.itest .and. j.eq.jtest) then
*       write (lp,'(i9,3a/(i9,3f23.17))')
*    &  nstep,
*    &  '                   dens',
*    &  '                  thkns',
*    &  '                   dpth',
*    &  (k,tthe(k,1),    dprs(k)*qonem,    pres(k+1)*qonem,
*    &   k,th3d(i,j,k,n),dp(i,j,k,n)*qonem,p(i,j,k+1)*qonem,
*    &  k=1,kk)
cdiag   write (lp,'(i9,3a/(i9,3f23.17))')
cdiag&  nstep,
cdiag&  '               tracer.1',
cdiag&  '                  thkns',
cdiag&  '                   dpth',
cdiag&  (k,ttrc(      k,1,1),  dprs(k)*qonem,   pres(k+1)*qonem,
cdiag&   k,tracer(i,j,k,n,1),dp(i,j,k,n)*qonem,p(i,j,k+1)*qonem,
cdiag&  k=1,kk)
cdiag   call flush(lp)
cdiag endif !debug
c
      if     (lconserve) then  !usually .false.
c
c ---   enforce water column conservation
c
        do ktr=1,ntracr+5
          q = asum(ktr,1)-asum(ktr,2)
          if     (q.eq.0.0) then
            offset(ktr) = 0.0
          elseif (abs(asum(ktr,2)).lt.2.0*abs(q)) then
            offset(ktr) = sign(zp5,q*asum(ktr,2))  !        -0.5 or  +0.5
          else
            offset(ktr) =          q/asum(ktr,2)   !between -0.5 and +0.5
          endif
        enddo !ktr
        do k=1,kk
          if     (hybflg.eq.0) then  !T&S
            temp(i,j,k,n)=temp(i,j,k,n)*(1.0+offset(1))
            saln(i,j,k,n)=saln(i,j,k,n)*(1.0+offset(2))
            th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
          elseif (hybflg.eq.1) then  !th&S
            saln(i,j,k,n)=saln(i,j,k,n)*(1.0+offset(2))
            th3d(i,j,k,n)=th3d(i,j,k,n)*(1.0+offset(3))
            temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,
     &                           saln(i,j,k,n))
          elseif (hybflg.eq.2) then  !th&T
            temp(i,j,k,n)=temp(i,j,k,n)*(1.0+offset(1))
            th3d(i,j,k,n)=th3d(i,j,k,n)*(1.0+offset(3))
            saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,
     &                           temp(i,j,k,n))
          endif
          if (mxlmy) then
            q2( i,j,k,n)=q2( i,j,k,n)*(1.0+offset(4))
            q2l(i,j,k,n)=q2l(i,j,k,n)*(1.0+offset(5))
          endif
          do ktr= 1,ntracr
            tracer(i,j,k,n,ktr)=tracer(i,j,k,n,ktr)*(1.0+offset(ktr+5))
          enddo !ktr
c
          if     (.false.) then !debugging
            zthk = dp(i,j,k,n)
            asum(1,3) = asum(1,3) + temp(i,j,k,n)*zthk
            asum(2,3) = asum(2,3) + saln(i,j,k,n)*zthk
            asum(3,3) = asum(3,3) + th3d(i,j,k,n)*zthk
            if (mxlmy) then
              asum(4,3) = asum(4,3) +  q2( i,j,k,n)*zthk
              asum(5,3) = asum(5,3) +  q2l(i,j,k,n)*zthk
            endif
            do ktr= 1,ntracr
              asum(ktr+5,3) = asum(ktr+5,3) + tracer(i,j,k,n,ktr)*zthk
            enddo !ktr
          endif !debuging
        enddo !k
c
        if     (.false. .and. !debugging
     &          i.eq.itest .and. j.eq.jtest) then
          do ktr= 1,ntracr+5
            write(lp,'(a,1p4e16.8,i3)')
     &        'hybgen,sum:',
     &        asum(ktr,1)/p(i,j,kk+1),
     &        asum(ktr,2)/p(i,j,kk+1),
     &        asum(ktr,3)/p(i,j,kk+1),
     &        offset(ktr),ktr
          enddo !ktr
        endif !debugging .and. i.eq.itest .and. j.eq.jtest
        if     (.false. .and. !debugging
     &          j.eq.jtest) then
          ktr=6
*         if     (abs(offset(ktr)).gt.1.e-08) then
          if     (abs(offset(ktr)).gt.1.e-12) then
            write(lp,'(a,1p4e16.8,i3)')
     &        'hybgen,sum:',
     &        asum(ktr,1)/p(i,j,kk+1),
     &        asum(ktr,2)/p(i,j,kk+1),
     &        asum(ktr,3)/p(i,j,kk+1),
     &        offset(ktr),i
          endif !large offset
        endif !debugging .and. j.eq.jtest
      endif !lconserve
c
cdiag if (i.eq.itest .and. j.eq.jtest) then
*       write (lp,'(i9,3a/(i9,3f23.17))')
*    &  nstep,
*    &  '                   dens',
*    &  '                  thkns',
*    &  '                   dpth',
*    &  (k,tthe(k,1),    dprs(k)*qonem,    pres(k+1)*qonem,
*    &   k,th3d(i,j,k,n),dp(i,j,k,n)*qonem,p(i,j,k+1)*qonem,
*    &  k=1,kk)
cdiag   write (lp,'(i9,3a/(i9,3f23.17))')
cdiag&  nstep,
cdiag&  '               tracer.1',
cdiag&  '                  thkns',
cdiag&  '                   dpth',
cdiag&  (k,ttrc(      k,1,1),  dprs(k)*qonem,   pres(k+1)*qonem,
cdiag&   k,tracer(i,j,k,n,1),dp(i,j,k,n)*qonem,p(i,j,k+1)*qonem,
cdiag&  k=1,kk)
cdiag   call flush(lp)
cdiag endif
c
 103  format (i9,2i5,a/(33x,i3,2f8.3,f8.3,f9.3,f9.2))
cdiag if (i.eq.itest .and. j.eq.jtest) then
cdiag   write (lp,103) nstep,itest+i0,jtest+j0,
cdiag&  '    hybgen, do 22:  temp    saln    dens     thkns     dpth',
cdiag&  (k,ttem(k,1),tsal(k,1),tthe(k,1)+thbase,
cdiag&   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag&   k,temp(i,j,k,n),saln(i,j,k,n),
cdiag&   th3d(i,j,k,n)+thbase,dp(i,j,k,n)*qonem,
cdiag&   p(i,j,k+1)*qonem,
cdiag&  k=1,kk),
cdiag&  (k,ttem(k,1),tsal(k,1),tthe(k,1)+thbase,
cdiag&   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag&   k=kk+1,kp)
cdiag   call flush(lp)
cdiag endif !debug
c
 2    continue  !i
c
c --- to avoid roundoff errors in -dpudpv- after restart, make sure p=p(dp)
c
      do 1 k=1,kk
      do 1 i=max(1-margin,ifp(j,l)),min(ii+margin,ilp(j,l))
      p(i,j,k+1)=p(i,j,k)+dp(i,j,k,n)
 1    continue !i;k;l
c
      if(mxlkta) then
        do 71 l=1,isp(j)
        do 71 i=max(1-margin,ifp(j,l)),min(ii+margin,ilp(j,l))
c
        do 71 k=1,kk
        if(dpmixl(i,j,n).gt.p(i,j,k  ) .and.
     &     dpmixl(i,j,n).le.p(i,j,k+1)      ) then
          t1sav(i,j,n)=tmix(i,j)
          s1sav(i,j,n)=smix(i,j)
          tmlb(i,j,n)=temp(i,j,k,n)
          smlb(i,j,n)=saln(i,j,k,n)
          nmlb(i,j,n)=k
        end if
c
 71     continue !k;i;l
      end if
c
      return
      end
      subroutine hybgenbj(m,n, j)
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      include 'common_blocks.h'
c
      integer m,n, j
c
c --- --------------------------------------------
c --- hybrid grid generator, single j-row (part B).
c --- --------------------------------------------
c
      logical, parameter :: lpcm=.false.  !PCM, instead of PLM, remapping
c
      integer i,k,ka,k1,kp,kbot,ktop,ksubl,l
      real    tu(kdm+1,2),tv(kdm+1,2),dprs(kdm+1),pres(kdm+2)
      real    q,thknss,usum,vsum,rpsum,pwidth,
     &        qbot,qcen,qtop,zbot,zcen,ztop,zthk,dpthin
c
c --- vertical momentum flux across moving interfaces (the s-dot term in the
c --- momentum equation) - required to locally conserve momentum when hybgen
c --- moves vertical coordinates.
c
      dpthin = 0.001*onemm
c
      do 412 l=1,isu(j)
      do 412 i=max(1-margin,ifu(j,l)),min(ii+margin,ilu(j,l))
      thknss=max(thkmin*onem,
     &           min(depthu(i,j)-onem,
     &               0.5*(dpmixl(i,j,n)+dpmixl(i-1,j,n))))
c
c --- mark the layer containing the mixed layer base by -ksubl- if the
c --- kraus-turner mixed layer model is selected. otherwise set ksubl to -1.
      if     (mxlkta) then
        ksubl=-1
        do k=1,kk
          if (pu(i,j,k  ).lt.thknss .and.
     &        pu(i,j,k+1).ge.thknss+onemm) then
            ksubl=k
          endif
        enddo
      else
        ksubl=-1
      endif
c
c --- store one-dimensional arrays of -u- and -p- for the 'old' vertical grid
      pres(1)=pu(i,j,1)
      kp=0
      do k=1,kk
        k1=k+kp
        if (k.ne.ksubl) then
          tu(k1,1)=u(i,j,k,n)
          pres(k1+1)=pu(i,j,k+1)
          dprs(k1)  =pres(k1+1)-pres(k1)
        else                              !  k = ksubl
c ---     expand layer into two sublayers, above and below mixed layer base
          tu(k,1)=tu(k-1,1)
          pres(k+1)=thknss
          dprs(k)  =pres(k+1)-pres(k)
          q=(pu(i,j,k)-thknss)/(pu(i,j,k+1)-thknss)
          tu(k+1,1)=u(i,j,k,n)+q*(tu(k,1)-u(i,j,k,n))
          pres(k+2)=pu(i,j,k+1)
          dprs(k+1)=pres(k+2)-pres(k+1)
          kp=1
        endif !k.ne.ksubl:else
      enddo !k
c
      if      (lpcm) then  !usually .false.
c
c ---   PCM (zero slope, recovers original hybgen behaviour).
c
        do k= 1,kk+kp
          tu(k,2)=0.0
        enddo
      else
c
c ---   PLM (non-zero slope, but no new extrema)
c ---   layer value is (:,1)-0.5*(:,2) at top    interface,
c ---              and (:,1)+0.5*(:,2) at bottom interface.
c
c ---   monotonized central-difference limiter (van Leer, 1977,
c ---   JCP 23 pp 276-299).  For a discussion of PLM limiters, see
c ---   Finite Volume Methods for Hyperbolic Problems by R.J. Leveque.
c
        do k= 1,kk+kp
          if     (k.eq.1 .or. k.eq.kk+kp .or.
     &            dprs(k).le.dpthin      .or.
     &            dprs(k).gt.dp0kp(k)        ) then
c ---       top, bottom, thin and isopycnal layers have zero slope.
            tu(k,2)=0.0
          else
c ---       interior non-isopycnal layer
c ---       use qcen in place of 0.5 to allow for non-uniform grid
            qcen = dprs(k)/(dprs(k)+0.5*(dprs(k-1)+dprs(k+1)))
c
            ztop = 2.0*(tu(k,  1)-tu(k-1,1))
            zbot = 2.0*(tu(k+1,1)-tu(k,  1))
            zcen =qcen*(tu(k+1,1)-tu(k-1,1))
            if     (ztop*zbot.gt.0.0) then
              tu(k,2)=sign(min(abs(zcen),abs(zbot),abs(ztop)),zbot)
            else
              tu(k,2)=0.0  !local extrema, so no slope
            endif
          endif !top/bottom/thin/isopycnal or PLM layer
        enddo !k
      endif !PCM:PLM
c
c --- remap -u- profiles from the 'old' vertical grid onto the
c --- 'new' vertical grid, using PLM.
      zbot=0.0
      kbot=1
      do k=1,kk
        ztop=zbot  !top is bottom of previous layer
        ktop=kbot
        if     (ztop.ge.pres(ktop+1)) then
          ktop=ktop+1
        endif
cdiag       if (ktop.eq.kbot .and. i.eq.itest .and. j.eq.jtest) then
cdiag         write(lp,'(a,2i3,f23.16)')
cdiag&          'k,ktop,dp =',k,ktop,(ztop-pres(ktop+1))*qonem
cdiag       endif !debug
        zthk=dpu(i,j,k,n)
        zbot=pu(i,j,k)+zthk
        pu(i,j,k+1)=zbot
        if     (zthk.gt.dpthin .and. ztop.lt.pu(i,j,kk+1)) then
c ---     normal layer
          kbot=ktop
          do while (pres(kbot+1).lt.zbot.and.kbot.lt.kk+kp)
            kbot=kbot+1
          enddo
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write(lp,'(a,2i3,2f23.16)')
cdiag&            'k,z =',k,k,ztop*qonem,zbot*qonem
cdiag           write(lp,'(a,2i3,2f23.16)')
cdiag&            'k,p =',ktop,kbot,
cdiag&            pres(ktop)*qonem,pres(kbot+1)*qonem
cdiag         endif !debug
          if     (ktop.eq.kbot) then
c ---       single layer
            if     (pu(i,j,k)  .ne.pres(kbot)   .or.
     &              pu(i,j,k+1).ne.pres(kbot+1)     ) then
c ---         part of a single layer
              zcen = 0.5*(ztop+zbot)
              if     (dprs(kbot).gt.dpthin) then
                q = 0.5 - (pres(kbot+1)-zcen)/dprs(kbot)
              else
                q = 0.0
              endif
              u(i,j,k,n)=tu(kbot,1)+q*tu(kbot,2)
            endif !part of single layer
          else
c ---       multiple layers.
            if     (ktop.le.k .and. kbot.ge.k) then
              ka = k
            elseif (kbot-ktop.ge.3) then
              ka = (kbot+ktop)/2
            elseif (dprs(ktop).ge.dprs(kbot)) then
              ka = ktop
            else
              ka = kbot
            endif !choose ka
c ---       calculate as perturbation from layer ka (reduces roundoff)
            qtop = pres(ktop+1)-ztop !partial layer thickness
            zcen = 0.5*(ztop+pres(ktop+1))
            if     (dprs(ktop).gt.dpthin) then
              q = 0.5 - (pres(ktop+1)-zcen)/dprs(ktop)
            else
              q = 0.0
            endif
            usum=((tu(ktop,1)+q*tu(ktop,2))-tu(ka,1))*qtop
c
            do k1= ktop+1,kbot-1
              usum=usum + (tu(k1,1)-tu(ka,1))*dprs(k1)
            enddo !k1
c
            qbot = zbot-pres(kbot) !partial layer thickness
            zcen = 0.5*(pres(kbot)+zbot)
            if     (dprs(kbot).gt.dpthin) then
              q = 0.5 - (pres(kbot+1)-zcen)/dprs(kbot)
            else
              q = 0.0
            endif
            usum=usum + ((tu(kbot,1)+q*tu(kbot,2))-tu(ka,1))*qbot
c
            rpsum=1.0d0/zthk
            u(i,j,k,n)=tu(ka,1) + usum*rpsum
          endif !single or multiple layers
*       else
* ---     thin or bottomed layer, do nothing?
        endif !normal:thin layer
      enddo !k
c
 104  format (i9,2i5,a/(33x,i3,f8.3,f9.3,f9.2))
cdiag if (i.eq.itest .and. j.eq.jtest) then
cdiag   write (lp,104) nstep,itest+i0,jtest+j0,
cdiag&  '   hybgen, do 412:  u       thkns     dpth',
cdiag&  (k,tu(k,1),
cdiag&   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag&   k,u(i,j,k,n),
cdiag&   dpu(i,j,k,n)*qonem,pu(i,j,k+1)*qonem,
cdiag&   k=1,kk),
cdiag&  (k,tu(k,1),
cdiag&   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag&   k=kk+1,kp)
cdiag endif !debug
c
 412  continue !l;i
c
      do 512 l=1,isv(j)
      do 512 i=max(1-margin,ifv(j,l)),min(ii+margin,ilv(j,l))
      thknss=max(thkmin*onem,
     &           min(depthv(i,j)-onem,
     &               0.5*(dpmixl(i,j,n)+dpmixl(i,j-1,n))))
c
c --- mark the layer containing the mixed layer base by -ksubl- if the
c --- kraus-turner mixed layer model is selected. otherwise set ksubl to -1.
      if     (mxlkta) then
        ksubl=-1
        do k=1,kk
          if (pv(i,j,k  ).lt.thknss .and.
     &        pv(i,j,k+1).ge.thknss+onemm) then
            ksubl=k
          endif
        enddo
      else
        ksubl=-1
      endif
c
c --- store one-dimensional arrays of -v- and -p- for the 'old' vertical grid
      pres(1)=pv(i,j,1)
      kp=0
      do k=1,kk
        k1=k+kp
        if (k.ne.ksubl) then
          tv(k1,1)=v(i,j,k,n)
          pres(k1+1)=pv(i,j,k+1)
          dprs(k1)  =pres(k1+1)-pres(k1)
        else                              !  k = ksubl
c ---     expand layer into two sublayers, above and below mixed layer base
          tv(k,1)=tv(k-1,1)
          pres(k+1)=thknss
          q=(pv(i,j,k)-thknss)/(pv(i,j,k+1)-thknss)
          tv(k+1,1)=v(i,j,k,n)+q*(tv(k,1)-v(i,j,k,n))
          pres(k+2)=pv(i,j,k+1)
          dprs(k+1)=pres(k+2)-pres(k+1)
          kp=1
        endif !k.ne.ksubl:else
      enddo !k
c
      if      (lpcm) then  !usually .false.
c
c ---   PCM (zero slope, recovers original hybgen behaviour).
c
        do k= 1,kk+kp
          tv(k,2)=0.0
        enddo
      else
c
c ---   PLM (non-zero slope, but no new extrema)
c ---   layer value is (:,1)-0.5*(:,2) at top    interface,
c ---              and (:,1)+0.5*(:,2) at bottom interface.
c
c ---   monotonized central-difference limiter (van Leer, 1977,
c ---   JCP 23 pp 276-299).  For a discussion of PLM limiters, see
c ---   Finite Volume Methods for Hyperbolic Problems by R.J. Leveque.
c
        do k= 1,kk+kp
          if     (k.eq.1 .or. k.eq.kk+kp .or.
     &            dprs(k).le.dpthin      .or.
     &            dprs(k).gt.dp0kp(k)        ) then
c ---       top, bottom, thin and isopycnal layers have zero slope.
            tv(k,2)=0.0
          else
c ---       interior non-isopycnal layer
c ---       use qcen in place of 0.5 to allow for non-uniform grid
            qcen = dprs(k)/(dprs(k)+0.5*(dprs(k-1)+dprs(k+1)))
c
            ztop = 2.0*(tv(k,  1)-tv(k-1,1))
            zbot = 2.0*(tv(k+1,1)-tv(k,  1))
            zcen =qcen*(tv(k+1,1)-tv(k-1,1))
            if     (ztop*zbot.gt.0.0) then
              tv(k,2)=sign(min(abs(zcen),abs(zbot),abs(ztop)),zbot)
            else
              tv(k,2)=0.0  !local extrema, so no slope
            endif
          endif !top/bottom/thin/isopycnal or PLM layer
        enddo !k
      endif !PCM:PLM
c
c --- remap -v- profiles from the 'old' vertical grid onto the
c --- 'new' vertical grid, using PLM.
      zbot=0.0
      kbot=1
      do k=1,kk
        ztop=zbot  !top is bottom of previous layer
        ktop=kbot
        if     (ztop.ge.pres(ktop+1)) then
          ktop=ktop+1
        endif
cdiag       if (ktop.eq.kbot .and. i.eq.itest .and. j.eq.jtest) then
cdiag         write(lp,'(a,2i4,1pe15.6)')
cdiag&          'k,ktop,dp = ',k,ktop,(ztop-pres(ktop+1))*qonem
cdiag       endif !debug
        zthk=dpv(i,j,k,n)
        zbot=pv(i,j,k)+zthk
        pv(i,j,k+1)=zbot
        if     (zthk.gt.dpthin .and. ztop.lt.pv(i,j,kk+1)) then
c ---     normal layer
          kbot=ktop
          do while (pres(kbot+1).lt.zbot.and.kbot.lt.kk+kp)
            kbot=kbot+1
          enddo
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write(lp,'(a,2i4,2f9.3)')
cdiag&            'k,z = ',k,k,ztop*qonem,zbot*qonem
cdiag           write(lp,'(a,2i4,2f9.3)')
cdiag&            'k,p = ',ktop,kbot,
cdiag&            pres(ktop)*qonem,pres(kbot+1)*qonem
cdiag         endif !debug
          if     (ktop.eq.kbot) then
c ---       single layer
            if     (pv(i,j,k)  .ne.pres(kbot)   .or.
     &              pv(i,j,k+1).ne.pres(kbot+1)     ) then
c ---         part of a single layer
              zcen = 0.5*(ztop+zbot)
              if     (dprs(kbot).gt.dpthin) then
                q = 0.5 - (pres(kbot+1)-zcen)/dprs(kbot)
              else
                q = 0.0
              endif
              v(i,j,k,n)=tv(kbot,1)+q*tv(kbot,2)
            endif !part of single layer
          else
c ---       multiple layers.
            if     (ktop.le.k .and. kbot.ge.k) then
              ka = k
            elseif (kbot-ktop.ge.3) then
              ka = (kbot+ktop)/2
            elseif (dprs(ktop).ge.dprs(kbot)) then
              ka = ktop
            else
              ka = kbot
            endif !choose ka
c ---       calculate as perturbation from layer ka (reduces roundoff)
            qtop = pres(ktop+1)-ztop !partial layer thickness
            zcen = 0.5*(ztop+pres(ktop+1))
            if     (dprs(ktop).gt.dpthin) then
              q = 0.5 - (pres(ktop+1)-zcen)/dprs(ktop)
            else
              q = 0.0
            endif
            vsum=((tv(ktop,1)+q*tv(ktop,2))-tv(ka,1))*qtop
c
            do k1= ktop+1,kbot-1
              vsum=vsum + (tv(k1,1)-tv(ka,1))*dprs(k1)
            enddo !k1
c
            qbot = zbot-pres(kbot) !partial layer thickness
            zcen = 0.5*(pres(kbot)+zbot)
            if     (dprs(kbot).gt.dpthin) then
              q = 0.5 - (pres(kbot+1)-zcen)/dprs(kbot)
            else
              q = 0.0
            endif
            vsum=vsum + ((tv(kbot,1)+q*tv(kbot,2))-tv(ka,1))*qbot
c
            rpsum=1.0d0/zthk
            v(i,j,k,n)=tv(ka,1) + vsum*rpsum
          endif !single or multiple layers
*       else
* ---     thin or bottomed layer, do nothing?
        endif !normal:thin layer
c
      enddo !k
c
cdiag if (i.eq.itest .and. j.eq.jtest) then
cdiag   write (lp,104) nstep,itest+i0,jtest+j0,
cdiag&  '   hybgen, do 512:  v       thkns     dpth',
cdiag&  (k,tv(k,1),
cdiag&   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag&   k,v(i,j,k,n),
cdiag&   dpv(i,j,k,n)*qonem,pv(i,j,k+1)*qonem,
cdiag&   k=1,kk),
cdiag&  (k,tv(k,1),
cdiag&   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag&   k=kk+1,kp)
cdiag endif !debug
c
 512  continue !l;i
c
      return
      end
c
c
c> Revision history:
c>
c> Feb. 2000 -- total rewrite to convert to 'newzp' approach
c> Jul. 2000 -- added hybgenj for OpenMP parallelization
c> Oct. 2000 -- added hybgenbj to simplify OpenMP logic
c> Nov. 2000 -- fill massless layers on sea floor with salinity from above
c> Nov. 2000 -- unmixing of deepest inflated layer uses th&T&S from above
c> Nov. 2000 -- ignored isopycnic variance is now 0.002
c> Nov. 2000 -- iterate to correct for cabbeling
c> Nov. 2000 -- allow for "blocking" interior layers
c> Nov. 2000 -- hybflg selects conserved fields (any two of T/S/th)
c> Nov. 2002 -- replace PCM remapping with PLM when non-isopycnal
c> Apr. 2003 -- added dp00i for thinner minimum layers away from the surface
c> Dec. 2003 -- fixed tracer bug when deepest inflated layer is too light
c> Dec. 2003 -- improved water column conservation
c> Dec. 2003 -- compile time option for explicit water column conservation
c> Dec. 2003 -- ignored isopycnic variance is now 0.0001
c> Jan. 2004 -- shifted qqmn,qqmx range now used in cushion function
c> Mar. 2004 -- minimum thickness no longer enforced in near-bottom layers
c> Mar. 2004 -- ignored isopycnic variance is now epsil (i.e. very small)
c> Mar. 2004 -- relaxation to isopycnic layers controled via hybrlx
c> Mar. 2004 -- relaxation removes the need to correct for cabbeling
c> Mar. 2004 -- modified unmixing selection criteria
c> Mar. 2004 -- added isotop (topiso) for isopycnal layer minimum depths
c> Jun. 2005 -- hybrlx (qhybrlx) now input via blkdat.input
c> Jan. 2007 -- hybrlx now only active below "fixed coordinate" surface layers
