      subroutine mixlay(mld,r,p,flag,ii,jj,kk)
      implicit none
c
      integer ii,jj,kk
      real    mld(ii,jj),r(ii,jj,kk),p(ii,jj,kk+1),flag
c
c**********
c*
c  1) calculate the mixed layer depth based on the density difference
c     w.r.t. the surface value.  can also be used for a temperature
c     mixed layer by providing -temp in r.
c
c  2) input arguments:
c       mld   - density (or -temperature) at the mld (> r(:,:,1))
c       r     - density (or -temperature) in layer space
c       p     - layer interface depths (non-negative m)
c                 p(:,:,   1) is the surface
c                 p(:,:,kk+1) is the bathymetry
c       flag  - data void (land) marker
c       ii    - 1st dimension of a,p,az
c       jj    - 2nd dimension of a,p,az
c       kk    - 3rd dimension of a  (number of layers)
c
c  3) output arguments:
c       mld   - mixed layer depth
c
c  4) except at data voids, on input must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c           r(:,:,   1) <= mld(:,:)
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, July 2003.
c*
c**********
c
      logical    ldebug_dpmixl
      parameter (ldebug_dpmixl=.true. )
c
      integer i,j,k
      integer itest,jtest
      real    q,z,zc,zm
c
      itest = 327
      jtest =  29
*     itest = 148
*     jtest = 173
c
c
      do j= 1,jj
        do i= 1,ii
          if     (r(i,j,1).eq.flag) then
            mld(i,j) = flag  ! land
          else
            z = p(i,j,kk+1)
            do k= 2,kk
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (6,'(2i5,i3,a,2f7.3,f9.2)')
     &            i,j,k,
     &            '   r,rmld,zc =',r(i,j,k),mld(i,j),
     &                             0.5*(p(i,j,k)+p(i,j,k+1))
              endif
*               
              if     (r(i,j,k).ge.mld(i,j)) then
c
c               MLD is between the centers of layers k-1 and k
c
                zm = 0.5*(p(i,j,k)+p(i,j,k-1))
                zc = 0.5*(p(i,j,k)+p(i,j,k+1))
                q  = (r(i,j,k)-mld(i,j))/(r(i,j,k)-r(i,j,k-1))
                z  = q*zm + (1.0-q)*zc
                exit
              endif
            enddo !k
            mld(i,j) = z
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine mixlay_loc(mld,temp,saln,p,flag,ii,jj,kk, tmljmp)
      implicit none
c
      integer ii,jj,kk
      real    mld(ii,jj),
     &        temp(ii,jj,kk),saln(ii,jj,kk),p(ii,jj,kk+1),flag,tmljmp
c
c**********
c*
c  1) calculate the mixed layer depth based on the density difference
c     w.r.t. the surface value equivalent to a temperature difference
c     of tmljmp.  uses locally referenced potential density.
c
c  2) input arguments:
c       temp   - temperature in layer space
c       saln   - salinity    in layer space
c       p      - layer interface depths (non-negative m)
c                  p(:,:,   1) is the surface
c                  p(:,:,kk+1) is the bathymetry
c       flag   - data void (land) marker
c       ii     - 1st dimension of a,p,az
c       jj     - 2nd dimension of a,p,az
c       kk     - 3rd dimension of a  (number of layers)
c       tmljmp - data void (land) marker
c
c  3) output arguments:
c       mld    - mixed layer depth
c
c  4) except at data voids, on input must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c           r(:,:,   1) <= mld(:,:)
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, December 2006.
c*
c**********
c
      real       epsil
      parameter (epsil=1.0e-11)
c
c-----------------------------------------------------------------------------
c
c --- sub-coefficients for locally referenced sigma
c --- a fit towards Jackett & McDougall (1995)
      real, parameter, dimension(7) ::
     &  alphap = (/ -0.1364705627213484   , 0.04681812123458564,
     &               0.80700383913187     ,-0.007453530323180844,
     &              -0.002944183249153631 , 0.00003435702568990446,
     &               0.0000348657661057688 /)
     & ,betap  = (/  0.05064226654169138  ,-0.0003571087848996894,
     &              -0.0000876148051892879, 5.252431910751829e-6,
     &               1.579762259448864e-6 ,-3.466867400295792e-8,
     &              -1.687643078774232e-8 /)
     & ,gammap = (/ -5.526396144304812e-6 , 4.885838128243163e-8,
     &               9.96026931578033e-9  ,-7.251389796582352e-10,
     &              -3.987360250058777e-11, 4.006307891935698e-12,
     &               8.26367520608008e-13 /)
c
      real    sigloc,dsiglocdt,dsiglocds
      real    s,t,prs
      real    c1p,c2p,c3p,c4p,c5p,c6p,c7p
c --- locally referenced sigma, a fit towards Jackett & McDougall (1995)
c --- t: potential temperature; s: psu; prs: pressure
      c1p(prs)=alphap(1)+1.e-5*prs*(betap(1)+1.e-5*prs*gammap(1))
      c2p(prs)=alphap(2)+1.e-5*prs*(betap(2)+1.e-5*prs*gammap(2))
      c3p(prs)=alphap(3)+1.e-5*prs*(betap(3)+1.e-5*prs*gammap(3))
      c4p(prs)=alphap(4)+1.e-5*prs*(betap(4)+1.e-5*prs*gammap(4))
      c5p(prs)=alphap(5)+1.e-5*prs*(betap(5)+1.e-5*prs*gammap(5))
      c6p(prs)=alphap(6)+1.e-5*prs*(betap(6)+1.e-5*prs*gammap(6))
      c7p(prs)=alphap(7)+1.e-5*prs*(betap(7)+1.e-5*prs*gammap(7))
      sigloc(t,s,prs)=c1p(prs)+c3p(prs)*s+
     &       t*(c2p(prs)+c5p(prs)*s+t*(c4p(prs)+c7p(prs)*s+c6p(prs)*t))
      dsiglocdt(t,s,prs)=(c2p(prs)+c5p(prs)*s+
     &       2.*t*(c4p(prs)+c7p(prs)*s+1.5*c6p(prs)*t))
      dsiglocds(t,s,prs)=(c3p(prs)+t*(c5p(prs)+t*c7p(prs)))
c-----------------------------------------------------------------------------
c
      logical    ldebug_dpmixl
      parameter (ldebug_dpmixl=.true. )
c
      integer i,j,k
      integer itest,jtest
      real    onem
      real    zgrid(kk+1),thloc(kk),dp(kk),prsk,sigmlj
      REAL    thsur,thtop,thbot,thjmp(kk)
      REAL    alfadt,betads
c
      itest = 327
      jtest =  29
c
      onem = 9806.0
c
      do j= 1,jj
        do i= 1,ii
          if     (temp(i,j,1).eq.flag) then
            mld(i,j) = flag  ! land
          else
            sigmlj = -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0)
            sigmlj = max(sigmlj,tmljmp*0.03)  !cold-water fix
*
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,i3,a,2f7.4)')
     &          i,j,k,
     &          '   sigmlj =',
     &          -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0),
     &          sigmlj
            endif     
*
            thloc(1) = sigloc(temp(i,j,1),saln(i,j,1),0.0)
            zgrid(1) = -0.5*p(i,j,2)
               dp(1) =      p(i,j,2)
            do k= 2,kk
              prsk  = onem*p(i,j,k)
              alfadt=0.5*(dsiglocdt(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocdt(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (temp(i,j,k-1)-temp(i,j,k))
              betads=0.5*(dsiglocds(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocds(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (saln(i,j,k-1)-saln(i,j,k))
              thloc(k) = thloc(k-1)-alfadt-betads
              zgrid(k) = -0.5*(p(i,j,k+1) + p(i,j,k))
                 dp(k) =       p(i,j,k+1) - p(i,j,k)
            enddo !k
            zgrid(kk+1) = -p(i,j,kk+1)
c
            mld(i,j) = -zgrid(kk+1)  !bottom
            thjmp(1) = 0.0
            thsur = thloc(1)
            do k=2,kk
              thsur    = min(thloc(k),thsur)  !ignore surface inversion
              thjmp(k) = max(thloc(k)-thsur,
     &                       thjmp(k-1)) !stable profile simplifies the code
*               
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (6,'(2i5,i3,a,2f7.3,f7.4,f9.2)')
     &            i,j,k,
     &            '   th,thsur,jmp,zc =',
     &            thloc(k),thsur,thjmp(k),-zgrid(k)
              endif
c               
              if (thjmp(k).ge.sigmlj) then
c               
c ---           find the two densities on the interface between layers
c ---           k-1 and k, using PLM assuming layers k-2 and k+1 are PCM.
c
                if     (k.eq.2) then
                  thtop = thjmp(1)
                else
                  thtop = thjmp(k-1) +
     &                      min(thjmp(k-1)-thjmp(k-2),
     &                          thjmp(k)  -thjmp(k-1) )
                endif !k.eq.2:else
                if     (k.eq.kk) then
                  thbot = thjmp(k)
                else
                  thsur      = min(thloc(k+1),thsur)
                  thjmp(k+1) = max(thloc(k+1)-thsur,
     &                             thjmp(k))
                  thbot = thjmp(k) -
     &                      min(thjmp(k+1)-thjmp(k),
     &                          thjmp(k)  -thjmp(k-1) )
                endif !k.eq.kk:else
                if     (thtop.gt.thbot) then
                  thtop = 0.5*(thtop+thbot)  !make stable at interface
                  thbot = thtop
                endif
c                   
                if      (thtop.gt.sigmlj) then
c                 
c ---             in bottom half of layer k-1
c
                  mld(i,j) =
     &              -zgrid(k-1) +
     &                       0.5*dp(k-1)*
     &                       (sigmlj+epsil-thjmp(k-1))/
     &                       (thtop +epsil-thjmp(k-1))
*                 
                if (ldebug_dpmixl .and.
     &              i.eq.itest.and.j.eq.jtest) then
                  write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &              i,j,k,
     &              '   bot half: z,dp,q =',
     &               -zgrid(k-1),
     &                dp(k-1),
     &                   0.5*(sigmlj+epsil-thjmp(k-1))/
     &                       (thtop +epsil-thjmp(k-1))
                endif
*                 
                elseif (thbot.ge.sigmlj) then
c               
c ---             at layer interface
c
                  mld(i,j) =
     &              -zgrid(k-1) + 0.5*dp(k-1)
*                 
                  if (ldebug_dpmixl .and.
     &                i.eq.itest.and.j.eq.jtest) then
                    write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &                i,j,k,
     &                '  interface: z,dp,q =',
     &                 -zgrid(k-1),
     &                  dp(k-1),
     &                  -0.5
                  endif
*                 
                else
c                 
c ---             in top half of layer k
c
                  mld(i,j) =
     &              -zgrid(k) -
     &                       0.5*dp(k)*
     &                       (1.0-(sigmlj  +epsil-thbot)/
     &                            (thjmp(k)+epsil-thbot) )
*                 
                  if (ldebug_dpmixl .and.
     &                i.eq.itest.and.j.eq.jtest) then
                    write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &                i,j,k,
     &                '   top half: z,dp,q =',
     &                 -zgrid(k),
     &                  dp(k),
     &                 -0.5*(1.0-(sigmlj  +epsil-thbot)/
     &                           (thjmp(k)+epsil-thbot) )
                  endif
*                 
                endif !part of layer
*                 
                if (ldebug_dpmixl .and.
     &              i.eq.itest.and.j.eq.jtest) then
                  write (6,'(2i5,i3,a,2f7.3,f7.4,f9.2)')
     &              i,j,k,
     &              '   thsur,top,bot,dpmixl =',
     &              thsur,thtop,thbot,mld(i,j)
                endif
*                 
                exit  !calculated dpmixl
              endif  !found dpmixl layer
            enddo !k
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine mixlay_locppm(mld,temp,saln,p,flag,ii,jj,kk, tmljmp)
      implicit none
c
      integer ii,jj,kk
      real    mld(ii,jj),
     &        temp(ii,jj,kk),saln(ii,jj,kk),p(ii,jj,kk+1),flag,tmljmp
c
c**********
c*
c  1) calculate the mixed layer depth based on the density difference
c     w.r.t. the surface value equivalent to a temperature difference
c     of tmljmp.  uses locally referenced potential density.
c
c  2) input arguments:
c       temp   - temperature in layer space
c       saln   - salinity    in layer space
c       p      - layer interface depths (non-negative m)
c                  p(:,:,   1) is the surface
c                  p(:,:,kk+1) is the bathymetry
c       flag   - data void (land) marker
c       ii     - 1st dimension of a,p,az
c       jj     - 2nd dimension of a,p,az
c       kk     - 3rd dimension of a  (number of layers)
c       tmljmp - data void (land) marker
c
c  3) output arguments:
c       mld    - mixed layer depth
c
c  4) except at data voids, on input must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c           r(:,:,   1) <= mld(:,:)
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, December 2006.
c*
c**********
c
      real       epsil
      parameter (epsil=1.0e-11)
c
c-----------------------------------------------------------------------------
c
c --- sub-coefficients for locally referenced sigma
c --- a fit towards Jackett & McDougall (1995)
      real, parameter, dimension(7) ::
     &  alphap = (/ -0.1364705627213484   , 0.04681812123458564,
     &               0.80700383913187     ,-0.007453530323180844,
     &              -0.002944183249153631 , 0.00003435702568990446,
     &               0.0000348657661057688 /)
     & ,betap  = (/  0.05064226654169138  ,-0.0003571087848996894,
     &              -0.0000876148051892879, 5.252431910751829e-6,
     &               1.579762259448864e-6 ,-3.466867400295792e-8,
     &              -1.687643078774232e-8 /)
     & ,gammap = (/ -5.526396144304812e-6 , 4.885838128243163e-8,
     &               9.96026931578033e-9  ,-7.251389796582352e-10,
     &              -3.987360250058777e-11, 4.006307891935698e-12,
     &               8.26367520608008e-13 /)
c
      real    sigloc,dsiglocdt,dsiglocds
      real    s,t,prs
      real    c1p,c2p,c3p,c4p,c5p,c6p,c7p
c --- locally referenced sigma, a fit towards Jackett & McDougall (1995)
c --- t: potential temperature; s: psu; prs: pressure
      c1p(prs)=alphap(1)+1.e-5*prs*(betap(1)+1.e-5*prs*gammap(1))
      c2p(prs)=alphap(2)+1.e-5*prs*(betap(2)+1.e-5*prs*gammap(2))
      c3p(prs)=alphap(3)+1.e-5*prs*(betap(3)+1.e-5*prs*gammap(3))
      c4p(prs)=alphap(4)+1.e-5*prs*(betap(4)+1.e-5*prs*gammap(4))
      c5p(prs)=alphap(5)+1.e-5*prs*(betap(5)+1.e-5*prs*gammap(5))
      c6p(prs)=alphap(6)+1.e-5*prs*(betap(6)+1.e-5*prs*gammap(6))
      c7p(prs)=alphap(7)+1.e-5*prs*(betap(7)+1.e-5*prs*gammap(7))
      sigloc(t,s,prs)=c1p(prs)+c3p(prs)*s+
     &       t*(c2p(prs)+c5p(prs)*s+t*(c4p(prs)+c7p(prs)*s+c6p(prs)*t))
      dsiglocdt(t,s,prs)=(c2p(prs)+c5p(prs)*s+
     &       2.*t*(c4p(prs)+c7p(prs)*s+1.5*c6p(prs)*t))
      dsiglocds(t,s,prs)=(c3p(prs)+t*(c5p(prs)+t*c7p(prs)))
c-----------------------------------------------------------------------------
c
      logical    ldebug_dpmixl
      parameter (ldebug_dpmixl=.true. )
c
      integer i,j,k
      integer itest,jtest
      real    onem
      real    zgrid(kk+1),thloc(kk),dp(kk),prsk,sigmlj,z
      REAL    thsur,thtop,thjmp(kk)
      REAL    alfadt,betads
c
      itest = 327
      jtest =  29
c
      onem  = 9806.0  !pressure units
c
      do j= 1,jj
        do i= 1,ii
          if     (temp(i,j,1).eq.flag) then
            mld(i,j) = flag  ! land
          else
            sigmlj = -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0)
            sigmlj = max(sigmlj,tmljmp*0.03)  !cold-water fix
*
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,i3,a,2f7.4)')
     &          i,j,k,
     &          '   sigmlj =',
     &          -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0),
     &          sigmlj
            endif     
*
            thloc(1) = sigloc(temp(i,j,1),saln(i,j,1),0.0)
            zgrid(1) = -0.5*p(i,j,2)
               dp(1) =      p(i,j,2)
            do k= 2,kk
              prsk  = onem*p(i,j,k)
              alfadt=0.5*(dsiglocdt(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocdt(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (temp(i,j,k-1)-temp(i,j,k))
              betads=0.5*(dsiglocds(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocds(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (saln(i,j,k-1)-saln(i,j,k))
              thloc(k) = thloc(k-1)-alfadt-betads
              zgrid(k) = -0.5*(p(i,j,k+1) + p(i,j,k))
                 dp(k) =       p(i,j,k+1) - p(i,j,k)
              zgrid(k) = min( zgrid(k), zgrid(k-1) - 0.001 ) !negative
                 dp(k) = max(    dp(k), 0.001 )
            enddo !k
            zgrid(kk+1) = -p(i,j,kk+1)
c
            mld(i,j) = -zgrid(kk+1)  !bottom
            thjmp(1) = 0.0
            thsur = thloc(1)
            do k=2,kk
              thsur    = min(thloc(k),thsur)  !ignore surface inversion
              thjmp(k) = max(thloc(k)-thsur,
     &                       thjmp(k-1)) !stable profile simplifies the code
*               
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (6,'(2i5,i3,a,2f7.3,f7.4,f9.2)')
     &            i,j,k,
     &            '   th,thsur,jmp,zc =',
     &            thloc(k),thsur,thjmp(k),-zgrid(k)
              endif
c               
              if (thjmp(k).ge.sigmlj) then
c
c ---           find the density on the interface between layers
c ---           k-1 and k, using the same cubic polynominal as PQM
c
                if     (k.eq.2) then
c ---             linear between cell centers
                  thtop = thjmp(1) + (thjmp(2)-thjmp(1))*
     &                               dp(1)/(dp(1)+dp(2))
                elseif (k.eq.kk) then
c ---             linear between cell centers
                  thtop = thjmp(kk) + (thjmp(kk-1)-thjmp(kk))*
     &                                 dp(kk)/(dp(kk)+dp(kk-1))
                else
                  thsur      = min(thloc(k+1),thsur)
                  thjmp(k+1) = max(thloc(k+1)-thsur,
     &                             thjmp(k))
                  z     = zgrid(k-1) - 0.5*dp(k-1)
                  thtop = thjmp(k-2)*((z        -zgrid(k-1))*
     &                                (z        -zgrid(k  ))*
     &                                (z        -zgrid(k+1)) )/
     &                               ((zgrid(k-2)-zgrid(k-1))*
     &                                (zgrid(k-2)-zgrid(k  ))*
     &                                (zgrid(k-2)-zgrid(k+1)) ) +
     &                    thjmp(k-1)*((z        -zgrid(k-2))*
     &                                (z        -zgrid(k  ))*
     &                                (z        -zgrid(k+1)) )/
     &                               ((zgrid(k-1)-zgrid(k-2))*
     &                                (zgrid(k-1)-zgrid(k  ))*
     &                                (zgrid(k-1)-zgrid(k+1)) ) +
     &                    thjmp(k  )*((z        -zgrid(k-2))*
     &                                (z        -zgrid(k-1))*
     &                                (z        -zgrid(k+1)) )/
     &                               ((zgrid(k  )-zgrid(k-2))*
     &                                (zgrid(k  )-zgrid(k-1))*
     &                                (zgrid(k  )-zgrid(k+1)) ) +
     &                    thjmp(k+1)*((z        -zgrid(k-2))*
     &                                (z        -zgrid(k-1))*
     &                                (z        -zgrid(k  )) )/
     &                               ((zgrid(k+1)-zgrid(k-2))*
     &                                (zgrid(k+1)-zgrid(k-1))*
     &                                (zgrid(k+1)-zgrid(k  )) )
                  thtop = max( thjmp(k-1), min( thjmp(k), thtop ) )
                endif !k.eq.2:k.eq.kk:else
c                   
                if      (thtop.ge.sigmlj) then
c                 
c ---             in bottom half of layer k-1, use linear interpolation
c
                  mld(i,j) =
     &              -zgrid(k-1) +
     &                       0.5*dp(k-1)*
     &                       (sigmlj+epsil-thjmp(k-1))/
     &                       (thtop +epsil-thjmp(k-1))
*                 
                if (ldebug_dpmixl .and.
     &              i.eq.itest.and.j.eq.jtest) then
                  write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &              i,j,k,
     &              '   bot half: z,dp,q =',
     &               -zgrid(k-1),
     &                dp(k-1),
     &                   0.5*(sigmlj+epsil-thjmp(k-1))/
     &                       (thtop +epsil-thjmp(k-1))
                endif
*                 
                else
c                 
c ---             in top half of layer k,  use linear interpolation
c
                  mld(i,j) =
     &              -zgrid(k) -
     &                       0.5*dp(k)*
     &                       (1.0-(sigmlj  +epsil-thtop)/
     &                            (thjmp(k)+epsil-thtop) )
*                 
                  if (ldebug_dpmixl .and.
     &                i.eq.itest.and.j.eq.jtest) then
                    write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &                i,j,k,
     &                '   top half: z,dp,q =',
     &                 -zgrid(k),
     &                  dp(k),
     &                 -0.5*(1.0-(sigmlj  +epsil-thtop)/
     &                           (thjmp(k)+epsil-thtop) )
                  endif
*                 
                endif !part of layer
*                 
                if (ldebug_dpmixl .and.
     &              i.eq.itest.and.j.eq.jtest) then
                  write (6,'(2i5,i3,a,f7.3,f7.4,f9.2)')
     &              i,j,k,
     &              '   thsur,top,dpmixl =',
     &              thsur,thtop,mld(i,j)
                endif
*                 
                exit  !calculated dpmixl
              endif  !found dpmixl layer
            enddo !k
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine mixlay_loclcc(mld,temp,saln,p,flag,ii,jj,kk, tmljmp)
      implicit none
c
      integer ii,jj,kk
      real    mld(ii,jj),
     &        temp(ii,jj,kk),saln(ii,jj,kk),p(ii,jj,kk+1),flag,tmljmp
c
c**********
c*
c  1) calculate the mixed layer depth based on the density difference
c     w.r.t. the surface value equivalent to a temperature difference
c     of tmljmp.  uses locally referenced potential density.
c     LCC (linear between cell centers) version.
c
c  2) input arguments:
c       temp   - temperature in layer space
c       saln   - salinity    in layer space
c       p      - layer interface depths (non-negative m)
c                  p(:,:,   1) is the surface
c                  p(:,:,kk+1) is the bathymetry
c       flag   - data void (land) marker
c       ii     - 1st dimension of a,p,az
c       jj     - 2nd dimension of a,p,az
c       kk     - 3rd dimension of a  (number of layers)
c       tmljmp - data void (land) marker
c
c  3) output arguments:
c       mld    - mixed layer depth
c
c  4) except at data voids, on input must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c           r(:,:,   1) <= mld(:,:)
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, December 2006.
c*
c**********
c
      real       epsil
      parameter (epsil=1.0e-11)
c
c-----------------------------------------------------------------------------
c
c --- sub-coefficients for locally referenced sigma
c --- a fit towards Jackett & McDougall (1995)
      real, parameter, dimension(7) ::
     &  alphap = (/ -0.1364705627213484   , 0.04681812123458564,
     &               0.80700383913187     ,-0.007453530323180844,
     &              -0.002944183249153631 , 0.00003435702568990446,
     &               0.0000348657661057688 /)
     & ,betap  = (/  0.05064226654169138  ,-0.0003571087848996894,
     &              -0.0000876148051892879, 5.252431910751829e-6,
     &               1.579762259448864e-6 ,-3.466867400295792e-8,
     &              -1.687643078774232e-8 /)
     & ,gammap = (/ -5.526396144304812e-6 , 4.885838128243163e-8,
     &               9.96026931578033e-9  ,-7.251389796582352e-10,
     &              -3.987360250058777e-11, 4.006307891935698e-12,
     &               8.26367520608008e-13 /)
c
      real    sigloc,dsiglocdt,dsiglocds
      real    s,t,prs
      real    c1p,c2p,c3p,c4p,c5p,c6p,c7p
c --- locally referenced sigma, a fit towards Jackett & McDougall (1995)
c --- t: potential temperature; s: psu; prs: pressure
      c1p(prs)=alphap(1)+1.e-5*prs*(betap(1)+1.e-5*prs*gammap(1))
      c2p(prs)=alphap(2)+1.e-5*prs*(betap(2)+1.e-5*prs*gammap(2))
      c3p(prs)=alphap(3)+1.e-5*prs*(betap(3)+1.e-5*prs*gammap(3))
      c4p(prs)=alphap(4)+1.e-5*prs*(betap(4)+1.e-5*prs*gammap(4))
      c5p(prs)=alphap(5)+1.e-5*prs*(betap(5)+1.e-5*prs*gammap(5))
      c6p(prs)=alphap(6)+1.e-5*prs*(betap(6)+1.e-5*prs*gammap(6))
      c7p(prs)=alphap(7)+1.e-5*prs*(betap(7)+1.e-5*prs*gammap(7))
      sigloc(t,s,prs)=c1p(prs)+c3p(prs)*s+
     &       t*(c2p(prs)+c5p(prs)*s+t*(c4p(prs)+c7p(prs)*s+c6p(prs)*t))
      dsiglocdt(t,s,prs)=(c2p(prs)+c5p(prs)*s+
     &       2.*t*(c4p(prs)+c7p(prs)*s+1.5*c6p(prs)*t))
      dsiglocds(t,s,prs)=(c3p(prs)+t*(c5p(prs)+t*c7p(prs)))
c-----------------------------------------------------------------------------
c
      logical    ldebug_dpmixl
      parameter (ldebug_dpmixl=.true. )
c
      integer i,j,k
      integer itest,jtest
      real    onem
      real    zgrid(kk+1),thloc(kk),dp(kk),prsk,sigmlj
      REAL    thsur,thtop,thbot,thjmp(kk)
      REAL    alfadt,betads
c
      itest = 327
      jtest =  29
c
      onem = 9806.0
c
      do j= 1,jj
        do i= 1,ii
          if     (temp(i,j,1).eq.flag) then
            mld(i,j) = flag  ! land
          else
            sigmlj = -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0)
            sigmlj = max(sigmlj,tmljmp*0.03)  !cold-water fix
*
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,i3,a,2f7.4)')
     &          i,j,k,
     &          '   sigmlj =',
     &          -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0),
     &          sigmlj
            endif     
*
            thloc(1) = sigloc(temp(i,j,1),saln(i,j,1),0.0)
            zgrid(1) = -0.5*p(i,j,2)
               dp(1) =      p(i,j,2)
            do k= 2,kk
              prsk  = onem*p(i,j,k)
              alfadt=0.5*(dsiglocdt(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocdt(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (temp(i,j,k-1)-temp(i,j,k))
              betads=0.5*(dsiglocds(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocds(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (saln(i,j,k-1)-saln(i,j,k))
              thloc(k) = thloc(k-1)-alfadt-betads
              zgrid(k) = -0.5*(p(i,j,k+1) + p(i,j,k))
                 dp(k) =       p(i,j,k+1) - p(i,j,k)
            enddo !k
            zgrid(kk+1) = -p(i,j,kk+1)
c
            mld(i,j) = -zgrid(kk+1)  !bottom
            thjmp(1) = 0.0
            thsur = thloc(1)
            do k=2,kk
              thsur    = min(thloc(k),thsur)  !ignore surface inversion
              thjmp(k) = max(thloc(k)-thsur,
     &                       thjmp(k-1)) !stable profile simplifies the code
*               
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (6,'(2i5,i3,a,2f7.3,f7.4,f9.2)')
     &            i,j,k,
     &            '   th,thsur,jmp,zc =',
     &            thloc(k),thsur,thjmp(k),-zgrid(k)
              endif
c               
              if (thjmp(k).ge.sigmlj) then
c                 
c ---           Between the centers of layers k-1 and k
c
                mld(i,j) =
     &              -zgrid(k-1) +
     &                       0.5*(dp(k-1)+dp(k))*
     &                       (sigmlj        -thjmp(k-1))/
     &                       (thjmp(k)+epsil-thjmp(k-1))
*                 
                if (ldebug_dpmixl .and.
     &              i.eq.itest.and.j.eq.jtest) then
                  write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &              i,j,k,
     &              '   cell cen: z,dp,q =',
     &               -zgrid(k-1),
     &                0.5*(dp(k-1)+dp(k)),
     &                (sigmlj        -thjmp(k-1))/
     &                (thjmp(k)+epsil-thjmp(k-1))
                  write (6,'(2i5,i3,a,f7.3,f9.2)')
     &              i,j,k,
     &              '       thsur,dpmixl =',
     &              thsur,mld(i,j)
                endif
*                 
                exit  !calculated dpmixl
              endif  !found dpmixl layer
            enddo !k
          endif
        enddo !i
      enddo !j
      return
      end
