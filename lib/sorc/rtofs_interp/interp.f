      module mod_interp

      implicit none

      CONTAINS
C
C***********************************************************************
C
      SUBROUTINE INTP_INI(
     >IM1,JM1,IM2,JM2,lon1,lat1,LON2,LAT2,mask1,MASK2,
     >IG1,JG1,ierr,
     >ia,ja,ib,jb,ic,jc,KM1,ibndr1,jbndr1,
     >xdlon1,xdlat1,XDLON2,XDLAT2,COSANG,SINANG,
     >nvrbs,iiunit)
C       

C     Calculate IG1( , ),JG1( , ). By definition, for each G2 sea
C     gridpoint (i,j) (MASK2(i,j)=0), whose coordinates are
C     (LON2(i,j),LAT2(i,j)),
C     ( IG1(i,j),JG1(i,j) ) = (i1,j1)
C     is the point on G1 grid for which (i,j) lies in the quadrangle
C     whose vertices are *valid* (see definition of validity below) G1
C     gridpoints (i1,j1), (i1+1,j1), (i1+1,j1+1), (i1,j1+1) if these
C     vertices are not all land points; if they are all land points,
C     IG1(i,j) is set to 0.
C        A G1 gridpoint (i,j) is considered *valid* if it is not flagged
C        with mask1(i,j)=-1 value. (Such are gridpoints where no valid
C        coordinate values are available.)
C           If ia,ja,ib,jb,ic,jc are present, the 4 vertices are,
C           instead of the above, (i1,j1), (ia(i1,j1),ja(i1,j1)),
C           (ib(i1,j1),jb(i1,j1)), (ic(i1,j1),jc(i1,j1)).
C   If there is no such quadrangle, i.e. if a valid G2 point (i,j) is
C   outside G1 domain, IG1(i,j) is set to -2. If (i,j) is a G2 land
C   point (MASK2(i,j).eq.1) inside G1 domain IG1(i,j) is set to -1.
C   If (i,j) is not a valid G2 point (which is supposed to be flagged
C   with MASK2(i,j)=-1 value) IG1(i,j) is set to -3. Whenever a
C   nonpositive value is assigned to IG1(i,j) no value is
C   assigned to JG1(i,j).
C     
C     Also, if present, calculate COSANG( , ), SINANG( , ). If 
C     for each G2 gridpoint (i,j) ANG is the angle between x direction
C     (the direction of growing first index) on G1 grid and x direction
C     at point (i,j) on G2 grid (the angle may have either sign and is
C     measured from the latter direction to the former) then
C     COSANG(i,j)=cos(ANG), SINANG(i,j)=sin(ANG) .
C

      implicit none

      integer,intent(in):: IM1,JM1,IM2,JM2
                  !<- dimensions for G1 and G2 grid arrays
      real,dimension(IM1,JM1),intent(in):: lon1,lat1
                  !<- arrays of longitudes and latitudes for G1 grid.
                  ! In fact, it may be coordinates x,y in any
                  ! coordinate system 
      real,dimension(IM2,JM2),intent(in):: LON2,LAT2
                  !<- arrays of longitudes and latitudes for G2 grid.
                  ! If coordinates x,y (different from latitudes/
                  ! longitudes) are used in lon1, lat1 above, the
                  ! coordinates in LON2, LAT2 must be in the same
                  ! coordinate system x,y
      integer,dimension(IM1,JM1),intent(in):: mask1
                  !<- sea/land mask for G1 grid, 0 - sea, 1 - land;
                  ! -1 for not valid gridpoints (see above)
      integer,dimension(IM2,JM2),intent(in):: MASK2
                  !<- sea/land mask for G2 grid, 0 - sea, 1 - land;
                  ! -1 for not valid gridpoints (see above)
      INTEGER,DIMENSION(IM2,JM2),INTENT(OUT):: IG1,JG1
                  !<- arrays that determine in which G1 gridcells G2
                  ! gridpoints lie, see above
      INTEGER,INTENT(INOUT):: ierr
                  !<- out: 0 for normal return, nonzero otherwise;
                  ! in: if data are read from disk (see iiu) and if the
                  ! read input data fail to coincide with the
                  ! corresponding arguments then: if ierr=0 on input the
                  ! subroutine will return abnormally and if ierr!=0 on
                  ! input the data will be calculated as if with iiu<0
      integer,optional,dimension(IM1,JM1),intent(in):: ia,ja,ib,jb,ic,jc
                  !<- determine the 4 vertices of each G1 gridcell,
                  ! see above
      integer,optional,intent(in):: KM1
                  !<- length of G1 grid boundary (see ibndr1, jbndr1).
                  ! If present (absent), both ibndr1,jbndr1 must also be
                  ! present (absent)
      integer,optional,dimension(0:),intent(in):: ibndr1,jbndr1
                  !<- arrays representing i- and j-indices of those
                  ! G1 gridpoints which belong to the G1 domain
                  ! boundary; the representation is supposed to be
                  ! continuous, i.e. subsequent array elements represent
                  ! adjacent gridpoints; the boundary must be closed:
                  ! ibndr1(0)=ibndr1(KM1), jbndr1(0)=jbndr1(KM1).
                  !    If these arguments are absent, it is assumed
                  ! that the G1 domain boundary consists of all those
                  ! gridpoints (i,j) for which i=1 or i=IM1 or j=1 or
                  ! j=JM1.
      real,optional,dimension(IM1,JM1),intent(in):: xdlon1,xdlat1
                  !<- latitudes and longitudes of x-direction vectors
                  ! at each G1 gridpoints. The arguments must be either
                  ! both present or both absent. If they are absent it
                  ! is assumed that x-direction at each G1 gridpoint
                  ! (i0,j0) is the direction of i increasing from i0
                  ! with j=j0=const. These are input data for
                  ! computation of COSANG, SINANG (see above/below)
      real,optional,dimension(IM2,JM2),intent(in):: XDLON2,XDLAT2
                  !<- latitudes and longitudes of x-direction vectors
                  ! at each G2 gridpoints. The arguments must be either
                  ! both present or both absent. If they are absent it
                  ! is assumed that x-direction at each G2 gridpoint
                  ! (i0,j0) is the direction of i increasing from i0
                  ! with j=j0=const. These are input data for
                  ! computation of COSANG, SINANG (see above/below)
      REAL,OPTIONAL,DIMENSION(IM2,JM2),INTENT(OUT):: COSANG,SINANG
                  !<- cosine and sine of the angle between the
                  ! x direction on G2 grid and x direction on G1
                  ! grid at each G2 gridpoint. The angle is measured
                  ! from G2 x direction to G1 x direction. (For
                  ! definition of x directions, see above.)
      integer,optional,intent(in):: nvrbs
                  !<- verbosity level. With nvrbs=0 or if the argument
                  ! is absent the subroutine is silent unless it
                  ! returns abnormally. Positive value recommended for
                  ! debugging/testing
      integer,optional,intent(in):: iiunit
                  !<- if iiunit>0 the OUT arguments are not calculated
                  ! but read from disk unit number iiunit, along with
                  ! copies of IN arguments, in the order:
                  ! IM1,JM1,IM2,JM2,lon1,lat1,LON2,LAT2,mask1,MASK2,
                  ! IG1,JG1 (1 record to read) and, if present,
                  ! COSANG, SINANG (next record to read).
                  ! The read values for IN arguments must coincide with
                  ! the corresponding values supplied through actual
                  ! arguments; the reading is done without opening/
                  ! rewinding, from the current position;
                  !    if iiunit=0 or is absent no disk exchange is
                  ! undertaken; computations proceed as usual;
                  !    if iiunit<0 the computations proceed as usual and
                  ! the above arguments in the above order are written
                  ! to disk unit number -iiunit, in the same way as they
                  ! are read when iiunit>0.

      integer i,j,i1,j1,i2,j2,i2p,j2p,i0,j0,i1a,j1a,i1b,j1b,i1c,j1c,
     >is,ie,js,je,isp,iep,jsp,jep,istep,nvrb,i10,j10,
     >ios,ios1,ierr_in,ierr1,iiu,ivrbs,n,n1,k,k1,km,kmm,kb1,KM2,KM2M
!     integer,dimension(:),allocatable:: ibnd1,jbnd1
      integer,dimension(0:IM1*JM1):: ibnd1,jbnd1
      integer,dimension(IM2*JM2):: IPEANO2,JPEANO2
      integer,dimension(0:IM1,0:JM1):: msk1
      integer,dimension(0:IM2,0:JM2):: MSK2
      real x1,x1a,x1b,x1c,y1,y1a,y1b,y1c,
     >r1r2,dlon2,dlat2,dlon1on2,dlat1on2
      real,parameter:: pi=3.1415926535897932,rad=pi/180.
      real,dimension(IM1,JM1):: dlon1,dlat1
      logical inG1,inG1cell,cross,stuck
      integer IM1r,JM1r,IM2r,JM2r
      real,dimension(IM1,JM1):: lon1r,lat1r
      real,dimension(IM2,JM2):: LON2r,LAT2r
      integer,dimension(IM1,JM1):: mask1r
      integer,dimension(IM2,JM2):: MASK2r
C

      ierr_in=ierr
      ierr=0
C
      if (PRESENT(nvrbs)) then
        nvrb=nvrbs
      else
        nvrb=0
      end if
      if (PRESENT(COSANG).neqv.PRESENT(SINANG)) then
        print*,'INTP_INI: **ERROR:** both or neither '//
     >  'SINANG, COSANG must be present'
        ierr=1
        RETURN
      end if
      if ( (PRESENT(ia).neqv.PRESENT(ja)) .or.
     >     (PRESENT(ia).neqv.PRESENT(ib)) .or.
     >     (PRESENT(ia).neqv.PRESENT(jb)) .or.
     >     (PRESENT(ia).neqv.PRESENT(ic)) .or.
     >     (PRESENT(ia).neqv.PRESENT(jc)) ) then
        print*,'INTP_INI: **ERROR** ia,ja,ib,jb,ic,jc must be all '//
     >  'present or all not present'
        ierr=1
        RETURN
      end if
      if ( (PRESENT(xdlon1).neqv.PRESENT(xdlat1)) ) then
        print*,'INTP_INI: **ERROR:** both or neither '//
     >  'xdlon1, xdlat1 must be present'
        ierr=1
        RETURN
      end if
      if ( (PRESENT(XDLON2).neqv.PRESENT(XDLAT2)) ) then
        print*,'INTP_INI: **ERROR:** both or neither '//
     >  'XDLON2, XDLAT2 must be present'
        ierr=1
        RETURN
      end if
C
      IF ( (PRESENT(KM1).neqv.PRESENT(ibndr1)) .or.
     >(PRESENT(KM1).neqv.PRESENT(jbndr1)) ) THEN
        print*,'INTP_INI: **ERROR:** all or none of arguments '//
     >  'KM1, ibndr1,jbndr1 must be present'
        ierr=1
        RETURN
      END IF

      if (PRESENT(COSANG)) then
        ivrbs=10
        if (PRESENT(xdlon1)) then
          dlon1=xdlon1
          dlat1=xdlat1
        else
          do j=1,JM1
            do i=1,IM1
              if (i.eq.IM1) then
                dlon1(i,j)=lon1(i,j)-lon1(i-1,j)
                dlat1(i,j)=lat1(i,j)-lat1(i-1,j)
              else if (i.eq.1) then
                dlon1(i,j)=lon1(i+1,j)-lon1(i,j)
                dlat1(i,j)=lat1(i+1,j)-lat1(i,j)
              else
                dlon1(i,j)=lon1(i+1,j)-lon1(i-1,j)
                dlat1(i,j)=lat1(i+1,j)-lat1(i-1,j)
              end if
            end do
          end do
        end if
      end if
C
!     if (allocated(ibnd1)) then
c       print*,'INTP_INI: ibnd1 previously allocated, deallocating...'
!       deallocate(ibnd1)
!     end if
!     if (allocated(jbnd1)) then
c       print*,'INTP_INI: jbnd1 previously allocated, deallocating...'
!       deallocate(jbnd1)
!     end if
C
      iiu=0
      if (PRESENT(iiunit)) iiu=iiunit
      IF (iiu.gt.0) THEN
        read (iiu,iostat=ios) IM1r,JM1r,IM2r,JM2r,
     >  lon1r,lat1r,LON2r,LAT2r,mask1r,MASK2r,
     >  IG1,JG1
        ios1=0
        if (PRESENT(COSANG)) read (iiu,iostat=ios1) COSANG,SINANG
        if (ios.ne.0 .or. ios1.ne.0) then
          print*,'INTP_INI: ***ERROR*** reading INTP_INI data ',
     >    ios,ios1
          if (ierr_in.eq.0) then
            ierr=2
            RETURN
          else
            print*,'INTP_INI: output data will be '//
     >      'computed, because of the above error'
            iiu=-iiunit
          end if
c         ierr=2
        end if
        if (iiu.gt.0) then
          if (IM1r.ne.IM1.or.JM1r.ne.JM1.or.IM2r.ne.IM2.or.JM2r.ne.JM2
     >    .or.ANY(lon1r.ne.lon1).or.ANY(lat1r.ne.lat1)
     >    .or.ANY(LON2r.ne.LON2).or.ANY(LAT2r.ne.LAT2)
     >    .or.ANY(mask1r.ne.mask1).or.ANY(MASK2r.ne.MASK2)) then
            print*,'INTP_INI: **ERROR: read INTP_INI input data'//
     >      ' differ from input arguments',
     >      IM1r,IM1,JM1r,JM1,IM2r,IM2,JM2r,JM2,
     >      maxval(abs(lon1r-lon1)),maxval(abs(lat1r-lat1)),
     >      maxval(abs(LON2r-LON2)),maxval(abs(LAT2r-LAT2)),
     >      maxval(abs(mask1r-mask1)),maxval(abs(MASK2r-MASK2))
            if (ierr_in.eq.0) then
              ierr=3
            else
              print*,'INTP_INI: output data will be '//
     >        'computed, because of the above error'
              iiu=-iiunit
            end if
          end if
        end if
        if (iiu.gt.0) RETURN
      END IF

C  1. Find boundary of G1 domain as 1D arrays ibnd1, jbnd1 (G1 domain is
C     where mask1(i,j).ge.0)

      if (PRESENT(KM1)) km=KM1

      IF (PRESENT(KM1) .and. km.ne.0) THEN

      kmm=0   ! only for printout
!!    km=KM1
      if (ibndr1(0).ne.ibndr1(km) .or. jbndr1(0).ne.jbndr1(km)) then
        print*,'INTP_INI: **ERROR** domain boundary (input arguments '//
     >  'ibndr1,jbndr1) not closed ',
     >  ibndr1(0),jbndr1(0),ibndr1(km),jbndr1(km)
        ierr=5
        RETURN
      end if
!     allocate(ibnd1(0:km),jbnd1(0:km))
c        print*,'INTP_INI: done allocate(ibnd1(0:km),jbnd1(0:km))'
      ibnd1(0:km)=ibndr1(0:km)
      jbnd1(0:km)=jbndr1(0:km)

      ELSE

!     kmm=2*(IM1+JM1-2)
      kmm=IM1*JM1
!     allocate(ibnd1(0:kmm),jbnd1(0:kmm))

C  1.1. Find 2D array's boundary point (i1,j1) (where i=1 or i=IM1 or
C       j=1 or j=JM1) which is also a G1 domain point 

      call FBP(IM1,JM1,mask1,i1,j1,ierr)
      if (ierr.ne.0) then
        print*,
     >  'INTP_INI: **ERROR** FAILED to find init. bound. domain 1 point'
        RETURN
      end if
      i=i1
      j=j1
c       print*,i,j

C  1.2. Find domain boundary

      msk1(1:IM1,1:JM1)=mask1
      msk1(:,0)=-1
      msk1(IM1+1,:)=-1
      msk1(:,JM1+1)=-1
      msk1(0,:)=-1
      ibnd1(0)=i
      jbnd1(0)=j
      km=0
      do k=1,kmm
!       if (msk1(i,j+1).ge.0) then
        if (msk1(i,j-1).eq.-1) then
          if (msk1(i+1,j).ge.0) then
            i=i+1
          else
            j=j+1
          end if
!       else if (msk1(i-1,j).ge.0) then
        else if (msk1(i+1,j).eq.-1) then
          if (msk1(i,j+1).ge.0) then
            j=j+1
          else
            i=i-1
          end if
!       else if (msk1(i,j-1).ge.0) then
        else if (msk1(i,j+1).eq.-1) then
          if (msk1(i-1,j).ge.0) then
            i=i-1
          else
            j=j-1
          end if
!       else if (msk1(i+1,j).ge.0) then
        else if (msk1(i-1,j).eq.-1) then
          if (msk1(i,j-1).ge.0) then
            j=j-1
          else
            i=i+1
          end if
!       else
        else if (msk1(i-1,j-1).eq.-1) then
          j=j-1
        else if (msk1(i+1,j-1).eq.-1) then
          i=i+1
        else if (msk1(i+1,j+1).eq.-1) then
          j=j+1
        else if (msk1(i-1,j+1).eq.-1) then
          i=i-1
        else
          print*,'INTP_INI: **ERROR** BAD DOMAIN 1 BOUNDARY ',i,j
          ierr=5
          RETURN
        end if
        ibnd1(k)=i
        jbnd1(k)=j
!!      msk1(i,j)=-2
c         print*,k,kmm,km,i,j
        if (i.eq.ibnd1(0) .and. j.eq.jbnd1(0)) then
          km=k
          exit
        end if
      end do
      if (km.eq.0) then
        print*,'INTP_INI: **ERROR** domain boundary not closed ',
     >  ibnd1(0),jbnd1(0),ibnd1(km),jbnd1(km)
        ierr=5
        RETURN
      end if

      END IF
      if (nvrb.gt.0) print*,'INTP_INI: km=',km,' kmm=',kmm
c       do k=1,km
c         print*,k,ibnd1(k),jbnd1(k),mask1(ibnd1(k),jbnd1(k))
c       end do

C  2. Find discrete analog of Peano curve for G2 domain (integer
C     arrays IPEANO2, JPEANO2)
C        (Now it's not very Peano, as it may jump across empty areas
C        i.e. ones where MASK2<0. It is rather a trivial Hamiltonian
C        path in grid 2 considered as a graph, with additional edges
C        drawn over empty areas.)

      IG1=-3
      is=1
      ie=IM2
      istep=1
      k=0
      do j=1,JM2
        do i=is,ie,istep
          if (MASK2(i,j).ge.0) then
            k=k+1
            IPEANO2(k)=i
            JPEANO2(k)=j
          end if
        end do
        istep=-istep
        i=is
        is=ie
        ie=i
      end do
      KM2=k
          
      if (nvrb.gt.0) print*,'INTP_INI: KM2=',KM2

C  3. For each valid G2 point (i2,j2) [i.e. for each (i2,j2) such
C     that MASK2(i2,j2)>=0, i.e. for each (i2,j2) lying on the Peano
C     curve], find the G1 cell such that (i2,j2) lies inside it

C  3.1. Find if 1st of Peano curve G2 points lies inside G1 domain.
C       Finish with this point (i2,j2), i.e. find IG1(i2,j2), JG1(i2,j2)
C       if it does lie inside domain 1, otherwise assign IG1(i2,j2)=-2

      inG1=.false.
      i1=ibnd1(0)
      j1=jbnd1(0)
      i2=IPEANO2(1)
      j2=JPEANO2(1)
      jloop: do j=1,JM1
        do i=1,IM1
          call G2Point_G1Cell(i2,j2,i,j,inG1,i1,j1)
          if (inG1) EXIT jloop
        end do
      end do jloop
      if (.not. inG1) IG1(i2,j2)=-2

C  3.2. Moving along the Peano curve, for each G2 point (i2,j2)
C       find the G1 cell such that (i2,j2) lies inside it. Such
C       a cell must exist if (i2,j2) lies inside G1 domain; in this
C       case, assign IG1(i2,j2), JG1(i2,j2), which define the cell;
C       otherwise assign IG1(i2,j2)=-2

c                print*,'PC: 1st point:',inG1,i2,j2,i1,j1
      kloop: do k=2,KM2

!  3.2.1.  Find if the current G2 point (i2,j2) on Peano curve
!          is in the same position with respect to G1 domain (i.e.
!          inside or outside it) as the previous G2 point on Peano
!          curve. If it is outside G1 domain assign IG1(i2,j2)=-2 and
!          go to the next G2 point

        i2p=i2
        j2p=j2
        i2=IPEANO2(k)
        j2=JPEANO2(k)
        call G2Point_G1Cell(i2,j2,i1,j1,inG1cell,i1,j1)
c          if (present(ia).and.inG1cell)
c    >     print*,'PC: G1 cell same as previous ',k,inG1,i2,j2,i1,j1
        if (inG1cell) then
          inG1=.true.
          CYCLE
        end if
        cross=.false.
           n1=0  ! for debugging
        do kb1=1,km
          cross=Intersects(
     >    LON2(i2p,j2p),LAT2(i2p,j2p),LON2(i2,j2),LAT2(i2,j2),
     >    lon1(ibnd1(kb1-1),jbnd1(kb1-1)),
     >    lat1(ibnd1(kb1-1),jbnd1(kb1-1)),
     >    lon1(ibnd1(kb1),jbnd1(kb1)),lat1(ibnd1(kb1),jbnd1(kb1)))
!!          cross=cross .neqv. Intersects(
          if (cross) then
            inG1=.not.inG1
               n1=n1+1  ! for debugging
            k1=kb1
          end if
!         inG1=inG1 .neqv. cross   !<- does same as above
!!         if (cross) EXIT <- deleted. One segment of G2 Peano curve
!!                            may intersect G1 boundary more than once,
!!                            so the criterion of the curve getting
!!                            inside G1 domain from outside, or vice
!!                            versa, is an odd number of crossings
        end do

!!        inG1=inG1.neqv.cross

        i10=i1                     !
        j10=j1                     !
        if (inG1 .and. cross) then ! this is for some acceleration but
          i1=ibnd1(k1)             ! it is to be seen if it yields any
          j1=jbnd1(k1)             !
        end if                     !

        if (.not. inG1) then
           IG1(i2,j2)=-2 ! this is better than in INTERP_INIT: now all external points get IG1=-2 . Check if it is OK in Coupler and change comments in mod_interp.f.new
          CYCLE
        end if
c            if (present(ia)) print*,'PC: k=',k,inG1,i2,j2,i1,j1

!  3.2.2.  Starting from (i1,j1), on perimeters of expanding squares,
!          find (i1,j1) such as G2Point_In_G1Cell(i2,j2,i1,j1), i.e.
!          such as G2 point (i2,j2) lies inside grid 1 cell defined
!          by G1 point (i1,j1)

        if (i1.ne.i10 .or. j1.ne.j10) then   ! this is superfluous if
          call G2Point_G1Cell(i2,j2,i1,j1,inG1cell,i1,j1) ! the above
          if (inG1cell) CYCLE                ! acceleration insertion
        end if                               ! is removed

        is=i1
        ie=i1
        js=j1
        je=j1
        
        do while (.true.)
          stuck=.true.
          isp=is
          iep=ie
          jsp=js
          jep=je
          is=max(isp-1,1)
          ie=min(iep+1,IM1)
          js=max(jsp-1,1)
          je=min(jep+1,JM1)
          if (is.lt.isp) then
            stuck=.false.
            do j=js,je
              call G2Point_G1Cell(i2,j2,is,j,inG1cell,i1,j1)
              if (inG1cell) CYCLE kloop
            end do
          end if
          if (ie.gt.iep) then
            stuck=.false.
            do j=js,je
              call G2Point_G1Cell(i2,j2,ie,j,inG1cell,i1,j1)
              if (inG1cell) CYCLE kloop
            end do
          end if
          if (js.lt.jsp) then
            stuck=.false.
            do i=isp,iep
              call G2Point_G1Cell(i2,j2,i,js,inG1cell,i1,j1)
              if (inG1cell) CYCLE kloop
            end do
          end if
          if (je.gt.jep) then
            stuck=.false.
            do i=isp,iep
              call G2Point_G1Cell(i2,j2,i,je,inG1cell,i1,j1)
              if (inG1cell) CYCLE kloop
            end do
          end if

! End 3.2.2

          if (stuck) then
            ierr=4
            inG1cell=.false.
            jloop1: do j=1,JM1
              do i=1,IM1
                call G2Point_G1Cell(i2,j2,i,j,inG1cell,i1,j1)
                if (inG1cell) EXIT jloop1
              end do
            end do jloop1
            if (inG1cell) then
              print*,'INTP_INI: the G2 point DOES lie in a G1 cell '//
     >        'but it was not found by expanding squares ',
     >        i1,j1,lon1(i1,j1),lat1(i1,j1)
            else
              print*,'INTP_INI: the G2 point does NOT lie in any G1 '//
     >        'cell'
            end if
            print*,'INTP_INI: **ERROR** STUCK, the sought G1 cell '//
     >      'must exist but is not found ',
     >      k,i2,j2,LON2(i2,j2),LAT2(i2,j2),MASK2(i2,j2),n1,IG1(i2,j2),
     >      IG1(i2p,j2p)
            RETURN
              cycle kloop
          end if
        end do
c         print*,'PC: G1 cell found ',k,inG1,i2,j2,i1,j1
C
      end do kloop
C

      IF (iiu.lt.0) THEN
        write (-iiu,iostat=ios) IM1,JM1,IM2,JM2,
     >  lon1,lat1,LON2,LAT2,mask1,MASK2,
     >  IG1,JG1
        ios1=0
        if (present(COSANG)) write (-iiu,iostat=ios1) COSANG,SINANG
        if (ios.ne.0 .or. ios1.ne.0) then
          print*,'INTP_INI: **ERROR** writing output data ',
     >    ios,ios1
          ierr=6
        end if
      END IF
C

      if (nvrb.gt.0) then
        print*,'INTP_INI: done, IM1,JM1,IM2,JM2: ',IM1,JM1,IM2,JM2
        k=0
        n=0
        k1=0
        i1=0
        j1=0
        j2=0
        do j=1,JM2
          do i=1,IM2
            if (MASK2(i,j).lt.0) then
              k1=k1+1
            else if (MASK2(i,j).eq.1) then
              i1=i1+1
            end if
            if (IG1(i,j).gt.0) then
              k=k+1
            else if (IG1(i,j).eq.-2) then
              j1=j1+1
              if (MASK2(i,j).eq.0) j2=j2+1
            else if (IG1(i,j).eq.0) then
              n=n+1
            end if
          end do
        end do
        print*,'INTP_INI: Total # of G2 points (2D array size) ',IM2*JM2
        print*,'INTP_INI: Total # of valid G2 points           ',
     >  IM2*JM2-k1
        print*,'INTP_INI: # of land G2 points                  ',i1
        print*,'INTP_INI: # of sea G2 points                   ',
     >  IM2*JM2-k1-i1
        print*,'INTP_INI: # of sea G2 points inside G1 domain  ',
     >  IM2*JM2-k1-i1-j2
        print*,'INTP_INI: # of interpolatable G2 (sea) points  ',k
        print*,'INTP_INI: # of G2 points outside G1 domain     ',j1
        print*,'INTP_INI: # of sea G2 points outside G1 domain ',j2
        print*,'INTP_INI: # of sea G2 points in land G1 cells  ',n
      end if
C

      RETURN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine FBP(IM,JM,mask,i0,j0,ierr)

      implicit none

      integer IM,JM,mask(IM,JM),i0,j0,ierr

      integer i,j
C

      ierr=0
      do i=1,IM
        if (mask(i,1).ge.0) then
          i0=i
          j0=1
          return
        end if
      end do
      do i=IM,1,-1
        if (mask(i,JM).ge.0) then
          i0=i
          j0=JM
          return
        end if
      end do
      do j=1,JM
        if (mask(IM,j).ge.0) then
          i0=IM
          j0=j
          return
        end if
      end do
      do j=JM,1,-1
        if (mask(1,j).ge.0) then
          i0=1
          j0=j
          return
        end if
      end do
      ierr=10
      return
      end subroutine FBP

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine G2Point_G1Cell(i,j,i1,j1,inside,i1out,j1out)

      implicit none

      integer,intent(in):: i,j,i1,j1
      logical,intent(out):: inside
      integer,intent(out):: i1out,j1out

      logical cw
C

      inside=.false.
      if (MASK2(i,j).lt.0) then ! temporary, for debugging only
        print*,'INTP_INI: **ERROR** invalid G2 point in G2Point_G1Cell'
        STOP
      end if

      if (present(ia)) then
        i1a=ia(i1,j1)
        if (i1a.lt.1) return
        if (i1a.gt.IM1) return
        j1a=ja(i1,j1)
        if (j1a.lt.1) return
        if (j1a.gt.JM1) return
        i1b=ib(i1,j1)
        if (i1b.lt.1) return
        if (i1b.gt.IM1) return
        j1b=jb(i1,j1)
        if (j1b.lt.1) return
        if (j1b.gt.JM1) return
        i1c=ic(i1,j1)
        if (i1c.lt.1) return
        if (i1c.gt.IM1) return
        j1c=jc(i1,j1)
        if (j1c.lt.1) return
        if (j1c.gt.JM1) return
      else
        if (i1.ge.IM1) return
        if (j1.ge.JM1) return
        i1a=i1+1
        j1a=j1
        i1b=i1+1
        j1b=j1+1
        i1c=i1
        j1c=j1+1
      end if

CB              Check if (i,j) is inside quadrangle
C                (i1,j1),(i1a,j1a),(i1b,j1b),(i1c,j1c)
C                It is not assumed here that this list of vertices
C                is counterclockwise

      if (mask1(i1,j1).eq.-1) return
      if (mask1(i1a,j1a).eq.-1) return
      if (mask1(i1b,j1b).eq.-1) return
      if (mask1(i1c,j1c).eq.-1) return

      x1=lon1(i1,j1)-LON2(i,j)
      y1=lat1(i1,j1)-LAT2(i,j)
      x1a=lon1(i1a,j1a)-LON2(i,j)
      y1a=lat1(i1a,j1a)-LAT2(i,j)
      cw=x1*y1a-x1a*y1.lt.0.
      x1b=lon1(i1b,j1b)-LON2(i,j)
      y1b=lat1(i1b,j1b)-LAT2(i,j)
      if (x1a*y1b-x1b*y1a.lt.0. .neqv. cw) return
      x1c=lon1(i1c,j1c)-LON2(i,j)
      y1c=lat1(i1c,j1c)-LAT2(i,j)
      if (x1b*y1c-x1c*y1b.lt.0. .neqv. cw) return
      if (x1c*y1-x1*y1c.lt.0. .neqv. cw) return
CE

      inside=.true.
      i1out=i1
      j1out=j1
      if (MASK2(i,j).eq.0) then
        if (mask1(i1,j1).gt.0 .and. mask1(i1a,j1a).gt.0 .and.
     >    mask1(i1b,j1b).gt.0 .and. mask1(i1c,j1c).gt.0) then
          IG1(i,j)=0
          return
        else
          IG1(i,j)=i1
          JG1(i,j)=j1
        end if
      else  ! MASK(i,j) must be 1 (land), since (i,j) is supposed
            ! a valid G2 point, since it is supposed to lie inside
            ! a G1 cell
        IG1(i,j)=-1
        return
      end if

      if (PRESENT(COSANG)) then
        if (present(XDLON2)) then
          dlon2=XDLON2(i,j)
          dlat2=XDLAT2(i,j)
        else
          if (i.eq.IM2) then
            dlon2=LON2(i,j)-LON2(i-1,j)
            dlat2=LAT2(i,j)-LAT2(i-1,j)
          else if (i.eq.1) then
            dlon2=LON2(i+1,j)-LON2(i,j)
            dlat2=LAT2(i+1,j)-LAT2(i,j)
          else
            dlon2=LON2(i+1,j)-LON2(i-1,j)
            dlat2=LAT2(i+1,j)-LAT2(i-1,j)
          end if
        end if

        dlon2=dlon2*cos(LAT2(i,j)*rad)
c          print*,'INTP_INI to call BLIN',k,i,j

        CALL BLIN(lon1(i1,j1),lon1(i1a,j1a),
     >     lon1(i1b,j1b),lon1(i1c,j1c),
     >  lat1(i1,j1),lat1(i1a,j1a),
     >     lat1(i1b,j1b),lat1(i1c,j1c),
     >  dlon1(i1,j1),dlon1(i1a,j1a),
     >     dlon1(i1b,j1b),dlon1(i1c,j1c), 
     >  LON2(i,j),LAT2(i,j),
     >  dlon1on2,
     >  ivrbs,ierr1)

        if (ierr1.ne.0) then
          ierr=100
          print*,'INTP_INI: **ERROR** in BLIN ',ierr
          RETURN
        end if

        dlon1on2=dlon1on2*cos(LAT2(i,j)*rad)

        CALL BLIN(lon1(i1,j1),lon1(i1a,j1a),
     >     lon1(i1b,j1b),lon1(i1c,j1c),
     >  lat1(i1,j1),lat1(i1a,j1a),
     >     lat1(i1b,j1b),lat1(i1c,j1c),
     >  dlat1(i1,j1),dlat1(i1a,j1a),
     >     dlat1(i1b,j1b),dlat1(i1c,j1c), 
     >  LON2(i,j),LAT2(i,j),
     >  dlat1on2,
     >  ivrbs,ierr1)

        r1r2=sqrt((dlon2**2+dlat2**2)*(dlon1on2**2+dlat1on2**2))
        SINANG(i,j)=(dlon2*dlat1on2-dlon1on2*dlat2)/r1r2
        COSANG(i,j)=(dlon2*dlon1on2+dlat2*dlat1on2)/r1r2
C
      end if

      return
      end subroutine G2Point_G1Cell

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      logical function Intersects(x1s,y1s,x1e,y1e,x2s,y2s,x2e,y2e)

      implicit none

      real x1s,y1s,x1e,y1e,x2s,y2s,x2e,y2e

      real dx1,dy1,dx2,dy2,dxs,dys,D,Dt,Du
C

      Intersects=.false.

      dx1=x1e-x1s
      dy1=y1e-y1s
      dx2=x2e-x2s
      dy2=y2e-y2s
      D=dx2*dy1-dx1*dy2
      dxs=x2s-x1s
      dys=y2s-y1s
      Dt=dys*dx2-dxs*dy2
      Du=dys*dx1-dxs*dy1
      if (D.lt.0.) then
        D=-D
        Dt=-Dt
        Du=-Du
      end if
      if (Dt.le.0.) return
      if (Dt.gt.D) return
      if (Du.le.0.) return

      Intersects=Du.le.D

      return
      end function Intersects

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
      END SUBROUTINE INTP_INI
C
C***********************************************************************
C
      SUBROUTINE INTERP_INIT(
     >IM1,JM1,IM2,JM2,lon1,lat1,LON2,LAT2,mask1,MASK2,
     >IG1,JG1,ierr,
     >ia,ja,ib,jb,ic,jc,xdlon1,xdlat1,XDLON2,XDLAT2,iiunit,
     >COSANG,SINANG)
C       
      implicit none

      integer,intent(in):: IM1,JM1,IM2,JM2
      real,dimension(IM1,JM1),intent(in):: lon1,lat1
      real,dimension(IM2,JM2),intent(in):: LON2,LAT2
      integer,dimension(IM1,JM1),intent(in):: mask1
      integer,dimension(IM2,JM2),intent(in):: MASK2
      INTEGER,DIMENSION(IM2,JM2),INTENT(OUT):: IG1,JG1
      INTEGER,INTENT(INOUT):: ierr
      integer,optional,dimension(IM1,JM1),intent(in):: ia,ja,ib,jb,ic,jc
      real,optional,dimension(IM1,JM1),intent(in):: xdlon1,xdlat1
      real,optional,dimension(IM2,JM2),intent(in):: XDLON2,XDLAT2
      integer,optional,intent(in):: iiunit
      REAL,DIMENSION(IM2,JM2),OPTIONAL,INTENT(OUT):: COSANG,SINANG

      integer IM1r,JM1r,IM2r,JM2r
      real,dimension(IM1,JM1):: lon1r,lat1r,dlon1,dlat1
      real,dimension(IM2,JM2):: LON2r,LAT2r
      integer,dimension(IM1,JM1):: mask1r
      integer,dimension(IM2,JM2):: MASK2r
      integer i,j,i1,j1,ie1,je1,i1a,j1a,i1b,j1b,i1c,j1c,
     >ios,ios1,ierr1,ivrbs,ierr_in,iiu
      real x1,x1a,x1b,x1c,y1,y1a,y1b,y1c,
     >r1r2,dlon2,dlat2,dlon1on2,dlat1on2
      real,parameter:: pi=3.1415926535897932,rad=pi/180.
C
C
C     Calculate IG1( , ),JG1( , ). By definition, for each G2 sea
C     gridpoint (i,j) (MASK2(i,j)=0), whose coordinates are
C     (LON2(i,j),LAT2(i,j)),
C     ( IG1(i,j),JG1(i,j) ) = (i1,j1)
C     is the point on G1 grid for which (i,j) lies in the
C     quadrangle whose vertices are G1 gridpoints (i1,j1), (i1+1,j1), 
C     (i1+1,j1+1), (i1,j1+1), if these vertices are not all land
C     points; if they are all land points, IG1(i,j) is set to 0 and
C     no value is assigned to JG1(i,j).
C            If ia,ja,ib,jb,ic,jc are present, the 4 vertices are,
C            instead of the above, (i1,j1), (ia(i1,j1),ja(i1,j1)),
C            (ib(i1,j1),jb(i1,j1)), (ic(i1,j1),jc(i1,j1))
C        If there is no such a quadrangle (i.e. (i,j) is outside G1
C     domain), IG1(i,j) is set to -1 if it is a land point and to -2
C     otherwise; no value is assigned to JG1(i,j)
C        If (i,j) is a G2 land point (MASK2(i,j).ne.0), IG1(i,j) is set
C     to -1; no value is assigned to JG1(i,j) 
C
C     Also, if present, calculate COSANG( , ), SINANG( , ). If 
C     for each G2 gridpoint (i,j) ANG is the angle between x direction
C     (the direction of growing first index) at point (IG1(i,j),
C     JG1(i,j)) on G1 grid and x direction at point (i,j) on 
C     G2 grid (the angle may have either sign and is measured from the 
C     latter direction to the former), then COSANG(i,j)=cos(ANG), 
C     SINANG(i,j)=sin(ANG) .
C

      ierr_in=ierr
      ierr=0

      if (PRESENT(COSANG).neqv.PRESENT(SINANG)) then
        print*,'INTERP_INIT: **ERROR:** both or neither '//
     >  'SINANG, COSANG must be present'
        ierr=1
        RETURN
      end if
      if ( (PRESENT(ia).neqv.PRESENT(ja)) .or.
     >     (PRESENT(ia).neqv.PRESENT(ib)) .or.
     >     (PRESENT(ia).neqv.PRESENT(jb)) .or.
     >     (PRESENT(ia).neqv.PRESENT(ic)) .or.
     >     (PRESENT(ia).neqv.PRESENT(jc)) ) then
        print*,'INTERP_INIT: **ERROR** ia,ja,ib,jb,ic,jc must be all '//
     >  'present or all not present'
        ierr=1
        RETURN
      end if
      if ( (PRESENT(xdlon1).neqv.PRESENT(xdlat1)) ) then
        print*,'INTERP_INIT: **ERROR:** both or neither '//
     >  'xdlon1, xdlat1 must be present'
        ierr=1
        RETURN
      end if
      if ( (PRESENT(XDLON2).neqv.PRESENT(XDLAT2)) ) then
        print*,'INTERP_INIT: **ERROR:** both or neither '//
     >  'XDLON2, XDLAT2 must be present'
        ierr=1
        RETURN
      end if
C
      iiu=0
      IF (PRESENT(iiunit)) THEN
      iiu=iiunit
      IF (iiu.gt.0) THEN
        read (iiunit,iostat=ios) IM1r,JM1r,IM2r,JM2r,
     >  lon1r,lat1r,LON2r,LAT2r,mask1r,MASK2r,
     >  IG1,JG1
        ios1=0
        if (PRESENT(COSANG)) read (iiunit,iostat=ios1) COSANG,SINANG
        if (ios.ne.0 .or. ios1.ne.0) then
          print*,'INTERP_INIT: ***ERROR*** reading INTERP_INIT data'
          if (ierr_in.eq.0) then
            ierr=2
            RETURN
          else
            print*,'INTERP_INIT: output data will be '//
     >      'computed, because of the above error'
            iiu=-iiunit
          end if
          ierr=2
        end if
        if (iiu.gt.0) then
          if (IM1r.ne.IM1.or.JM1r.ne.JM1.or.IM2r.ne.IM2.or.JM2r.ne.JM2
     >    .or.ANY(lon1r.ne.lon1).or.ANY(lat1r.ne.lat1)
     >    .or.ANY(LON2r.ne.LON2).or.ANY(LAT2r.ne.LAT2)
     >    .or.ANY(mask1r.ne.mask1).or.ANY(MASK2r.ne.MASK2)) then
            print*,'INTERP_INIT: **ERROR: read INTERP_INIT input data'//
     >      ' differ from input arguments'
            if (ierr_in.eq.0) then
              ierr=3
            else
              print*,'INTERP_INIT: output data will be '//
     >        'computed, because of the above error'
              iiu=-iiunit
            end if
          end if
        end if
        if (iiu.gt.0) RETURN
      END IF
      END IF
C
      if (PRESENT(COSANG)) then
        ivrbs=10
        if (PRESENT(xdlon1)) then
          dlon1=xdlon1
          dlat1=xdlat1
        else
          do j=1,JM1
            do i=1,IM1
              if (i.eq.IM2) then
                dlon1(i,j)=lon1(i,j)-lon1(i-1,j)
                dlat1(i,j)=lat1(i,j)-lat1(i-1,j)
              else if (i.eq.1) then
                dlon1(i,j)=lon1(i+1,j)-lon1(i,j)
                dlat1(i,j)=lat1(i+1,j)-lat1(i,j)
              else
                dlon1(i,j)=lon1(i+1,j)-lon1(i-1,j)
                dlat1(i,j)=lat1(i+1,j)-lat1(i-1,j)
              end if
            end do
          end do
        end if
      end if
C
      do j=1,JM2
        iloop: do i=1,IM2

          IF (MASK2(i,j).ne.0) THEN
            IG1(i,j)=-1
            CYCLE
          END IF
          IG1(i,j)=-2
C
          if (present(ia)) then
            je1=JM1
            ie1=IM1
          else
            je1=JM1-1
            ie1=IM1-1
          end if

          do j1=1,je1
            do i1=1,ie1

              if (present(ia)) then
                i1a=ia(i1,j1)
                if (i1a.lt.1) cycle
                if (i1a.gt.ie1) cycle
                j1a=ja(i1,j1)
                if (j1a.lt.1) cycle
                if (j1a.gt.je1) cycle
                i1b=ib(i1,j1)
                if (i1b.lt.1) cycle
                if (i1b.gt.ie1) cycle
                j1b=jb(i1,j1)
                if (j1b.lt.1) cycle
                if (j1b.gt.je1) cycle
                i1c=ic(i1,j1)
                if (i1c.lt.1) cycle
                if (i1c.gt.ie1) cycle
                j1c=jc(i1,j1)
                if (j1c.lt.1) cycle
                if (j1c.gt.je1) cycle
              else
                i1a=i1+1
                j1a=j1
                i1b=i1+1
                j1b=j1+1
                i1c=i1
                j1c=j1+1
              end if

              if (mask1(i1,j1).eq.-1) cycle
              if (mask1(i1a,j1a).eq.-1) cycle
              if (mask1(i1b,j1b).eq.-1) cycle
              if (mask1(i1c,j1c).eq.-1) cycle

CB               Check if (i,j) is inside quadrangle
C                (i1,j1),(i1a,j1a),(i1b,j1b),(i1c,j1c)
C                *** It is supposed that this list of vertices ***
C                *** is counterclockwise -                     ***
C                otherwise the procedure will fail as if (i,j) is
C                automatically outside the quadrangle
              x1=lon1(i1,j1)-LON2(i,j)
              y1=lat1(i1,j1)-LAT2(i,j)
              x1a=lon1(i1a,j1a)-LON2(i,j)
              y1a=lat1(i1a,j1a)-LAT2(i,j)
              if (x1*y1a-x1a*y1.lt.0.) cycle
              x1b=lon1(i1b,j1b)-LON2(i,j)
              y1b=lat1(i1b,j1b)-LAT2(i,j)
              if (x1a*y1b-x1b*y1a.lt.0.) cycle
              x1c=lon1(i1c,j1c)-LON2(i,j)
              y1c=lat1(i1c,j1c)-LAT2(i,j)
              if (x1b*y1c-x1c*y1b.lt.0.) cycle
              if (x1c*y1-x1*y1c.lt.0.) cycle

C               if G2 gridpoint (i,j) is surrounded by G1
C               landpoints, interpolation is impossible:
              if (mask1(i1,j1).ne.0 .and. mask1(i1a,j1a).ne.0 .and.
     >          mask1(i1b,j1b).ne.0 .and. mask1(i1c,j1c).ne.0) then
                IF (.FALSE.) THEN
                  print '(/" A G2 sea gridpoint surrounded by G1 ",
     >            "land gridpoints")'
                  print '("  G2 gridpoint: (i,j)=(",i3,",",
     >            i3,") , (lambda,phi)=(",f7.2,",",f7.2,
     >            ")")',i,j,LON2(i,j),LAT2(i,j)
                  print '(" i1=",i3," j1=",i3)',i1,j1
                  print '(" The quadrangle: ",4(3x,2f7.2))',
     >            x1,y1,  x1a,y1a,  x1b,y1b,  x1c,y1c
                END IF
                IG1(i,j)=0
                CYCLE iloop
              end if
              IG1(i,j)=i1
              JG1(i,j)=j1
C
              if (PRESENT(COSANG)) then
                if (present(XDLON2)) then
                  dlon2=XDLON2(i,j)
                  dlat2=XDLAT2(i,j)
                else
                  if (i.eq.IM2) then
                    dlon2=LON2(i,j)-LON2(i-1,j)
                    dlat2=LAT2(i,j)-LAT2(i-1,j)
                  else if (i.eq.1) then
                    dlon2=LON2(i+1,j)-LON2(i,j)
                    dlat2=LAT2(i+1,j)-LAT2(i,j)
                  else
                    dlon2=LON2(i+1,j)-LON2(i-1,j)
                    dlat2=LAT2(i+1,j)-LAT2(i-1,j)
                  end if
                end if

                dlon2=dlon2*cos(LAT2(i,j)*rad)

                CALL BLIN(lon1(i1,j1),lon1(i1a,j1a),
     >             lon1(i1b,j1b),lon1(i1c,j1c),
     >          lat1(i1,j1),lat1(i1a,j1a),
     >             lat1(i1b,j1b),lat1(i1c,j1c),
     >          dlon1(i1,j1),dlon1(i1a,j1a),
     >             dlon1(i1b,j1b),dlon1(i1c,j1c), 
     >          LON2(i,j),LAT2(i,j),
     >          dlon1on2,
     >          ivrbs,ierr1)

                if (ierr1.ne.0) then
                  ierr=100
                  print*,'INTERP_INIT: **ERROR** in BLIN ',ierr
                  RETURN
                end if

                dlon1on2=dlon1on2*cos(LAT2(i,j)*rad)

                CALL BLIN(lon1(i1,j1),lon1(i1a,j1a),
     >             lon1(i1b,j1b),lon1(i1c,j1c),
     >          lat1(i1,j1),lat1(i1a,j1a),
     >             lat1(i1b,j1b),lat1(i1c,j1c),
     >          dlat1(i1,j1),dlat1(i1a,j1a),
     >             dlat1(i1b,j1b),dlat1(i1c,j1c), 
     >          LON2(i,j),LAT2(i,j),
     >          dlat1on2,
     >          ivrbs,ierr1)

                r1r2=sqrt((dlon2**2+dlat2**2)*(dlon1on2**2+dlat1on2**2))
                SINANG(i,j)=(dlon2*dlat1on2-dlon1on2*dlat2)/r1r2
                COSANG(i,j)=(dlon2*dlon1on2+dlat2*dlat1on2)/r1r2
              end if

              CYCLE iloop

            end do
          end do
C
        end do iloop
      end do
C
      IF (iiu.lt.0) THEN
        write (-iiu,iostat=ios) IM1,JM1,IM2,JM2,
     >  lon1,lat1,LON2,LAT2,mask1,MASK2,
     >  IG1,JG1
        ios1=0
        if (present(COSANG)) write (-iiu,iostat=ios1) COSANG,SINANG
        if (ios.ne.0 .or. ios1.ne.0) then
          print*,'INTERP_INIT: **ERROR** writing INTERP_INIT data ',
     >    ios,ios1
          ierr=4
        end if
      END IF
C
      RETURN
      END SUBROUTINE INTERP_INIT
C
C***********************************************************************
C
      SUBROUTINE INTERP(IM1,JM1,IM2,JM2,lon1,lat1,LON2,LAT2,
     >mask1,MASK2,IG1,JG1,fg1,
     >FG2,ierr,
     >ia,ja,ib,jb,ic,jc,NOSPVAL)

C=======================================================================
C       
C           Interpolation from G1 grid to G2 grid 
C           ---------------------------------------
C       
C   lon1(IM1,JM1), lat1(IM1,JM1) (IN) is G1 grid coordinates
C   LON2(IM2,JM2), LAT2(IM2,JM2) (IN) is G2 grid coordinates
C
C   mask1 (IN) is sea/land mask on G1 grid (0 - sea, 1 - land)
C   MASK2 (IN) is sea/land mask on G2 grid (0 - sea, 1 - land)

C   IG1(IM2,JM2), JG1(IM2,JM2) (IN): for each G2 gridpoint (i,j),
C   whose coordinates are (LON2(i,j),LAT2(i,j)),
C   ( IG1(i,j),JG1(i,j) ) = (i1,j1)
C   is the point on G1 grid for which (i,j) lies in the
C   quadrangle whose angles are G1 gridpoints (i1,j1), (i1+1,j1), 
C   (i1+1,j1+1), (i1,j1+1).
C            If ia,ja,ib,jb,ic,jc are present, the 4 vertices are,
C            instead of the above, (i1,j1), (ia(i1,j1),ja(i1,j1)),
C            (ib(i1,j1),jb(i1,j1)), (ic(i1,j1),jc(i1,j1))
C       
C   fg1 (IN) is data on G1 grid to be interpolated from
C   FG2 (OUT) is data on G2 grid to be obtained by interpolation
C       
C
C=======================================================================

      implicit none

      integer,intent(in):: IM1,JM1,IM2,JM2
                  !<- sea INTP_INI
      real,dimension(IM1,JM1),intent(in):: lon1,lat1
                  !<- sea INTP_INI
      real,dimension(IM2,JM2),intent(in):: LON2,LAT2
                  !<- sea INTP_INI
      integer,dimension(IM1,JM1),intent(in):: mask1
                  !<- sea INTP_INI
      integer,dimension(IM2,JM2),intent(in):: MASK2
                  !<- sea INTP_INI
      integer,dimension(IM2,JM2),intent(in):: IG1,JG1
                  !<- sea INTP_INI
      real,dimension(IM1,JM1),intent(in):: fg1(IM1,JM1)
                  !<- sea above
      REAL,DIMENSION(IM1,JM1),INTENT(OUT):: FG2(IM2,JM2)
                  !<- sea above
      INTEGER,INTENT(OUT):: ierr
                  !<- 0 for normal and nonzero for abnormal return
      integer,optional,dimension(IM1,JM1),intent(in):: ia,ja,ib,jb,ic,jc
                  !<- sea INTP_INI
      logical,optional,intent(in):: NOSPVAL
                  !<- if present and =.true., no value is assigned to
                  ! FG2(i,j) for those G2 gridpoints (i,j) which lie
                  ! outside G1 domain (i.e. do not lie in any G1 grid
                  ! cell) or are G2 landpoints. Otherwise, special
                  ! value -1.E30 is assigned at such G2 gridpoints
      
      integer ierr1,ivrbs,k,i,j,i1,j1,i1a,j1a,i1b,j1b,i1c,j1c
      real s,x,y,f,x1,y1,f1,x1a,y1a,f1a,x1b,y1b,f1b,x1c,y1c,f1c,
     >very_large_negative
      parameter (very_large_negative=-1.E30)
      logical SPVAL
      logical forgive_G1land_surrounding
C       

c        print*,'C: INTERP ENTERED, IM1,JM1,IM2,JM2: ',IM1,JM1,IM2,JM2
      if ( (PRESENT(ia).neqv.PRESENT(ja)) .or.
     >     (PRESENT(ia).neqv.PRESENT(ib)) .or.
     >     (PRESENT(ia).neqv.PRESENT(jb)) .or.
     >     (PRESENT(ia).neqv.PRESENT(ic)) .or.
     >     (PRESENT(ia).neqv.PRESENT(jc)) ) then
        print*,'INTERP: **ERROR** ia,ja,ib,jb,ic,jc must be all '//
     >  'present or all not present'
        ierr=1
        RETURN
      end if

      if (PRESENT(NOSPVAL)) then
        SPVAL=.not.NOSPVAL
      else
        SPVAL=.true.
      end if

      forgive_G1land_surrounding=.not.SPVAL ! alternative: make
                                            ! forgive_G1land_surrounding
                                            ! an argument

      ierr=0
      ivrbs=10
C
      DO j=1,JM2
      DO i=1,IM2
C
      i1=IG1(i,j)
      j1=JG1(i,j)

      IF (MASK2(i,j).ne.0 .or. i1.le.0) THEN
        if (SPVAL) FG2(i,j)=very_large_negative
        CYCLE
      END IF

      if (present(ia)) then
        i1a=ia(i1,j1)         ! here, it is supposed that the previous
        j1a=ja(i1,j1)         ! IF statement ensures that here i1>0,
        i1b=ib(i1,j1)         ! which, in its turn, ensures that
        j1b=jb(i1,j1)         ! i1,i1a,i1b,i1c stay within [1,IM1] and
        i1c=ic(i1,j1)         ! j1,j1a,j1b,j1c stay within [1,JM1]
        j1c=jc(i1,j1)         !
      else
        i1a=i1+1
        j1a=j1
        i1b=i1+1
        j1b=j1+1
        i1c=i1
        j1c=j1+1
      end if
C
Cb     Bilinear interpolation is used to obtain a G2 gridpoint
Cb  value from the values at the four surrounding G1 gridpoints.
Cb     To prevent interpolation from land, G1 land gridpoint
Cb  values are substituted by the arithmetic mean of the k remaining
Cb  G1 sea gridpoint values (0<k<4) surrounding the given G2
Cb  gridpoint.
Cb
      s=0.
      k=0
      x=LON2(i,j)
      y=LAT2(i,j)
      x1=lon1(i1,j1)
      y1=lat1(i1,j1)
      if (mask1(i1,j1).eq.0) then
        f1=FG1(i1,j1)
        k=k+1
        s=s+f1
      else
        f1=very_large_negative
      end if
      x1a=lon1(i1a,j1a)
      y1a=lat1(i1a,j1a)
      if (mask1(i1a,j1a).eq.0) then
        f1a=FG1(i1a,j1a)
        k=k+1
        s=s+f1a
      else
        f1a=very_large_negative
      end if
      x1b=lon1(i1b,j1b)
      y1b=lat1(i1b,j1b)
      if (mask1(i1b,j1b).eq.0) then
        f1b=FG1(i1b,j1b)
        k=k+1
        s=s+f1b
      else
        f1b=very_large_negative
      end if
      x1c=lon1(i1c,j1c)
      y1c=lat1(i1c,j1c)
      if (mask1(i1c,j1c).eq.0) then
        f1c=FG1(i1c,j1c)
        k=k+1
        s=s+f1c
      else
        f1c=very_large_negative
      end if
      if (k.eq.0) then
        if (forgive_G1land_surrounding) then
          if (SPVAL) FG2(i,j)=very_large_negative
          CYCLE
        end if
        print '(/" INTERP: **ERROR**  Pure G1 landpoint surrounding ",
     >  "is not supposed to occur at this G2 point (i,j) because ",
     >  "IG1(i,j) is positive")'
        print '("  G2 point: (i,j)=(",i3,",",
     >  i3,") , (lambda,phi)=(",f7.2,",",f7.2,
     >  ")")',i,j,LON2(i,j),LAT2(i,j)
        print '(" i1=",i3," j1=",i3)',i1,j1
        print '(" The surrounding G1 points:",4(3x,2f7.2))',
     >  x1,y1,  x1a,y1a,  x1b,y1b,  x1c,y1c
        print '(" mask values at the surrounding G1 points:"/4i8)',
     >  mask1(i1,j1),mask1(i1a,j1a),mask1(i1b,j1b),mask1(i1c,j1c)
        ierr=1
        RETURN
      else if (k.lt.4) then
        s=s/k
        if (f1.eq.very_large_negative) f1=s
        if (f1a.eq.very_large_negative) f1a=s
        if (f1b.eq.very_large_negative) f1b=s
        if (f1c.eq.very_large_negative) f1c=s
      end if
C
C
      CALL BLIN(x1,x1a,x1b,x1c,y1,y1a,y1b,y1c,f1,f1a,f1b,f1c,x,y,f,
     >ivrbs,ierr1)

      if (ierr1.ne.0) then
        print '("INTERP: **ERROR** in BLIN, i,j,i1,j1: ",2i5,2x,2i5)',
     >  i,j,i1,j1
        ierr=2
        if (ivrbs.le.0) RETURN
      end if
      FG2(i,j)=f
Ce
C
      END DO
      END DO
C
      RETURN
      END SUBROUTINE INTERP
C
C***********************************************************************
C
C
      SUBROUTINE BLIN(x1,x2,x3,x4,y1,y2,y3,y4,f1,f2,f3,f4,x,y,f,
     >                 ivrbs,ierr)

      implicit none

      real,intent(in):: x1,x2,x3,x4,y1,y2,y3,y4,f1,f2,f3,f4,x,y
      real,intent(out):: f
      integer,intent(inout):: ivrbs
      integer,intent(out):: ierr

      real dx1,dx2,dx3,dx4,dy1,dy2,dy3,dy4,r12,r23,r34,r41,
     >a1,b1,c1,d1,a2,b2,c2,d2,a,b,c,D,s,sq,t
C
C
C   Bilinear interpolation subroutine.
C   (xi,yi,fi) = data grid & values surrounding model point (x,y)
C   f = interpolated value at the model grid point.
C
      ierr=0
C
Cb  Check if (x,y) lies inside contour 1-2-3-4-1. If not,
Cb  emergency return
      dx1=x1-x
      dy1=y1-y
      dx2=x2-x
      dy2=y2-y
      dx3=x3-x
      dy3=y3-y
      dx4=x4-x
      dy4=y4-y
      r12=dx1*dy2-dx2*dy1
      r23=dx2*dy3-dx3*dy2
      r34=dx3*dy4-dx4*dy3
      r41=dx4*dy1-dx1*dy4
      if ( r12*r23.lt.0. .or. r23*r34.lt.0. .or. r34*r41.lt.0. ) then
        if (ivrbs.gt.0) then
        print '(/'' BLIN: (x,y) POINT NOT INSIDE THE TETRAGON 1234''/
     >  ''        (contour 1-2-3-4-1 does not go round point (x,y)''/
     >  ''         either clockwise or counterclockwise)'')'
        print '(''        (x1,y1): '',1p2e16.7)',x1,y1
        print '(''        (x2,y2): '',1p2e16.7)',x2,y2
        print '(''        (x3,y3): '',1p2e16.7)',x3,y3
        print '(''        (x4,y4): '',1p2e16.7)',x4,y4
        print '(''            (x,y): '',1p2e17.8)',x,y
        ivrbs=ivrbs-1
        end if
        ierr=1
        return
      end if
Ce
C
      a1=x1-x2+x3-x4
      b1=-x1+x4
      c1=-x1+x2
      d1=x1-x
      a2=y1-y2+y3-y4
      b2=-y1+y4
      c2=-y1+y2
      d2=y1-y 
      a=a1*b2-a2*b1
      b=a1*d2-a2*d1+c1*b2-c2*b1
      c=c1*d2-c2*d1
      D=b**2-4.*a*c
      if (D.lt.0.) D=0.
      sq=sqrt(D)
      if (c.le.0.) then
        if (b.ge.0.) then
          s=2.*c/(-b-sq)
        else
          s=(-b+sq)/(2.*a)
        end if
      else
        if (b.le.0.) then
          s=2.*c/(-b+sq)
        else
          s=(-b-sq)/(2.*a)
        end if
      end if
      t=-(b1*s+d1)/(a1*s+c1)
      f=f1*(1.-s)*(1.-t)+f2*(1.-s)*t+f3*s*t+f4*s*(1.-t)
      return
      END SUBROUTINE BLIN
C
C************************************************************
C
      SUBROUTINE EXTRAP(IM,JM,f,mask,intpmask,ierr,
     >itmaxmax,itmax,lakeval,ivrbs,it,f_r)

C  Extrapolates 2D array f(IM,JM) initially defined at gridpoints
C  (i,j) for which intpmask(i,j)=.true. Gridpoints with
C  mask(i,j)=.false. are considered not belonging to the domain;
C  for such points, existing values f(i,j) are ignored and no new
C  values are assigned. Extrapolation is done along grid steps that
C  are within the connected component of the domain.
C     If an entire connected component needs extrapolation (no
C  initially defined input), the routine fills it at each gridpoint
C  (i,j) with  lakeval  if this argument is present or with f_r(i,j) if
C   f_r  is present and  lakeval  is not, and if the connected component
C  is not the entire domain; otherwise returns with a positive  ierr .
C     If argument it(IM,JM) is present, the routine computes, for
C  each gridpoint being filled, the minimum number it(i,j) of gridsteps
C  to a pre-filled (i.e. for which intpmask(i,j)=.true.) gridpoint
C  (i,j) (whose value f(i,j), possibly along with other values, is
C  used for extrapolation). In this case, if  itmax  is present, it
C  is assigned the max. value of it(,); if  itmaxmax  is present, points
C  with it(,)>itmaxmax are not filled with any value, nor is it(,)
C  computed for these points.
C     If it(IM,JM) is not present, itmax, if present, is assigned the
C  maximum of minimum number of gridsteps to a gridpoint with a
C  previously defined value, which may be either an initially defined
C  (input) value (where intpmask(,)=.true.) or an already extrapolated
C  value. itmaxmax, if present, puts a limit on itmax so that no
C  extrapolation is done if the nearest gridpoint with a previously
C  defined value is farther than  itmaxmax  gridsteps. Thus, note
C  that arguments itmax, itmaxmax have different meaning depending
C  on the presence of argument  it .
C     Optional argument  f_r  serves those gripoints which failed to
C  be filled by extrapolation and smooth averaging at extrapolation
C  points is carried out; see also below.

      implicit none

      integer,intent(in):: IM,JM                         !   see
      real,intent(inout):: f(IM,JM)                      !  above
      logical,intent(in):: mask(IM,JM),intpmask(IM,JM)   !
      integer,intent(out):: ierr     ! if there are lakes and lakeval
                                     ! is present, retuns a negative;
                                     ! otherwise if return is abnormal,
                                     ! including presence of a lake
                                     ! when lakeval is not present,
                                     ! returns a positive; otherwise
                                     ! returns 0
      integer,optional,intent(in):: itmaxmax             !   see
      integer,optional,intent(out):: itmax               !  above
      real,optional,intent(in):: lakeval                 !
      integer,optional,intent(in):: ivrbs     ! prints lake info if >0
      integer,optional,intent(out):: it(IM,JM)           ! see above
      real,optional,intent(in):: f_r(IM,JM) ! reference values for f.
                                            ! If present, fills the
                                            ! unfilled gridpoints and
                                            ! extrapolated values f(i,j)
                                            ! are replaced by weighted
                                            ! average betw. themselves
                                            ! and f_r(i,j), with smooth
                                            ! transition. Must only be
                                            ! present together with
                                            !  itmaxmax  and  it .

      integer i,j,i1,j1,i2,j2,ii,jj,i1p,j1p,i2p,j2p,k,k1,n,m,ivr,itmm
      logical,dimension(IM,JM):: cc,imask
      logical nolake
      real s,w
C

      if (PRESENT(f_r) .and. .not.(PRESENT(itmaxmax).and.
     >PRESENT(it))) then
        print*,'EXTRAP: f_r present but itmaxmax or it is not, aborted'
        ierr=3
        RETURN
      end if

      ierr=0
      imask=intpmask
      if (PRESENT(ivrbs)) then
        ivr=ivrbs
      else
        ivr=0
      end if

      if (PRESENT(itmax)) itmax=0

      DO j=1,JM
      iloop: DO i=1,IM

      m=0

      IF (mask(i,j) .AND. .NOT.imask(i,j)) THEN

        i1=i
        i2=i
        j1=j
        j2=j
        cc(i,j)=.true.
        n=0
        s=0.

        do while (n.eq.0)

          m=m+1

          if (PRESENT(itmaxmax)) then
            if (m.gt.itmaxmax) then
              if (PRESENT(f_r)) f(i,j)=f_r(i,j)
              CYCLE iloop
            end if
          end if

          i1p=i1
          i2p=i2
          j1p=j1
          j2p=j2
          i1=max(i1-1,1)
          i2=min(i2+1,IM)
          j1=max(j1-1,1)
          j2=min(j2+1,JM)

          if (i1p.eq.1.and.i2p.eq.IM.and.j1p.eq.1.and.j2p.eq.JM) then
            print '("EXTRAP: FAILED/TERMINATED. After ",i5," steps ",
     >      "connected component for i=",i5,", j=",i5,
     >      " became the entire domain")',m,i,j 
            call PRM
            ierr=1
            RETURN
          end if

          nolake=.false.
          if (j1.lt.j1p) then
            cc(i1p:i2p,j1)=cc(i1p:i2p,j1p) .and. mask(i1p:i2p,j1)
            nolake=ANY(cc(i1p:i2p,j1))
            if (i1.lt.i1p) cc(i1,j1)=.false.
          end if
          if (i2.gt.i2p) then
            cc(i2,j1p:j2p)=cc(i2p,j1p:j2p) .and. mask(i2,j1p:j2p)
            nolake=nolake.or.ANY(cc(i2,j1p:j2p))
            if (j1.lt.j1p) cc(i2,j1)=.false.
          end if
          if (j2.gt.j2p) then
            cc(i1p:i2p,j2)=cc(i1p:i2p,j2p) .and. mask(i1p:i2p,j2)
            nolake=nolake.or.ANY(cc(i1p:i2p,j2))
            if (i2.gt.i2p) cc(i2,j2)=.false.
          end if
          if (i1.lt.i1p) then
            cc(i1,j1p:j2p)=cc(i1p,j1p:j2p) .and. mask(i1,j1p:j2p)
            nolake=nolake.or.ANY(cc(i1,j1p:j2p))
            if (j2.gt.j2p) cc(i1,j2)=.false.
          end if

          if (.not.nolake) then
            if (ivr.gt.0 .or.
     >      .not. (PRESENT(lakeval).or.PRESENT(f_r)) ) then
              print '("EXTRAP: Domain not connected. On step",i5,
     >        " i=",i5," j=",i5," turned out a lake point")',m,i,j
              call PRM
            end if
            if (PRESENT(lakeval) .or. PRESENT(f_r)) then
              imask(i,j)=.true.
              if (PRESENT(lakeval)) then
                f(i,j)=lakeval
                if (ivr.gt.0) then
                  print*,'EXTRAP: lake value lakeval=',lakeval,
     >            ' assigned'
                end if
              else
                f(i,j)=f_r(i,j)
                if (ivr.gt.0) then
                  print*,'EXTRAP: at lake point, f_r value ',f_r(i,j),
     >            ' assigned'
                end if
              end if
              ierr=ierr-1
              CYCLE iloop
            else
              print*,'EXTRAP: connectedness is required when '//
     >        'neither lakeval nor f_r is present. TERMINATED'
              ierr=2
              RETURN
            end if
          end if
          do k=1,2
            do ii=i1+1,i2
              if (cc(ii-1,j1)) cc(ii,j1)=mask(ii,j1)
            end do
            do jj=j1+1,j2
              if (cc(i2,jj-1)) cc(i2,jj)=mask(i2,jj)
            end do
            do ii=i2-1,i1,-1
              if (cc(ii+1,j2)) cc(ii,j2)=mask(ii,j2)
            end do
            do jj=j2-1,j1,-1
              if (cc(i1,jj+1)) cc(i1,jj)=mask(i1,jj)
            end do

            do jj=j1+1,j2
              if (cc(i1,jj-1)) cc(i1,jj)=mask(i1,jj)
            end do
            do ii=i1+1,i2
              if (cc(ii-1,j2)) cc(ii,j2)=mask(ii,j2)
            end do
            do jj=j2-1,j1,-1
              if (cc(i2,jj+1)) cc(i2,jj)=mask(i2,jj)
            end do
            do ii=i2-1,i1,-1
              if (cc(ii+1,j1)) cc(ii,j1)=mask(ii,j1)
            end do
          end do

          IF (PRESENT(it)) THEN
            k1=IM+JM
            if (PRESENT(itmaxmax)) then
              itmm=itmaxmax
            else
              itmm=k1
            end if
            do ii=i1+1,i2
              if (cc(ii,j1).and.imask(ii,j1)) then
                k=it(ii,j1)+m
                k1=min(k,k1)
                if (k.le.itmm) then
                  s=s+f(ii,j1)
                  n=n+1
                end if
              end if  
            end do
            do jj=j1+1,j2
              if (cc(i2,jj).and.imask(i2,jj)) then
                k=it(i2,jj)+m
                k1=min(k,k1)
                if (k.le.itmm) then
                  s=s+f(i2,jj)
                  n=n+1
                end if
              end if  
            end do
            do ii=i2-1,i1,-1
              if (cc(ii,j2).and.imask(ii,j2)) then
                k=it(ii,j2)+m
                k1=min(k,k1)
                if (k.le.itmm) then
                  s=s+f(ii,j2)
                  n=n+1
                end if  
              end if  
            end do
            do jj=j2-1,j1,-1
              if (cc(i1,jj).and.imask(i1,jj)) then
                k=it(i1,jj)+m
                k1=min(k,k1)
                if (k.le.itmm) then
                  s=s+f(i1,jj)
                  n=n+1
                end if  
              end if  
            end do
          ELSE
            do ii=i1+1,i2
              if (cc(ii,j1).and.imask(ii,j1)) then
                s=s+f(ii,j1)
                n=n+1
              end if  
            end do
            do jj=j1+1,j2
              if (cc(i2,jj).and.imask(i2,jj)) then
                s=s+f(i2,jj)
                n=n+1
              end if  
            end do
            do ii=i2-1,i1,-1
              if (cc(ii,j2).and.imask(ii,j2)) then
                s=s+f(ii,j2)
                n=n+1
              end if  
            end do
            do jj=j2-1,j1,-1
              if (cc(i1,jj).and.imask(i1,jj)) then
                s=s+f(i1,jj)
                n=n+1
              end if  
            end do
          END IF

        end do

        f(i,j)=s/n
        if (PRESENT(it)) then
          it(i,j)=k1
          if (PRESENT(f_r)) then
            w=max(0.,(itmaxmax+1.-k1)/(itmaxmax+1.))
            f(i,j)=w*f(i,j)+(1.-w)*f_r(i,j)
          end if
        else
          k1=m
        end if
        if (PRESENT(itmax)) itmax=max(itmax,k1)
        imask(i,j)=.true.

      END IF

      END DO iloop
      END DO

      RETURN

      CONTAINS
        subroutine PRM
            i1=max(1,i-10)
            i2=min(IM,i+10)
            j1=max(1,j-10)
            j2=min(JM,j+10)
            do jj=j1,j2
              print*,'EXTRAP: j=',jj,' i from ',i1,' to ',i2,'  mask: ',
     >        mask(i1:i2,jj)
            end do
            do jj=j1,j2
              print*,'EXTRAP: j=',jj,' i from ',i1,' to ',i2,' imask: ',
     >        imask(i1:i2,jj)
            end do
        end subroutine PRM
      END SUBROUTINE EXTRAP
C
C************************************************************
C
      END module mod_interp
