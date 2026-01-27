      program ncodaz_inc2mom6nc_glb
      use mod_mom6  ! HYCOM mom6 array interface
      use mod_za    ! HYCOM array I/O interface
      use mod_ncio  ! A. Srinivasan tsis netcdf module
      use netcdf    ! Netcdf module

c
c modified from a COAPS utility by Alex Bozec
      implicit none
c
c --- convert NCODA layer increments to MOM6 Netcdf for inc. update
c --- requires MOM6 restart template and 
c --- the HYCOM regional.grid for the MOM6 domain.
c --- uses the ncoda increments in the background layers, and the 
c --- ncoda binary containing the background state layer thicknesses 
c --- which add to depth
c
      character*256    flnm_it,flnm_is,flnm_iu,flnm_iv
      character*256    flnm_ots,flnm_ouv,filename
      character*256    flnm_t,flnm_s,flnm_u,flnm_v,flnm_p,flnm_h
      logical          larctic,lsymetr
      integer          i,ia,j,k,ntq,mtq,mro,ib,jb
      integer          kmax
      integer          yrflag
      real             xmin,xmax,misval
      real             zl
      integer          itest, jtest
      double precision time3(3)
c --- NCODA increments
      integer   kncoda
      real, allocatable :: incoda(:,:)
      real, allocatable :: sum(:,:)

      real, allocatable :: tncoda(:,:,:),sncoda(:,:,:)
     &                    ,pncoda(:,:,:)
      real, allocatable :: uncoda(:,:,:),vncoda(:,:,:)
      real, allocatable :: arrayncoda(:,:,:)

c --- MOM6 restart fields
      integer   fid
      character(:),allocatable   :: vname
      integer                    :: vtype
      integer                    :: vdim1d(1),vdim3d(3),vdim4d(4)
c --- 4D fields
      real, allocatable, dimension (:,:,:,:) ::
     &   u_inc,v_inc,temp_inc,saln_inc
      real, allocatable, dimension (:,:,:,:) :: zh
c --- 1D fields
      real, allocatable, dimension (:) ::
     & lath,lonh,latq,lonq,Layer,intd,time
     & ,zz


      lp     = 6
      yrflag = 3

c
c --- 'flnm_it' = name of mom6 restart file containing pot. temp.  (input)
c --- 'flnm_is' = name of mom6 restart file containing salinity (input)
c --- 'flnm_iu' = name of mom6 restart file containing U  (input)
c --- 'flnm_iv' = name of mom6 restart file containing V (input)
c
c --- 'flnm_ots' = name of MOM6 increment file for T,S and h (output)
c --- 'flnm_ouv' = name of MOM6 increment file for U, V and h (output)
c
      read (*,'(a)') flnm_it
      read (*,'(a)') flnm_is
      read (*,'(a)') flnm_iu
      read (*,'(a)') flnm_iv
      write (lp,'(2a)') ' input MOM6 T file: ',trim(flnm_it)
      write (lp,'(2a)') ' input MOM6 S file: ',trim(flnm_is)
      write (lp,'(2a)') ' input MOM6 U file: ',trim(flnm_iu)
      write (lp,'(2a)') ' input MOM6 V file: ',trim(flnm_iv)

c     read (*,'(a)') flnm_h
c     write (lp,'(2a)') 'input ncoda-written background state h file:',
c    &      trim(flnm_h)
c     call flush(lp)
      read (*,'(a)') flnm_ots
      write (lp,'(2a)') 'output MOM6 TSh  file: ',trim(flnm_ots)
      read (*,'(a)') flnm_ouv
      write (lp,'(2a)') 'output MOM6 UV  file: ',trim(flnm_ouv)
      if (trim(flnm_ots) .eq. trim(flnm_ouv)) then
         write (lp,'(2a)') 'output MOM6 TSh and UV should be different'
         call flush(lp)
         stop
      endif
      call flush(lp)
c --- 'ii    ' = zonal size of the NCODA increments
c --- 'jj    ' = meridional size of the MOM6 ncoda increments
c --- 'kk    ' = number of layers in restart
!c --- 'lsymet' = T model is symmetric, F model is non-symmetric
!c --- 'larcti' = T model has an Arctic patch
      call blkini(ii,'ii    ')
      call blkini(jj,'jj    ')
      call blkini(kk,'kk    ')
      !call blkinl(lsymetr,'lsymet')
      !call blkinl(larctic,'larcti')



c
c --- allocate a few state variables 
c
      


      call mom6_alloc
c --- mom6 dimensions
c
      call rd_dimen(ntq,mto,kk,mro, flnm_iu,'u')
      call rd_dimen(nto,mtq,kk,mro, flnm_iv,'v')
      misval = -1.e20  !no missing values in input fields
c
      lsymetr = mtq .eq. jj+1 .and. ntq .eq. ii+1
      !larctic = mto .eq. jdm-1 .and. nto .eq. idm
c
      write(lp,*)
      write(lp,*) 'nto,mto  = ',nto,mto
      write(lp,*) 'ntq,mtq  = ',ntq,mtq
      write(lp,*) 'ii,jj,kk = ',ii, jj, kk
      write(lp,*) 'lsymetr  = ',lsymetr
      !write(lp,*) 'larctic  = ',larctic
      write(lp,*)
      write(lp,*) 'misval  = ',misval
      write(lp,*)
      call zhflsh(lp)
c
c
c --- some array allocations are in-line below
c
      write(lp,*) 'before Allocate'

      allocate(lath(mto))
      allocate(lonh(nto))
      allocate(latq(mtq))
      allocate(lonq(ntq))
      allocate(Layer(kk))

      allocate(time(1))
      write(lp,*) 'After allocate'

c
c --- read the mom6 file.
c
c     irec = 1
c --- read invariant variables
      write(lp,*) 'File:',flnm_it
      !write(lp,*) 'fid:',fid

c --- get coordinates t-points
      call nciopn(flnm_it,fid)
      call nciorv(flnm_it,fid,"lath",lath(:))
      call nciorv(flnm_it,fid,"lonh",lonh(:))
      call nciorv(flnm_it,fid,"Layer",layer(:))
      call nciorv(flnm_it,fid,"Time",time(:))
      call nciocl(flnm_it,fid)
c --- get coordinates u-points
      call nciopn(flnm_iu,fid)
      call nciorv(flnm_iu,fid,"lonq",lonq(:))
      call nciocl(flnm_iu,fid)
c --- get coordinates v-points
      call nciopn(flnm_iv,fid)
      call nciorv(flnm_iv,fid,"latq",latq(:))
      call nciocl(flnm_iv,fid)


c ---  NCODA inc
c --- 'kncoda' = number   of ncoda levels
c --- 'flnm_t' = name of ncoda temperature increment file, or "NONE" to exit
c --- 'flnm_s' = name of ncoda salinity    increment file
c --- 'flnm_u' = name of ncoda u-velocity  increment file, or "NONE"
c --- 'flnm_v' = name of ncoda v-velocity  increment file, or "NONE"
c --- 'flnm_p' = name of ncoda density displacement  file, or "NONE"
c --- 'flnm_h' = name of the background field layer thick. as ncoda binary

      call blkini2(i,j,'itest ','kncoda')
      if (j.eq.1) then 
        itest  = i     
        call blkini(jtest, 'jtest ')
        call blkini(kncoda,  'kncoda')
      else
        itest  = 0     
        jtest  = 0     
        kncoda   = i     
      endif
      
      if (kncoda.ne.kk) then
        write(6,*)'error, kncoda has to be kk for lyr option'
        stop
      endif
c --- allocate ncoda fields
      allocate(temp_inc(nto,mto,kncoda,1))
      allocate(saln_inc(nto,mto,kncoda,1))
      allocate(u_inc(ntq,mto,kncoda,1))
      allocate(v_inc(nto,mtq,kncoda,1))
      allocate(zh(nto,mto,kncoda,1))

c --- read the nominal layer zz, for example the values in coordinate 'Layer' 
c --- in the mom6 archives
      allocate (zz(kncoda))
      do k=1,kncoda
         call blkinr(zl,'zl    ','(a6," =",f10.4)')
         zz(k)=zl
      enddo

c --- zh is the background field thickness, which is already normalized to add to depth
c ----read from an ncoda restart file
      read (*,'(a)') flnm_t
      write (lp,'(2a)') 'Tncoda file: ',trim(flnm_t)
      if     (flnm_t.eq."NONE") then
         write(lp,*)
         write(lp,*) '***** EXIT ncoda_inc2mom6ncr *****'
         write(lp,*)
         call flush(lp)
         stop !ncoda_cycle
      endif
      read (*,'(a)') flnm_s
      write (lp,'(2a)') 'Sncoda file: ',trim(flnm_s)
      read (*,'(a)') flnm_u
      write (lp,'(2a)') 'Uncoda file: ',trim(flnm_u)
      read (*,'(a)') flnm_v
      write (lp,'(2a)') 'Vncoda file: ',trim(flnm_v)
      read (*,'(a)') flnm_p
      write (lp,'(2a)') 'Pncoda file: ',trim(flnm_p)
      read (*,'(a)') flnm_h
      write (lp,'(2a)') 'h background file: ',trim(flnm_h)

C

c --- read increments
c --- deallocate these arrays at end of ncoda_cycle loop
      allocate( tncoda(ii,jj,kncoda),
     &          sncoda(ii,jj,kncoda),
     &          pncoda(ii,jj,kncoda),
     &          arrayncoda(ii,jj,kncoda)) 
c
      print*,'Alex idm,jdm,jdmn,kncoda:',ii,jj,kncoda

      write(lp,*) 'open  ',trim(flnm_t)
      call flush(lp)
c read all EMC ncoda binaries as stream 
      open(9,file=flnm_t,form='unformatted',access='stream',
     &     status='old')

        read(unit=9) arrayncoda
      do k=1,kncoda
       tncoda(:,:,k)=arrayncoda(:,1:jj,k)
      enddo
c***
      do k=1,kncoda
        write(lp,*) 'Temp incr = ',minval(tncoda(:,:,k)),
     &                             maxval(tncoda(:,:,k))
        if (min(itest,jtest).gt.0) 
     &  write(lp,*) itest,jtest,'Temp incr= ', tncoda(itest,jtest,k)
      enddo

      close(unit=9)
      write(lp,*) 'close ',trim(flnm_t)
      call flush(lp)
c
      write(lp,*) 'open  ',trim(flnm_s)
      call flush(lp)
      open(9,file=flnm_s,form='unformatted',access='stream',
     &     status='old')
        read(unit=9) arrayncoda
      do k=1,kncoda
       sncoda(:,:,k)=arrayncoda(:,1:jj,k)
      enddo
c***
      do k=1,kncoda
        write(lp,*) 'Salt incr = ',minval(sncoda(:,:,k)),
     &                             maxval(sncoda(:,:,k))
        if (min(itest,jtest).gt.0) 
     &  write(lp,*) 'Salt incr= ', sncoda(itest,jtest,k)
      enddo

      close(unit=9)
      write(lp,*) 'close ',trim(flnm_s)
      call flush(lp)
c
      if     (flnm_u.ne."NONE") then
c ---   deallocate these arrays at end of ncoda_cycle loop
        allocate( uncoda(ii,jj,kncoda),
     &            vncoda(ii,jj,kncoda) )
c
        write(lp,*) 'open  ',trim(flnm_u)
        call flush(lp)


       open(9,file=flnm_u,form='unformatted',access='stream',
     &      status='old')
        read(unit=9) arrayncoda

      do k=1,kncoda
        do j=1,jj
          do i=1,ii
c           if (i.eq.1) then
c             ib=ii !periodic
c           else
c             ib=i-1
c           endif
c            uncoda(i,j,k)=0.5*(arrayncoda(i,j,k)+arrayncoda(ib,j,k))
c uncoda is re-staggered in ncoda
            uncoda(i,j,k)=arrayncoda(i,j,k)
          enddo !i
        enddo !j
      enddo !k
c***
      do k=1,kncoda
        write(lp,*) 'uvel incr = ',minval(uncoda(:,:,k)),
     &                             maxval(uncoda(:,:,k))
      enddo

        close(unit=9)
        write(lp,*) 'close ',trim(flnm_u)
        call flush(lp)
c
        write(lp,*) 'open  ',trim(flnm_v)
        call flush(lp)
       open(9,file=flnm_v,form='unformatted',access='stream',
     &      status='old')
        read(unit=9) arrayncoda
         
      do k=1,kncoda
        do j=1,jj
c         jb=max(j-1,1)
          do i=1,ii
c           vncoda(i,j,k)=0.5*(arrayncoda(i,j,k)+arrayncoda(i,jb,k))
c vncoda is re-staggered in ncoda
           vncoda(i,j,k)=arrayncoda(i,j,k)
          enddo !i
        enddo !j
      enddo !k
c***
      do k=1,kncoda
        write(lp,*) 'vvel incr = ',minval(vncoda(:,:,k)),
     &                             maxval(vncoda(:,:,k))
      enddo

        close(unit=9)
        write(lp,*) 'close ',trim(flnm_v)
        call flush(lp)
      endif !u&vncoda
c
      if     (flnm_p.ne."NONE") then
        write(lp,*) 'open  ',trim(flnm_p)
        call flush(lp)
      open(9,file=flnm_p,form='unformatted',access='stream',
     &     status='old')
        read(unit=9) arrayncoda
      do k=1,kncoda
       pncoda(:,:,k)=arrayncoda(:,1:jj,k)
      enddo
c***
      do k=1,kncoda
        write(lp,*) 'intd incr = ',minval(pncoda(:,:,k)),
     &                             maxval(pncoda(:,:,k))
      enddo
        close(unit=9)
        write(lp,*) 'close ',trim(flnm_p)
        call flush(lp)
      endif !pncoda


c --- get MOM6 z-layers from an ncoda restart
      open(9, file=flnm_h, form='unformatted', access='stream', 
     &     status='old')
c --- ZG sum h has to add to depth, already does
      read(9)arrayncoda
      do k=1,kncoda
      do j= 1,jj
        do i= 1,ii
          zh(i,j,k,1)=arrayncoda(i,j,k)
        enddo
      enddo
        write(lp,*) 'zh backgr = ',minval(zh(:,:,k,1)),
     &                             maxval(zh(:,:,k,1))
      enddo

c --- store increments in proper dimension arrays
      temp_inc(:,:,:,1) = 0.0
      saln_inc(:,:,:,1) = 0.0 
      do j= 1,mto
        do i= 1,nto
          temp_inc(i,j,:,1)=tncoda(i,j,:)
          saln_inc(i,j,:,1)=sncoda(i,j,:)
        enddo
      enddo

      u_inc(:,:,:,1) = 0.0
      do j= 1,mto
        do i= 1,nto
          u_inc(i,j,:,1)=uncoda(i,j,:)
        enddo
      enddo
c --- fill last column
      if (lsymetr) then
        do j= 1,mto
          i=ntq
          u_inc(i,j,:,1)=u_inc(1,j,:,1)
        enddo
      endif

      v_inc(:,:,:,1) = 0.0
      do j= 1,mto
        do i= 1,nto
          v_inc(i,j,:,1)=vncoda(i,j,:)
        enddo
      enddo
c --- fill last row
      if (lsymetr) then
!!Alex not do anything for now since no velocity correction in Arctic
!NOTE in the future see MOM6 handles Arctic patch and reproduce for v
!        do i= 1,nto
!          j=mtq
!          v_inc(i,j,:,1)=v_inc(i,mtq-1,:,1)
!        enddo
      endif
c --- write MOM6 TS,h increment file
      !create file
      filename=flnm_ots

      CALL nciocf(filename,fid)

       !create dimension and coordinate variable 1
       vname="lonh"
       vtype=NF90_DOUBLE
       vdim1d(1)=1
       CALL nciodd(filename,fid,vname,nto)
       CALL nciocv(filename,fid,vname,vtype,vdim1d)
       CALL nciowa(filename,fid,vname,'long_name','Longitude')
       CALL nciowa(filename,fid,vname,'cartesian_axis','X')
       CALL nciowa(filename,fid,vname,'units','degrees_east')


       !create dimension and coordinate variable 2
       vname="lath"
       vtype=NF90_DOUBLE
       vdim1d(1)=2
       CALL nciodd(filename,fid,vname,mto)
       CALL nciocv(filename,fid,vname,vtype,vdim1d)
       CALL nciowa(filename,fid,vname,'long_name','Latitude')
       CALL nciowa(filename,fid,vname,'cartesian_axis','Y')
       CALL nciowa(filename,fid,vname,'units','degrees_north')

       !create dimension and coordinate variable 5
c       vname="Level"
       vname="Layer"
       vtype=NF90_DOUBLE
       vdim1d(1)=3
       CALL nciodd(filename,fid,vname,kncoda)
c      CALL nciodd(filename,fid,vname,kncoda+1)
       CALL nciocv(filename,fid,vname,vtype,vdim1d)
c       CALL nciowa(filename,fid,vname,'long_name','z-levels')
       CALL nciowa(filename,fid,vname,'long_name','Layer z-rho')
       CALL nciowa(filename,fid,vname,'cartesian_axis','Z')
       CALL nciowa(filename,fid,vname,'units','meter')
       CALL nciowa(filename,fid,vname,'positive','up')
       print*,'after Dim'

!       vname="Time"
!       vtype=NF90_DOUBLE
!       vdim1d(1)=6
!       CALL nciodd(filename,fid,vname,NF90_UNLIMITED)
!       CALL nciocv(filename,fid,vname,vtype,vdim1d)
!       CALL nciowa(filename,fid,vname,'long_name','Time')
!       CALL nciowa(filename,fid,vname,'cartesian_axis','T')
!       CALL nciowa(filename,fid,vname,'units','days')


       !create data variable
       vname="pt_inc"
       vtype=NF90_DOUBLE
       vdim3d=(/1,2,3/)
       call nciocv(filename,fid,vname,vtype,vdim3d)
       call nciowa(filename,fid,vname,'long_name',
     &      'NCODA Potential Temperature increments')
       call nciowa(filename,fid,vname,'units','degC')

       !create data variable
       vname="s_inc"
       vtype=NF90_DOUBLE
       vdim3d=(/1,2,3/)
       call nciocv(filename,fid,vname,vtype,vdim3d)
       call nciowa(filename,fid,vname,'long_name',
     &                  'NCODA Salinity increments')
       call nciowa(filename,fid,vname,'units','PPT')

       !create data variable
       vname="zh"
       vtype=NF90_DOUBLE
       vdim3d=(/1,2,3/)
       call nciocv(filename,fid,vname,vtype,vdim3d)
       call nciowa(filename,fid,vname,'long_name',
     &          'MOM6 z-levels thickness')
       call nciowa(filename,fid,vname,'units','m')

       !end define mode
       call ncioed(filename,fid)
       print*, 'after var defnition'

       ! write data into variables
       call nciowv(filename,fid,"lath",lath)
       call nciowv(filename,fid,"lonh",lonh)
       call nciowv(filename,fid,"Layer",zz(1:kncoda))
       print*, 'after writing Layer'
       !call nciowv(filename,fid,"Time",time)
       print*, 'after writing dim'

       call nciowv(filename,fid,"pt_inc",temp_inc)
       write(lp,*) 'Pot. temperature inc. written'
       call nciowv(filename,fid,"s_inc",saln_inc)
       write(lp,*) 'Salinity inc. written'
       call nciowv(filename,fid,"zh",zh)
       write(lp,*) 'backgrnd thk.  written'
C
       ! close file
       call nciocl(filename,fid)

c --- write MOM6 U,V,h increment file
      !create file
      filename=flnm_ouv

      CALL nciocf(filename,fid)

       !create dimension and coordinate variable 1
       vname="lonh"
       vtype=NF90_DOUBLE
       vdim1d(1)=1
       CALL nciodd(filename,fid,vname,nto)
       CALL nciocv(filename,fid,vname,vtype,vdim1d)
       CALL nciowa(filename,fid,vname,'long_name','Longitude')
       CALL nciowa(filename,fid,vname,'cartesian_axis','X')
       CALL nciowa(filename,fid,vname,'units','degrees_east')


       !create dimension and coordinate variable 2
       vname="lath"
       vtype=NF90_DOUBLE
       vdim1d(1)=2
       CALL nciodd(filename,fid,vname,mto)
       CALL nciocv(filename,fid,vname,vtype,vdim1d)
       CALL nciowa(filename,fid,vname,'long_name','Latitude')
       CALL nciowa(filename,fid,vname,'cartesian_axis','Y')
       CALL nciowa(filename,fid,vname,'units','degrees_north')

       !create dimension and coordinate variable 5
       vname="Level"
       vtype=NF90_DOUBLE
       !vdim1d(1)=5
       vdim1d(1)=3
       CALL nciodd(filename,fid,vname,kncoda)
       CALL nciocv(filename,fid,vname,vtype,vdim1d)
       CALL nciowa(filename,fid,vname,'long_name','z-levels')
       CALL nciowa(filename,fid,vname,'cartesian_axis','Z')
       CALL nciowa(filename,fid,vname,'units','meter')
       CALL nciowa(filename,fid,vname,'positive','up')
       print*,'after Dim'

       !create dimension and coordinate variable 3
       vname="lonq"
       vtype=NF90_DOUBLE
       !vdim1d(1)=3
       vdim1d(1)=4
       CALL nciodd(filename,fid,vname,ntq)
       CALL nciocv(filename,fid,vname,vtype,vdim1d)
       CALL nciowa(filename,fid,vname,'long_name','Longitude')
       CALL nciowa(filename,fid,vname,'cartesian_axis','X')
       CALL nciowa(filename,fid,vname,'units','degrees_east')

       !create dimension and coordinate variable 4
       vname="latq"
       vtype=NF90_DOUBLE
       !vdim1d(1)=4
       vdim1d(1)=5
       CALL nciodd(filename,fid,vname,mtq)
       CALL nciocv(filename,fid,vname,vtype,vdim1d)
       CALL nciowa(filename,fid,vname,'long_name','Latitude')
       CALL nciowa(filename,fid,vname,'cartesian_axis','Y')
       CALL nciowa(filename,fid,vname,'units','degrees_north')

!       vname="Time"
!       vtype=NF90_DOUBLE
!       vdim1d(1)=6
!       CALL nciodd(filename,fid,vname,NF90_UNLIMITED)
!       CALL nciocv(filename,fid,vname,vtype,vdim1d)
!       CALL nciowa(filename,fid,vname,'long_name','Time')
!       CALL nciowa(filename,fid,vname,'cartesian_axis','T')
!       CALL nciowa(filename,fid,vname,'units','days')


       !create data variable
       vname="u_inc"
       vtype=NF90_DOUBLE
       vdim3d=(/4,2,3/)
       call nciocv(filename,fid,vname,vtype,vdim3d)
       call nciowa(filename,fid,vname,'long_name',
     &              'NCODA Zonal velocity increments')
       call nciowa(filename,fid,vname,'units','m s-1')

       !create data variable
       vname="v_inc"
       vtype=NF90_DOUBLE
       vdim3d=(/1,5,3/)
       call nciocv(filename,fid,vname,vtype,vdim3d)
       call nciowa(filename,fid,vname,'long_name',
     &         'NCODA Meridional velocity increments')
       call nciowa(filename,fid,vname,'units','m s-1')

       !end define mode
       call ncioed(filename,fid)
       print*, 'after var defnition'

       ! write data into variables
       call nciowv(filename,fid,"lath",lath)
       call nciowv(filename,fid,"lonh",lonh)
       call nciowv(filename,fid,"latq",latq)
       call nciowv(filename,fid,"lonq",lonq)
       call nciowv(filename,fid,"Level",zz(1:kncoda))
       !call nciowv(filename,fid,"Time",time)
       print*, 'after writing dim'

       call nciowv(filename,fid,"u_inc",u_inc)
       write(lp,*) 'U-vel inc. written'
       call nciowv(filename,fid,"v_inc",v_inc)
       write(lp,*) 'V-vel inc. written'
C
       ! close file U,V,h
       call nciocl(filename,fid)

       end program ncodaz_inc2mom6nc_glb

      subroutine m2h_p(f_nc,nto,mto,kk,misval,
     &                 field,ii,jj, lsymetr,larctic)
      implicit none
c
      logical lsymetr,larctic
      integer nto,mto,kk,ii,jj
      real    f_nc(nto,mto,kk),misval,field(ii,jj,kk)
c
c --- spval  = hycom data void marker, 2^100 or about 1.2676506e30
******real, parameter :: spval=2.0**100
      real, parameter :: spval=0.0  !no spval, use 0.0
c
c --- convert p-grid mom6 array to hycom.
c
      integer i,ia,j,k
c
      do k= 1,kk
        do j= 1,mto
          do i= 1,nto
            if     (f_nc(i,j,k).ne.misval) then
               field(i,j,k) = f_nc(i,j,k)
            else
               field(i,j,k) = spval
            endif
          enddo !i
        enddo !j
        if     (lsymetr) then
          do i= 1,nto
            field(i,jj,k) = spval
          enddo !i
          do j= 1,jj
            field(ii,j,k) = spval
          enddo !j
        elseif (larctic) then  !p-grid scalar field, mto=jj-1
          do i= 1,nto
            ia = nto-mod(i-1,nto)
            field(i,jj,k) = field(ia,jj-1,k)
          enddo !i
        endif !lsymetr:larctic
      enddo !k
      return
      end

      subroutine m2h_u(f_nc,ntq,mto,kk,misval,
     &                 field,ii,jj,lsymetr,larctic)
      implicit none
c
      logical lsymetr,larctic
      integer ntq,mto,kk,ii,jj
      real    f_nc(ntq,mto,kk),misval,field(ii,jj,kk)
c
c --- convert u-grid mom6 array to hycom u-grid.
c --- mom6  standard has "q" at i+0.5,j+0.5 w.r.t. p.ij
c --- mom6  symetric has "q" at i-0.5,j-0.5 w.r.t. p.ij
c --- hycom          has "q" at i-0.5,j-0.5 w.r.t. p.ij
c
      integer i,ia,j,k
c
      if     (lsymetr) then
        do k= 1,kk
          do j= 1,mto
            do i= 1,ntq
              if     (f_nc(i,j,k).ne.misval) then
                 field(i,j,k) = f_nc(i,j,k)
              else
                 field(i,j,k) = 0.0
              endif
            enddo !i
          enddo !j
          do i= 1,ii  !must be land, i.e. zero
            field(i,jj,k) = 0.0
          enddo !i
        enddo !k
      else
        do k= 1,kk
          do j= 1,mto
            do i= 1,ntq
              ia  = mod(i,ntq)+1
              if     (f_nc(i,j,k).ne.misval) then
                 field(ia,j,k) = f_nc(i,j,k)
              else
                 field(ia,j,k) = 0.0
              endif
            enddo !i
          enddo !j
          if     (larctic) then  !u-grid vector field, mto=jj-1
            do i= 1,ntq
              ia = mod(ntq-(i-1),ntq)+1
              field(i,jj,k) = -field(ia,jj-1,k)
            enddo !i
          endif
        enddo !k
      endif !lsymetr:else
      return
      end

      subroutine m2h_v(f_nc,nto,mtq,kk,misval,
     &                 field,ii,jj,lsymetr,larctic)
      implicit none
c
      logical lsymetr,larctic
      integer nto,mtq,kk,ii,jj
      real    f_nc(nto,mtq,kk),misval,field(ii,jj,kk)
c
c --- convert v-grid mom6 array to hycom.
c --- mom6  standard has "q" at i+0.5,j+0.5 w.r.t. p.ij
c --- mom6  symetric has "q" at i-0.5,j-0.5 w.r.t. p.ij
c --- hycom          has "q" at i-0.5,j-0.5 w.r.t. p.ij
c
      integer i,ia,j,k
c
      if     (lsymetr) then
        do k= 1,kk
          do j= 1,mtq
            do i= 1,nto
              if     (f_nc(i,j,k).ne.misval) then
                 field(i,j,k) = f_nc(i,j,k)
              else
                 field(i,j,k) = 0.0
              endif
            enddo !i
          enddo !j
          do j= 1,jj   !must be land
            field(ii,j,k) = 0.0
          enddo !j
        enddo !k
      else
        do k= 1,kk
          do j= 1,min(mtq,jj-1)  !mtq if larctic
            do i= 1,nto
              if     (f_nc(i,j,  k).ne.misval) then
                 field(i,j+1,k) = f_nc(i,j,k)
              else
                 field(i,j+1,k) = 0.0
              endif
            enddo !i
          enddo !j
          do i= 1,nto  !must be land
            field(i,1,k) = 0.0
          enddo !i
        enddo !k
      endif !lsymetr:else
      return
      end
