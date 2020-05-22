      subroutine horout(array,
     &                  artype,yrflag,time3,iexpt,lhycom,
     &                  name,namec,names,units, k,ltheta, frmt,io)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF fortran 90 interface
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom,ltheta,lexist
      integer          artype,yrflag,iexpt,k,io
      double precision time3(3)
      real             array(ii,jj),thetak
c
c     write out array to unit io based on frmt.
c
c     array size and frmt        must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF I/O,
c       frmt=='netCDF'        for NetCDF I/O,
c       frmt=='MERSEA'        for NetCDF I/O with MERSEA conventions,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.
c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c
c     This version supports frmt=='netCDF' (and 'MERSEA') and needs 
c     version 3.5 of the NetCDF library, from: 
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLonDimID,pLatVarID,pLonVarID
      integer          :: pYDimID,pXDimID,pYVarID,pXVarID
      integer          :: MTDimID,MTVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6
c
      logical          :: lopen
      integer          :: i,j,l,iyear,month,iday,ihour,
     &                          iyrms,monms,idms,ihrms
      real             :: hmin,hmax
      double precision :: dt,yr0,year
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      logical,          save :: laxis
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        if     (frmt(1:l).eq.'HYCOM')  then
c
c         HYCOM .[ab] I/O.
c
          call zbiost(ii,jj)
          iotype = 1
          write(lp,'(/a/)') 'horout - HYCOM I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BIN')    then
c
c         unformatted sequential I/O.
c
          iotype = 2
          write(lp,'(/a/)') 'horout - unformatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:8).eq.'(2f10.4,' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O (lon lat value).
c
          iotype = -3
          write(lp,'(/a,a/)') 'horout - formatted sequential I/O',
     &                        ' (longitude latitude value)'
          call flush(lp)
        elseif (frmt(1:1).eq.'(' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O.
c
          iotype = 3
          write(lp,'(/a/)') 'horout - formatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'netCDF' .or.
     &          frmt(1:l).eq.'NetCDF'     ) then
c
c         NetCDF I/O.
c
          laxis = .true.
          do i= 2,ii
            laxis = laxis .and. 
     &              maxval(abs(plat(1,:)-plat(i,:))).le.1.e-2
          enddo
          do j= 2,jj
            laxis = laxis .and. 
     &              maxval(abs(plon(:,1)-plon(:,j))).le.1.e-2
          enddo
c
          iotype = 4
          if     (laxis) then
            write(lp,'(/2a/)') 'horout - NetCDF I/O (lat/lon axes)'
          else
            write(lp,'(/2a/)') 'horout - NetCDF I/O (curvilinear)'
          endif
          call flush(lp)
        elseif (frmt(1:l).eq.'MERSEA') then
c
c         NetCDF I/O, with MERSEA layout.
c
          laxis = .true.
          do i= 2,ii
            laxis = laxis .and. 
     &              maxval(abs(plat(1,:)-plat(i,:))).le.1.e-2
          enddo
          do j= 2,jj
            laxis = laxis .and. 
     &              maxval(abs(plon(:,1)-plon(:,j))).le.1.e-2
          enddo
c
          if     (.not. laxis) then
            write(lp,'(/2a/)')   'error in horout - ',
     &        'MERSEA requires lat/lon axes'
            call flush(lp)
            stop
          endif
c
          iotype = -4
          write(lp,'(/2a/)') 'horout - MERSEA I/O (lat/lon axes)'
          call flush(lp)
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       initialize labeli.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          if     (yrflag.lt.3) then
            write (labeli(51:72),112) time3(3)/year,cmonth(month),iday
          else
            write (labeli(51:72),113) cmonth(month),iday,iyear
          endif
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 112    format ('  year',f7.2,' (',a3,i3.2,')')
 113    format ('  date: ',a3,i3.2,',',i5,'  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
c     complete the label
c
      label = labeli
      if     (artype.eq.3 .and. index(name,'/mass').ne.0) then
        label(52:55) = 'eddy'
      endif
      if     (k.eq.0) then
        label(33:50)=name
      elseif (ltheta) then
        write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
      else
        write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
      endif
c
      if     (iotype.eq.1) then
c
c       HYCOM .[ab] I/O.
c
        call zbiopi(lopen, io)
        if     (.not.lopen) then
          call zbiopn('new', io)
          call zhopen(io, 'formatted', 'new', 0)
        endif
        call zbiowr(array, ip,.false., hmin,hmax, io, .false.)
        write(io,'(a,a,2g15.6)') label(33:81),':',hmin,hmax
        call flush(io)
        write(lp,'(a,a,2g15.6)') label(33:81),':',hmin,hmax
        call flush(lp)
      elseif (iotype.eq.2) then
c
c       unformatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        write(io) array
        call flush(io)
        write(lp,'(a)') label(33:81)
        call flush(lp)
      elseif (iotype.eq.-3) then
c
c       formatted sequential I/O (lon lat value)
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do j= 1,jj
          do i= 1,ii
            if     (array(i,j).ne.fill_value) then
              write(io,frmt) plon(i,j),plat(i,j),array(i,j)
            endif
          enddo
        enddo
        call flush(io)
        write(lp,'(a)') label(33:81)
        call flush(lp)
      elseif (iotype.eq.3) then
c
c       formatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        write(io,frmt) array
        call flush(io)
        write(lp,'(a)') label(33:81)
        call flush(lp)
      elseif(abs(iotype).eq.4) then
c
c       NetCDF I/O
c
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
c
        call ncrange(array,ii,jj,1, fill_value, hmin,hmax)
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
c
c          create a new NetCDF and write data to it
c
          call ncheck(nf90_create(trim(ncfile),nf90_noclobber,ncfileID))
          ! define the dimensions
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_def_dim(ncfileID,
     &                               "MT", nf90_unlimited,MTDimID))
          endif
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_def_dim(ncfileID,
     &                               "latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "longitude", ii,pLonDimID))
          elseif (laxis) then
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Longitude", ii,pLonDimID))
          else
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Y",         jj,pYDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "X",         ii,pXDimID))
          endif
          ! create the global attributes
          call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                             "Conventions",
     &                             "CF-1.0"))
          if (lhycom) then
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "HYCOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM archive file"))
            elseif (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM mean archive file"))
            else
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM std. archive file"))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archv2ncdf2d"))
            if     (iotype.eq.-4) then !MERSEA
              if     (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "daily average"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "instantaneous"))
              endif
              write(ncenv,
     &          '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &          iyear,month,iday,ihour
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "field_date",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('MERSEA_B_DATE',ncenv)
              if     (ncenv.eq.'TODAY') then
                write(ncenv,
     &            '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &            iyear,month,iday,ihour
              endif
              if     (ncenv.ne.' ') then
                read(ncenv,'(i4,1x,i2,1x,i2,1x,i2)')
     &            iyrms,monms,idms,ihrms
                if     (iyrms.lt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "forecast"))
                elseif (iyrms.gt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "hindcast"))
                else   !iyrms.eq.iyear
                  if     (monms.lt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "forecast"))
                  elseif (monms.gt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "hindcast"))
                  else   !monms.eq.month
                    if     (idms.lt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "forecast"))
                    elseif (idms.gt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "hindcast"))
                    else   !idms.eq.iday
                      if     (ihrms.lt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "forecast"))
                      elseif (ihrms.gt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "hindcast"))
                      else   !ihrms.eq.ihour
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "nowcast"))
                      endif  !ihrms
                    endif !idms
                  endif !monms
                endif !iyrms
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_date",
     &                                   trim(ncenv)))
              endif
              ncenv = ' '
              call getenv('MERSEA_B_TYPE',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_type",
     &                                   trim(ncenv)))
              endif
            endif
          else
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "MICOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               "MICOM"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "source",
     &                               "MICOM archive file"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archm2ncdf2d"))
          endif
          ! create the variables and attributes
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                               MTDimID,MTVarID))
            if     (yrflag.eq.0) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "360_day"))
            elseif (yrflag.eq.1) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            elseif (yrflag.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            else
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 1900-12-31 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "standard"))
            endif
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "axis","T"))
            call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                               MTDimID,datVarID))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "long_name",
     &                               "date"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "units",
     &                               "day as %Y%m%d.%f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "C_format",
     &                               "%13.4f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "FORTRAN_format",
     &                               "(f13.4)"))
            if     (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            elseif (artype.eq.3) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            endif
          endif !not MERSEA
          if     (laxis) then
            if     (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            if     (abs((plat(1,jj)-plat(1,1))-
     &                  (plat(1, 2)-plat(1,1))*(jj-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "axis","Y"))
            if     (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)-plon(1,1))-
     &                  (plon( 2,1)-plon(1,1))*(ii-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "axis","X"))
          else !.not.laxis
            call ncheck(nf90_def_var(ncfileID,"Y", nf90_int,
     &                               pYDimID,pYVarID))
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "axis","Y"))
            call ncheck(nf90_def_var(ncfileID,"X", nf90_int,
     &                               pXDimID,pXVarID))
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "axis","X"))
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               (/pXDimID, pYDimID/), pLatVarID))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               (/pXDimID, pYDimID/), pLonVarID))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          endif !laxis:else
          ! model 2d variable
          if     (iotype.eq.4) then !not for MERSEA
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID,   MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
          else !MERSEA      
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID/),
     &                               varID))
          endif                                                         
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin, hmax/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               label(33:50)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               label(33:55)//label(73:81)))
          endif
          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_put_var(ncfileID,MTVarID, time3(3)))
            call ncheck(nf90_put_var(ncfileID,datVarID,date    ))
          endif
          if     (laxis) then
            call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                               (/plat(1,:)/)))     !1-d Latitudes
            call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                               (/plon(:,1)/)))     !1-d Longtudes
          else
            call ncheck(nf90_put_var(ncfileID,pYVarID,
     &                               (/(j, j=1,jj)/)))
            call ncheck(nf90_put_var(ncfileID,pXVarID,
     &                               (/(i, i=1,ii)/)))
            call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(:,:)))
            call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,:)))
          endif
          ! write to model variable
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:)))
          ! close NetCDF file
          call ncheck(nf90_close(ncfileID))
        else
c
c          Write data to existing NetCDF file
c
          ! open NetCDF file
          call ncheck(nf90_open(trim(ncfile), nf90_write, ncfileID))
          ! get dimension ID's
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_inq_dimid(ncfileID,"MT",MTDimID))
          endif
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "longitude",pLonDimID))
          elseif (laxis) then
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Longitude",pLonDimID))
          else
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Y",        pYDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "X",        pXDimID))
          endif
          ! switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable
          if     (iotype.eq.4) then !not for MERSEA
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID,   MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
          else !MERSEA
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID/),
     &                               varID))
          endif
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin, hmax/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               label(33:50)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               label(33:55)//label(73:81)))
          endif
          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
          ! get varID and write to array
          call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
          !write values into array
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:)))
          ! close file 
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a,a,2g15.6)') label(33:81),':',hmin,hmax
        call flush(lp)
      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop
      endif
      return
      end

      subroutine horout_3d(array,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namec,names,units,
     &                     kf,kl,ltheta, frmt,io)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF Interface
      
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom,ltheta,lexist
      integer          artype,yrflag,iexpt,kf,kl,io
      double precision time3(3)
      real             array(ii,jj,kl),thetak
c
c     write out a 3-d layer array to unit io based on frmt.
c
c     2-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF I/O,
c       frmt=='netCDF'        for NetCDF I/O,
c       frmt=='MERSEA'        for NetCDF I/O with MERSEA conventions,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.
c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c
c     This version supports frmt=='netCDF' (and 'MERSEA') and needs 
c     version 3.5 of the NetCDF library, from: 
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLonDimID,pLatVarID,pLonVarID,
     &                    lyrDimID,lyrVarID
      integer          :: pYDimID,pXDimID,pYVarID,pXVarID
      integer          :: MTDimID,MTVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6
c
      logical          :: lopen
      integer          :: i,j,k,l,iyear,month,iday,ihour,
     &                            iyrms,monms,idms,ihrms
      real             :: hmin(999),hmax(999)
      double precision :: dt,yr0,year
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      logical,          save :: laxis
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        if     (frmt(1:l).eq.'HYCOM')  then
c
c         HYCOM .[ab] I/O.
c
          call zbiost(ii,jj)
          iotype = 1
          write(lp,'(/a/)') 'horout_3d - HYCOM I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BIN')    then
c
c         unformatted sequential I/O.
c
          iotype = 2
          write(lp,'(/a/)') 'horout_3d - unformatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:8).eq.'(2f10.4,' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O (lon lat value).
c
          iotype = -3
          write(lp,'(/a,a/)') 'horout - formatted sequential I/O',
     &                        ' (longitude latitude value)'
          call flush(lp)
        elseif (frmt(1:1).eq.'(' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O.
c
          iotype = 3
          write(lp,'(/a/)') 'horout_3d - formatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'netCDF' .or. 
     &          frmt(1:l).eq.'NetCDF'     ) then
c
c         NetCDF I/O.
c
          laxis = .true.
          do i= 2,ii
            laxis = laxis .and. 
     &              maxval(abs(plat(1,:)-plat(i,:))).le.1.e-2
          enddo
          do j= 2,jj
            laxis = laxis .and. 
     &              maxval(abs(plon(:,1)-plon(:,j))).le.1.e-2
          enddo
c
          iotype = 4
          if     (laxis) then
            write(lp,'(/2a/)') 'horout_3d - NetCDF I/O (lat/lon axes)'
          else
            write(lp,'(/2a/)') 'horout_3d - NetCDF I/O (curvilinear)'
          endif
          call flush(lp)
        elseif (frmt(1:l).eq.'MERSEA') then
c
c         NetCDF I/O, with MERSEA layout.
c
          laxis = .true.
          do i= 2,ii
            laxis = laxis .and. 
     &              maxval(abs(plat(1,:)-plat(i,:))).le.1.e-2
          enddo
          do j= 2,jj
            laxis = laxis .and. 
     &              maxval(abs(plon(:,1)-plon(:,j))).le.1.e-2
          enddo
c
          if     (.not. laxis) then
            write(lp,'(/2a/)')   'error in horout_3d - ',
     &        'MERSEA requires lat/lon axes'
            call flush(lp)
            stop
          endif
c
          iotype = -4
          write(lp,'(/2a/)') 'horout_3d - MERSEA I/O (lat/lon axes)'
          call flush(lp)
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout_3d - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       initialize labeli.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          if     (yrflag.lt.3) then
            write (labeli(51:72),112) time3(3)/year,cmonth(month),iday
          else
            write (labeli(51:72),113) cmonth(month),iday,iyear
          endif
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 112    format ('  year',f7.2,' (',a3,i3.2,')')
 113    format ('  date: ',a3,i3.2,',',i5,'  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      label = labeli
      if     (artype.eq.3 .and. index(name,'/mass').ne.0) then
        label(52:55) = 'eddy'
      endif
c
      if     (iotype.eq.1) then
c
c       HYCOM .[ab] I/O.
c
        call zbiopi(lopen, io)
        if     (.not.lopen) then
          call zbiopn('new', io)
          call zhopen(io, 'formatted', 'new', 0)
        endif
        call zbiowr3(array(1,1,kf),kl-kf+1,
     +               ip,.false., hmin(kf),hmax(kf), io, .false.)
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          write(io,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(io)
          write(lp,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(lp)
        enddo
      elseif (iotype.eq.2) then
c
c       unformatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          write(io) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.-3) then
c
c       formatted sequential I/O (lon lat value).
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          do j= 1,jj
            do i= 1,ii
              if     (array(i,j,k).ne.fill_value) then
                write(io,frmt) plon(i,j),plat(i,j),array(i,j,k)
              endif
            enddo
          enddo
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.3) then
c
c       formatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          write(io,frmt) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif(abs(iotype).eq.4) then
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
c
        call ncrange(array(1,1,kf),ii,jj,kl-kf+1, fill_value, hmin,hmax)
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
c
c         create a new NetCDF and write data to it
c
          call ncheck(nf90_create(trim(ncfile),nf90_noclobber,ncfileID))
          ! define the dimensions
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_def_dim(ncfileID,
     &                               "MT", nf90_unlimited,MTDimID))
          endif
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_def_dim(ncfileID,
     &                               "latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "longitude", ii,pLonDimID))
          elseif (laxis) then
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Longitude", ii,pLonDimID))
          else
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Y",         jj,pYDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "X",         ii,pXDimID))
          endif
          call ncheck(nf90_def_dim(ncfileID,"Layer",kl-kf+1,lyrDimID))
          ! create the global attributes
          call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                             "Conventions",
     &                             "CF-1.0"))
          if (lhycom) then
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "HYCOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM archive file"))
            elseif (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM mean archive file"))
            else
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM std. archive file"))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archv2ncdf2d"))
            if     (iotype.eq.-4) then !MERSEA
              if     (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "daily average"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "instantaneous"))
              endif
              write(ncenv,
     &          '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &          iyear,month,iday,ihour
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "field_date",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('MERSEA_B_DATE',ncenv)
              if     (ncenv.eq.'TODAY') then
                write(ncenv,
     &            '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &            iyear,month,iday,ihour
              endif
              if     (ncenv.ne.' ') then
                read(ncenv,'(i4,1x,i2,1x,i2,1x,i2)')
     &            iyrms,monms,idms,ihrms
                if     (iyrms.lt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "forecast"))
                elseif (iyrms.gt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "hindcast"))
                else   !iyrms.eq.iyear
                  if     (monms.lt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "forecast"))
                  elseif (monms.gt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "hindcast"))
                  else   !monms.eq.month
                    if     (idms.lt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "forecast"))
                    elseif (idms.gt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "hindcast"))
                    else   !idms.eq.iday
                      if     (ihrms.lt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "forecast"))
                      elseif (ihrms.gt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "hindcast"))
                      else   !ihrms.eq.ihour
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "nowcast"))
                      endif  !ihrms
                    endif !idms
                  endif !monms
                endif !iyrms
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_date",
     &                                   trim(ncenv)))
              endif
              ncenv = ' '
              call getenv('MERSEA_B_TYPE',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_type",
     &                                   trim(ncenv)))
              endif
            endif
          else
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "MICOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               "MICOM"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "source",
     &                               "MICOM archive file"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archm2ncdf2d"))
          endif
          ! create the variables and attributes
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                               MTDimID,MTVarID))
            if     (yrflag.eq.0) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "360_day"))
            elseif (yrflag.eq.1) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            elseif (yrflag.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            else
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 1900-12-31 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "standard"))
            endif
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "axis","T"))
            call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                               MTDimID,datVarID))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "long_name",
     &                               "date"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "units",
     &                               "day as %Y%m%d.%f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "C_format",
     &                               "%13.4f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "FORTRAN_format",
     &                               "(f13.4)"))
            if     (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            elseif (artype.eq.3) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            endif
          endif !not MERSEA
          if     (ltheta) then
            call ncheck(nf90_def_var(ncfileID,"Layer", nf90_float,
     &                               lyrDimID,lyrVarID))
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "units","SigmaTheta"))     
          else
            call ncheck(nf90_def_var(ncfileID,"Layer", nf90_int,
     &                               lyrDimID,lyrVarID))
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "units","layer"))     
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "positive","down"))
          endif
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "axis","Z"))
          if     (laxis) then
            if     (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            if     (abs((plat(1,jj)-plat(1,1))-
     &                  (plat(1, 2)-plat(1,1))*(jj-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "axis","Y"))
            if     (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)-plon(1,1))-
     &                  (plon( 2,1)-plon(1,1))*(ii-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "axis","X"))
          else !.not.laxis
            call ncheck(nf90_def_var(ncfileID,"Y", nf90_int,
     &                               pYDimID,pYVarID))
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "axis","Y"))
            call ncheck(nf90_def_var(ncfileID,"X", nf90_int,
     &                               pXDimID,pXVarID))
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "axis","X"))
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               (/pXDimID, pYDimID/), pLatVarID))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               (/pXDimID, pYDimID/), pLonVarID))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          endif !laxis:else
          ! model 3d variable
          if     (iotype.eq.4) then !not for MERSEA
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLonDimID, pLatDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pXDimID,   pYDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
          else !MERSEA
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, lyrDimID/),
     &                               varID))
          endif
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_put_var(ncfileID,MTVarID, time3(3)))
            call ncheck(nf90_put_var(ncfileID,datVarID,date    ))
          endif
          if     (laxis) then
            call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                               (/plat(1,:)/)))     !1-d Latitudes
            call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                               (/plon(:,1)/)))     !1-d Longtudes
          else
            call ncheck(nf90_put_var(ncfileID,pYVarID,
     &                               (/(j, j=1,jj)/)))
            call ncheck(nf90_put_var(ncfileID,pXVarID,
     &                               (/(i, i=1,ii)/)))
            call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(:,:)))
            call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,:)))
          endif
          if     (ltheta) then
            call ncheck(nf90_put_var(ncfileID,lyrVarID,
     &                               theta(kf:kl)))
          else
            call ncheck(nf90_put_var(ncfileID,lyrVarID,
     &                               (/(k, k=kf,kl)/)))
          endif
          ! write to model variable
          call ncheck(nf90_put_var(ncfileID,varID,
     &                             array(1:ii,1:jj,kf:kl)))
          ! close NetCDF file
          call ncheck(nf90_close(ncfileID))
        else
c
c         Write data to existing NetCDF file
c
          ! open NetCDF file
          call ncheck(nf90_open(trim(ncfile), nf90_write, ncfileID))
          ! get dimension ID's
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_inq_dimid(ncfileID,"MT",    MTDimID))
          endif
          call ncheck(nf90_inq_dimid(ncfileID,"Layer",lyrDimID))
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "longitude",pLonDimID))
          elseif (laxis) then
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Longitude",pLonDimID))
          else
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Y",        pYDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "X",        pXDimID))
          endif
          !  switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable
          if     (iotype.eq.4) then !not for MERSEA
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLonDimID, pLatDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pXDimID,   pYDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
          else !MERSEA
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, lyrDimID/),
     &                               varID))
          endif
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
          ! inquire variable ID
          call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
          !write values into array
          call ncheck(nf90_put_var(ncfileID,varID,
     &                             array(1:ii,1:jj,kf:kl)))
          ! close file
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a49,a,2g15.6)') 
     &    trim(name)//label(51:81),':',hmin(1),hmax(1)
        call flush(lp)
      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout_3d - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop
      endif
      return
      end

      subroutine horout_3z(array,zz,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namec,names,units, kz, frmt,io)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF Interface
      
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom, lexist
      integer          artype,yrflag,iexpt,kz,io
      double precision time3(3)
      real             array(ii,jj,kz),zz(kz)
c
c     write out a 3-d z-level array to unit io based on frmt.
c
c     3-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF I/O,
c       frmt=='netCDF'        for NetCDF I/O,
c       frmt=='MERSEA'        for NetCDF I/O with MERSEA conventions,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.
c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c
c     This version supports frmt=='netCDF' (and 'MERSEA') and needs 
c     version 3.5 of the NetCDF library, from: 
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLonDimID,pLatVarID,pLonVarID,
     &                    lyrDimID,lyrVarID
      integer          :: pYDimID,pXDimID,pYVarID,pXVarID
      integer          :: MTDimID,MTVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6
c
      logical          :: lopen
      integer          :: i,j,k,l,iyear,month,iday,ihour,
     &                            iyrms,monms,idms,ihrms
      real             :: hmin(999),hmax(999)
      double precision :: dt,yr0,year
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      logical,          save :: laxis
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        if     (frmt(1:l).eq.'HYCOM')  then
c
c         HYCOM .[ab] I/O.
c
          call zbiost(ii,jj)
          iotype = 1
          write(lp,'(/a/)') 'horout_3z - HYCOM I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BIN')    then
c
c         unformatted sequential I/O.
c
          iotype = 2
          write(lp,'(/a/)') 'horout_3z - unformatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:8).eq.'(2f10.4,' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O (lon lat value).
c
          iotype = -3
          write(lp,'(/a,a/)') 'horout - formatted sequential I/O',
     &                        ' (longitude latitude value)'
          call flush(lp)
        elseif (frmt(1:1).eq.'(' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O.
c
          iotype = 3
          write(lp,'(/a/)') 'horout_3z - formatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'netCDF' .or.
     &          frmt(1:l).eq.'NetCDF'     ) then
c
c         NetCDF I/O.
c
          laxis = .true.
          do i= 2,ii
            laxis = laxis .and. 
     &              maxval(abs(plat(1,:)-plat(i,:))).le.1.e-2
          enddo
          do j= 2,jj
            laxis = laxis .and. 
     &              maxval(abs(plon(:,1)-plon(:,j))).le.1.e-2
          enddo
c
          iotype = 4
          if     (laxis) then
            write(lp,'(/2a/)') 'horout_3z - NetCDF I/O (lat/lon axes)'
          else
            write(lp,'(/2a/)') 'horout_3z - NetCDF I/O (curvilinear)'
          endif
          call flush(lp)
        elseif (frmt(1:l).eq.'MERSEA') then
c
c         NetCDF I/O, with MERSEA layout.
c
          laxis = .true.
          do i= 2,ii
            laxis = laxis .and. 
     &              maxval(abs(plat(1,:)-plat(i,:))).le.1.e-2
          enddo
          do j= 2,jj
            laxis = laxis .and. 
     &              maxval(abs(plon(:,1)-plon(:,j))).le.1.e-2
          enddo
c
          if     (.not. laxis) then
            write(lp,'(/2a/)')   'error in horout_3z - ',
     &        'MERSEA requires lat/lon axes'
            call flush(lp)
            stop
          endif
c
          iotype = -4
          write(lp,'(/2a/)') 'horout_3z - MERSEA I/O (lat/lon axes)'
          call flush(lp)
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout_3z - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       initialize labeli.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          if     (yrflag.lt.3) then
            write (labeli(51:72),112) time3(3)/year,cmonth(month),iday
          else
            write (labeli(51:72),113) cmonth(month),iday,iyear
          endif
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 112    format ('  year',f7.2,' (',a3,i3.2,')')
 113    format ('  date: ',a3,i3.2,',',i5,'  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      label = labeli
      if     (artype.eq.3 .and. index(name,'/mass').ne.0) then
        label(52:55) = 'eddy'
      endif
c
      if     (iotype.eq.1) then
c
c       HYCOM .[ab] I/O.
c
        call zbiopi(lopen, io)
        if     (.not.lopen) then
          call zbiopn('new', io)
          call zhopen(io, 'formatted', 'new', 0)
        endif
        call zbiowr3(array,kz,
     +               ip,.false., hmin,hmax, io, .false.)
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          write(io,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(io)
          write(lp,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(lp)
        enddo
      elseif (iotype.eq.2) then
c
c       unformatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          write(io) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.-3) then
c
c       formatted sequential I/O (lon lat value).
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          do j= 1,jj
            do i= 1,ii
              if     (array(i,j,k).ne.fill_value) then
                write(io,frmt) plon(i,j),plat(i,j),array(i,j,k)
              endif
            enddo
          enddo
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.3) then
c
c       formatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          write(io,frmt) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (abs(iotype).eq.4) then
c
c       NetCDF I/O
c
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
c
        call ncrange(array,ii,jj,kz, fill_value, hmin,hmax)
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
          ! create a new NetCDF and write data to it
          call ncheck(nf90_create(trim(ncfile),nf90_noclobber,ncfileID))
          ! define the dimensions
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_def_dim(ncfileID,
     &                               "MT", nf90_unlimited,MTDimID))
          endif
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_def_dim(ncfileID,
     &                               "latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "longitude", ii,pLonDimID))
          elseif (laxis) then
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Longitude", ii,pLonDimID))
          else
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Y",         jj,pYDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "X",         ii,pXDimID))
          endif
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_def_dim(ncfileID,"depth",kz,lyrDimID))
          else
            call ncheck(nf90_def_dim(ncfileID,"Depth",kz,lyrDimID))
          endif
          ! create the global attributes
          call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                             "Conventions",
     &                             "CF-1.0"))
          if (lhycom) then
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "HYCOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM archive file"))
            elseif (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM mean archive file"))
            else
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM std. archive file"))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archv2ncdf3z"))
            if     (iotype.eq.-4) then !MERSEA
              if     (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "daily average"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "instantaneous"))
              endif
              write(ncenv,
     &          '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &          iyear,month,iday,ihour
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "field_date",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('MERSEA_B_DATE',ncenv)
              if     (ncenv.eq.'TODAY') then
                write(ncenv,
     &            '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &            iyear,month,iday,ihour
              endif
              if     (ncenv.ne.' ') then
                read(ncenv,'(i4,1x,i2,1x,i2,1x,i2)')
     &            iyrms,monms,idms,ihrms
                if     (iyrms.lt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "forecast"))
                elseif (iyrms.gt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "hindcast"))
                else   !iyrms.eq.iyear
                  if     (monms.lt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "forecast"))
                  elseif (monms.gt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "hindcast"))
                  else   !monms.eq.month
                    if     (idms.lt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "forecast"))
                    elseif (idms.gt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "hindcast"))
                    else   !idms.eq.iday
                      if     (ihrms.lt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "forecast"))
                      elseif (ihrms.gt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "hindcast"))
                      else   !ihrms.eq.ihour
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "nowcast"))
                      endif  !ihrms
                    endif !idms
                  endif !monms
                endif !iyrms
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_date",
     &                                   trim(ncenv)))
              endif
              ncenv = ' '
              call getenv('MERSEA_B_TYPE',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_type",
     &                                   trim(ncenv)))
              endif
            endif
          else
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "MICOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               "MICOM"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "source",
     &                               "MICOM archive file"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archm2ncdf3z"))
          endif
          ! create the variables and attributes
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                               MTDimID,MTVarID))
            if     (yrflag.eq.0) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "360_day"))
            elseif (yrflag.eq.1) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            elseif (yrflag.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            else
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 1900-12-31 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "standard"))
            endif
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "axis","T"))
            call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                               MTDimID,datVarID))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "long_name",
     &                               "date"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "units",
     &                               "day as %Y%m%d.%f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "C_format",
     &                               "%13.4f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "FORTRAN_format",
     &                               "(f13.4)"))
            if     (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            elseif (artype.eq.3) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            endif
          endif !not MERSEA
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_def_var(ncfileID,"depth", nf90_float,
     &                               lyrDimID,lyrVarID))
          else
            call ncheck(nf90_def_var(ncfileID,"Depth", nf90_float,
     &                               lyrDimID,lyrVarID))
          endif
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "standard_name","depth"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "units","m"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "positive","down"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "axis","Z"))
          if     (laxis) then
            if     (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            if     (abs((plat(1,jj)-plat(1,1))-
     &                  (plat(1, 2)-plat(1,1))*(jj-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "axis","Y"))
            if     (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)-plon(1,1))-
     &                  (plon( 2,1)-plon(1,1))*(ii-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "axis","X"))
          else !.not.laxis
            call ncheck(nf90_def_var(ncfileID,"Y", nf90_int,
     &                               pYDimID,pYVarID))
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "axis","Y"))
            call ncheck(nf90_def_var(ncfileID,"X", nf90_int,
     &                               pXDimID,pXVarID))
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "axis","X"))
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               (/pXDimID, pYDimID/), pLatVarID))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               (/pXDimID, pYDimID/), pLonVarID))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          endif !laxis:else
          ! model 3Z variable
          if     (iotype.eq.4) then !not for MERSEA
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLonDimID, pLatDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pXDimID,   pYDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
          else !MERSEA
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                            (/pLonDimID, pLatDimID, lyrDimID/),
     &                               varID))
          endif
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_put_var(ncfileID,MTVarID, time3(3)))
            call ncheck(nf90_put_var(ncfileID,datVarID,date    ))
          endif
          if     (laxis) then
            call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                               (/plat(1,:)/)))     !1-d Latitudes
            call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                               (/plon(:,1)/)))     !1-d Longtudes
          else
            call ncheck(nf90_put_var(ncfileID,pYVarID,
     &                               (/(j, j=1,jj)/)))
            call ncheck(nf90_put_var(ncfileID,pXVarID,
     &                               (/(i, i=1,ii)/)))
            call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(:,:)))
            call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,:)))
          endif
          call ncheck(nf90_put_var(ncfileID,lyrVarID,zz(:)))
          ! write to model variable
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:,:)))
          ! close NetCDF file
          call ncheck(nf90_close(ncfileID))
        else
c
c        Write data to existing NetCDF file
c
          ! open NetCDF file
          call ncheck(nf90_open(trim(ncfile), nf90_write, ncfileID))
          ! get dimension ID's
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_inq_dimid(ncfileID,"MT",MTDimID))
            call ncheck(nf90_inq_dimid(ncfileID,"Depth",lyrDimID))
          else
            call ncheck(nf90_inq_dimid(ncfileID,"depth",lyrDimID))
          endif
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "longitude",pLonDimID))
          elseif (laxis) then
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Longitude",pLonDimID))
          else
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Y",        pYDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "X",        pXDimID))
          endif
          !  switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable
          if     (iotype.eq.4) then !not for MERSEA
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLonDimID, pLatDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pXDimID,   pYDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
          else !MERSEA
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                            (/pLonDimID, pLatDimID, lyrDimID/),
     &                               varID))
          endif
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
          ! inquire variable ID
          call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
          !write values into array
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:,:)))
          ! close file
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a49,a,2g15.6)') 
     &    trim(name)//label(51:81),':',hmin(1),hmax(1)
        call flush(lp)
      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout_3z - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop
      endif
      return
      end

      subroutine horout_jk(array, platj,jlatn,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namec,names,units,
     &                     kf,kl,ltheta, frmt,io)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF Interface
      
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom,ltheta,lexist
      integer          jlatn,artype,yrflag,iexpt,kf,kl,io
      double precision time3(3)
      real             array(jlatn,kl),
     &                 platj(jlatn),thetak
c
c     write out a 2-d layer array to unit io based on frmt.
c
c     2-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF I/O,
c       frmt=='netCDF'        for NetCDF I/O,
c
c     This version supports frmt=='netCDF' and needs 
c     version 3.5 of the NetCDF library, from: 
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLatVarID,
     &                    lyrDimID,lyrVarID
      integer          :: MTDimID,MTVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6
c
      logical          :: lopen
      integer          :: i,j,k,l,iyear,month,iday,ihour,
     &                            iyrms,monms,idms,ihrms
      real             :: hmin(999),hmax(999)
      double precision :: dt,yr0,year
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        if     (frmt(1:l).eq.'netCDF' .or. 
     &          frmt(1:l).eq.'NetCDF'     ) then
c
c         NetCDF I/O.
c
          iotype = 4
          write(lp,'(/a/)') 'horout_jk - NetCDF I/O (lat axes)'
          call flush(lp)
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout_jk - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       initialize labeli.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          if     (yrflag.lt.3) then
            write (labeli(51:72),112) time3(3)/year,cmonth(month),iday
          else
            write (labeli(51:72),113) cmonth(month),iday,iyear
          endif
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 112    format ('  year',f7.2,' (',a3,i3.2,')')
 113    format ('  date: ',a3,i3.2,',',i5,'  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      label = labeli
      if     (artype.eq.3 .and. index(name,'/mass').ne.0) then
        label(52:55) = 'eddy'
      endif
c
      if    (iotype.eq.4) then
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
c
        call ncrange(array(1,kf), 1,jlatn,kl-kf+1,
     &                            fill_value, hmin,hmax)
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
c
c         create a new NetCDF and write data to it
c
          call ncheck(nf90_create(trim(ncfile),nf90_noclobber,ncfileID))
          ! define the dimensions
          call ncheck(nf90_def_dim(ncfileID,
     &                             "MT", nf90_unlimited,MTDimID))
          call ncheck(nf90_def_dim(ncfileID,
     &                             "Latitude",  jlatn,pLatDimID))
          call ncheck(nf90_def_dim(ncfileID,"Layer",kl-kf+1,lyrDimID))
          ! create the global attributes
          call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                             "Conventions",
     &                             "CF-1.0"))
          if (lhycom) then
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "HYCOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM archive file"))
            elseif (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM mean archive file"))
            else
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM std. archive file"))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archv2ncdfsf"))
          else
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "MICOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               "MICOM"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "source",
     &                               "MICOM archive file"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archm2ncdfsf"))
          endif
          ! create the variables and attributes
          call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                             MTDimID,MTVarID))
          if     (yrflag.eq.0) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-16 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "360_day"))
          elseif (yrflag.eq.1) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-16 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          elseif (yrflag.eq.2) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          else
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 1900-12-31 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "standard"))
          endif
          call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                             "axis","T"))
          call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                             MTDimID,datVarID))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "long_name",
     &                             "date"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "units",
     &                             "day as %Y%m%d.%f"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "C_format",
     &                             "%13.4f"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "FORTRAN_format",
     &                             "(f13.4)"))
          if     (artype.eq.2) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_methods",
     &                               "mean"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_extent",
     &                               cell))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_methods",
     &                               "mean"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_extent",
     &                               cell))
          elseif (artype.eq.3) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_methods",
     &                               "standard_deviation"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_extent",
     &                               cell))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_methods",
     &                               "standard_deviation"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_extent",
     &                               cell))
          endif
          if     (ltheta) then
            call ncheck(nf90_def_var(ncfileID,"Layer", nf90_float,
     &                               lyrDimID,lyrVarID))
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "units","SigmaTheta"))     
          else
            call ncheck(nf90_def_var(ncfileID,"Layer", nf90_int,
     &                               lyrDimID,lyrVarID))
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "units","layer"))     
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "positive","down"))
          endif
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "axis","Z"))
          call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                             pLatDimID,pLatVarID))
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "standard_name","latitude"))
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "units","degrees_north"))
          if     (abs((platj(jlatn)-platj(1))-
     &                (platj(2)    -platj(1))*(jlatn-1)).lt.1.e-2) then
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "point_spacing","even"))  !ferret
          endif
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "axis","Y"))
          ! model 3d variable
          call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                             (/pLatDimID,
     &                               lyrDimID, MTDimID/),
     &                             varID))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "coordinates",
     &                             "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables
          call ncheck(nf90_put_var(ncfileID,MTVarID,time3(3)))
          call ncheck(nf90_put_var(ncfileID,datVarID,date   ))
          call ncheck(nf90_put_var(ncfileID,pLatVarID,platj(:)))
          if     (ltheta) then
            call ncheck(nf90_put_var(ncfileID,lyrVarID,
     &                               theta(kf:kl)))
          else
            call ncheck(nf90_put_var(ncfileID,lyrVarID,
     &                               (/(k, k=kf,kl)/)))
          endif
          ! write to model variable
          call ncheck(nf90_put_var(ncfileID,varID,
     &                             array(1:jlatn,kf:kl)))
          ! close NetCDF file
          call ncheck(nf90_close(ncfileID))
        else
c
c         Write data to existing NetCDF file
c
          ! open NetCDF file
          call ncheck(nf90_open(trim(ncfile), nf90_write, ncfileID))
          ! get dimension ID's
          call ncheck(nf90_inq_dimid(ncfileID,"MT",    MTDimID))
          call ncheck(nf90_inq_dimid(ncfileID,"Layer",lyrDimID))
          call ncheck(nf90_inq_dimid(ncfileID,
     &                               "Latitude",pLatDimID))
          !  switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable
          call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                             (/pLatDimID,
     &                               lyrDimID, MTDimID/),
     &                             varID))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "coordinates",
     &                             "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
          ! inquire variable ID
          call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
          !write values into array
          call ncheck(nf90_put_var(ncfileID,varID,
     &                             array(1:jlatn,kf:kl)))
          ! close file
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a49,a,2g15.6)') 
     &    trim(name)//label(51:81),':',hmin(1),hmax(1)
        call flush(lp)
      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout_jk - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop
      endif
      return
      end

      subroutine horout_jz(array,zz, platj,jlatn,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namec,names,units, kz, frmt,io)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF Interface
      
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom, lexist
      integer          jlatn,artype,yrflag,iexpt,kz,io
      double precision time3(3)
      real             array(jlatn,kz),zz(kz),
     &                 platj(jlatn)
c
c     write out a 3-d z-level array to unit io based on frmt.
c
c     3-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c
c     Supported I/O types are:
c       frmt=='NetCDF' for NetCDF I/O,
c       frmt=='netCDF' for NetCDF I/O,
c
c     This version supports frmt=='netCDF' and needs 
c     version 3.5 of the NetCDF library, from: 
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLatVarID,
     &                    lyrDimID,lyrVarID
      integer          :: MTDimID,MTVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6
c
      logical          :: lopen
      integer          :: i,j,k,l,iyear,month,iday,ihour,
     &                            iyrms,monms,idms,ihrms
      real             :: hmin(999),hmax(999)
      double precision :: dt,yr0,year
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        if     (frmt(1:l).eq.'netCDF' .or.
     &          frmt(1:l).eq.'NetCDF'     ) then
c
c         NetCDF I/O.
c
          iotype = 4
          write(lp,'(/2a/)') 'horout_jz - NetCDF I/O (lat/lon axes)'
          call flush(lp)
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout_jz - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       initialize labeli.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          if     (yrflag.lt.3) then
            write (labeli(51:72),112) time3(3)/year,cmonth(month),iday
          else
            write (labeli(51:72),113) cmonth(month),iday,iyear
          endif
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 112    format ('  year',f7.2,' (',a3,i3.2,')')
 113    format ('  date: ',a3,i3.2,',',i5,'  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      label = labeli
      if     (artype.eq.3 .and. index(name,'/mass').ne.0) then
        label(52:55) = 'eddy'
      endif
c
      if     (abs(iotype).eq.4) then
c
c       NetCDF I/O
c
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
c
        call ncrange(array, 1,jlatn,kz, fill_value, hmin,hmax)
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
          ! create a new NetCDF and write data to it
          call ncheck(nf90_create(trim(ncfile),nf90_noclobber,ncfileID))
          ! define the dimensions
          call ncheck(nf90_def_dim(ncfileID,
     &                             "MT", nf90_unlimited,MTDimID))
          call ncheck(nf90_def_dim(ncfileID,
     &                             "Latitude",  jlatn,pLatDimID))
          call ncheck(nf90_def_dim(ncfileID,"Depth",kz,lyrDimID))
          ! create the global attributes
          call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                             "Conventions",
     &                             "CF-1.0"))
          if (lhycom) then
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "HYCOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM archive file"))
            elseif (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM mean archive file"))
            else
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM std. archive file"))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archv2ncdfjz"))
            ncenv = ' '
          else
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "MICOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               "MICOM"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "source",
     &                               "MICOM archive file"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archm2ncdfjz"))
          endif
          ! create the variables and attributes
          call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                             MTDimID,MTVarID))
          if     (yrflag.eq.0) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                          "days since 0001-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "360_day"))
          elseif (yrflag.eq.1) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                          "days since 0001-01-16 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          elseif (yrflag.eq.2) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                          "days since 0001-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          else
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                          "days since 1900-12-31 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "standard"))
          endif
          call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                             "axis","T"))
          call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                             MTDimID,datVarID))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "long_name",
     &                             "date"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "units",
     &                             "day as %Y%m%d.%f"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "C_format",
     &                             "%13.4f"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "FORTRAN_format",
     &                             "(f13.4)"))
          if     (artype.eq.2) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_methods",
     &                               "mean"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_extent",
     &                               cell))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_methods",
     &                               "mean"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_extent",
     &                               cell))
          elseif (artype.eq.3) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_methods",
     &                               "standard_deviation"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_extent",
     &                               cell))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_methods",
     &                               "standard_deviation"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_extent",
     &                               cell))
          endif
          call ncheck(nf90_def_var(ncfileID,"Depth", nf90_float,
     &                             lyrDimID,lyrVarID))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "units","m"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "positive","down"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "axis","Z"))
          call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                             pLatDimID,pLatVarID))
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "standard_name","latitude"))
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "units","degrees_north"))
          if     (abs((platj(jlatn)-platj(1))-
     &                (platj(2)    -platj(1))*(jlatn-1)).lt.1.e-2) then
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "point_spacing","even"))  !ferret
          endif
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "axis","Y"))
          ! model 3Z variable
          call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                             (/pLatDimID,
     &                               lyrDimID, MTDimID/),
     &                             varID))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "coordinates",
     &                             "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables
          call ncheck(nf90_put_var(ncfileID,MTVarID,time3(3)))
          call ncheck(nf90_put_var(ncfileID,datVarID,date   ))
          call ncheck(nf90_put_var(ncfileID,pLatVarID,platj(:)))
          call ncheck(nf90_put_var(ncfileID,lyrVarID,zz(:)))
          ! write to model variable
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:)))
          ! close NetCDF file
          call ncheck(nf90_close(ncfileID))
        else
c
c        Write data to existing NetCDF file
c
          ! open NetCDF file
          call ncheck(nf90_open(trim(ncfile), nf90_write, ncfileID))
          ! get dimension ID's
          call ncheck(nf90_inq_dimid(ncfileID,"MT",MTDimID))
          call ncheck(nf90_inq_dimid(ncfileID,"Depth",lyrDimID))
          call ncheck(nf90_inq_dimid(ncfileID,
     &                               "Latitude",pLatDimID))
          !  switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable
          call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                             (/pLatDimID,
     &                               lyrDimID, MTDimID/),
     &                             varID))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "coordinates",
     &                             "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
          ! inquire variable ID
          call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
          !write values into array
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:)))
          ! close file
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a49,a,2g15.6)') 
     &    trim(name)//label(51:81),':',hmin(1),hmax(1)
        call flush(lp)
      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout_jz - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop
      endif
      return
      end

      subroutine ncheck(status)
      use mod_xc   ! HYCOM communication API
      use netcdf   ! NetCDF fortran 90 interface
      implicit none
c
      integer, intent(in) :: status
c
c     subroutine to handle NetCDF errors
c
      if (status /= nf90_noerr) then
        write(lp,'(/a)')   'error in horout - from NetCDF library'
        write(lp,'(a/)')   trim(nf90_strerror(status))
        call flush(lp)
        stop
      end if
      end subroutine ncheck

      subroutine ncrange(h,ii,jj,kk, fill_value, hmin,hmax)
      implicit none
c
      integer, intent(in ) :: ii,jj,kk
      real,    intent(in ) :: h(ii,jj,kk),fill_value
      real,    intent(out) :: hmin,hmax
c
c     return range of array, ignoring fill_value
c
      integer i,j,k
      real    hhmin,hhmax
c
      hhmin =  abs(fill_value)
      hhmax = -abs(fill_value)
      do k= 1,kk
        do j= 1,jj
          do i= 1,ii
            if     (h(i,j,k).ne.fill_value) then
              hhmin = min(hhmin,h(i,j,k))
              hhmax = max(hhmax,h(i,j,k))
            endif
          enddo
        enddo
      enddo
      hmin = hhmin
      hmax = hhmax
      end subroutine ncrange
