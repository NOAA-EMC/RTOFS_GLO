! ==============================================================================
! Module: hycom_io_utils
! Description: Utility routines for reading/writing HYCOM (archive/depth) files:
! -  (b-file) metadata, (a-file) direct-access, and NetCDF diagnostics.
! ==============================================================================

module hycom_io_utils
  use netcdf
  implicit none
  private
  public :: read_archv_meta, read_da_record, open_da_file, &
            write_da_record, write_depth_b, write_anom_nc

contains

  ! ----------------------------------------------------------------------------
  ! Subroutine: open_da_file
  ! Description: Opens an unformatted direct-access file and handles I/O errors.
  ! ----------------------------------------------------------------------------
  subroutine open_da_file(iunit, filename, file_status, file_recl)
    integer, intent(in)          :: iunit
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: file_status
    integer, intent(in)          :: file_recl
    integer                      :: ios

    open(unit=iunit, file=filename, form='unformatted', status=file_status, &
         access='direct', recl=file_recl, iostat=ios)

    if (ios /= 0) then
      write(6,*) 'Error: cannot open ', trim(filename)
      write(6,*) 'ios   = ', ios
      write(6,*) 'nrecl = ', file_recl
      call exit(3)
    end if
  end subroutine open_da_file

  ! ----------------------------------------------------------------------------
  ! Subroutine: read_da_record
  ! Description: Reads a single 2D grid record from a direct-access file.
  ! ----------------------------------------------------------------------------
  subroutine read_da_record(a, idm, jdm, krec, iunit, filename)
    integer, intent(in)          :: idm, jdm, krec, iunit
    real, intent(out)            :: a(idm, jdm)
    character(len=*), intent(in) :: filename
    integer                      :: ios

    read(iunit, rec=krec, iostat=ios) a

#ifdef ENDIAN_IO
    call endian_swap(a, idm*jdm)
#endif
    if (ios /= 0) then
      write(6,*) 'Error: cannot read record ', krec, ' from file ', trim(filename)
      call exit(4)
    end if
  end subroutine read_da_record

  ! ----------------------------------------------------------------------------
  ! Subroutine: write_da_record
  ! Description: Writes a single 2D grid record (with 4096-word padding) to a
  !              direct-access file.
  ! ----------------------------------------------------------------------------
  subroutine write_da_record(a, idm, jdm, krec, iunit, npad, spval)
    integer, intent(in) :: idm, jdm, krec, iunit, npad
    real, intent(in)    :: a(idm, jdm), spval
    real                :: pad(4096)

    if (npad == 0) then
      write(iunit, rec=krec) a
    else
      pad(1:npad) = spval
      write(iunit, rec=krec) a, pad(1:npad)
    end if
  end subroutine write_da_record

  ! ----------------------------------------------------------------------------
  ! Subroutine: write_depth_b
  ! Description: Copies the 5-line header from the original regional.depth.b
  !              and writes the updated min/max depth line for the corrected .b.
  ! ----------------------------------------------------------------------------
  subroutine write_depth_b(orig_b, corr_b, dmin, dmax)
    character(len=*), intent(in) :: orig_b, corr_b
    real, intent(in)             :: dmin, dmax
    integer                      :: ios, k
    character(len=240)           :: cline

    open(unit=12, file=orig_b, form='formatted', status='old', iostat=ios)
    if (ios /= 0) then
      write(6,*) 'Error: cannot open ', trim(orig_b)
      call exit(3)
    end if

    open(unit=22, file=corr_b, form='formatted', status='replace', iostat=ios)
    if (ios /= 0) then
      write(6,*) 'Error: cannot open ', trim(corr_b)
      call exit(3)
    end if

    do k = 1, 5
      read(12, '(a)') cline
      write(22, '(a)') trim(cline)
    end do
    write(22, '(a,2f12.5)') 'min,max depth = ', dmin, dmax

    close(unit=12)
    close(unit=22)
  end subroutine write_depth_b

  ! ----------------------------------------------------------------------------
  ! Subroutine: write_anom_nc
  ! Description: Writes depth_anom, depth_orig, and depth_corr to a NetCDF file.
  ! ----------------------------------------------------------------------------
  subroutine write_anom_nc(nc_filename, idm, jdm, anom, depth_orig, depth_corr, spval)
    character(len=*), intent(in) :: nc_filename
    integer, intent(in)          :: idm, jdm
    real, intent(in)             :: anom(idm, jdm), depth_orig(idm, jdm), depth_corr(idm, jdm)
    real, intent(in)             :: spval

    integer :: ncid, dimids(2), nx_dimid, ny_dimid
    integer :: varid_anom, varid_orig, varid_corr

    call check_nc(nf90_create(trim(nc_filename), ior(nf90_clobber, nf90_64bit_offset), ncid))

    call check_nc(nf90_def_dim(ncid, 'nx', idm, nx_dimid))
    call check_nc(nf90_def_dim(ncid, 'ny', jdm, ny_dimid))
    dimids = (/ nx_dimid, ny_dimid /)

    call check_nc(nf90_def_var(ncid, 'depth_anom', nf90_float, dimids, varid_anom))
    call check_nc(nf90_put_att(ncid, varid_anom, '_FillValue', spval))
    call check_nc(nf90_put_att(ncid, varid_anom, 'units', 'm'))
    call check_nc(nf90_put_att(ncid, varid_anom, 'long_name', 'static_depth minus archive_summed_depth'))

    call check_nc(nf90_def_var(ncid, 'depth_orig', nf90_float, dimids, varid_orig))
    call check_nc(nf90_put_att(ncid, varid_orig, '_FillValue', spval))
    call check_nc(nf90_put_att(ncid, varid_orig, 'units', 'm'))
    call check_nc(nf90_put_att(ncid, varid_orig, 'long_name', 'original static bathymetry'))

    call check_nc(nf90_def_var(ncid, 'depth_corr', nf90_float, dimids, varid_corr))
    call check_nc(nf90_put_att(ncid, varid_corr, '_FillValue', spval))
    call check_nc(nf90_put_att(ncid, varid_corr, 'units', 'm'))
    call check_nc(nf90_put_att(ncid, varid_corr, 'long_name', 'corrected bathymetry (depth_orig - depth_anom)'))

    call check_nc(nf90_enddef(ncid))

    call check_nc(nf90_put_var(ncid, varid_anom, anom))
    call check_nc(nf90_put_var(ncid, varid_orig, depth_orig))
    call check_nc(nf90_put_var(ncid, varid_corr, depth_corr))

    call check_nc(nf90_close(ncid))
  end subroutine write_anom_nc

  subroutine check_nc(status)
    integer, intent(in) :: status
    if (status /= nf90_noerr) then
      write(6,*) 'NetCDF Error: ', trim(nf90_strerror(status))
      call exit(5)
    end if
  end subroutine check_nc

  ! ----------------------------------------------------------------------------
  ! Subroutine: read_archv_meta
  ! Description: Extracts needed model dimensions and parameters from the 
  !              plain-text archive .b file.
  ! ----------------------------------------------------------------------------
  subroutine read_archv_meta(filename_b, iexpt, yrflag, idm, jdm, kdm, nsurf, nlay, thbase, sigma, time)
    character(len=*), intent(in)  :: filename_b
    integer, intent(out)          :: iexpt, yrflag, idm, jdm, kdm, nsurf, nlay
    real, intent(out)             :: thbase, sigma(99), time
    
    integer            :: ios, k, l
    character(len=6)   :: cvarin
    character(len=240) :: cline

    open(unit=12, file=filename_b, form='formatted', status='old', iostat=ios)
    if (ios /= 0) then
      write(6,*) 'Error: cannot open ', trim(filename_b)
      write(6,*) 'ios   = ', ios
      call exit(3)
    end if

    ! Skip the first 4 title lines and iversn
    read(12,*) 
    read(12,*) 
    read(12,*) 
    read(12,*) 
    read(12,*) 
    
    read(12,*) iexpt, cvarin
    if (cvarin /= 'iexpt ') then
      write(6,*) 'Error in hycom_profile: bad .b file'
      write(6,*) 'filename: ', trim(filename_b)
      call exit(4)
    end if
    
    read(12,*) yrflag
    read(12,*) idm
    read(12,*) jdm

    ! Find NSURF (Number of surface 2D fields before 3D layers begin)
    read(12,'(a)') cline
    do l = 1, 99
      read(12,'(a)', iostat=ios) cline
      if (ios /= 0) then
        exit
      else if (cline(1:8) == 'thknss  ') then
        exit
      end if
    end do
    nsurf = l - 1

    ! Find NLAY (Number of variables per 3D layer, e.g., u, v, dp, temp, saln...)
    do l = 2, 99
      read(12,'(a)', iostat=ios) cline
      if (ios /= 0) then
        exit
      else if (cline(1:8) == 'thknss  ') then
        exit
      end if
    end do
    nlay = l - 1

    ! Find KDM (Total number of vertical layers)
    rewind(unit=12)
    do k = 1, nsurf + 10
      read(12,'(a)') cline
    end do

    do k = 1, 999
      read(12,'(a)', iostat=ios) cline
      if (ios /= 0) exit
      if (cline(1:8) /= 'thknss  ') exit

      do l = 2, nlay
        read(12,'(a)', iostat=ios) cline
        if (ios /= 0) exit
      end do
    end do
    kdm = k - 1

    close(unit=12)

    write(6,*)
    write(6,*) 'Finished read: ', trim(filename_b)
    Write(6,*) 'Dimensions: '
    Write(6,*) 'nx: ', idm, 'ny: ', jdm, 'nz: ', kdm
    write(6,*)
  end subroutine read_archv_meta

end module hycom_io_utils
