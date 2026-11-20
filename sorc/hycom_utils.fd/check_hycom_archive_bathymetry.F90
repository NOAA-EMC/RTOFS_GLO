! ==============================================================================
! Program: check_hycom_archive_bathymetry
!
! Description: 
! - Checks that a HYCOM archive is consistent with the bathymetry (depth) file.
! - Generates an anomaly binary (.a) and NetCDF (.nc) containing the difference
!   between the sum of the fluid layers and the bathymetry.
! - Optionally writes corrected bathymetry (.a and .b) where mismatch > 1 cm.
! ==============================================================================

program check_hycom_archive_bathymetry
  use hycom_io_utils
  implicit none

  real, parameter :: qonem = 1.0 / 9806.0 ! 1/(g*1000)
  real, parameter :: spval = 2.0**100

  real, allocatable :: depth(:,:), depth_corr(:,:), dp(:,:), pk(:,:)
  real              :: pad(4096)

  integer :: narg
  character(len=240) :: cfilea, cfileb, cfiled, cfiledb, cfilem, cfilenc, cfilec, cfilecb

  integer :: idm, jdm, kdm, nsurf, nlay, iexpt, yrflag
  integer :: ibads, nbad1, nbad2
  integer :: npad
  real    :: thbase, sigma(99), time

  integer :: i, j, k, krec0, ios, nrecl
  real    :: zmin, zmax, dmin, dmax

  ! Read command-line arguments (Requires archv.a, depth.a, anom.a; optional: corrected_depth.a)
  narg = command_argument_count()

  if (narg == 3 .or. narg == 4) then
    call get_command_argument(1, cfilea) ! Archive (.a) file
    call get_command_argument(2, cfiled) ! Bathymetry (depth) file
    call get_command_argument(3, cfilem) ! Name of the anomaly file (.a)
    if (narg == 4) then
      call get_command_argument(4, cfilec) ! Optional corrected bathymetry file (.a)
    end if
  else
    write(6,*) 'Usage: check_hycom_archive_bathymetry archv.a depth.a anom.a [corrected_depth.a]'
    call exit(1)
  end if

  ! Extract model parameters from the ".b" metadata file
  cfileb = cfilea(1:len_trim(cfilea)-1) // 'b' ! b-file corresponding to the `cfilea`
  call read_archv_meta(cfileb, iexpt, yrflag, idm, jdm, kdm, nsurf, nlay, thbase, sigma, time)

  ! Calculate padding and allocate arrays based on dimensions read from metadata
  npad = 4096 - mod(idm * jdm, 4096)
  if (npad == 4096) npad = 0

  allocate(depth(idm, jdm), depth_corr(idm, jdm), dp(idm, jdm), pk(idm, jdm), stat=ios)
  if (ios /= 0) then
    write(6,*) 'Error: could not allocate memory for arrays'
    call exit(2)
  end if

  ! Determine record length portably (system dependent: words vs bytes)
  if (npad == 0) then
    inquire(iolength=nrecl) dp
  else
    inquire(iolength=nrecl) dp, pad(1:npad)
  end if

  ! Read bathymetry (depths)
  call open_da_file(51, cfiled, 'old', nrecl) ! Depth file (read)
  call read_da_record(depth, idm, jdm, 1, 51, cfiled)
  close(51)
  
  ! Replace negative (land) bathymetry values with special value flag
  do j = 1, jdm
    do i = 1, idm
      if (depth(i, j) < 0.0) depth(i, j) = spval
    end do
  end do

  ! Read layer thicknesses (dp) from the archive and sum them into pk (pressure/thickness)
  call open_da_file(11, cfilea, 'old', nrecl) ! Archive file (read)
  pk(:,:) = 0.0
  do k = 1, kdm ! all layers
    krec0 = nsurf + nlay * (k - 1)
    call read_da_record(dp, idm, jdm, krec0 + 1, 11, cfilea)
    
    do j = 1, jdm
      do i = 1, idm
        if (dp(i, j) /= spval) then ! over sea points
          pk(i, j) = pk(i, j) + dp(i, j)
        else
          pk(i, j) = spval ! over land points
        end if
      end do
    end do
  end do
  close(11)

  ! Compare archive layer sums against the bathymetric depth
  ibads = 0
  nbad1 = 0
  nbad2 = 0
  zmin =  huge(zmin)
  zmax = -huge(zmax)
  dmin =  huge(dmin)
  dmax = -huge(dmax)

  do j = 1, jdm
    do i = 1, idm
      
      ! Check for Land/Sea mask mismatches
      if (depth(i, j) == spval .and. pk(i, j) /= spval) then
        nbad1 = nbad1 + 1
        if (nbad1 <= 100 .or. mod(nbad1, 100) == 1) then
          write(6,"(a,2i6)") 'sea over land:', i, j
        end if
      end if
      
      if (depth(i, j) /= spval .and. pk(i, j) == spval) then
        nbad2 = nbad2 + 1
        if (nbad2 <= 100 .or. mod(nbad2, 100) == 1) then
          write(6,"(a,2i6)") 'land over sea:', i, j
        end if
      end if

      ! Calculate depth anomaly (Difference between static depth and summed layer depth)
      if (pk(i, j) /= spval) then
        dp(i, j) = depth(i, j) - pk(i, j) * qonem
        zmin = min(zmin, dp(i, j))
        zmax = max(zmax, dp(i, j))
        
        if (abs(dp(i, j)) > 0.01) then  ! Mismatch > 1cm
          ibads = ibads + 1
          depth_corr(i, j) = depth(i, j) - dp(i, j)

          if (ibads == 1) then
            write(6,'(a)') 'Format of the mismatch: i, j, dp, depth'
          end if

          if (ibads <= 100 .or. mod(ibads, 100) == 1) then
            write(6,'(a,2i6,2f14.8)') 'large archive depth mismatch: ', i, j, dp(i, j), depth(i, j)
          end if
        else
          depth_corr(i, j) = depth(i, j)
        end if

        dmin = min(dmin, depth_corr(i, j))
        dmax = max(dmax, depth_corr(i, j))
      else
        dp(i, j)         = spval
        depth_corr(i, j) = spval
      end if

    end do
  end do

  ! Print diagnostic summary
  write(6,*)
  if (nbad1 + nbad2 == 0) then
    write(6,'(A)') 'ARCHIVE LAND/SEA is OK'
  end if
  if (nbad1 /= 0) then
    write(6,'(A,A,I9,A)') 'ARCHIVE', ' has', nbad1, ' sea values over land'
  end if
  if (nbad2 /= 0) then
    write(6,'(A,A,I9,A)') 'ARCHIVE', ' has', nbad2, ' land values over sea'
  end if
  
  if (ibads /= 0) then
    write(6,*)
    write(6,*) 'error - wrong bathymetry for this archive file'
    write(6,*) 'number of depth mismatches = ', ibads
  end if
  write(6,*)

  ! Write the calculated anomalies out to the direct-access file (anom.a)
  write(6,'(A,2F14.8)') 'Depth anomaly: min,max =', zmin, zmax
  call open_da_file(21, cfilem, 'replace', nrecl) ! Anomaly file (write)
  call write_da_record(dp, idm, jdm, 1, 21, npad, spval)
  close(21)

  ! Write NetCDF version of anomaly and corrected depth
  cfilenc = cfilem(1:len_trim(cfilem)-1) // 'nc'
  call write_anom_nc(cfilenc, idm, jdm, dp, depth, depth_corr, spval)
  write(6,'(A,A)') 'NetCDF file written to: ', trim(cfilenc)

  ! Write corrected bathymetry (.a and .b) if 4th argument was provided
  if (narg == 4) then
    call open_da_file(23, cfilec, 'replace', nrecl)
    call write_da_record(depth_corr, idm, jdm, 1, 23, npad, spval)
    close(23)

    cfiledb = cfiled(1:len_trim(cfiled)-1) // 'b'
    cfilecb = cfilec(1:len_trim(cfilec)-1) // 'b'
    call write_depth_b(cfiledb, cfilecb, dmin, dmax)
    write(6,'(A,2F12.5)') 'Corrected depth min,max =', dmin, dmax
    write(6,'(A,A)') 'Corrected bathymetry written to: ', trim(cfilec)
  end if

end program check_hycom_archive_bathymetry
