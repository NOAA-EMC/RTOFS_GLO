program read_bin_qc_obs

!
! Read quality controlled observations:
! - From a binary formatted file.
! - Write to a netcdf formatted file.  
!
! Input:
! - File: `yyyymmddhh.<STR>`, oType, oPlat
!   - `STR` relates to the following:
! - `oType`: observation type (e.g., ssh, sst, sss, etc).
! - `oPlat`: observation platform.
!
! Output:
! - NetCDF file: `<oType>_<oPlat>_yyyymmddhh.nc`
!
! - Remarks:
!   1. In a few cases, it is possible that STR == oType.
!

  implicit none

  integer, parameter :: num_inputs = 3 ! Number of input arguments
  character(len=100) :: prog_name      ! Name of program (exec)
  character(len=100) :: in_fName       ! Name of the input file
  character(len=100) :: oType, oPlat   ! Observation type and platform

  real, parameter :: missing_value = -999.0      ! Missing value
  integer :: iargc_count
  logical :: exists, input_read_ok

  integer :: n_read, n_lvl, vrsn

  integer, dimension(:), allocatable :: &
     ob_cyc, ob_ltc, ob_sat, ob_smpl, ob_trck

  real, dimension(:), allocatable :: &
     ob_age, ob_lat, ob_lon, ob_qc, ob_ssh, ob_sla

  character, allocatable :: ob_dtg(:)  *14
  character, allocatable :: ob_rcpt(:) *14

! set default
  input_read_ok = .false.  ! Unless file is found, assume no file is found.
  
  ! Get command-line (input) arguments
  iargc_count = IARGC()

  if (iargc_count < num_inputs) then
    call getarg(0, prog_name)
    print *, " "
    print *, trim(prog_name)
    print *, "Expected number of inputs:", num_inputs
    print *, "But ", iargc_count, "were found. Fix and try again."
    print *, " "
    stop
  endif

  call getarg(1, in_fName)
  call getarg(2, oType)
  call getarg(3, oPlat)

  inquire(file=trim(in_fName), exist=exists)
  print *, " "
  if ( exists) then
    input_read_ok = .true.
  else
    print *, "Error reading input file name: " , trim(in_fName)
    stop 'EXIT.'
  end if

  print *, "Reading input file name:" , trim(in_fName)
  open(10, file=trim(in_fName), status='old', &
           access='sequential', form='unformatted')
  read (10) n_read, n_lvl, vrsn

  if (n_read > 0) then
    allocate( ob_age(n_read), ob_cyc(n_read), ob_lat(n_read), ob_lon(n_read), &
              ob_qc(n_read), ob_sat(n_read), ob_smpl(n_read), ob_ssh(n_read), &
              ob_trck(n_read), ob_ltc(n_read), ob_dtg(n_read), ob_rcpt(n_read), &
              ob_sla(n_read))

    read (10) ob_age
    read (10) ob_cyc
    read (10) ob_lat
    read (10) ob_lon
    read (10) ob_qc
    read (10) ob_sat
    read (10) ob_smpl
    read (10) ob_ssh
    read (10) ob_trck
    read (10) ob_ltc
    read (10) ob_dtg
    read (10) ob_rcpt

    if (vrsn > 1) then
       read (10) ob_sla
    else
       ob_sla(:) = missing_value
    endif

!    print *, ob_age(1), ob_cyc(1), ob_lat(1), ob_lon(1), &
!      ob_qc(1), ob_sat(1), ob_smpl(1), ob_ssh(1), &
!      ob_trck(1), ob_ltc(1), ob_dtg(1), ob_rcpt(1), ob_sla(1)
     print *, ob_sat

    deallocate( ob_age, ob_cyc, ob_lat, ob_lon, &
                ob_qc, ob_sat, ob_smpl, ob_ssh, &
                ob_trck, ob_ltc, ob_dtg, ob_rcpt, ob_sla)
  endif
  close(10)

  print *, n_read, n_lvl, vrsn


end program read_bin_qc_obs
