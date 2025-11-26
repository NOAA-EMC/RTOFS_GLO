program read_bin_qc_obs

!
! Read quality controlled observations:
! - From a binary formatted file.
! - Write to a netcdf formatted file.  
!
! Input:
! - File: `yyyymmddhh.<STR>`
! - Where `STR`: relates to observation type/observation platform.
!
! Output:
! - NetCDF file: `<STR>_yyyymmddhh.nc`
!

  implicit none

  integer, parameter :: num_inputs = 1
  character(len=100) :: prog_name, in_fName
  integer :: status, iargc_count, &
             n_new, n_lvl, vrsn
  
  ! Get command-line (input) arguments
  iargc_count = IARGC()

  if (iargc_count < num_inputs) then
    call getarg(0, prog_name)
    print *, " "
    print *, trim(prog_name)
    print *, "Expected number of inputs:", num_inputs
    print *, "Fix and try again."
    print *, " "
    stop
  endif

  call getarg(1, in_fName, status)
  print *, " "
  if (status >= 0) then
    print *, "Input file name:"
    print *, trim(in_fName)
  else
    print *, "Error getting input argument"
  end if
  print *, " "

  open(10, file=trim(in_fName), status='old', form='unformatted')
  read (10) n_new, n_lvl, vrsn
  close(10)
  print *, n_new, n_lvl, vrsn
end program read_bin_qc_obs
