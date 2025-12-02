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

  use read_bin, only : getInputs

  implicit none

  integer, parameter :: num_inputs = 3 ! Number of input arguments
  character(len=100) :: in_fName       ! Name of the input file
  character(len=100) :: oType, oPlat   ! Observation type and platform

  character(len=200) :: output_path    ! Path to output
  character(len=100) :: out_fName      ! Name of the output file

  logical :: exists, input_read_ok

! Read inputs
! -----------
  input_read_ok = .false.  ! Unless input file is found, assume failure.
  
  call getInputs(num_inputs, in_fName, oType, oPlat)

  inquire(file=trim(in_fName), exist=exists)
  print *, " "
  if ( exists) then
    input_read_ok = .true. ! Input file exists: success!
  else
    print *, "Error reading input file name: " , trim(in_fName)
    stop 'EXIT.'
  end if

! Do the work
! -----------

! Read input file
  if (oType == "ssh") then
!   call ssh_reader(in_fName)
  end if
! call reader

! Write output file
  out_fName = trim(in_fName) // ".nc" 
! call writer


! Finish
! ------
  print *, "NetCDF formatted file with contents read from: ", in_fName
  print *, "Has been written out to: ", out_fName
  print *, "All done."
  print *, " "

end program read_bin_qc_obs
