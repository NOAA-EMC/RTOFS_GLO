program read_bin_qc_obs

!
! Read quality controlled observations:
! - From a binary formatted file.
! - Write to a netcdf formatted file.  
!
! Input:
! - File: `yyyymmddhh.<STR>`, oType, oPath
!   - `STR` Relates to the following:
! - `oType`: Observation type (e.g., ssh, sst, sss, etc) and/or platform.
! - `oPath`: Path to where output should be written out.
!
! Output:
! - NetCDF file: `<oPath>/yyyymmddhh.<STR>.nc`
!
! - Remarks:
!   None.
!

  use read_bin, only : getInputs, &
   sst_converter, ssh_converter

  implicit none

  logical, parameter :: verbose = .true. ! Write (true) diagnostic information to STDOUT

  integer, parameter :: num_inputs = 4 ! Number of input arguments
  character(len=100) :: in_fName       ! Name of the input file
  character(len=100) :: oType          ! Observation type and/or platform

  character(len=200) :: oPath          ! Path to output
  character(len=100) :: out_fName      ! Name of the output file

  logical :: exists, input_read_file_ok

! Read inputs
! -----------
  input_read_file_ok = .false.  ! Unless input file is found, assume failure.
  
  call getInputs(num_inputs, in_fName, oType, oPath, out_fName)

  inquire(file=trim(in_fName), exist=exists)
  print *, " "
  if ( exists) then
    input_read_file_ok = .true. ! Input file exists: success!
  else
    print *, "Error reading input file name: " , trim(in_fName)
    stop 'EXIT.'
  end if

! Convert binary to netcdf
! -------------------------

  if (oType == "sst") &
    call sst_converter(in_fName, oType, oPath, out_fName)

  if (oType == "ssh") &
    call ssh_converter(in_fName, oType, oPath, out_fName)

! Finish
! ------
  if (verbose) then
    print *, "NetCDF formatted file with contents read from: ", trim(in_fName)
    print *, "Has been written out. Check: ", trim(oPath) // '/' // trim(out_fName)
    print *, " "
    print *, "All done."
    print *, " "
  end if

end program read_bin_qc_obs
