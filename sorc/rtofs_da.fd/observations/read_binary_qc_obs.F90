program read_bin_qc_obs

!
! File format conversion of quality controlled observations:
! - Read from a binary file.
! - Write to a netcdf file.
!

  use read_binary_write_nc_mod, only : getInputs, &
                                       sst_converter, &
                                       ice_converter, &
                                       ssh_converter, &
                                       sss_converter, &
                                       mdb_converter, &
                                       velocity_converter, &
                                       sfc_converter, &
                                       profile_converter

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

  if (oType == "sst") then
    call sst_converter(in_fName, oType, oPath, out_fName)
  elseif (oType == "ice") then
    call ice_converter(in_fName, oType, oPath, out_fName)
  elseif (oType == "ssh") then
    call ssh_converter(in_fName, oType, oPath, out_fName)
  elseif (oType == "sss") then
    call sss_converter(in_fName, oType, oPath, out_fName)
  elseif (oType == "mdb") then
    call mdb_converter(in_fName, oType, oPath, out_fName)
  elseif (oType == "velocity") then
    call velocity_converter(in_fName, oType, oPath, out_fName)
  elseif (oType == "sfc") then
    call sfc_converter(in_fName, oType, oPath, out_fName)
  elseif (oType == "profile") then
    call profile_converter(in_fName, oType, oPath, out_fName)
  else
    print *, "Input observation type: ", trim(oType), " is not supported."
    stop     'Fix and try again.'
  endif

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
