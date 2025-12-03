!> To read in binary formatted file(s) that contains 
!! quality controlled observations information
module read_bin

implicit none

private :: ssh_write_to_netcdf
public :: getInputs, ssh_converter

real, parameter :: missing_value = -999.0  !< Missing value
integer, parameter :: unit = 10            !< File unit number

integer :: n_read !< Number of "observations"
integer :: n_lvl  !< Number of "levels", =1, unless profile observations
integer :: vrsn   !< Version number of writer/reader

contains

!> This subroutine reads inputs set in: read_bin_qc_obs
subroutine getInputs(num_inputs, &
   input_file, observation_type, output_path, output_file)
  integer, intent(in) :: num_inputs 

  character(len=*), intent(out) :: input_file         ! Name of the input file
  character(len=*), intent(out) :: observation_type   ! Observation type and platform
  character(len=*), intent(out) :: output_path        ! Path to output
  character(len=*), intent(out) :: output_file        ! Output file name
 
  ! local variables
  character(len=100) :: prog_name      ! Name of program (exec)
  integer :: iargc_count

  ! Get command-line (input) arguments
  iargc_count = IARGC()

  if (iargc_count < num_inputs) then
    call getarg(0, prog_name)
    print *, " "
    print *, trim(prog_name)
    print *, "Expected number of inputs:", num_inputs, &
             "But ", iargc_count, "were found. See below, fix and try again."
    print *, " "
    print *, trim(prog_name), " Input file name, observation type, Path to output, output file name"
    print *, " "
    stop
  endif

  call getarg(1, input_file)
  call getarg(2, observation_type)
  call getarg(3, output_path)
  call getarg(4, output_file)
end subroutine getInputs


!> Reads (binary) ssh obsertations
subroutine ssh_converter(input_file, observation_type, output_path, output_file)
  character(len=*), intent(in) :: input_file         ! Name of the input file
  character(len=*), intent(in) :: observation_type   ! Observation type and platform
  character(len=*), intent(in) :: output_path        ! Path to output
  character(len=*), intent(in) :: output_file        ! Output file name

  ! local variables

  integer, dimension(:), allocatable :: &
    cyc, ltc, sat, smpl, trck

  real, dimension(:), allocatable :: &
    age, lat, lon, qc, ssh, sla

  character, allocatable :: dtg(:)  *14 
  character, allocatable :: rcpt(:) *14 

! print *, "Reading input file name:" , trim(input_file)

  open(unit, file=trim(input_file), status='old', &
    access='sequential', form='unformatted')

  read (unit) n_read, n_lvl, vrsn
  if (n_read > 0) then
    allocate( age(n_read), cyc(n_read), lat(n_read), lon(n_read), &
              qc(n_read), sat(n_read), smpl(n_read), ssh(n_read), &
              trck(n_read), ltc(n_read), dtg(n_read), rcpt(n_read), &
              sla(n_read))

    read (unit) age
    read (unit) cyc
    read (unit) lat
    read (unit) lon
    read (unit) qc
    read (unit) sat
    read (unit) smpl
    read (unit) ssh
    read (unit) trck
    read (unit) ltc
    read (unit) dtg
    read (unit) rcpt

    if (vrsn > 1) then
       read (unit) sla
    else
       sla(:) = missing_value
    endif

    ! write to netcdf file
    call ssh_write_to_netcdf(output_path, output_file, n_read, &
     sat, cyc, trck, dtg, lat, lon, ssh, sla, qc)

    deallocate( age, cyc, lat, lon, &
                qc, sat, smpl, ssh, &
                trck, ltc, dtg, rcpt, sla)
  endif
  close(unit)


end subroutine ssh_converter


!> Writes ssh obsertations to a netCDF file
 subroutine ssh_write_to_netcdf(output_path, output_file, n_read, &
      sat, cyc, trck, dtg, lat, lon, ssh, sla, qc)

  ! print *, "Saving file: ", trim(output_path) // '/' // trim(output_file)

end subroutine ssh_write_to_netcdf


end module read_bin
