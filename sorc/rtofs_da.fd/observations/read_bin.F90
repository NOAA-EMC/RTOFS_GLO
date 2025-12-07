!> To read in binary formatted file(s) that contains 
!! quality controlled observations information
module read_bin

use netcdf

implicit none

private :: sst_write_to_netcdf, &
           ssh_write_to_netcdf, &
           check

public :: getInputs, &
          sst_converter, &
          ssh_converter

logical, parameter :: verbose = .true.     !< Write (true) diagnostic info to STDOUT

real, parameter :: missing_value = -999.0  !< Missing value
integer, parameter :: unit = 10            !< File unit number
integer, parameter :: len_sst_date_str = 12    !< Length of dtg character, it is a DATE!
integer, parameter :: len_ssh_date_str = 14    !< Length of dtg character, it is a DATE!

integer :: n_read !< Number of "observations"
integer :: n_chn  !< Number of satellite channels; in binary file, but not used.
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


!> Reads (binary) sst obsertations
subroutine sst_converter(input_file, observation_type, output_path, output_file)
  character(len=*), intent(in) :: input_file         ! Name of the input file
  character(len=*), intent(in) :: observation_type   ! Observation type and platform
  character(len=*), intent(in) :: output_path        ! Path to output
  character(len=*), intent(in) :: output_file        ! Output file name

  ! local variables
  integer, dimension(:), allocatable :: &
    flg, sat, water_mass_class_ind

  real, dimension(:), allocatable :: &
    age, bias, err, lat, lon, qc, sst

  character, allocatable :: dtg(:)  * len_sst_date_str

! print *, "Reading input file name:" , trim(input_file)
  open(unit, file=trim(input_file), status='old', &
    access='sequential', form='unformatted')

  read (unit) n_read, n_chn, vrsn
  if (n_read > 0) then
    allocate( age(n_read), bias(n_read), dtg(n_read), &
              err(n_read), flg(n_read), &
              lat(n_read), lon(n_read), &
              qc(n_read),  sst(n_read), sat(n_read), &
              water_mass_class_ind(n_read))

    read (unit) age
    read (unit) bias
    read (unit) dtg
    read (unit) err
    read (unit) flg
    read (unit) lat
    read (unit) lon
    read (unit) qc
    read (unit) sst
    read (unit) sat ! Named "typ", set via include/coda_types.h 
    read (unit) water_mass_class_ind

    ! write to netcdf file
    call sst_write_to_netcdf(output_path, output_file, n_read, &
     bias, dtg, err, flg, lat, lon, qc, sst, sat, water_mass_class_ind)

    deallocate( age, bias, dtg, err, flg, lat, lon, &
              qc,  sst, sat, water_mass_class_ind)

  endif
  close(unit)
end subroutine sst_converter


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

  character, allocatable :: dtg(:)  * len_ssh_date_str
  character, allocatable :: rcpt(:) * len_ssh_date_str

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
 subroutine ssh_write_to_netcdf(path, output_file, n_read, &
      sat, cyc, trck, date_str, lat, lon, ssh, sla, qc)

  character(len=*), intent(in) :: path, output_file
  integer, intent(in) :: n_read
  integer, dimension(n_read), intent(in) :: sat, cyc, trck
  real, dimension(n_read), intent(in) :: lat, lon, ssh, sla, qc
  character(len=len_ssh_date_str), dimension(n_read), intent(in) :: date_str

  ! Local variables
  integer :: ncid, dimid_n, dimid_len_str
  integer :: varid_sat, varid_cyc, varid_trck, varid_date, &
             varid_lat, varid_lon, varid_ssh, varid_sla, varid_qc
  character(len=512) :: file_path
  character(len=*), parameter :: title = &
    "Quality controlled sea surface height from NCEP RTOFS"

  !  Construct full file path
  file_path = trim(path) // '/' // trim(output_file)
  if (verbose) print *, "Saving file: ", file_path

  call check( nf90_create(trim(file_path), NF90_CLOBBER, ncid)) ! Create netCDF file

  ! Define dimensions
  call check( nf90_def_dim(ncid, "n_read", n_read, dimid_n)) 
  call check( nf90_def_dim(ncid, "string_length", len_ssh_date_str, dimid_len_str))

  ! Define global attribute
  call check( nf90_put_att(ncid, NF90_GLOBAL, "title", trim(title)))

  ! Define variables
  call check( nf90_def_var(ncid, "sat",   NF90_INT,   dimid_n, varid_sat))
  call check( nf90_def_var(ncid, "cyc",   NF90_INT,   dimid_n, varid_cyc))
  call check( nf90_def_var(ncid, "track", NF90_INT,   dimid_n, varid_trck))
  call check( nf90_def_var(ncid, "lat",   NF90_FLOAT, dimid_n, varid_lat))
  call check( nf90_def_var(ncid, "lon",   NF90_FLOAT, dimid_n, varid_lon))
  call check( nf90_def_var(ncid, "ssh",   NF90_FLOAT, dimid_n, varid_ssh))
  call check( nf90_def_var(ncid, "sla",   NF90_FLOAT, dimid_n, varid_sla))
  call check( nf90_def_var(ncid, "qc",    NF90_FLOAT, dimid_n, varid_qc))

  ! Note: Order of dimensions (strings need to be handled with care!)
  call check( nf90_def_var(ncid, "date",  NF90_CHAR,  (/dimid_len_str, dimid_n/), varid_date))

  call check( nf90_enddef(ncid)) ! End define mode

  ! Write data to variables
  call check( nf90_put_var(ncid, varid_sat, sat))
  call check( nf90_put_var(ncid, varid_cyc, cyc))
  call check( nf90_put_var(ncid, varid_trck, trck))
  call check( nf90_put_var(ncid, varid_date, date_str))
  call check( nf90_put_var(ncid, varid_lat, lat))
  call check( nf90_put_var(ncid, varid_lon, lon))
  call check( nf90_put_var(ncid, varid_ssh, ssh))
  call check( nf90_put_var(ncid, varid_sla, sla))
  call check( nf90_put_var(ncid, varid_qc, qc))

  call check( nf90_close(ncid)) ! Close the netCDF file
end subroutine ssh_write_to_netcdf


!> Writes sst obsertations to a netCDF file
subroutine sst_write_to_netcdf(path, fname, n_read, &
     bias, date_str, err, flg, lat, lon, qc, sst, sat, wmid)

  character(len=*), intent(in) :: path, fname
  integer, intent(in) :: n_read
  integer, dimension(n_read), intent(in) :: flg, sat, wmid
  real, dimension(n_read), intent(in) :: bias, err, lat, lon, qc, sst
  character(len=len_sst_date_str), dimension(n_read), intent(in) :: date_str

  ! Local variables
  integer :: ncid, dimid_n, dimid_len_str
  integer :: varid_bias, varid_date, varid_err, varid_flg, &
             varid_lat, varid_lon, varid_qc, varid_sst, varid_sat, varid_wmid
  character(len=512) :: file_path
  character(len=*), parameter :: title = &
    "Quality controlled sea surface temperature from NCEP RTOFS"

  ! Construct full file path
  file_path = trim(path) // '/' // trim(fname)
  if (verbose) print *, "Saving file: ", file_path

  call check( nf90_create(trim(file_path), NF90_CLOBBER, ncid)) ! Create netCDF file

  ! Define Dimensions
  call check( nf90_def_dim(ncid, "n_read", n_read, dimid_n)) 
  call check( nf90_def_dim(ncid, "string_length", len_sst_date_str, dimid_len_str))

  ! Define global attribute
  call check( nf90_put_att(ncid, NF90_GLOBAL, "title", trim(title)))

  ! Define Variables
  call check( nf90_def_var(ncid, "bias", NF90_FLOAT, dimid_n, varid_bias))
  call check( nf90_def_var(ncid, "date", NF90_CHAR,  (/dimid_len_str, dimid_n/), varid_date))
  call check( nf90_def_var(ncid, "err",  NF90_FLOAT, dimid_n, varid_err))
  call check( nf90_def_var(ncid, "flag", NF90_INT,   dimid_n, varid_flg))
  call check( nf90_def_var(ncid, "lat",  NF90_FLOAT, dimid_n, varid_lat))
  call check( nf90_def_var(ncid, "lon",  NF90_FLOAT, dimid_n, varid_lon))
  call check( nf90_def_var(ncid, "qc",   NF90_FLOAT, dimid_n, varid_qc))
  call check( nf90_def_var(ncid, "sst",  NF90_FLOAT, dimid_n, varid_sst))
  call check( nf90_def_var(ncid, "sat_id",        NF90_INT,   dimid_n, varid_sat))  ! Satellite ID
  call check( nf90_def_var(ncid, "water_mass_id", NF90_INT,   dimid_n, varid_wmid)) !Water Mass Index

  call check( nf90_enddef(ncid)) ! End define mode

  ! Write data to variables
  call check( nf90_put_var(ncid, varid_bias, bias))
  call check( nf90_put_var(ncid, varid_date, date_str))
  call check( nf90_put_var(ncid, varid_err,  err))
  call check( nf90_put_var(ncid, varid_flg,  flg))
  call check( nf90_put_var(ncid, varid_lat,  lat))
  call check( nf90_put_var(ncid, varid_lon,  lon))
  call check( nf90_put_var(ncid, varid_qc,   qc))
  call check( nf90_put_var(ncid, varid_sst,  sst))
  call check( nf90_put_var(ncid, varid_sat,  sat))
  call check( nf90_put_var(ncid, varid_wmid, wmid))

  call check( nf90_close(ncid)) ! Close the netCDF file
end subroutine sst_write_to_netcdf

!> From https://home.chpc.utah.edu/~thorne/computing/Examples_netCDF.pdf
subroutine check(istatus)
  integer, intent (in) :: istatus
  if (istatus /= nf90_noerr) then
    print*, trim(adjustl(nf90_strerror(istatus)))
  end iF
end subroutine check


end module read_bin
