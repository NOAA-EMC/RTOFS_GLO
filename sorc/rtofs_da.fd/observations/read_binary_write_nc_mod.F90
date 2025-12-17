!> To read in binary formatted file(s) that contains 
!! quality controlled observations information
module read_binary_write_nc_mod

use netcdf

implicit none

private :: sst_write_to_netcdf, &
           ice_write_to_netcdf, &
           ssh_write_to_netcdf, &
           sss_write_to_netcdf, &
           mdb_write_to_netcdf, &
           velocity_write_to_netcdf, &
           sfc_write_to_netcdf, &
!          profile_write_to_netcdf, &
           check

public :: getInputs, &
          sst_converter, &
          ice_converter, &
          ssh_converter, &
          sss_converter, &
          mdb_converter, &
          velocity_converter, &
          sfc_converter, &
          profile_converter

logical, parameter :: verbose = .true.     !< Write (true) diagnostic info to STDOUT

real, parameter :: missing_value = -999.0  !< Missing value
integer, parameter :: unit = 10            !< File unit number
integer, parameter :: len_sst_date_str = 12    !< Length of dtg character, it is a DATE!
integer, parameter :: len_ssh_date_str = 14    !< Length of dtg character, it is a DATE!
integer, parameter :: len_sgn = 7              !< Length of sgn character.

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


!> Reads (binary) ice obsertations
subroutine ice_converter(input_file, observation_type, output_path, output_file)
  character(len=*), intent(in) :: input_file         ! Name of the input file
  character(len=*), intent(in) :: observation_type   ! Observation type and platform
  character(len=*), intent(in) :: output_path        ! Path to output
  character(len=*), intent(in) :: output_file        ! Output file name

  ! local variables
  integer, dimension(:), allocatable :: &
    flg, sat

  real, dimension(:), allocatable :: &
    age, ice, lat, lon, qc

  character, allocatable :: dtg(:)  * len_sst_date_str

! print *, "Reading input file name:" , trim(input_file)
  open(unit, file=trim(input_file), status='old', &
    access='sequential', form='unformatted')

  read (unit) n_read, n_lvl, vrsn
  if (n_read > 0) then
    allocate( age(n_read), dtg(n_read), &
              flg(n_read), ice(n_read), &
              lat(n_read), lon(n_read), &
              qc(n_read),  sat(n_read))

    read (unit) age
    read (unit) dtg
    read (unit) flg
    read (unit) ice
    read (unit) lat
    read (unit) lon
    read (unit) qc
    read (unit) sat ! Named "typ", set via include/coda_types.h 

    ! write to netcdf file
    call ice_write_to_netcdf(output_path, output_file, n_read, &
     dtg, flg, lat, lon, qc, ice, sat)

    deallocate( age, dtg, flg, ice, lat, lon, &
              qc,  sat)

  endif
  close(unit)
end subroutine ice_converter


!> Reads (binary) sss obsertations
subroutine sss_converter(input_file, observation_type, output_path, output_file)
  character(len=*), intent(in) :: input_file         ! Name of the input file
  character(len=*), intent(in) :: observation_type   ! Observation type and platform
  character(len=*), intent(in) :: output_path        ! Path to output
  character(len=*), intent(in) :: output_file        ! Output file name

  ! local variables
  integer, dimension(:), allocatable :: &
    flg, sat

  real, dimension(:), allocatable :: &
    age, err, lat, lon, qc, sss, sst

  character, allocatable :: dtg(:)  * len_sst_date_str
  character, allocatable :: rcpt(:) * len_sst_date_str

! print *, "Reading input file name:" , trim(input_file)
  open(unit, file=trim(input_file), status='old', &
    access='sequential', form='unformatted')

  read (unit) n_read, n_lvl, vrsn
  if (n_read > 0) then
    allocate( age(n_read), err(n_read), flg(n_read), &
              lat(n_read), lon(n_read), qc(n_read),  &
              sat(n_read), sss(n_read), sst(n_read), &
              dtg(n_read), rcpt(n_read))

    read (unit) age
    read (unit) err
    read (unit) flg
    read (unit) lat
    read (unit) lon
    read (unit) qc
    read (unit) sat ! Named "typ", set via include/coda_types.h 
    read (unit) sss
    read (unit) sst
    read (unit) dtg
    
    if (vrsn == 2 ) then
      read (unit) rcpt   ! Receipt time
    else
      rcpt = dtg
    endif

    ! write to netcdf file
    call sss_write_to_netcdf(output_path, output_file, n_read, &
     dtg, lat, lon, sat, sss, sst, err, flg, qc)

    deallocate( age, err, flg, lat, lon, qc, sat, sss, sst, dtg, rcpt)

  endif
  close(unit)
end subroutine sss_converter


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


!> Reads (binary) Argo-SSS Matchup Data Base (MDB). 3 days delayed by construction
subroutine mdb_converter(input_file, observation_type, output_path, output_file)
  character(len=*), intent(in) :: input_file         ! Name of the input file
  character(len=*), intent(in) :: observation_type   ! Observation type and platform
  character(len=*), intent(in) :: output_path        ! Path to output
  character(len=*), intent(in) :: output_file        ! Output file name

  !local variables
  integer, dimension(:), allocatable :: &
    mdb_type

  real, dimension(:), allocatable :: &
    argo_clm, argo_lat, argo_lon, argo_lvl, argo_sss, argo_sst, &
    mdb_dist, mdb_err, mdb_lat, mdb_lon, mdb_sss, mdb_sst, &
    mdb_time

! print *, "Reading input file name:" , trim(input_file)
  open(unit, file=trim(input_file), status='old', &
    access='sequential', form='unformatted')

read (unit) n_read, n_lvl, vrsn
  if (n_read > 0) then
    allocate( argo_clm(n_read), argo_lat(n_read), argo_lon(n_read), argo_lvl(n_read), &
              argo_sss(n_read), argo_sst(n_read), mdb_dist(n_read), mdb_err(n_read), &
              mdb_lat(n_read), mdb_lon(n_read), mdb_sss(n_read), mdb_sst(n_read), &
              mdb_time(n_read), mdb_type(n_read))

    read (unit) argo_clm
    read (unit) argo_lat
    read (unit) argo_lon
    read (unit) argo_lvl
    read (unit) argo_sss
    read (unit) argo_sst
    read (unit) mdb_dist
    read (unit) mdb_err
    read (unit) mdb_lat
    read (unit) mdb_lon
    read (unit) mdb_sss
    read (unit) mdb_sst
    read (unit) mdb_time
    read (unit) mdb_type ! Set via include/coda_types.h

    ! write to netcdf file
    call mdb_write_to_netcdf(output_path, output_file, n_read, &
     argo_lat, argo_lon, argo_lvl, argo_sss, argo_sst, &
     mdb_dist, mdb_err, mdb_lat, mdb_lon, mdb_sss, mdb_sst, &
     mdb_time, mdb_type)

    deallocate( argo_clm, argo_lat, argo_lon, argo_lvl, &
                argo_sss, argo_sst, mdb_dist, mdb_err, &
                mdb_lat, mdb_lon, mdb_sss, mdb_sst, &
                mdb_time, mdb_type)

  endif
  close(unit)
end subroutine mdb_converter


!> Read (binary) velocity obsertations
subroutine velocity_converter(input_file, observation_type, output_path, output_file)
  character(len=*), intent(in) :: input_file         ! Name of the input file
  character(len=*), intent(in) :: observation_type   ! Observation type and platform
  character(len=*), intent(in) :: output_path        ! Path to output
  character(len=*), intent(in) :: output_file        ! Output file name

  !local variables
  integer, dimension(:), allocatable :: &
    ndx, u_type, v_type

  real, dimension(:), allocatable :: &
    age, btm, lat, lon, lvl, &
    u, u_err, u_qc, v, v_err, v_qc

  character, allocatable :: dtg(:)  * len_sst_date_str
  character, allocatable :: rcpt(:) * len_sst_date_str
  character, allocatable :: sgn(:)  * len_sgn

! print *, "Reading input file name:" , trim(input_file)
  open(unit, file=trim(input_file), status='old', &
    access='sequential', form='unformatted')

  read (unit) n_read, n_lvl, vrsn
  if (n_read > 0) then
    allocate( age(n_read), btm(n_read), lat(n_read), lon(n_read), &
              lvl(n_read), ndx(n_read), u(n_read), u_err(n_read), &
              u_qc(n_read), u_type(n_read), v(n_read), v_err(n_read), &
              v_qc(n_read), v_type(n_read), dtg(n_read), rcpt(n_read), &
              sgn(n_read))

    read (unit) age
    read (unit) btm
    read (unit) lat
    read (unit) lon
    read (unit) lvl
    read (unit) ndx
    read (unit) u
    read (unit) u_err
    read (unit) u_qc
    read (unit) u_type
    read (unit) v
    read (unit) v_err
    read (unit) v_qc
    read (unit) v_type
    read (unit) dtg
    read (unit) rcpt
    read (unit) sgn

    ! write to netcdf file
    call velocity_write_to_netcdf(output_path, output_file, n_read, &
     dtg, lat, lon, u, v, u_qc, v_qc, u_type, v_type)

    deallocate( age, btm, lat, lon, lvl, ndx, u, u_err, u_qc, u_type, &
                v, v_err, v_qc, v_type, dtg, rcpt, sgn)

  endif
  close(unit)
end subroutine velocity_converter


!> Reads (binary) surface (sfc) obsertations
subroutine sfc_converter(input_file, observation_type, output_path, output_file)
  character(len=*), intent(in) :: input_file         ! Name of the input file
  character(len=*), intent(in) :: observation_type   ! Observation type and platform
  character(len=*), intent(in) :: output_path        ! Path to output
  character(len=*), intent(in) :: output_file        ! Output file name

  ! local variables
  integer, dimension(:), allocatable :: &
    drg, flg, sss_type, sst_type, wm

  real, dimension(:), allocatable :: &
    age, lat, lon, lvl, sss, sst, sss_qc, sst_qc

  character, allocatable :: dtg(:)  * len_sst_date_str
  character, allocatable :: rcpt(:) * len_sst_date_str
  character, allocatable :: sgn(:)  * len_sgn

! print *, "Reading input file name:" , trim(input_file)
  open(unit, file=trim(input_file), status='old', &
    access='sequential', form='unformatted')

  read (unit) n_read, n_lvl, vrsn
  if (n_read > 0) then
    allocate( age(n_read), drg(n_read), flg(n_read), lat(n_read), &
              lon(n_read), lvl(n_read), &
              sss(n_read), sss_qc(n_read), sss_type(n_read), &
              sst(n_read), sst_qc(n_read), sst_type(n_read), &
              wm(n_read),  dtg(n_read), rcpt(n_read), sgn(n_read))

    read (unit) age
    read (unit) drg
    read (unit) flg
    read (unit) lat
    read (unit) lon
    read (unit) lvl
    read (unit) sss
    read (unit) sss_qc
    read (unit) sss_type
    read (unit) sst
    read (unit) sst_qc
    read (unit) sst_type
    read (unit) wm        ! water-mass-classification index
    read (unit) dtg
    read (unit) rcpt
    read (unit) sgn

    ! write to netcdf file
    call sfc_write_to_netcdf(output_path, output_file, n_read, &
     flg, dtg, lat, lon, lvl, sss, sst, sss_qc, sst_qc, &
     sss_type, sst_type, wm)

    deallocate( age, drg, flg, lat, lon, lvl, &
                sss, sss_qc, sss_type, sst, sst_qc, sst_type, &
                wm,  dtg, rcpt, sgn)

  endif
  close(unit)
end subroutine sfc_converter


!> Reads (binary) profile obsertations
subroutine profile_converter(input_file, observation_type, output_path, output_file)
  character(len=*), intent(in) :: input_file         ! Name of the input file
  character(len=*), intent(in) :: observation_type   ! Observation type and platform
  character(len=*), intent(in) :: output_path        ! Path to output
  character(len=*), intent(in) :: output_file        ! Output file name

  ! local variables
  integer  :: i
  integer, dimension(:), allocatable :: &
    n_sal_lev, n_temp_lev, sal_type, temp_type
  integer, dimension(:,:), allocatable :: flg

  real, dimension(:), allocatable :: &
    btm, lat, lon, sal_qc, temp_qc
  real, dimension(:,:), allocatable :: &
    lvl, sal, sal_err, sal_prb, temp, temp_err, temp_prb, &
    clim_sal, clim_sal_std, clim_temp, clim_temp_std

  character, allocatable :: dtg(:)  * len_sst_date_str
  character, allocatable :: rcpt(:) * len_sst_date_str
  character, allocatable :: sgn(:)  * len_sgn

! print *, "Reading input file name:" , trim(input_file)
  open(unit, file=trim(input_file), status='old', &
    access='sequential', form='unformatted')

  read (unit) n_read, n_lvl, vrsn
  if (n_read > 0) then
    allocate( btm(n_read), lat(n_read), lon(n_read), &
              n_sal_lev(n_read), n_temp_lev(n_read), &
              sal_type(n_read), sal_qc(n_read), &
              temp_type(n_read), temp_qc(n_read), &
              lvl(n_lvl, n_read), sal(n_lvl, n_read), &
              sal_err(n_lvl, n_read), sal_prb(n_lvl, n_read), &
              temp(n_lvl, n_read), temp_err(n_lvl, n_read), &
              temp_prb(n_lvl, n_read), clim_sal(n_lvl, n_read), &
              clim_sal_std(n_lvl, n_read), clim_temp(n_lvl, n_read), &
              clim_temp_std(n_lvl, n_read), flg(n_lvl, n_read), &
              dtg(n_read), rcpt(n_read), sgn(n_read))

    read (unit) btm ! bottom depth
    read (unit) lat
    read (unit) lon
    read (unit) n_sal_lev  ! number of salinity levels
    read (unit) n_temp_lev ! number of temperature levels
    read (unit) sal_type   ! Named "typ", set via include/coda_types.h
    read (unit) sal_qc
    read (unit) temp_type  ! Named "typ", set via include/coda_types.h
    read (unit) temp_qc

    do i = 1, n_read
      read (unit) lvl          (1:n_temp_lev(i), i)
      read (unit) sal          (1:n_temp_lev(i), i)
      read (unit) sal_err      (1:n_temp_lev(i), i)
      read (unit) sal_prb      (1:n_temp_lev(i), i)   ! profile-error-probability
      read (unit) temp         (1:n_temp_lev(i), i)
      read (unit) temp_err     (1:n_temp_lev(i), i)   ! profile-error-probability
      read (unit) temp_prb     (1:n_temp_lev(i), i)
      read (unit) clim_sal     (1:n_temp_lev(i), i)
      read (unit) clim_sal_std (1:n_temp_lev(i), i)
      read (unit) clim_temp    (1:n_temp_lev(i), i)
      read (unit) clim_temp_std(1:n_temp_lev(i), i)
      read (unit) flg          (1:n_temp_lev(i), i)
    enddo

    read (unit) dtg
    read (unit) rcpt
    read (unit) sgn

    ! write to netcdf file
!   call profile_write_to_netcdf(output_path, output_file, n_read, &
!                               )
    deallocate( btm, lat, lon, n_sal_lev, n_temp_lev, &
              sal_type, sal_qc, temp_type, temp_qc, &
              lvl, sal, sal_err, sal_prb, &
              temp, temp_err, temp_prb, clim_sal, &
              clim_sal_std, clim_temp, &
              clim_temp_std, flg, dtg, rcpt, sgn)
  endif
  close(unit)
end subroutine profile_converter


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
  call check( nf90_def_dim(ncid, "nobs", n_read, dimid_n)) 
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
  call check( nf90_def_dim(ncid, "nobs", n_read, dimid_n)) 
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


!> Writes ice obsertations to a netCDF file
subroutine ice_write_to_netcdf(path, output_file, n_read, &
     date_str, flg, lat, lon, qc, ice, sat)

  character(len=*), intent(in) :: path, output_file
  integer, intent(in) :: n_read
  integer, dimension(n_read), intent(in) :: flg, sat
  real, dimension(n_read), intent(in) :: lat, lon, qc, ice
  character(len=len_sst_date_str), dimension(n_read), intent(in) :: date_str

  ! Local variables
  integer :: ncid, dimid_n, dimid_len_str
  integer :: varid_date, varid_flg, varid_lat, varid_lon, &
             varid_qc, varid_ice, varid_sat
  character(len=512) :: file_path
  character(len=*), parameter :: title = &
    "Quality controlled ice concentration from NCEP RTOFS"

  !  Construct full file path
  file_path = trim(path) // '/' // trim(output_file)
  if (verbose) print *, "Saving file: ", file_path

  call check( nf90_create(trim(file_path), NF90_CLOBBER, ncid)) ! Create netCDF file

  ! Define dimensions
  call check( nf90_def_dim(ncid, "nobs", n_read, dimid_n)) 
  call check( nf90_def_dim(ncid, "string_length", len_sst_date_str, dimid_len_str))

  ! Define global attribute
  call check( nf90_put_att(ncid, NF90_GLOBAL, "title", trim(title)))

  ! Define variables
  call check( nf90_def_var(ncid, "date",  NF90_CHAR,  (/dimid_len_str, dimid_n/), varid_date))
  call check( nf90_def_var(ncid, "flag",  NF90_INT,   dimid_n, varid_flg))
  call check( nf90_def_var(ncid, "lat",   NF90_FLOAT, dimid_n, varid_lat))
  call check( nf90_def_var(ncid, "lon",   NF90_FLOAT, dimid_n, varid_lon))
  call check( nf90_def_var(ncid, "qc",    NF90_FLOAT, dimid_n, varid_qc))
  call check( nf90_def_var(ncid, "aice",  NF90_FLOAT, dimid_n, varid_ice))
  call check( nf90_def_var(ncid, "sat",   NF90_INT,   dimid_n, varid_sat))

  ! Attributes for ice concentration (aice)
  call check( nf90_put_att(ncid, varid_ice, "units", "percent"))

  call check( nf90_enddef(ncid)) ! End define mode

  ! Write data to variables
  call check( nf90_put_var(ncid, varid_date, date_str))
  call check( nf90_put_var(ncid, varid_flg,  flg))
  call check( nf90_put_var(ncid, varid_lat,  lat))
  call check( nf90_put_var(ncid, varid_lon,  lon))
  call check( nf90_put_var(ncid, varid_qc,   qc))
  call check( nf90_put_var(ncid, varid_ice,  ice))
  call check( nf90_put_var(ncid, varid_sat,  sat))

  call check( nf90_close(ncid)) ! Close the netCDF file
end subroutine ice_write_to_netcdf


!> Writes sss obsertations to a netCDF file
subroutine sss_write_to_netcdf(path, output_file, n_read, &
     date_str, lat, lon, sat, sss, sst, err, flg, qc)

  character(len=*), intent(in) :: path, output_file
  integer, intent(in) :: n_read
  integer, dimension(n_read), intent(in) :: flg, sat
  real, dimension(n_read), intent(in) :: lat, lon, sss, sst, err, qc
  character(len=len_sst_date_str), dimension(n_read), intent(in) :: date_str

  ! Local variables
  integer :: ncid, dimid_n, dimid_len_str
  integer :: varid_date, varid_lat, varid_lon, &
             varid_sat, varid_sss, varid_sst, &
             varid_err, varid_flg, varid_qc
  character(len=512) :: file_path
  character(len=*), parameter :: title = &
    "Quality controlled sea surface salinity from NCEP RTOFS"

  !  Construct full file path
  file_path = trim(path) // '/' // trim(output_file)
  if (verbose) print *, "Saving file: ", file_path

  call check( nf90_create(trim(file_path), NF90_CLOBBER, ncid)) ! Create netCDF file

  ! Define dimensions
  call check( nf90_def_dim(ncid, "nobs", n_read, dimid_n)) 
  call check( nf90_def_dim(ncid, "string_length", len_sst_date_str, dimid_len_str))

  ! Define global attribute
  call check( nf90_put_att(ncid, NF90_GLOBAL, "title", trim(title)))

  ! Define variables
  call check( nf90_def_var(ncid, "date", NF90_CHAR,  (/dimid_len_str, dimid_n/), varid_date))
  call check( nf90_def_var(ncid, "lat",  NF90_FLOAT, dimid_n, varid_lat))
  call check( nf90_def_var(ncid, "lon",  NF90_FLOAT, dimid_n, varid_lon))
  call check( nf90_def_var(ncid, "sat",  NF90_INT,   dimid_n, varid_sat))
  call check( nf90_def_var(ncid, "sss",  NF90_FLOAT, dimid_n, varid_sss))
  call check( nf90_def_var(ncid, "sst",  NF90_FLOAT, dimid_n, varid_sst))
  call check( nf90_def_var(ncid, "err",  NF90_FLOAT, dimid_n, varid_err))
  call check( nf90_def_var(ncid, "flag", NF90_INT,   dimid_n, varid_flg))
  call check( nf90_def_var(ncid, "qc",   NF90_FLOAT, dimid_n, varid_qc))

  call check( nf90_enddef(ncid)) ! End define mode

  ! Write data to variables
  call check( nf90_put_var(ncid, varid_date, date_str))
  call check( nf90_put_var(ncid, varid_lat,  lat))
  call check( nf90_put_var(ncid, varid_lon,  lon))
  call check( nf90_put_var(ncid, varid_sat,  sat))
  call check( nf90_put_var(ncid, varid_sss,  sss))
  call check( nf90_put_var(ncid, varid_sst,  sst))
  call check( nf90_put_var(ncid, varid_err,  err))
  call check( nf90_put_var(ncid, varid_flg,  flg))
  call check( nf90_put_var(ncid, varid_qc,   qc))

  call check( nf90_close(ncid)) ! Close the netCDF file
end subroutine sss_write_to_netcdf


!> Writes MDB to a netCDF file
subroutine mdb_write_to_netcdf(path, output_file, n_read, &
     argo_lat, argo_lon, argo_depth, argo_sss, argo_sst, &
     dist, err, mdb_lat, mdb_lon, mdb_sss, mdb_sst, &
     time, type)

  character(len=*), intent(in) :: path, output_file
  integer, intent(in) :: n_read
  integer, dimension(n_read), intent(in) :: type
  real, dimension(n_read), intent(in) :: argo_lat, argo_lon, argo_depth, &
                                         argo_sss, argo_sst, dist, err, &
                                         mdb_lat, mdb_lon, mdb_sss, mdb_sst, time
  ! Local variables
  integer :: ncid, dimid_n
  integer :: varid_argo_lat, varid_argo_lon, varid_argo_depth, &
             varid_argo_sss, varid_argo_sst, varid_dist, varid_err, &
             varid_mdb_lat, varid_mdb_lon, varid_mdb_sss, varid_mdb_sst, &
             varid_time, varid_type
  character(len=512) :: file_path
  character(len=*), parameter :: title = &
    "Argo-satellite sea surface salinity match database from NCEP RTOFS"

  !  Construct full file path
  file_path = trim(path) // '/' // trim(output_file)
  if (verbose) print *, "Saving file: ", file_path

  call check( nf90_create(trim(file_path), NF90_CLOBBER, ncid)) ! Create netCDF file

  ! Define dimensions
  call check( nf90_def_dim(ncid, "nobs", n_read, dimid_n))

  ! Define global attribute
  call check( nf90_put_att(ncid, NF90_GLOBAL, "title", trim(title)))

  ! Define variables
  call check( nf90_def_var(ncid, "date",       NF90_FLOAT, dimid_n, varid_time))
  call check( nf90_def_var(ncid, "argo_lat",   NF90_FLOAT, dimid_n, varid_argo_lat))
  call check( nf90_def_var(ncid, "argo_lon",   NF90_FLOAT, dimid_n, varid_argo_lon))
  call check( nf90_def_var(ncid, "argo_depth", NF90_FLOAT, dimid_n, varid_argo_depth))
  call check( nf90_def_var(ncid, "argo_sss",   NF90_FLOAT, dimid_n, varid_argo_sss))
  call check( nf90_def_var(ncid, "argo_sst",   NF90_FLOAT, dimid_n, varid_argo_sst))
  call check( nf90_def_var(ncid, "dist",       NF90_FLOAT, dimid_n, varid_dist))
  call check( nf90_def_var(ncid, "error",      NF90_FLOAT, dimid_n, varid_err))
  call check( nf90_def_var(ncid, "mdb_lat",    NF90_FLOAT, dimid_n, varid_mdb_lat))
  call check( nf90_def_var(ncid, "mdb_lon",    NF90_FLOAT, dimid_n, varid_mdb_lon))
  call check( nf90_def_var(ncid, "mdb_sss",    NF90_FLOAT, dimid_n, varid_mdb_sss))
  call check( nf90_def_var(ncid, "mdb_sst",    NF90_FLOAT, dimid_n, varid_mdb_sst))
  call check( nf90_def_var(ncid, "obs_type",   NF90_INT,   dimid_n, varid_type))

  ! Attributes for dist
  call check( nf90_put_att(ncid, varid_dist, "units", "km"))

  call check( nf90_enddef(ncid)) ! End define mode

  ! write data to variables
  call check( nf90_put_var(ncid, varid_time,       time))
  call check( nf90_put_var(ncid, varid_argo_lat,   argo_lat))
  call check( nf90_put_var(ncid, varid_argo_lon,   argo_lon))
  call check( nf90_put_var(ncid, varid_argo_depth, argo_depth))
  call check( nf90_put_var(ncid, varid_argo_sss,   argo_sss))
  call check( nf90_put_var(ncid, varid_argo_sst,   argo_sst))
  call check( nf90_put_var(ncid, varid_dist,       dist))
  call check( nf90_put_var(ncid, varid_err,        err))
  call check( nf90_put_var(ncid, varid_mdb_lat,    mdb_lat))
  call check( nf90_put_var(ncid, varid_mdb_lon,    mdb_lon))
  call check( nf90_put_var(ncid, varid_mdb_sss,    mdb_sss))
  call check( nf90_put_var(ncid, varid_mdb_sst,    mdb_sst))
  call check( nf90_put_var(ncid, varid_type,       type))

  call check( nf90_close(ncid)) ! Close the netCDF file
end subroutine mdb_write_to_netcdf


!> Writes velocity obsertations to a netCDF file
subroutine velocity_write_to_netcdf(path, output_file, n_read, &
     date_str, lat, lon, u, v, u_qc, v_qc, u_type, v_type)

  character(len=*), intent(in) :: path, output_file
  integer, intent(in) :: n_read
  integer, dimension(n_read), intent(in) :: u_type, v_type
  real, dimension(n_read), intent(in) :: lat, lon, u, v, u_qc, v_qc
  character(len=len_sst_date_str), dimension(n_read), intent(in) :: date_str

  ! Local variables
  integer :: ncid, dimid_n, dimid_len_str
  integer :: varid_date, varid_lat, varid_lon, &
             varid_u, varid_v, varid_u_qc, varid_v_qc, &
             varid_u_type, varid_v_type
  character(len=512) :: file_path
  character(len=*), parameter :: title = &
    "Quality controlled velocity from NCEP RTOFS"

  ! Construct full file path
  file_path = trim(path) // '/' // trim(output_file)
  if (verbose) print *, "Saving file: ", file_path

  call check( nf90_create(trim(file_path), NF90_CLOBBER, ncid)) ! Create netCDF file

  ! Define dimensions
  call check( nf90_def_dim(ncid, "nobs", n_read, dimid_n))
  call check( nf90_def_dim(ncid, "string_length", len_sst_date_str, dimid_len_str))

  ! Define global attribute
  call check( nf90_put_att(ncid, NF90_GLOBAL, "title", trim(title)))

  ! Define variables
  call check( nf90_def_var(ncid, "date",   NF90_CHAR,  (/dimid_len_str, dimid_n/), varid_date))
  call check( nf90_def_var(ncid, "lat",    NF90_FLOAT, dimid_n, varid_lat))
  call check( nf90_def_var(ncid, "lon",    NF90_FLOAT, dimid_n, varid_lon))
  call check( nf90_def_var(ncid, "u",      NF90_FLOAT, dimid_n, varid_u))
  call check( nf90_def_var(ncid, "v",      NF90_FLOAT, dimid_n, varid_v))
  call check( nf90_def_var(ncid, "u_qc",   NF90_FLOAT, dimid_n, varid_u_qc))
  call check( nf90_def_var(ncid, "v_qc",   NF90_FLOAT, dimid_n, varid_v_qc))
  call check( nf90_def_var(ncid, "u_type", NF90_INT,   dimid_n, varid_u_type))
  call check( nf90_def_var(ncid, "v_type", NF90_INT,   dimid_n, varid_v_type))

  call check( nf90_enddef(ncid)) ! End define mode

  ! Write data to variables
  call check( nf90_put_var(ncid, varid_date,  date_str))
  call check( nf90_put_var(ncid, varid_lat,   lat))
  call check( nf90_put_var(ncid, varid_lon,   lon))
  call check( nf90_put_var(ncid, varid_u,     u))
  call check( nf90_put_var(ncid, varid_v,     v))
  call check( nf90_put_var(ncid, varid_u_qc,  u_qc))
  call check( nf90_put_var(ncid, varid_v_qc,  v_qc))
  call check( nf90_put_var(ncid, varid_u_type,u_type))
  call check( nf90_put_var(ncid, varid_v_type,v_type))

  call check( nf90_close(ncid)) ! Close the netCDF file
end subroutine velocity_write_to_netcdf


!> Writes surface obsertations to a netCDF file
subroutine sfc_write_to_netcdf(path, output_file, n_read, &
     flg, date_str, lat, lon, depth, sss, sst, &
     sss_qc, sst_qc, sss_type, sst_type, wmid)

  character(len=*), intent(in) :: path, output_file
  integer, intent(in) :: n_read
  integer, dimension(n_read), intent(in) :: flg, sss_type, sst_type, wmid
  real, dimension(n_read), intent(in) :: lat, lon, depth, sss, sst, &
                                         sss_qc, sst_qc
  character(len=len_sst_date_str), dimension(n_read), intent(in) :: date_str

  ! Local variables
  integer :: ncid, dimid_n, dimid_len_str
  integer :: varid_date, varid_lat, varid_lon, varid_depth, &
             varid_sss, varid_sst, varid_sss_qc, varid_sst_qc, &
             varid_sss_type, varid_sst_type, &
             varid_flg, varid_wmid
  character(len=512) :: file_path
  character(len=*), parameter :: title = &
    "Quality controlled surface observations from NCEP RTOFS"

  ! Construct full file path
  file_path = trim(path) // '/' // trim(output_file)
  if (verbose) print *, "Saving file: ", file_path

  call check( nf90_create(trim(file_path), NF90_CLOBBER, ncid)) ! Create netCDF file

  ! Define dimensions
  call check( nf90_def_dim(ncid, "nobs", n_read, dimid_n))
  call check( nf90_def_dim(ncid, "string_length", len_sst_date_str, dimid_len_str))

  ! Define global attribute
  call check( nf90_put_att(ncid, NF90_GLOBAL, "title", trim(title)))

  ! Define variables
  call check( nf90_def_var(ncid, "date",     NF90_CHAR,  (/dimid_len_str, dimid_n/), varid_date))
  call check( nf90_def_var(ncid, "lat",      NF90_FLOAT, dimid_n, varid_lat))
  call check( nf90_def_var(ncid, "lon",      NF90_FLOAT, dimid_n, varid_lon))
  call check( nf90_def_var(ncid, "depth",    NF90_FLOAT, dimid_n, varid_depth))
  call check( nf90_def_var(ncid, "sss",      NF90_FLOAT, dimid_n, varid_sss))
  call check( nf90_def_var(ncid, "sst",      NF90_FLOAT, dimid_n, varid_sst))
  call check( nf90_def_var(ncid, "sss_qc",   NF90_FLOAT, dimid_n, varid_sss_qc))
  call check( nf90_def_var(ncid, "sst_qc",   NF90_FLOAT, dimid_n, varid_sst_qc))
  call check( nf90_def_var(ncid, "sss_type", NF90_INT,   dimid_n, varid_sss_type))
  call check( nf90_def_var(ncid, "sst_type", NF90_INT,   dimid_n, varid_sst_type))
  call check( nf90_def_var(ncid, "flag",     NF90_INT,   dimid_n, varid_flg))
  call check( nf90_def_var(ncid, "water_mass_id", NF90_INT,   dimid_n, varid_wmid)) !Water Mass Index

  call check( nf90_enddef(ncid)) ! End define mode

  ! Write data to variables
  call check( nf90_put_var(ncid, varid_date,    date_str))
  call check( nf90_put_var(ncid, varid_lat,     lat))
  call check( nf90_put_var(ncid, varid_lon,     lon))
  call check( nf90_put_var(ncid, varid_depth,   depth))
  call check( nf90_put_var(ncid, varid_sss,     sss))
  call check( nf90_put_var(ncid, varid_sst,     sst))
  call check( nf90_put_var(ncid, varid_sss_qc,  sss_qc))
  call check( nf90_put_var(ncid, varid_sst_qc,  sst_qc))
  call check( nf90_put_var(ncid, varid_sss_type,sss_type))
  call check( nf90_put_var(ncid, varid_sst_type,sst_type))
  call check( nf90_put_var(ncid, varid_flg,     flg))
  call check( nf90_put_var(ncid, varid_wmid,    wmid))

  call check( nf90_close(ncid)) ! Close the netCDF file
end subroutine sfc_write_to_netcdf


!> From https://home.chpc.utah.edu/~thorne/computing/Examples_netCDF.pdf
subroutine check(istatus)
  integer, intent (in) :: istatus
  if (istatus /= nf90_noerr) then
    print*, trim(adjustl(nf90_strerror(istatus)))
  end iF
end subroutine check


end module read_binary_write_nc_mod
