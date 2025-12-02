!> To read in binary formatted file(s) that contains 
!! quality controlled observations information
module read_bin

implicit none; private

public :: getInputs

real, parameter:: missing_value = -999.0  !< Missing value

integer :: n_read !< Number of "observations"
integer :: n_lvl  !< Number of "levels", =1, unless profile observations
integer :: vrsn   !< Version number of writer/reader

contains

!> This subroutine reads inputs set in: read_bin_qc_obs
subroutine getInputs(num_inputs, &
    input_file, obsType, obsPlat)
    integer, intent(in) :: num_inputs 

    character(len=*), intent(out) :: input_file         ! Name of the input file
    character(len=*), intent(out) :: obsType, obsPlat   ! Observation type and platform
 
    ! local variables
    character(len=100) :: prog_name      ! Name of program (exec)
    integer :: iargc_count

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

    call getarg(1, input_file)
    call getarg(2, obsType)
    call getarg(3, obsPlat)
end subroutine getInputs


!integer, dimension(:), allocatable :: &
!     ob_cyc, ob_ltc, ob_sat, ob_smpl, ob_trck

!real, dimension(:), allocatable :: &
!     ob_age, ob_lat, ob_lon, ob_qc, ob_ssh, ob_sla

!  character, allocatable :: ob_dtg(:)  *14 
!  character, allocatable :: ob_rcpt(:) *14 
! print *, "Reading input file name:" , trim(in_fName)
! open(10, file=trim(in_fName), status='old', &
!          access='sequential', form='unformatted')
! read (10) n_read, n_lvl, vrsn

! if (n_read > 0) then
!   allocate( ob_age(n_read), ob_cyc(n_read), ob_lat(n_read), ob_lon(n_read), &
!             ob_qc(n_read), ob_sat(n_read), ob_smpl(n_read), ob_ssh(n_read), &
!             ob_trck(n_read), ob_ltc(n_read), ob_dtg(n_read), ob_rcpt(n_read), &
!             ob_sla(n_read))

!   read (10) ob_age
!   read (10) ob_cyc
!   read (10) ob_lat
!   read (10) ob_lon
!   read (10) ob_qc
!   read (10) ob_sat
!   read (10) ob_smpl
!   read (10) ob_ssh
!   read (10) ob_trck
!   read (10) ob_ltc
!   read (10) ob_dtg
!   read (10) ob_rcpt

!   if (vrsn > 1) then
!      read (10) ob_sla
!   else
!      ob_sla(:) = missing_value
!   endif

!   deallocate( ob_age, ob_cyc, ob_lat, ob_lon, &
!               ob_qc, ob_sat, ob_smpl, ob_ssh, &
!               ob_trck, ob_ltc, ob_dtg, ob_rcpt, ob_sla)
! endif
! close(10)

end module read_bin
