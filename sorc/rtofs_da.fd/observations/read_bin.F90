!> To read in binary formatted file(s) that contains 
!! quality controlled observations information
module read_bin

implicit none; private

public :: getInputs

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

end module read_bin
