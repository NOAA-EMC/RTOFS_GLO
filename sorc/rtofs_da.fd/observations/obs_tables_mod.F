!
! The following uses:
! ncoda_qc/include/
! - coda_types.h
! - qc_codes.h
! but with internally self-consistent copies.
!

      module obs_tables_mod
      implicit none

      include 'coda_types.h'
      include 'qc_codes.h'

      integer, parameter :: len_type_lbl = 20
      integer, parameter :: num_qc_codes = 33
      integer, parameter :: len_qc_lbl   = 20

      contains

      subroutine get_obs_type_table(type_ids, type_names,
     &                              type_inst_err, type_rerr_scl)
        integer, dimension(0:MX_TYPES), intent(out) :: type_ids
        character(len=len_type_lbl), dimension(0:MX_TYPES),
     &                               intent(out) :: type_names
        real, dimension(0:MX_TYPES), intent(out) :: type_inst_err
        real, dimension(0:MX_TYPES), intent(out) :: type_rerr_scl

        integer :: i

        do i = 0, MX_TYPES
          type_ids(i)      = i
          type_names(i)    = adjustl(data_lbl(i))
          type_inst_err(i) = inst_err(i)
          type_rerr_scl(i) = rerr_scl(i)
        end do
      end subroutine get_obs_type_table

      subroutine get_qc_code_table(qc_ids, qc_names)
        integer, dimension(num_qc_codes), intent(out) :: qc_ids
        character(len=len_qc_lbl), dimension(num_qc_codes),
     &                             intent(out) :: qc_names

        qc_ids = (/
     &    GOOD,
     &    INVRSN, DERIVED_SAL, VERT_GRAD_MN, VERT_GRAD_MX,
     &    RESPN, SPIKE, STRETCH, SPIKE_RESPN,
     &    STRETCH_RESPN, STRETCH_SPIKE, STRETCH_SPIKE_RESPN,
     &    WARM_SST, LAND_DST, LAND_ICE, FW_ICE,
     &    ICE_SSS, SHLW_SSS, LAND_SSS, FW_SSS,
     &    DIURNAL_SST, LAND_SST,
     &    TMP_MISS, SAL_MISS, TMP_SAL_MISS,
     &    DUP_DEPTH, DEPTH_ERR, DEC_DEPTH,
     &    TMP_OUTLIER, SAL_OUTLIER, TMP_SAL_OUTLIER,
     &    OUTLIER, SPEED_ERROR /)

        qc_names = (/
     &    'GOOD                ',
     &    'INVRSN              ', 'DERIVED_SAL         ',
     &    'VERT_GRAD_MN        ', 'VERT_GRAD_MX        ',
     &    'RESPN               ', 'SPIKE               ',
     &    'STRETCH             ', 'SPIKE_RESPN         ',
     &    'STRETCH_RESPN       ', 'STRETCH_SPIKE       ',
     &    'STRETCH_SPIKE_RESPN ',
     &    'WARM_SST            ', 'LAND_DST            ',
     &    'LAND_ICE            ', 'FW_ICE              ',
     &    'ICE_SSS             ', 'SHLW_SSS            ',
     &    'LAND_SSS            ', 'FW_SSS              ',
     &    'DIURNAL_SST         ', 'LAND_SST            ',
     &    'TMP_MISS            ', 'SAL_MISS            ',
     &    'TMP_SAL_MISS        ',
     &    'DUP_DEPTH           ', 'DEPTH_ERR           ',
     &    'DEC_DEPTH           ',
     &    'TMP_OUTLIER         ', 'SAL_OUTLIER         ',
     &    'TMP_SAL_OUTLIER     ',
     &    'OUTLIER             ', 'SPEED_ERROR         ' /)
      end subroutine get_qc_code_table

      end module obs_tables_mod
