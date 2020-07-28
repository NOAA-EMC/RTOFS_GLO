c #
c DISTRIBUTION STATEMENT B: Distribution authorized to U.S. Government
c agencies based upon the reasons of possible Premature Distribution
c and the possibility of containing Software Documentation as listed
c on Table 1 of DoD Instruction 5230.24, Distribution Statements on
c Technical Documents, of 23 August 2012. Other requests for this
c document shall be made to Dr. Ruth H. Preller, Superintendent,
c Oceanography Division, U.S. Naval Research Laboratory, DEPARTMENT
c OF THE NAVY, John C. Stennis Space Center, MS 39529-5004; (228)
c 688-4670 (voice); ruth.preller@nrlssc.navy.mil (e-mail).
c #
#if defined(NAN2003)
      logical function hycom_isnaninf(a)
      use ieee_arithmetic, only : ieee_is_finite
      implicit none
c
      real a
c
c**********
c*
c 1)  return .true. if a is NaN or +Inf or -Inf.
c
c 2)  version with ieee_arithmetic intrinsic module
c*
c**********
c
      hycom_isnaninf = .not. ieee_is_finite(a)
      end function hycom_isnaninf
#else
      logical function hycom_isnaninf(a)
      implicit none
c
      real a
c
c**********
c*
c 1)  return .true. if a is NaN or +Inf or -Inf.
c*
c**********
c
      hycom_isnaninf = .not. (a.ge.-huge(a) .and. a.le.huge(a))
      end
#endif
c
c> Revision history:
c>
c> Jan. 2014 - ieee_arithmetic intrinsic module version via /* NAN2003 */ macro
