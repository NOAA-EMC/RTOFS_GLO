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
      subroutine inimy
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
c
c --- hycom version 2.1
      implicit none
c
c -------------------------------------------------------------
c --- initialize mellor-yamada level 2.5 vertical mixing scheme
c -------------------------------------------------------------
c
      integer i,j,k
c
      include 'stmt_fns.h'
c
      a1my = 0.92
      b1my = 16.6
      a2my = 0.74
      b2my = 10.1
      c1my = 0.08
c
      e1my = 1.8
      e2my = 1.33
      e3my = 1.0
      sef  = 1.0
      smll = 1.e-8
c
c --- 'vonk'           = von karman constant
c --- 'ghc'            = constant for calculating tke production
c --- 'coef4','coef5'  = coefficients for estimating viscosity/diffusivity
c --- 'const1'         = constant for estimating surface and bottom bc's
c
      vonk   =  0.4
      ghc    = -6.0
      coef4  =  18.*a1my*a1my+9.*a1my*a2my
      coef5  =  9.*a1my*a2my
      const1 =  16.6**.6666667*sef
c
      do j=1,jdm
        do i=1,idm
          do k=0,kdm+1
            q2(i,j,k,1)=smll
            q2(i,j,k,2)=smll
            q2l(i,j,k,1)=smll
            q2l(i,j,k,2)=smll
            vctymy(i,j,k)=diwm(i,j)
            diftmy(i,j,k)=diws(i,j)
            difqmy(i,j,k)=diws(i,j)
          enddo
          do k=1,kdm+1
            vcty(i,j,k)=diwm(i,j)
            dift(i,j,k)=diws(i,j)
            difs(i,j,k)=diws(i,j)
c --- no nonlocal forcing
            ghats(i,j,k)=0.0
          enddo
        enddo
      enddo
      return
      end
c
c
c> Revision history:
c>
