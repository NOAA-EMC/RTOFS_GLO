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
c-----------------------------------------------------------------------------
c --- START OF REGION AND TILING SPECIFIC PARAMETERS
c --- See: README.src.newregion for more details.
c
c --- itdm  = total grid dimension in i direction
c --- jtdm  = total grid dimension in j direction
c --- kdm   =       grid dimension in k direction
      integer    itdm,jtdm,kdm
      parameter (itdm=500,jtdm=382,kdm=32)  ! GLBT0.72
c
c --- iqr   = maximum number of tiles in i direction
c --- jqr   = maximum number of tiles in j direction
      integer    iqr,jqr
      parameter (iqr=10,jqr=10)  ! multiple tiles (TYPE=ompi or mpi or shmem)
c
c --- idm   = maximum single tile grid dimension in i direction
c --- jdm   = maximum single tile grid dimension in j direction
      integer    idm,jdm
*     parameter (idm=itdm,jdm=jtdm)  ! always works if enough memory
      parameter (idm= 250,jdm= 191)  ! NMPI=4,8,16,24,32,40,47,64
c
c --- mxthrd= maximum number of OpenMP threads
      integer    mxthrd
      parameter (mxthrd=1)  ! NOMP=0,1
c
c --- kkwall= grid dimension in k direction for wall relax arrays
c --- kknest= grid dimension in k direction for nest relax arrays
      integer    kkwall,kknest
      parameter (kkwall=  1)  ! must be 1 or kdm
      parameter (kknest=  1)  ! must be 1 or kdm
c
c --- kkmy25= grid dimension in k direction for M-Y 2.5 arrays
      integer    kkmy25
      parameter (kkmy25= -1)  ! must be -1 or kdm
c
c --- nlgiss= size of lookup table for GISS
      integer    nlgiss
      parameter (nlgiss=  1)  ! must be 1 (no GISS) or 762
c
c --- mxtrcr= maximum number of tracers
      integer    mxtrcr
      parameter (mxtrcr=1)
c
c --- natm  = number of saved atmospheric fields
      integer    natm
      parameter (natm=2)      ! must be 2 (high freq.) or 4 (monthly)
c
c ---   END OF REGION AND TILING SPECIFIC PARAMETERS
c-----------------------------------------------------------------------------
