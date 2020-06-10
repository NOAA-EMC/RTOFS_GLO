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
c --- START OF USER SETABLE, BUT REGION INDEPEDENT, PARAMETERS
c
c --- iqr   = maximum number of tiles in i direction
c --- jqr   = maximum number of tiles in j direction
      integer    iqr,jqr
      parameter (iqr=299,jqr=299)  ! multiple tiles (TYPE=ompi or mpi or shmem)
c
c --- mxthrd= maximum number of OpenMP threads
      integer    mxthrd
      parameter (mxthrd=1)  ! NOMP=0,1
c
c --- mxtrcr= maximum number of tracers
      integer    mxtrcr
      parameter (mxtrcr=99)  !not used to allocate large arrays
c
c ---   END OF USER SETABLE, BUT REGION INDEPEDENT, PARAMETERS
c-----------------------------------------------------------------------------
