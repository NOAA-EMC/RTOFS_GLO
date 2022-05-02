!     Include file to define variables for Fortran to C interface(s)
!     Robert Grumbine 16 March 1998
!     NOSEEK added 25 March 1998
!
!     IO-related (bacio.c, banio.c) :
      INTEGER BAOPEN_RONLY, BAOPEN_WONLY, BAOPEN_RW
      INTEGER BACLOSE, BAREAD, BAWRITE, NOSEEK
      PARAMETER (BAOPEN_RONLY =  1)
      PARAMETER (BAOPEN_WONLY =  2)
      PARAMETER (BAOPEN_RW    =  4)
      PARAMETER (BACLOSE      =  8)
      PARAMETER (BAREAD       = 16)
      PARAMETER (BAWRITE      = 32)
      PARAMETER (NOSEEK       = 64)
      
