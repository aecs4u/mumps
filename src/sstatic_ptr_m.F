!
!  This file is part of MUMPS 5.8.2, released
!  on Mon Jan 12 15:17:08 UTC 2026
!
!
!  Copyright 1991-2026 CERFACS, CNRS, ENS Lyon, INP Toulouse, Inria,
!  Mumps Technologies, University of Bordeaux.
!
!  This version of MUMPS is provided to you free of charge. It is
!  released under the CeCILL-C license 
!  (see doc/CeCILL-C_V1-en.txt, doc/CeCILL-C_V1-fr.txt, and
!  https://cecill.info/licences/Licence_CeCILL-C_V1-en.html)
!
MODULE SMUMPS_STATIC_PTR_M
PUBLIC :: SMUMPS_TMP_PTR, SMUMPS_GET_TMP_PTR
REAL, DIMENSION(:), POINTER, SAVE :: SMUMPS_TMP_PTR
CONTAINS
SUBROUTINE SMUMPS_SET_STATIC_PTR(ARRAY)
REAL, DIMENSION(:), TARGET :: ARRAY
SMUMPS_TMP_PTR => ARRAY
RETURN
END SUBROUTINE SMUMPS_SET_STATIC_PTR
SUBROUTINE SMUMPS_GET_TMP_PTR(PTR)
#if defined(MUMPS_NOF2003)
REAL, DIMENSION(:), POINTER :: PTR
#else
REAL, DIMENSION(:), POINTER, INTENT(OUT) :: PTR
#endif
PTR => SMUMPS_TMP_PTR
RETURN
END SUBROUTINE SMUMPS_GET_TMP_PTR
END MODULE SMUMPS_STATIC_PTR_M
