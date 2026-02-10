!
! This file is part of MUMPS 5.8.2, released
! on Mon Jan 12 15:17:08 UTC 2026
!
!
! Copyright 1991-2026 CERFACS, CNRS, ENS Lyon, INP Toulouse, Inria,
! Mumps Technologies, University of Bordeaux.
!
! This version of MUMPS is provided to you free of charge. It is
! released under the CeCILL-C license 
! (see doc/CeCILL-C_V1-en.txt, doc/CeCILL-C_V1-fr.txt, and
! https://cecill.info/licences/Licence_CeCILL-C_V1-en.html)
!
MODULE CMUMPS_TPS_M
TYPE CMUMPS_TPS_T
    COMPLEX, DIMENSION(:), POINTER :: A
END TYPE CMUMPS_TPS_T
END MODULE CMUMPS_TPS_M
SUBROUTINE CMUMPS_TPS_M_RETURN()
RETURN
END SUBROUTINE CMUMPS_TPS_M_RETURN
