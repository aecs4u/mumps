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
MODULE MUMPS_L0_OMP_M
  LOGICAL, DIMENSION(:), POINTER :: NB_CORE_PER_THREAD_CHANGED
  INTEGER, DIMENSION(:), POINTER :: NB_CORE_PER_THREAD
  INTEGER :: THREAD_ID
  LOGICAL :: IS_ROOT_OF_L0_OMP
!$OMP   THREADPRIVATE ( THREAD_ID , IS_ROOT_OF_L0_OMP )
END MODULE MUMPS_L0_OMP_M
