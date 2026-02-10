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
MODULE MUMPS_INTR_TYPES
PRIVATE
PUBLIC :: MUMPS_ROOT_STRUC
!    Define arithmetic-independent MUMPS_ROOT_STRUC datatype
TYPE MUMPS_ROOT_STRUC
  INTEGER :: MBLOCK, NBLOCK, NPROW, NPCOL
  INTEGER :: MYROW, MYCOL
  INTEGER :: SCHUR_MLOC, SCHUR_NLOC, SCHUR_LLD
  INTEGER :: RHS_NLOC
  INTEGER :: ROOT_SIZE, TOT_ROOT_SIZE
!       descriptor for scalapack
  INTEGER, DIMENSION( 9 ) :: DESCRIPTOR
  INTEGER :: CNTXT_BLACS, LPIV
  INTEGER :: NB_SINGULAR_VALUES
  INTEGER, DIMENSION(:), POINTER :: IPIV
  INTEGER, DIMENSION(:), POINTER :: RG2L
  LOGICAL :: yes, gridinit_done
END TYPE MUMPS_ROOT_STRUC
END MODULE MUMPS_INTR_TYPES
