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
!***********************************************************************
MODULE MUMPS_SOL_L0OMP_M
!
!    Purpose:
!    =======
!    Manage locks for right-looking updates of RHSINTR unde L0 threads
!
!$    USE OMP_LIB, ONLY: OMP_LOCK_KIND
INTEGER, PARAMETER :: NB_LOCK_MAX = 18
!$    INTEGER(OMP_LOCK_KIND), &
!$    ALLOCATABLE, DIMENSION(:), SAVE :: LOCK_FOR_SCATTER
!
!
CONTAINS
!
SUBROUTINE MUMPS_SOL_L0OMP_LI( K400 )
!$    USE OMP_LIB, ONLY: OMP_INIT_LOCK
IMPLICIT NONE
!
!    Purpose:
!    =======
!    Initialize locks for forward solution with L0-threads feature
!    (LI suffix: Lock Initialization)
!
!    Argument:
!    ========
!    K400: the number of threads for L0-threads;
!          we use min(K400, NB_LOCK_MAX) locks.
!
INTEGER, INTENT(IN) :: K400
!
!$    INTEGER :: I
!
!    Executable statements
!    =====================
!
!$    IF (K400 .GT. 0) THEN
!$      ALLOCATE(LOCK_FOR_SCATTER(min(NB_LOCK_MAX,K400)))
!$      DO I = 1, min(NB_LOCK_MAX,K400)
!$        CALL OMP_INIT_LOCK(LOCK_FOR_SCATTER(I))
!$      ENDDO
!$    ENDIF
RETURN
END SUBROUTINE MUMPS_SOL_L0OMP_LI
SUBROUTINE MUMPS_SOL_L0OMP_LD( K400 )
!$    USE OMP_LIB, ONLY : OMP_DESTROY_LOCK
IMPLICIT NONE
!
!    Purpose:
!    =======
!    Destroy locks for forward solution with L0-threads feature
!    (LD suffix: Lock Destruction)
!
!    Argument:
!    ========
!    K400: the number of threads for L0-threads;
!          we use min(K400, NB_LOCK_MAX) locks.
INTEGER, INTENT(IN) :: K400
!
!$    INTEGER :: I
!
!    Executable statements
!    =====================
!
!$    IF (allocated(LOCK_FOR_SCATTER)) THEN
!$      IF (K400 .GT. 0) THEN
!$        DO I = 1, min(NB_LOCK_MAX,K400)
!$          CALL OMP_DESTROY_LOCK(LOCK_FOR_SCATTER(I))
!$        ENDDO
!$        DEALLOCATE(LOCK_FOR_SCATTER)
!$      ENDIF
!$    ENDIF
RETURN
END SUBROUTINE MUMPS_SOL_L0OMP_LD
END MODULE MUMPS_SOL_L0OMP_M
