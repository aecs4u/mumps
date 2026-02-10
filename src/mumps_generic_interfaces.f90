!
!  MUMPS Generic Interface Module - Prototype
!
!  This module demonstrates how generic interfaces provide a single,
!  type-safe API that automatically dispatches to the correct
!  precision-specific implementation.
!
!  Benefits:
!  - Users write: CALL MUMPS_FACTORIZE(id)
!  - Compiler selects: SMUMPS/DMUMPS/CMUMPS/ZMUMPS based on type
!  - Type safety at compile time
!  - Single documentation interface
!  - Backward compatible (original interfaces still available)
!
!  Author: Claude Code (Anthropic)
!  Date: 2026-02-10
!  Standard: Fortran 2023
!

MODULE MUMPS_GENERIC_INTERFACES
  USE SMUMPS_STRUC_DEF, ONLY: SMUMPS_STRUC
  USE DMUMPS_STRUC_DEF, ONLY: DMUMPS_STRUC
  USE CMUMPS_STRUC_DEF, ONLY: CMUMPS_STRUC
  USE ZMUMPS_STRUC_DEF, ONLY: ZMUMPS_STRUC

  IMPLICIT NONE
  PRIVATE

  !===========================================================================
  ! Generic Interface: MUMPS_INIT
  !===========================================================================
  ! Initialize MUMPS for any precision
  !
  ! Usage:
  !   TYPE(DMUMPS_STRUC) :: id
  !   CALL MUMPS_INIT(id)  ! Automatically calls DMUMPS
  !
  PUBLIC :: MUMPS_INIT
  INTERFACE MUMPS_INIT
    MODULE PROCEDURE SMUMPS_INIT
    MODULE PROCEDURE DMUMPS_INIT
    MODULE PROCEDURE CMUMPS_INIT
    MODULE PROCEDURE ZMUMPS_INIT
  END INTERFACE MUMPS_INIT

  !===========================================================================
  ! Generic Interface: MUMPS_ANALYZE
  !===========================================================================
  ! Perform analysis phase for any precision
  !
  PUBLIC :: MUMPS_ANALYZE
  INTERFACE MUMPS_ANALYZE
    MODULE PROCEDURE SMUMPS_ANALYZE
    MODULE PROCEDURE DMUMPS_ANALYZE
    MODULE PROCEDURE CMUMPS_ANALYZE
    MODULE PROCEDURE ZMUMPS_ANALYZE
  END INTERFACE MUMPS_ANALYZE

  !===========================================================================
  ! Generic Interface: MUMPS_FACTORIZE
  !===========================================================================
  ! Perform factorization for any precision
  !
  PUBLIC :: MUMPS_FACTORIZE
  INTERFACE MUMPS_FACTORIZE
    MODULE PROCEDURE SMUMPS_FACTORIZE
    MODULE PROCEDURE DMUMPS_FACTORIZE
    MODULE PROCEDURE CMUMPS_FACTORIZE
    MODULE PROCEDURE ZMUMPS_FACTORIZE
  END INTERFACE MUMPS_FACTORIZE

  !===========================================================================
  ! Generic Interface: MUMPS_SOLVE
  !===========================================================================
  ! Solve linear system for any precision
  !
  PUBLIC :: MUMPS_SOLVE
  INTERFACE MUMPS_SOLVE
    MODULE PROCEDURE SMUMPS_SOLVE
    MODULE PROCEDURE DMUMPS_SOLVE
    MODULE PROCEDURE CMUMPS_SOLVE
    MODULE PROCEDURE ZMUMPS_SOLVE
  END INTERFACE MUMPS_SOLVE

  !===========================================================================
  ! Generic Interface: MUMPS_FINALIZE
  !===========================================================================
  ! Clean up MUMPS for any precision
  !
  PUBLIC :: MUMPS_FINALIZE
  INTERFACE MUMPS_FINALIZE
    MODULE PROCEDURE SMUMPS_FINALIZE
    MODULE PROCEDURE DMUMPS_FINALIZE
    MODULE PROCEDURE CMUMPS_FINALIZE
    MODULE PROCEDURE ZMUMPS_FINALIZE
  END INTERFACE MUMPS_FINALIZE

  !===========================================================================
  ! Generic Interface: MUMPS_GET_DETERMINANT
  !===========================================================================
  ! Get determinant for any precision
  ! Demonstrates overloading with different return types
  !
  PUBLIC :: MUMPS_GET_DETERMINANT
  INTERFACE MUMPS_GET_DETERMINANT
    MODULE PROCEDURE SMUMPS_GET_DETERMINANT  ! Returns REAL
    MODULE PROCEDURE DMUMPS_GET_DETERMINANT  ! Returns DOUBLE PRECISION
    MODULE PROCEDURE CMUMPS_GET_DETERMINANT  ! Returns COMPLEX
    MODULE PROCEDURE ZMUMPS_GET_DETERMINANT  ! Returns COMPLEX*16
  END INTERFACE MUMPS_GET_DETERMINANT

CONTAINS

  !===========================================================================
  ! Wrapper Implementations
  !===========================================================================
  ! These wrappers call the existing precision-specific implementations
  ! In a full implementation, these would be in separate files or link
  ! directly to the compiled precision-specific routines.

  ! Single Precision Wrappers
  SUBROUTINE SMUMPS_INIT(id)
    TYPE(SMUMPS_STRUC), INTENT(INOUT) :: id
    ! Calls existing SMUMPS implementation
    CALL SMUMPS(id%JOB_INIT)
  END SUBROUTINE SMUMPS_INIT

  SUBROUTINE SMUMPS_ANALYZE(id)
    TYPE(SMUMPS_STRUC), INTENT(INOUT) :: id
    CALL SMUMPS(id%JOB_ANALYSIS)
  END SUBROUTINE SMUMPS_ANALYZE

  SUBROUTINE SMUMPS_FACTORIZE(id)
    TYPE(SMUMPS_STRUC), INTENT(INOUT) :: id
    CALL SMUMPS(id%JOB_FACTORIZATION)
  END SUBROUTINE SMUMPS_FACTORIZE

  SUBROUTINE SMUMPS_SOLVE(id)
    TYPE(SMUMPS_STRUC), INTENT(INOUT) :: id
    CALL SMUMPS(id%JOB_SOLVE)
  END SUBROUTINE SMUMPS_SOLVE

  SUBROUTINE SMUMPS_FINALIZE(id)
    TYPE(SMUMPS_STRUC), INTENT(INOUT) :: id
    CALL SMUMPS(id%JOB_TERMINATE)
  END SUBROUTINE SMUMPS_FINALIZE

  FUNCTION SMUMPS_GET_DETERMINANT(id) RESULT(det)
    TYPE(SMUMPS_STRUC), INTENT(IN) :: id
    REAL :: det
    det = id%RINFOG(12) * (10.0 ** id%RINFOG(13))
  END FUNCTION SMUMPS_GET_DETERMINANT

  ! Double Precision Wrappers
  SUBROUTINE DMUMPS_INIT(id)
    TYPE(DMUMPS_STRUC), INTENT(INOUT) :: id
    CALL DMUMPS(id%JOB_INIT)
  END SUBROUTINE DMUMPS_INIT

  SUBROUTINE DMUMPS_ANALYZE(id)
    TYPE(DMUMPS_STRUC), INTENT(INOUT) :: id
    CALL DMUMPS(id%JOB_ANALYSIS)
  END SUBROUTINE DMUMPS_ANALYZE

  SUBROUTINE DMUMPS_FACTORIZE(id)
    TYPE(DMUMPS_STRUC), INTENT(INOUT) :: id
    CALL DMUMPS(id%JOB_FACTORIZATION)
  END SUBROUTINE DMUMPS_FACTORIZE

  SUBROUTINE DMUMPS_SOLVE(id)
    TYPE(DMUMPS_STRUC), INTENT(INOUT) :: id
    CALL DMUMPS(id%JOB_SOLVE)
  END SUBROUTINE DMUMPS_SOLVE

  SUBROUTINE DMUMPS_FINALIZE(id)
    TYPE(DMUMPS_STRUC), INTENT(INOUT) :: id
    CALL DMUMPS(id%JOB_TERMINATE)
  END SUBROUTINE DMUMPS_FINALIZE

  FUNCTION DMUMPS_GET_DETERMINANT(id) RESULT(det)
    TYPE(DMUMPS_STRUC), INTENT(IN) :: id
    DOUBLE PRECISION :: det
    det = id%RINFOG(12) * (10.0D0 ** id%RINFOG(13))
  END FUNCTION DMUMPS_GET_DETERMINANT

  ! Complex Single Precision Wrappers
  SUBROUTINE CMUMPS_INIT(id)
    TYPE(CMUMPS_STRUC), INTENT(INOUT) :: id
    CALL CMUMPS(id%JOB_INIT)
  END SUBROUTINE CMUMPS_INIT

  SUBROUTINE CMUMPS_ANALYZE(id)
    TYPE(CMUMPS_STRUC), INTENT(INOUT) :: id
    CALL CMUMPS(id%JOB_ANALYSIS)
  END SUBROUTINE CMUMPS_ANALYZE

  SUBROUTINE CMUMPS_FACTORIZE(id)
    TYPE(CMUMPS_STRUC), INTENT(INOUT) :: id
    CALL CMUMPS(id%JOB_FACTORIZATION)
  END SUBROUTINE CMUMPS_FACTORIZE

  SUBROUTINE CMUMPS_SOLVE(id)
    TYPE(CMUMPS_STRUC), INTENT(INOUT) :: id
    CALL CMUMPS(id%JOB_SOLVE)
  END SUBROUTINE CMUMPS_SOLVE

  SUBROUTINE CMUMPS_FINALIZE(id)
    TYPE(CMUMPS_STRUC), INTENT(INOUT) :: id
    CALL CMUMPS(id%JOB_TERMINATE)
  END SUBROUTINE CMUMPS_FINALIZE

  FUNCTION CMUMPS_GET_DETERMINANT(id) RESULT(det)
    TYPE(CMUMPS_STRUC), INTENT(IN) :: id
    COMPLEX :: det
    REAL :: magnitude, exponent
    magnitude = id%RINFOG(12)
    exponent = id%RINFOG(13)
    det = CMPLX(magnitude * (10.0 ** exponent), 0.0)
  END FUNCTION CMUMPS_GET_DETERMINANT

  ! Complex Double Precision Wrappers
  SUBROUTINE ZMUMPS_INIT(id)
    TYPE(ZMUMPS_STRUC), INTENT(INOUT) :: id
    CALL ZMUMPS(id%JOB_INIT)
  END SUBROUTINE ZMUMPS_INIT

  SUBROUTINE ZMUMPS_ANALYZE(id)
    TYPE(ZMUMPS_STRUC), INTENT(INOUT) :: id
    CALL ZMUMPS(id%JOB_ANALYSIS)
  END SUBROUTINE ZMUMPS_ANALYZE

  SUBROUTINE ZMUMPS_FACTORIZE(id)
    TYPE(ZMUMPS_STRUC), INTENT(INOUT) :: id
    CALL ZMUMPS(id%JOB_FACTORIZATION)
  END SUBROUTINE ZMUMPS_FACTORIZE

  SUBROUTINE ZMUMPS_SOLVE(id)
    TYPE(ZMUMPS_STRUC), INTENT(INOUT) :: id
    CALL ZMUMPS(id%JOB_SOLVE)
  END SUBROUTINE ZMUMPS_SOLVE

  SUBROUTINE ZMUMPS_FINALIZE(id)
    TYPE(ZMUMPS_STRUC), INTENT(INOUT) :: id
    CALL ZMUMPS(id%JOB_TERMINATE)
  END SUBROUTINE ZMUMPS_FINALIZE

  FUNCTION ZMUMPS_GET_DETERMINANT(id) RESULT(det)
    TYPE(ZMUMPS_STRUC), INTENT(IN) :: id
    COMPLEX(KIND=8) :: det
    DOUBLE PRECISION :: magnitude, exponent
    magnitude = id%RINFOG(12)
    exponent = id%RINFOG(13)
    det = CMPLX(magnitude * (10.0D0 ** exponent), 0.0D0, KIND=8)
  END FUNCTION ZMUMPS_GET_DETERMINANT

END MODULE MUMPS_GENERIC_INTERFACES
