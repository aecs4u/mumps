!
!  MUMPS Generic Interface Demo
!
!  This example demonstrates how generic interfaces simplify user code
!  by providing a single API that works with all precisions.
!
!  Compile: gfortran -o generic_demo generic_interface_demo.f90 -lmumps_generic -ldmumps
!  Run: mpirun -np 4 ./generic_demo
!

PROGRAM GENERIC_INTERFACE_DEMO
  USE MUMPS_GENERIC_INTERFACES
  USE DMUMPS_STRUC_DEF
  USE CMUMPS_STRUC_DEF
  IMPLICIT NONE

  TYPE(DMUMPS_STRUC) :: id_real
  TYPE(CMUMPS_STRUC) :: id_complex
  DOUBLE PRECISION :: det_real
  COMPLEX :: det_complex
  INTEGER :: i

  PRINT *, "============================================"
  PRINT *, "MUMPS Generic Interface Demo"
  PRINT *, "============================================"
  PRINT *, ""

  !===========================================================================
  ! Example 1: Double Precision Solve
  !===========================================================================
  PRINT *, "Example 1: Double Precision System"
  PRINT *, "-----------------------------------"

  ! Initialize - compiler selects DMUMPS based on id_real type
  CALL MUMPS_INIT(id_real)
  PRINT *, "✓ Initialized (DMUMPS automatically selected)"

  ! Set up a simple 3x3 system
  id_real%N = 3
  id_real%NZ = 5  ! Number of non-zeros

  ! Allocate arrays
  ALLOCATE(id_real%IRN(5), id_real%JCN(5), id_real%A(5))
  ALLOCATE(id_real%RHS(3))

  ! Define matrix:
  !  [ 2  -1   0 ]
  !  [-1   2  -1 ]
  !  [ 0  -1   2 ]
  id_real%IRN = [1, 1, 2, 2, 3]
  id_real%JCN = [1, 2, 1, 2, 3]
  id_real%A   = [2.0D0, -1.0D0, -1.0D0, 2.0D0, 2.0D0]

  ! Right-hand side
  id_real%RHS = [1.0D0, 0.0D0, 1.0D0]

  ! Set parameters
  id_real%ICNTL(1) = -1  ! Suppress output
  id_real%ICNTL(2) = -1
  id_real%ICNTL(3) = -1
  id_real%ICNTL(4) = 0

  ! Solve using generic interface - same calls for all precisions!
  CALL MUMPS_ANALYZE(id_real)
  PRINT *, "✓ Analysis complete"

  CALL MUMPS_FACTORIZE(id_real)
  PRINT *, "✓ Factorization complete"

  CALL MUMPS_SOLVE(id_real)
  PRINT *, "✓ Solution computed"

  ! Get determinant using generic interface
  det_real = MUMPS_GET_DETERMINANT(id_real)
  PRINT *, "  Determinant:", det_real

  ! Display solution
  PRINT *, "  Solution vector:"
  DO i = 1, 3
    PRINT *, "    x(", i, ") = ", id_real%RHS(i)
  END DO

  ! Clean up
  CALL MUMPS_FINALIZE(id_real)
  DEALLOCATE(id_real%IRN, id_real%JCN, id_real%A, id_real%RHS)
  PRINT *, "✓ Finalized"
  PRINT *, ""

  !===========================================================================
  ! Example 2: Complex System
  !===========================================================================
  PRINT *, "Example 2: Complex System"
  PRINT *, "-------------------------"

  ! Same interface, different type - compiler automatically selects CMUMPS!
  CALL MUMPS_INIT(id_complex)
  PRINT *, "✓ Initialized (CMUMPS automatically selected)"

  ! Set up a simple 2x2 complex system
  id_complex%N = 2
  id_complex%NZ = 3

  ALLOCATE(id_complex%IRN(3), id_complex%JCN(3), id_complex%A(3))
  ALLOCATE(id_complex%RHS(2))

  ! Define matrix:
  !  [ 1+i   -i ]
  !  [  i   1-i ]
  id_complex%IRN = [1, 1, 2]
  id_complex%JCN = [1, 2, 2]
  id_complex%A   = [CMPLX(1.0, 1.0), CMPLX(0.0, -1.0), CMPLX(1.0, -1.0)]

  ! Right-hand side
  id_complex%RHS = [CMPLX(1.0, 0.0), CMPLX(0.0, 1.0)]

  ! Set parameters
  id_complex%ICNTL(1) = -1
  id_complex%ICNTL(2) = -1
  id_complex%ICNTL(3) = -1
  id_complex%ICNTL(4) = 0

  ! Identical calls to Example 1 - that's the power of generic interfaces!
  CALL MUMPS_ANALYZE(id_complex)
  PRINT *, "✓ Analysis complete"

  CALL MUMPS_FACTORIZE(id_complex)
  PRINT *, "✓ Factorization complete"

  CALL MUMPS_SOLVE(id_complex)
  PRINT *, "✓ Solution computed"

  ! Get determinant - same interface, returns COMPLEX
  det_complex = MUMPS_GET_DETERMINANT(id_complex)
  PRINT *, "  Determinant:", det_complex

  ! Display solution
  PRINT *, "  Solution vector:"
  DO i = 1, 2
    PRINT *, "    x(", i, ") = ", id_complex%RHS(i)
  END DO

  ! Clean up
  CALL MUMPS_FINALIZE(id_complex)
  DEALLOCATE(id_complex%IRN, id_complex%JCN, id_complex%A, id_complex%RHS)
  PRINT *, "✓ Finalized"
  PRINT *, ""

  !===========================================================================
  ! Summary
  !===========================================================================
  PRINT *, "============================================"
  PRINT *, "Benefits Demonstrated:"
  PRINT *, "  1. Same code works for all precisions"
  PRINT *, "  2. Type-safe at compile time"
  PRINT *, "  3. No manual precision selection needed"
  PRINT *, "  4. Cleaner, more maintainable code"
  PRINT *, "  5. Single documentation for all types"
  PRINT *, "============================================"

  !===========================================================================
  ! Code Comparison
  !===========================================================================
  PRINT *, ""
  PRINT *, "Code Comparison:"
  PRINT *, "----------------"
  PRINT *, ""
  PRINT *, "OLD WAY (Precision-Specific):"
  PRINT *, "  TYPE(DMUMPS_STRUC) :: id"
  PRINT *, "  CALL DMUMPS(id)  ! Must remember D for double"
  PRINT *, ""
  PRINT *, "NEW WAY (Generic Interface):"
  PRINT *, "  TYPE(DMUMPS_STRUC) :: id"
  PRINT *, "  CALL MUMPS_SOLVE(id)  ! Compiler knows it's double"
  PRINT *, ""
  PRINT *, "Result: Code is more readable and type-safe!"

END PROGRAM GENERIC_INTERFACE_DEMO
