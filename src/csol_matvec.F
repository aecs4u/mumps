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
SUBROUTINE CMUMPS_MV_ELT( N, NELT, ELTPTR, ELTVAR, A_ELT, &
  &                          X, Y, K50, MTYPE )
IMPLICIT NONE
!
!  Purpose
!  =======
!
!  To perform the matrix vector product
!      A_ELT X = Y    if MTYPE = 1
!      A_ELT^T X = Y  if MTYPE = 0
!
!  If K50 is different from 0, then the elements are
!  supposed to be in symmetric packed storage; the
!  lower part is stored by columns.
!  Otherwise, the element is square, stored by columns.
!
!  Note
!  ====
!
!  A_ELT is processed entry by entry and this code is not
!  optimized. In particular, one could gather/scatter
!  X / Y for each element to improve performance.
!
!  Arguments
!  =========
!
INTEGER N, NELT, K50, MTYPE
INTEGER ELTPTR( NELT + 1 ), ELTVAR( * )
COMPLEX A_ELT( * ), X( N ), Y( N )
!
!  Local variables
!  ===============
!
INTEGER IEL, I , J, SIZEI, IELPTR
INTEGER(8) :: K8
COMPLEX TEMP
COMPLEX ZERO
PARAMETER( ZERO = (0.0E0,0.0E0) )
!
!
!     Executable statements
!     =====================
!
Y = ZERO
K8 = 1_8
!     --------------------
!     Process the elements
!     --------------------
DO IEL = 1, NELT
  SIZEI  = ELTPTR( IEL + 1 ) - ELTPTR( IEL )
  IELPTR = ELTPTR( IEL ) - 1
  IF ( K50 .eq. 0 ) THEN
!         -------------------
!         Unsymmetric element
!         stored by columns
!         -------------------
    IF ( MTYPE .eq. 1 ) THEN
!           -----------------
!           Compute A_ELT x X
!           -----------------
      DO J = 1, SIZEI
        TEMP = X( ELTVAR( IELPTR + J ) )
        DO I = 1, SIZEI
          Y( ELTVAR( IELPTR + I ) ) = &
  &          Y( ELTVAR( IELPTR + I ) ) + &
  &             A_ELT( K8 ) * TEMP
          K8 = K8 + 1
        END DO
      END DO
    ELSE
!           -------------------
!           Compute A_ELT^T x X
!           -------------------
      DO J = 1, SIZEI
        TEMP = Y( ELTVAR( IELPTR + J ) )
        DO I = 1, SIZEI
          TEMP = TEMP + &
  &          A_ELT( K8 ) * X( ELTVAR( IELPTR + I ) )
          K8 = K8 + 1
        END DO
        Y( ELTVAR( IELPTR + J ) ) = TEMP
      END DO
    END IF
  ELSE
!         -----------------
!         Symmetric element
!         L stored by cols
!         -----------------
    DO J = 1, SIZEI
!           Diagonal counted once
      Y( ELTVAR( IELPTR + J ) ) = &
  &      Y( ELTVAR( IELPTR + J ) ) + &
  &           A_ELT( K8 ) * X( ELTVAR( IELPTR + J ) )
      K8 = K8 + 1
      DO I = J+1, SIZEI
!             Off diagonal + transpose
        Y( ELTVAR( IELPTR + I ) ) = &
  &        Y( ELTVAR( IELPTR + I ) ) + &
  &           A_ELT( K8 ) * X( ELTVAR( IELPTR + J ) )
        Y( ELTVAR( IELPTR + J ) ) = &
  &        Y( ELTVAR( IELPTR + J ) ) + &
  &           A_ELT( K8 ) * X( ELTVAR( IELPTR + I ) )
        K8 = K8 + 1
      END DO
    END DO
  END IF
END DO
RETURN
END SUBROUTINE CMUMPS_MV_ELT
SUBROUTINE CMUMPS_LOC_MV8 &
  &( N, NZ_loc8, IRN_loc, JCN_loc, A_loc, X, Y_loc, &
  &  LDLT, MTYPE)
IMPLICIT NONE
!
!     Purpose:
!     =======
!
!     Perform a distributed matrix vector product.
!        Y_loc <- A X   if MTYPE = 1
!        Y_loc <- A^T X if MTYPE = 0
!
!     Notes:
!     =====
!
!     1) assembly of all Y_loc still has to be done on exit.
!     2) X should be available on all processors.
!
!     Arguments:
!     =========
!
INTEGER N
INTEGER(8) :: NZ_loc8
INTEGER IRN_loc( NZ_loc8 ), JCN_loc( NZ_loc8 )
COMPLEX A_loc( NZ_loc8 ), X( N ), Y_loc( N )
INTEGER LDLT, MTYPE
!
!     Locals variables:
!     ================
!
INTEGER I, J
INTEGER(8) :: K8
COMPLEX ZERO
PARAMETER( ZERO = (0.0E0,0.0E0) )
Y_loc = ZERO
IF ( LDLT .eq. 0 ) THEN
!       Unsymmetric
  IF ( MTYPE .eq. 1 ) THEN
!         No transpose
    DO K8 = 1_8, NZ_loc8
      I = IRN_loc(K8)
      J = JCN_loc(K8)
      IF ((I .LE. 0) .OR. (I .GT. N) .OR. &
  &          (J .LE. 0) .OR. (J .GT. N) &
  &        ) CYCLE
    Y_loc(I) = Y_loc(I) + A_loc(K8) * X(J)
  ENDDO
  ELSE
!         Transpose
    DO K8 = 1_8, NZ_loc8
      I = IRN_loc(K8)
      J = JCN_loc(K8)
      IF ((I .LE. 0) .OR. (I .GT. N) &
  &        .OR. (J .LE. 0) .OR. (J .GT. N) &
  &        ) CYCLE
    Y_loc(J) = Y_loc(J) + A_loc(K8) * X(I)
  ENDDO
  END IF
ELSE
!       Lower (or upper) part of symmetric
!       matrix was provided (LDLT facto)
  DO K8 = 1_8, NZ_loc8
    I = IRN_loc(K8)
    J = JCN_loc(K8)
    IF ((I .LE. 0) .OR. (I .GT. N) .OR. &
  &        (J .LE. 0) .OR. (J .GT. N) &
  &        ) CYCLE
    Y_loc(I) = Y_loc(I) + A_loc(K8) * X(J)
    IF (J.NE.I) THEN
      Y_loc(J) = Y_loc(J) + A_loc(K8) * X(I)
    ENDIF
  ENDDO
END IF
RETURN
END SUBROUTINE CMUMPS_LOC_MV8
SUBROUTINE CMUMPS_MV8( N, NZ8, IRN, ICN, ASPK, X, Y, &
  &                      LDLT, MTYPE, MAXTRANS, PERM, &
  &                      IFLAG, IERROR )
!
!     Purpose:
!     =======
!
!     Perform matrix-vector product
!        Y <- A X if MTYPE = 1
!        Y <- A^T X if MTYPE = 0
!
!
!     Note:
!     ====
!
!     MAXTRANS should be set to 1 if a column permutation
!     was applied on A and we still want the matrix vector
!     product wrt the original matrix.
!
!     Arguments:
!     =========
!
INTEGER N, LDLT, MTYPE, MAXTRANS
INTEGER(8) :: NZ8
INTEGER IRN( NZ8 ), ICN( NZ8 )
INTEGER PERM( N )
COMPLEX ASPK( NZ8 ), X( N ), Y( N )
INTEGER, intent(inout) :: IFLAG, IERROR
!
!     Local variables
!     ===============
!
INTEGER I, J
INTEGER(8) :: K8
COMPLEX, DIMENSION(:), ALLOCATABLE :: PX
COMPLEX ZERO
INTEGER :: allocok
PARAMETER( ZERO = (0.0E0,0.0E0) )
Y = ZERO
ALLOCATE(PX(N), stat=allocok)
IF (allocok < 0) THEN
  IFLAG  = -13
  IERROR = N
  RETURN
ENDIF
!
!     --------------------------------------
!     Permute X if A has been permuted
!     with some max-trans column permutation
!     --------------------------------------
IF ( MAXTRANS .eq. 1 .and. MTYPE .eq. 1) THEN
  DO I = 1, N
    PX(I) = X( PERM( I ) )
  END DO
ELSE
  PX = X
END IF
IF ( LDLT .eq. 0 ) THEN
!
!     Complete unsymmetric matrix was provided (LU facto)
 IF (MTYPE .EQ. 1) THEN
  DO K8 = 1_8, NZ8
    I = IRN(K8)
    J = ICN(K8)
    IF ((I .LE. 0) .OR. (I .GT. N) .OR. (J .LE. 0) .OR. (J .GT. N) &
  &        ) CYCLE
    Y(I) = Y(I) + ASPK(K8) * PX(J)
  ENDDO
 ELSE
  DO K8 = 1_8, NZ8
    I = IRN(K8)
    J = ICN(K8)
    IF ((I .LE. 0) .OR. (I .GT. N) .OR. (J .LE. 0) .OR. (J .GT. N) &
  &        ) CYCLE
    Y(J) = Y(J) + ASPK(K8) * PX(I)
  ENDDO
 ENDIF
!
ELSE
!
!       Lower (or upper) part of symmetric
!       matrix was provided (LDLT facto)
  DO K8 = 1_8, NZ8
    I = IRN(K8)
    J = ICN(K8)
    IF ((I .LE. 0) .OR. (I .GT. N) .OR. (J .LE. 0) .OR. (J .GT. N) &
  &        ) CYCLE
    Y(I) = Y(I) + ASPK(K8) * PX(J)
    IF (J.NE.I) THEN
      Y(J) = Y(J) + ASPK(K8) * PX(I)
    ENDIF
  ENDDO
END IF
IF ( MAXTRANS .EQ. 1 .AND. MTYPE .eq. 0 ) THEN
PX = Y
DO I = 1, N
  Y( PERM( I ) ) = PX( I )
END DO
END IF
DEALLOCATE(PX)
RETURN
END SUBROUTINE CMUMPS_MV8
!
!
SUBROUTINE CMUMPS_LOC_OMEGA1 &
  &( N, NZ_loc8, IRN_loc, JCN_loc, A_loc, X, Y_loc, &
  &  LDLT, MTYPE)
IMPLICIT NONE
!
!     Purpose:
!     =======
!     Compute
!        * If MTYPE = 1
!            Y_loc(i) = Sum | Aij | | Xj |
!                        j
!        * If MTYPE = 0
!            Y_loc(j) = Sum | Aij | | Xi |
!
!
!     Notes:
!     =====
!
!     1) assembly of all Y_loc still has to be done.
!     2) X should be available on all processors.
!
!     Arguments:
!     =========
!
INTEGER N
INTEGER(8) :: NZ_loc8
INTEGER IRN_loc( NZ_loc8 ), JCN_loc( NZ_loc8 )
COMPLEX A_loc( NZ_loc8 ), X( N )
REAL Y_loc( N )
INTEGER LDLT, MTYPE
!
!     Local variables:
!     ===============
!
INTEGER I, J
INTEGER(8) :: K8
REAL, PARAMETER :: RZERO=0.0E0
!
Y_loc = RZERO
IF ( LDLT .eq. 0 ) THEN
!       Unsymmetric
  IF ( MTYPE .eq. 1 ) THEN
!         No transpose
    DO K8 = 1_8, NZ_loc8
      I = IRN_loc(K8)
      J = JCN_loc(K8)
      IF ((I .LE. 0) .OR. (I .GT. N) .OR. &
  &          (J .LE. 0) .OR. (J .GT. N) &
  &        ) CYCLE
      Y_loc(I) = Y_loc(I) + abs( A_loc(K8) * X(J) )
    ENDDO
  ELSE
!         Transpose
    DO K8 = 1_8, NZ_loc8
      I = IRN_loc(K8)
      J = JCN_loc(K8)
      IF ((I .LE. 0) .OR. (I .GT. N) &
  &        .OR. (J .LE. 0) .OR. (J .GT. N) &
  &        ) CYCLE
    Y_loc(J) = Y_loc(J) + abs( A_loc(K8) * X(I) )
    ENDDO
  END IF
ELSE
!       Lower (or upper) part of symmetric
!       matrix was provided (LDLT facto)
  DO K8 = 1_8, NZ_loc8
    I = IRN_loc(K8)
    J = JCN_loc(K8)
    IF ((I .LE. 0) .OR. (I .GT. N) .OR. &
  &        (J .LE. 0) .OR. (J .GT. N) &
  &        ) CYCLE
    Y_loc(I) = Y_loc(I) + abs( A_loc(K8) * X(J) )
    IF (J.NE.I) THEN
      Y_loc(J) = Y_loc(J) + abs( A_loc(K8) * X(I) )
    ENDIF
  ENDDO
END IF
RETURN
END SUBROUTINE CMUMPS_LOC_OMEGA1
