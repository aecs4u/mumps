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
 MODULE MUMPS_FAC_MAPROW_DATA_M
 IMPLICIT NONE
#if ! defined(NO_FDM_MAPROW)
!      =========================================
!      The MUMPS_FAC_MAPROW_DATA_M module stores
!      the MAPROW messages that arrive too early.
!      It is based on the MUMPS_FRONT_DATA_MGT_M
!      module.
!
!      An array of structures that contain MAPROW
!      information is used as a global variable in
!      this module. It is indexed by an "IWHANDLER"
!      (stored in the main IW array) that is
!      managed by the MUMPS_FRONT_DATA_MGT_M module.
!
!      The same handler can be used for other data
!      stored for active type 2 fronts (DESCBAND
!      information, typically)
!      ========================================
!
 PRIVATE
 PUBLIC :: MAPROW_STRUC_T, MUMPS_FMRD_INIT, MUMPS_FMRD_END, &
  &           MUMPS_FMRD_SAVE_MAPROW, MUMPS_FMRD_IS_MAPROW_STORED, &
  &           MUMPS_FMRD_RETRIEVE_MAPROW, &
  &           MUMPS_FMRD_FREE_MAPROW_STRUC
 TYPE MAPROW_STRUC_T
   INTEGER :: INODE, ISON, NSLAVES_PERE, NFRONT_PERE, &
  &              NASS_PERE, LMAP, NFS4FATHER
   INTEGER,POINTER, DIMENSION(:) :: SLAVES_PERE !size NSLAVES_PERE
   INTEGER,POINTER, DIMENSION(:) :: TROW        !size LMAP
 END TYPE MAPROW_STRUC_T
 TYPE (MAPROW_STRUC_T), POINTER, DIMENSION(:), SAVE :: FMRD_ARRAY
 CONTAINS
 FUNCTION MUMPS_FMRD_IS_MAPROW_STORED( IWHANDLER )
 LOGICAL :: MUMPS_FMRD_IS_MAPROW_STORED
 INTEGER, INTENT(IN) :: IWHANDLER
 IF (IWHANDLER .LT. 0 .OR. IWHANDLER .GT. size(FMRD_ARRAY)) THEN
   MUMPS_FMRD_IS_MAPROW_STORED = .FALSE.
 ELSE
   MUMPS_FMRD_IS_MAPROW_STORED = &
  &                           (FMRD_ARRAY(IWHANDLER)%INODE .GE. 0 )
   IF (FMRD_ARRAY(IWHANDLER)%INODE .EQ.0) THEN
     WRITE(*,*) " Internal error 1 in MUMPS_FMRD_IS_MAPROW_STORED"
     CALL MUMPS_ABORT()
   ENDIF
 ENDIF
 RETURN
 END FUNCTION MUMPS_FMRD_IS_MAPROW_STORED
!
 SUBROUTINE MUMPS_FMRD_INIT( INITIAL_SIZE, INFO )
!
!      Purpose:
!      =======
!
!      Module initialization
!
!      Arguments
!      =========
!
 INTEGER, INTENT(IN) :: INITIAL_SIZE
 INTEGER, INTENT(INOUT) :: INFO(2)
!
!      Local variables
!      ===============
!
 INTEGER :: I, IERR
!
 ALLOCATE(FMRD_ARRAY( INITIAL_SIZE ), stat=IERR)
 IF (IERR > 0 ) THEN
   INFO(1)=-13
   INFO(2)=INITIAL_SIZE
   RETURN
 ENDIF
 DO I=1, INITIAL_SIZE
   FMRD_ARRAY(I)%INODE=-9999
   NULLIFY(FMRD_ARRAY(I)%SLAVES_PERE)
   NULLIFY(FMRD_ARRAY(I)%TROW)
 ENDDO
 RETURN
 END SUBROUTINE MUMPS_FMRD_INIT
!
 SUBROUTINE MUMPS_FMRD_SAVE_MAPROW( &
  &   IWHANDLER, &
  &   INODE, ISON, NSLAVES_PERE, NFRONT_PERE, &
  &              NASS_PERE, LMAP, NFS4FATHER, &
  &   SLAVES_PERE, & !size NSLAVES_PERE
  &   TROW, &        !size LMAP
  &   INFO)
!
!      Arguments:
!      =========
!
 INTEGER, INTENT(IN) :: INODE, ISON, NSLAVES_PERE, NFRONT_PERE, &
  &                        NASS_PERE, LMAP, NFS4FATHER
 INTEGER, INTENT(IN) :: SLAVES_PERE (max(1,NSLAVES_PERE))
 INTEGER, INTENT(IN) :: TROW( LMAP)
 INTEGER, INTENT(INOUT) :: IWHANDLER, INFO(2)
!
!      Local variables:
!      ===============
!
 TYPE(MAPROW_STRUC_T) :: MAPROW_STRUC
!
 CALL MUMPS_FMRD_FILL_MAPROW( MAPROW_STRUC, &
  &   INODE, ISON, NSLAVES_PERE, NFRONT_PERE, &
  &              NASS_PERE, LMAP, NFS4FATHER, &
  &   SLAVES_PERE, & !size NSLAVES_PERE
  &   TROW, &        !size LMAP
  &   INFO)
 IF (INFO(1) .LT. 0) RETURN
 CALL MUMPS_FMRD_STORE_MAPROW(IWHANDLER, MAPROW_STRUC, INFO)
 RETURN
 END SUBROUTINE MUMPS_FMRD_SAVE_MAPROW
!
 SUBROUTINE MUMPS_FMRD_STORE_MAPROW(IWHANDLER, MAPROW_STRUC, INFO)
 USE MUMPS_FRONT_DATA_MGT_M, ONLY : MUMPS_FDM_START_IDX
!
!      Purpose:
!      =======
!
!      Given an IWHANDLER and a MAPROW structure, store the MAPROW
!      structure into the main array of the module.
!
!      If IWHANDLER is larger than the current array size, the
!      array is reallocated.
!
!      Arguments:
!      =========
!
 INTEGER, INTENT(INOUT) :: IWHANDLER, INFO(2)
 TYPE(MAPROW_STRUC_T), INTENT(IN) :: MAPROW_STRUC
!
!      Local variables:
!      ===============
!
 TYPE(MAPROW_STRUC_T), POINTER, DIMENSION(:) :: FMRD_ARRAY_TMP
 INTEGER :: OLD_SIZE, NEW_SIZE
 INTEGER :: I
 INTEGER :: IERR
!
 CALL MUMPS_FDM_START_IDX('A', 'MAPROW', IWHANDLER, INFO)
 IF (INFO(1) .LT. 0) RETURN
 IF (IWHANDLER > size(FMRD_ARRAY)) THEN
!        Reallocate in a bigger array
   OLD_SIZE = size(FMRD_ARRAY)
   NEW_SIZE = max( (OLD_SIZE * 3) / 2 + 1, IWHANDLER)
!        
   ALLOCATE(FMRD_ARRAY_TMP(NEW_SIZE),stat=IERR)
   IF (IERR.GT.0) THEN
     INFO(1)=-13
     INFO(2)=NEW_SIZE
     RETURN
   ENDIF
   DO I=1, OLD_SIZE
     FMRD_ARRAY_TMP(I)=FMRD_ARRAY(I)
   ENDDO
!        Similar to code in MUMPS_FMRD_INIT:
   DO I=OLD_SIZE+1, NEW_SIZE
     FMRD_ARRAY_TMP(I)%INODE = -9999
     NULLIFY(FMRD_ARRAY_TMP(I)%SLAVES_PERE)
     NULLIFY(FMRD_ARRAY_TMP(I)%TROW)
   ENDDO
   DEALLOCATE(FMRD_ARRAY)
   FMRD_ARRAY=>FMRD_ARRAY_TMP
   NULLIFY(FMRD_ARRAY_TMP)
 ENDIF
 FMRD_ARRAY(IWHANDLER) = MAPROW_STRUC
 RETURN
 END SUBROUTINE MUMPS_FMRD_STORE_MAPROW
 SUBROUTINE MUMPS_FMRD_FILL_MAPROW(MAPROW_STRUC, &
  &   INODE, ISON, NSLAVES_PERE, NFRONT_PERE, &
  &              NASS_PERE, LMAP, NFS4FATHER, &
  &   SLAVES_PERE, & !size NSLAVES_PERE
  &   TROW, &        !size LMAP
  &   INFO)
!
!      Purpose:
!      =======
!      Fill the MAPROW_STRUC into
!
!      Arguments:
!      =========
!
 INTEGER, INTENT(IN) :: INODE, ISON, NSLAVES_PERE, NFRONT_PERE, &
  &              NASS_PERE, LMAP, NFS4FATHER
 INTEGER, INTENT(IN) :: SLAVES_PERE(max(1,NSLAVES_PERE))
 INTEGER, INTENT(IN) :: TROW( LMAP)
 TYPE (MAPROW_STRUC_T), INTENT(OUT) :: MAPROW_STRUC
 INTEGER, INTENT(INOUT) :: INFO(2)
!
!      Local variables:
!      ===============
!
 INTEGER :: IERR, I
!
 MAPROW_STRUC%INODE        = INODE
 MAPROW_STRUC%ISON         = ISON
 MAPROW_STRUC%NSLAVES_PERE = NSLAVES_PERE
 MAPROW_STRUC%NFRONT_PERE  = NFRONT_PERE
 MAPROW_STRUC%NASS_PERE    = NASS_PERE
 MAPROW_STRUC%LMAP         = LMAP
 MAPROW_STRUC%NFS4FATHER   = NFS4FATHER
 ALLOCATE(MAPROW_STRUC%SLAVES_PERE(max(1,NSLAVES_PERE)), &
  &            MAPROW_STRUC%TROW(LMAP), stat=IERR)
 IF (IERR .GT.0) THEN
     INFO(1) = -13
     INFO(2) = NSLAVES_PERE + LMAP
     RETURN
 ENDIF
 DO I=1, NSLAVES_PERE
     MAPROW_STRUC%SLAVES_PERE(I) = SLAVES_PERE(I)
 ENDDO
 DO I=1, LMAP
     MAPROW_STRUC%TROW(I) = TROW(I)
 ENDDO
 RETURN
 END SUBROUTINE MUMPS_FMRD_FILL_MAPROW
!
 SUBROUTINE MUMPS_FMRD_FREE_MAPROW_STRUC(IWHANDLER)
 USE MUMPS_FRONT_DATA_MGT_M, ONLY : MUMPS_FDM_END_IDX
!
!      Purpose:
!      =======
!
!      Free internal arrays of MAPROW_STRUC.
!      Typically used after a MAPROW_STRUC has been retrieved
!      from the module and late-received message has finally
!      been processed.
!
!      MAPROW_STRUC normally corresponds to a local variable
!      of the calling routine and will not be reused.
!
!      Arguments:
!      =========
!
 INTEGER, INTENT(INOUT) :: IWHANDLER
!
!      Local variables:
!      ===============
!
 TYPE (MAPROW_STRUC_T), POINTER :: MAPROW_STRUC
!
 MAPROW_STRUC => FMRD_ARRAY(IWHANDLER)
 MAPROW_STRUC%INODE = -7777 ! Special value: negative means unused
 DEALLOCATE(MAPROW_STRUC%SLAVES_PERE, MAPROW_STRUC%TROW)
 NULLIFY     (MAPROW_STRUC%SLAVES_PERE, MAPROW_STRUC%TROW)
!      Release handler IWHANDLER and store it
!      in a new free position for future reuse
 CALL MUMPS_FDM_END_IDX('A', 'MAPROW', IWHANDLER)
 RETURN
 END SUBROUTINE MUMPS_FMRD_FREE_MAPROW_STRUC
!
 SUBROUTINE MUMPS_FMRD_RETRIEVE_MAPROW(IWHANDLER, MAPROW_STRUC)
!
!      Purpose:
!      =======
!
!      Given an IWHANDLER, return a pointer to a MAPROW structure,
!      containing information on a previously received MAPROW message.
!
!      Arguments:
!      =========
!
 INTEGER, INTENT(IN) :: IWHANDLER
#if defined(MUMPS_NOF2003)
 TYPE (MAPROW_STRUC_T), POINTER :: MAPROW_STRUC
#else
 TYPE (MAPROW_STRUC_T), POINTER, INTENT(OUT) :: MAPROW_STRUC
#endif
 MAPROW_STRUC => FMRD_ARRAY(IWHANDLER)
 RETURN
 END SUBROUTINE MUMPS_FMRD_RETRIEVE_MAPROW
!
 SUBROUTINE MUMPS_FMRD_END(INFO1)
!
!      Purpose:
!      =======
!      Module final termination.
!
!      Arguments:
!      =========
!
 INTEGER, INTENT(IN) :: INFO1
!      Local variables:
!      ===============
 INTEGER :: I, IWHANDLER
!
 IF (.NOT. associated(FMRD_ARRAY)) THEN
   WRITE(*,*) "Internal error 1 in MUMPS_FAC_FMRD_END"
   CALL MUMPS_ABORT()
 ENDIF
 DO I=1, size(FMRD_ARRAY)
   IF (FMRD_ARRAY(I)%INODE .GE. 0) THEN
!          Node is not free: possible only in
!          case of fatal error (INFO1 < 0)
      IF (INFO1 .GE.0) THEN
!            Should have been freed earlier while consuming MAPLIG
       WRITE(*,*) "Internal error 2 in MUMPS_FAC_FMRD_END",I
       CALL MUMPS_ABORT()
     ELSE
!            May happen in case an error has forced finishing
!            factorization before all MAPROW msgs were processed.
!            We copy the loop index I in the local variable IWHANDLER
!            because there would otherwise be a risk for the loop index
!            I to be modified by MUMPS_FMRD_FREE_MAPROW_STRUC
       IWHANDLER=I
       CALL MUMPS_FMRD_FREE_MAPROW_STRUC(IWHANDLER)
     ENDIF
   ENDIF
 ENDDO
 DEALLOCATE(FMRD_ARRAY)
 RETURN
 END SUBROUTINE MUMPS_FMRD_END
#endif
 END MODULE MUMPS_FAC_MAPROW_DATA_M
