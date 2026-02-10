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
MODULE MUMPS_FRONT_DATA_MGT_M
IMPLICIT NONE
PRIVATE
!     --------------------------------------------
!     This module contains routines to manage
!     handlers of various data associated to
!     active fronts *during the factorization*.
!
!     It should be initialized at the beginning
!     of the factorization and terminated at the
!     end of the factorization.
!
!     There are two types of data, see below.
!
!     'A' is for active type 2 fronts: list must
!         be empty at the end of the factorization
!
!     'F' will be for general fronts -- currently used
!         for BLR fronts, in three situations:
!         1/ factorization of type 2 symmetric active fronts 
!            (requires temporary storage of BLR panels)      
!         2/ LRSOLVE: BLR factors are kept until solution phase
!            (liberated in JOB=-2 or at the beginning of a new facto)      
!         3/ LRCB: CB is dynamically allocated and compressed
!            (liberated before the end of the factorization)      
!
!     Only handlers are managed in this module.
!     The data itself is in the module above using it.
!     For example, FAC_MAPROW_DATA_M manages MAPROW
!     messages that arrive too early. It handles an
!     array that contains all early MAPROW messages
!     and that is indexed with the handlers managed
!     by MUMPS_FRONT_DATA_MGT_M.
!
!     --------------------------------------------
!
!     ===============
!     Public routines
!     ===============
PUBLIC :: MUMPS_FDM_INIT, &
  &          MUMPS_FDM_END, &
  &          MUMPS_FDM_START_IDX, &
  &          MUMPS_FDM_END_IDX &
  &          , MUMPS_FDM_MOD_TO_STRUC &
  &          , MUMPS_FDM_STRUC_TO_MOD
#if ! defined(NO_SAVE_RESTORE)      
  &          , MUMPS_SAVE_RESTORE_FRONT_DATA
#endif
!     STACK_FREE_IDX(1:NB_FREE_IDX) holds the NB_FREE_IDX indices
!                                   of free handlers
!     STACK_FREE_IDX(NB_FREE_IDX+1:size(STACK_FREE_IDX)) is trash data
TYPE FDM_STRUC_T
  INTEGER :: NB_FREE_IDX
  INTEGER, DIMENSION(:), POINTER :: STACK_FREE_IDX => null()
  INTEGER, DIMENSION(:), POINTER :: COUNT_ACCESS   => null()
END TYPE FDM_STRUC_T
TYPE (FDM_STRUC_T), TARGET, SAVE :: FDM_A, FDM_F
#if ! defined(NO_SAVE_RESTORE)
INCLUDE 'mumps_save_restore_modes.h'
#endif
CONTAINS
!
SUBROUTINE MUMPS_FDM_INIT(WHAT, INITIAL_SIZE, INFO)
!
!     Purpose:
!     =======
!
!     Initialize handler data ('A' or 'F')
!
!     Arguments:
!     =========
!
INTEGER, INTENT(IN) :: INITIAL_SIZE
CHARACTER, INTENT(IN) :: WHAT  ! 'A' or 'F'
INTEGER, INTENT(INOUT) :: INFO(2)
!
!     Local variables:
!     ===============
!
INTEGER :: IERR
TYPE (FDM_STRUC_T), POINTER :: FDM_PTR
!
CALL MUMPS_FDM_SET_PTR(WHAT, FDM_PTR)
ALLOCATE( FDM_PTR%STACK_FREE_IDX(INITIAL_SIZE), &
  &          FDM_PTR%COUNT_ACCESS  (INITIAL_SIZE), stat=IERR )
IF (IERR < 0) THEN
  INFO(1) = -13
  INFO(2) = INITIAL_SIZE * 2
  RETURN
ENDIF
CALL MUMPS_FDM_SET_ALL_FREE(FDM_PTR)
RETURN
END SUBROUTINE MUMPS_FDM_INIT
!
SUBROUTINE MUMPS_FDM_END(WHAT)
!
!     Purpose:
!     =======
!     Free module datastructures associated to "WHAT" at
!     the end of a phase (typically factorization).
!
CHARACTER, INTENT(IN) :: WHAT
!
!     Local variables
!     ===============
!
TYPE (FDM_STRUC_T), POINTER :: FDM_PTR
!
CALL MUMPS_FDM_SET_PTR(WHAT, FDM_PTR)
IF (associated(FDM_PTR%STACK_FREE_IDX)) THEN
    DEALLOCATE(FDM_PTR%STACK_FREE_IDX)
    NULLIFY(FDM_PTR%STACK_FREE_IDX)
    FDM_PTR%NB_FREE_IDX=0
ELSE
!         Should not be called twice or when array is unassociated
    WRITE(*,*) "Internal error 1 in MUMPS_FDM_END", WHAT
    CALL MUMPS_ABORT()
ENDIF
IF (associated(FDM_PTR%COUNT_ACCESS)) THEN
    DEALLOCATE(FDM_PTR%COUNT_ACCESS)
    NULLIFY(FDM_PTR%COUNT_ACCESS)
ELSE
!     Should not be called twice or when array is unassociated
    WRITE(*,*) "Internal error 2 in MUMPS_FDM_END", WHAT
    CALL MUMPS_ABORT()
ENDIF
RETURN
END SUBROUTINE MUMPS_FDM_END
!
SUBROUTINE MUMPS_FDM_MOD_TO_STRUC(WHAT, id_FDM_ENCODING,INFO)
!
!     Purpose:
!     =======
!
!     Save module information in struture.
!     id_FDM_ENCODING corresponds to id%FDM_F_ENCODING
!     This version requires that WHAT is equal to 'F'.
!
!     id_FDM_ENDODING takes responsibility of pointing to module
!     FDM_F information. This typically allows data from the module
!     to be passed from factorization to solve through the instance
!     and manage multiple instances.
!
CHARACTER, INTENT(IN) :: WHAT
INTEGER, INTENT(INOUT) :: INFO(2)
#if defined(MUMPS_NOF2003)
CHARACTER, DIMENSION(:), POINTER :: id_FDM_ENCODING
#else
CHARACTER, DIMENSION(:), POINTER, intent(inout) :: &
  &                                           id_FDM_ENCODING
#endif
!
!     Local variables
!     ===============
!
!     Character array of arbitrary dimension 1     
CHARACTER :: CHAR_ARRAY(1)
INTEGER :: CHAR_LENGTH, IERR
!
IF (WHAT .NE. 'F') THEN
  WRITE(*,*) "Internal error 1 in MUMPS_FDM_MOD_TO_STRUC"
  CALL MUMPS_ABORT()
ENDIF
IF (associated(id_FDM_ENCODING)) THEN
!     Should be unassociated for this to work
  WRITE(*,*) "Internal error 2 in MUMPS_FDM_MOD_TO_STRUC"
  CALL MUMPS_ABORT()
ENDIF
CHAR_LENGTH=size(transfer(FDM_F,CHAR_ARRAY))
ALLOCATE(id_FDM_ENCODING(CHAR_LENGTH), stat=IERR )
IF (IERR < 0) THEN
  INFO(1) = -13
  INFO(2) = CHAR_LENGTH
  RETURN
ENDIF
!     ------------------------------
!     Fill contents of pointer array
!     with FDM_F derived datatype
!     ------------------------------
id_FDM_ENCODING = transfer(FDM_F,CHAR_ARRAY)
!     ----------------------------------------------
!     FDM_F is not to be used again before a call to
!     MUMPS_FDM_STRUC_TO_MOD, invalidate its content
!     ----------------------------------------------
FDM_F%NB_FREE_IDX=-9999999
NULLIFY(FDM_F%STACK_FREE_IDX)
NULLIFY(FDM_F%COUNT_ACCESS)
RETURN
END SUBROUTINE MUMPS_FDM_MOD_TO_STRUC
!
SUBROUTINE MUMPS_FDM_STRUC_TO_MOD(WHAT, id_FDM_ENCODING)
!
!     Purpose:
!     =======
!
!     Set module pointer information from id_FDM_ENCODING) typically
!     at beginning of solve. Suppress from structure since
!     responsibility of pointing to module data is now inside
!     the module.
!
CHARACTER, INTENT(IN) :: WHAT
#if defined(MUMPS_NOF2003)
CHARACTER, DIMENSION(:), POINTER :: id_FDM_ENCODING
#else
CHARACTER, DIMENSION(:), POINTER, INTENT(INOUT) &
  &                                        :: id_FDM_ENCODING
#endif
!
!     Local variables
!     ===============
!
IF (.NOT.associated(id_FDM_ENCODING)) THEN
  WRITE(*,*) "Internal error 1 in MUMPS_FDM_STRUC_TO_MOD"
ENDIF
FDM_F=transfer(id_FDM_ENCODING,FDM_F)
!     Module is now responsible for accessing data.
DEALLOCATE(id_FDM_ENCODING)
NULLIFY(id_FDM_ENCODING)
RETURN
END SUBROUTINE MUMPS_FDM_STRUC_TO_MOD
!
SUBROUTINE MUMPS_FDM_START_IDX(WHAT, FROM, IWHANDLER, INFO)
!
!     Purpose:
!     =======
!
!     Return a new free index/handler
!     (typically stored in IW)
!
CHARACTER, INTENT(IN)  :: WHAT
CHARACTER(LEN=*), INTENT(IN)  :: FROM !For debugging purposes only
INTEGER, INTENT(INOUT) :: IWHANDLER
INTEGER, INTENT(INOUT) :: INFO(2)
!
!     Local variables
!     ===============
!
INTEGER :: OLD_SIZE, NEW_SIZE, IERR
INTEGER :: I
INTEGER, DIMENSION(:), POINTER :: TMP_COUNT_ACCESS
TYPE(FDM_STRUC_T), POINTER :: FDM_PTR
CALL MUMPS_FDM_SET_PTR(WHAT, FDM_PTR)
!
IF (IWHANDLER .GT. 0) THEN
!       Already started, counter should at least be 1
   IF (FDM_PTR%COUNT_ACCESS(IWHANDLER) .LT. 1) THEN
    WRITE(*,*) "Internal error 1 in MUMPS_FDM_START_IDX", &
  &    FDM_PTR%COUNT_ACCESS(IWHANDLER)
    CALL MUMPS_ABORT()
  ENDIF
  GOTO 100
ENDIF
!
IF (FDM_PTR%NB_FREE_IDX .EQ. 0) THEN
  OLD_SIZE = size(FDM_PTR%STACK_FREE_IDX)
  NEW_SIZE = (OLD_SIZE * 3) / 2 + 1 ! or something else
  FDM_PTR%NB_FREE_IDX = NEW_SIZE - OLD_SIZE
  DEALLOCATE(FDM_PTR%STACK_FREE_IDX)
  ALLOCATE(FDM_PTR%STACK_FREE_IDX(NEW_SIZE), &
  &           TMP_COUNT_ACCESS(NEW_SIZE), stat=IERR)
  IF (IERR < 0) THEN
    INFO(1) = -13
    INFO(2) = NEW_SIZE
    RETURN
  ENDIF
!       All new handlers indices are created 
  DO I=1, FDM_PTR%NB_FREE_IDX
    FDM_PTR%STACK_FREE_IDX(I)=NEW_SIZE-I+1
  ENDDO
!       Count access: copy old ones
  DO I=1, OLD_SIZE
    TMP_COUNT_ACCESS(I)=FDM_PTR%COUNT_ACCESS(I)
  ENDDO
  DO I=OLD_SIZE+1, NEW_SIZE
    TMP_COUNT_ACCESS(I)=0
  ENDDO
  DEALLOCATE(FDM_PTR%COUNT_ACCESS)
  FDM_PTR%COUNT_ACCESS=>TMP_COUNT_ACCESS
ENDIF
!
IWHANDLER = FDM_PTR%STACK_FREE_IDX(FDM_PTR%NB_FREE_IDX)
FDM_PTR%NB_FREE_IDX = FDM_PTR%NB_FREE_IDX - 1
100 CONTINUE
!     Number of modules accessing this handler
FDM_PTR%COUNT_ACCESS(IWHANDLER)=FDM_PTR%COUNT_ACCESS(IWHANDLER)+1
RETURN
END SUBROUTINE MUMPS_FDM_START_IDX
!
SUBROUTINE MUMPS_FDM_END_IDX(WHAT, FROM, IWHANDLER)
!
!     Purpose:
!     =======
!
!     Notify than an index/handler has been freed.
!     Mark it free for future reuse.
!
CHARACTER, INTENT(IN) :: WHAT
CHARACTER(LEN=*), INTENT(IN) :: FROM ! for debug purposes only
INTEGER, INTENT(INOUT) :: IWHANDLER
TYPE(FDM_STRUC_T), POINTER :: FDM_PTR
!
CALL MUMPS_FDM_SET_PTR(WHAT, FDM_PTR)
IF (IWHANDLER .LE.0) THEN
!     Already ended
  WRITE(*,*) "Internal error 1 in MUMPS_FDM_END_IDX",IWHANDLER
  CALL MUMPS_ABORT()
ENDIF
FDM_PTR%COUNT_ACCESS(IWHANDLER)=FDM_PTR%COUNT_ACCESS(IWHANDLER)-1
IF (FDM_PTR%COUNT_ACCESS(IWHANDLER) .LT. 0) THEN
!     Negative counter!
  WRITE(*,*) "Internal error 2 in MUMPS_FDM_END_IDX", &
  &  IWHANDLER, FDM_PTR%COUNT_ACCESS(IWHANDLER)
  CALL MUMPS_ABORT()
ENDIF
IF (FDM_PTR%COUNT_ACCESS(IWHANDLER) .EQ.0 ) THEN
   IF (FDM_PTR%NB_FREE_IDX .GE. size(FDM_PTR%STACK_FREE_IDX)) THEN
    WRITE(*,*) "Internal error 3 in MUMPS_FDM_END_IDX"
    CALL MUMPS_ABORT()
  ENDIF
  FDM_PTR%NB_FREE_IDX = FDM_PTR%NB_FREE_IDX + 1
!       Having incremented the nb of free handlers we
!       store the index (IWHANDLER) that has been
!       effectively released for future reuse.
  FDM_PTR%STACK_FREE_IDX(FDM_PTR%NB_FREE_IDX) = IWHANDLER
  IWHANDLER = -8888 ! has been used and is now free
ENDIF
!
RETURN
END SUBROUTINE MUMPS_FDM_END_IDX
!     ===================
!     Private subroutines
!     ===================
SUBROUTINE MUMPS_FDM_SET_PTR(WHAT, FDM_PTR)
CHARACTER, INTENT(IN) :: WHAT
#if defined(MUMPS_NOF2003)
TYPE(FDM_STRUC_T), POINTER :: FDM_PTR
#else
TYPE(FDM_STRUC_T), POINTER, INTENT(OUT) :: FDM_PTR
#endif
!
IF ( WHAT .EQ. 'A' ) THEN
  FDM_PTR => FDM_A
ELSE IF ( WHAT .EQ. 'F' ) THEN
  FDM_PTR => FDM_F
ELSE
!     Should be called with either A or F
  WRITE(*,*) "Internal error 1 in MUMPS_FDM_INIT"
  WRITE(*,*) "Allowed arguments for WHAT are A or F"
  CALL MUMPS_ABORT()
ENDIF
END SUBROUTINE MUMPS_FDM_SET_PTR
SUBROUTINE MUMPS_FDM_SET_ALL_FREE(FDM_PTR)
!
!     Purpose:
!     =======
!     Initialize the stack of free elements for the first time
!
TYPE(FDM_STRUC_T), POINTER :: FDM_PTR
INTEGER :: I
FDM_PTR%NB_FREE_IDX = size(FDM_PTR%STACK_FREE_IDX)
DO I = 1, FDM_PTR%NB_FREE_IDX
  FDM_PTR%STACK_FREE_IDX(I)=FDM_PTR%NB_FREE_IDX-I+1
  FDM_PTR%COUNT_ACCESS  (I)=0
ENDDO
RETURN
END SUBROUTINE MUMPS_FDM_SET_ALL_FREE
!
#if ! defined(NO_SAVE_RESTORE)
!     ---------- MUMPS_SAVE_RESTORE_FRONT_DATA ----------------------- !
SUBROUTINE MUMPS_SAVE_RESTORE_FRONT_DATA(id_FDM_F_ENCODING &
  &                 ,unit,MYID,mode &
  &                 ,SIZE_GEST,SIZE_VARIABLES &
  &                 ,SIZE_INT, TOTAL_FILE_SIZE,TOTAL_STRUC_SIZE &
  &                 ,size_read,size_allocated,size_written &
  &                 ,INFO)
IMPLICIT NONE
!  =======
!  Purpose
!  =======
!
!     This routine is designed to manage a FDM_STRUC_T structure (save, restore, compute memory)
!     
!  ==========
!  Parameters
!  ==========
!
!     FDM_STRUC           : TYPE (FDM_STRUC_T) : the main structure
!
!     unit                : The unit of the file to be written or read
!
!     mode                : the type of operation to be performed by the routine 
!                           memory_save  = compute the size of the save file and of the structure
!                           save         = save the instace
!                           restore      = restore the instace
!
!     TOTAL_FILE_SIZE     : size of the file to be written or read
!
!     TOTAL_STRUC_SIZE    : size of the structure to be saved or restored
!
!     SIZE_INT            : size of an integer       
!
!     INFO                : copies of of INFO(1) and INFO(2) to allow save/restore of failled instaces
!      
CHARACTER, DIMENSION(:), POINTER :: id_FDM_F_ENCODING
INTEGER,intent(IN) :: unit,MYID
INTEGER,intent(IN) :: mode
INTEGER,INTENT(OUT) :: SIZE_GEST
INTEGER(8),intent(OUT) :: SIZE_VARIABLES
INTEGER(8),intent(IN) :: TOTAL_FILE_SIZE,TOTAL_STRUC_SIZE
INTEGER,intent(INOUT):: INFO(2)
INTEGER,intent(IN):: SIZE_INT
INTEGER(8),intent(INOUT):: size_read,size_allocated,size_written
INTEGER:: NbRecords,NbSubRecords
INTEGER:: SIZE_GEST_FDM_F
INTEGER(8):: SIZE_VARIABLES_FDM_F
INTEGER(4) :: I4
NbRecords=0
SIZE_GEST_FDM_F=0
SIZE_VARIABLES_FDM_F=0_8
SIZE_GEST=0
SIZE_VARIABLES=0_8
if((mode.EQ.memory_save_mode).OR.(mode.EQ.save_mode)) then
   call MUMPS_FDM_STRUC_TO_MOD("F",id_FDM_F_ENCODING)
endif
if(mode.EQ.memory_save_mode) then
   CALL MUMPS_SAVE_RESTORE_FDM_STRUC( &
  &        FDM_F &
  &        ,unit,MYID,memory_save_mode &
  &        ,SIZE_GEST_FDM_F &
  &        ,SIZE_VARIABLES_FDM_F &
  &        ,SIZE_INT,TOTAL_FILE_SIZE,TOTAL_STRUC_SIZE &
  &        ,size_read,size_allocated,size_written &
  &        ,INFO)
elseif(mode.EQ.save_mode) then
   CALL MUMPS_SAVE_RESTORE_FDM_STRUC( &
  &        FDM_F &
  &        ,unit,MYID,save_mode &
  &        ,SIZE_GEST_FDM_F &
  &        ,SIZE_VARIABLES_FDM_F &
  &        ,SIZE_INT,TOTAL_FILE_SIZE,TOTAL_STRUC_SIZE &
  &        ,size_read,size_allocated,size_written &
  &        ,INFO)
   IF ( INFO(1) .LT. 0 ) GOTO 100
elseif(mode.EQ.restore_mode) then
   CALL MUMPS_SAVE_RESTORE_FDM_STRUC( &
  &        FDM_F &
  &        ,unit,MYID,restore_mode &
  &        ,SIZE_GEST_FDM_F &
  &        ,SIZE_VARIABLES_FDM_F &
  &        ,SIZE_INT, TOTAL_FILE_SIZE,TOTAL_STRUC_SIZE &
  &        ,size_read,size_allocated,size_written &
  &        ,INFO)
   IF ( INFO(1) .LT. 0 ) GOTO 100
endif
if(mode.EQ.memory_save_mode) then
!     If the size to write (SIZE_VARIABLES) is greater than 2^31
!     Subrecords are created which need to be taken into account in
!     the file size computation
   NbSubRecords=int(SIZE_VARIABLES/huge(I4))
   IF(NbSubRecords.GT.0) then
      NbRecords=NbRecords+NbSubRecords
   ENDIF
elseif(mode.EQ.save_mode) then
   size_written=size_written+SIZE_VARIABLES &
  &        +int(SIZE_GEST,kind=8)
#if defined(MUMPS_NOF2003)
   size_written=size_written &
  &        +int(2*SIZE_INT*NbRecords,kind=8)
#endif
elseif(mode.EQ.restore_mode) then
   size_allocated=size_allocated+SIZE_VARIABLES
   size_read=size_read+SIZE_VARIABLES &
  &        +int(SIZE_GEST,kind=8)
#if defined(MUMPS_NOF2003)
   size_read=size_read &
  &        +int(2*SIZE_INT*NbRecords,kind=8)
#endif
endif
if(mode.EQ.memory_save_mode) then
   SIZE_VARIABLES=SIZE_VARIABLES+SIZE_VARIABLES_FDM_F
   SIZE_GEST=SIZE_GEST+SIZE_GEST_FDM_F
#if defined(MUMPS_NOF2003)
!        If the file is not written with access="stream"  (stream is
!        only in case of Fortran 2003), the record's length is written
!        at the beginning and at the end of each record.
!        This is done using 2 INTEGERs so we use 2*SIZE_INT more space
!        for each record
   SIZE_GEST=SIZE_GEST+2*SIZE_INT*NbRecords
#endif
endif
call MUMPS_FDM_MOD_TO_STRUC("F",id_FDM_F_ENCODING,INFO(1))
100 continue
RETURN
END SUBROUTINE MUMPS_SAVE_RESTORE_FRONT_DATA
!     ------------------ MUMPS_SAVE_RESTORE_BLR_STRUC ------------ !
SUBROUTINE MUMPS_SAVE_RESTORE_FDM_STRUC(FDM_STRUC &
  &     ,unit,MYID,mode &
  &     ,Local_SIZE_GEST,Local_SIZE_VARIABLES &
  &     ,SIZE_INT ,TOTAL_FILE_SIZE,TOTAL_STRUC_SIZE &
  &     ,size_read,size_allocated,size_written &
  &     ,INFO)
IMPLICIT NONE
!  =======
!  Purpose
!  =======
!
!     This routine is designed to manage a BLR_STRUC_T structure 
!     (save, restore, compute memory)
!     
!  ==========
!  Parameters
!  ==========
!
!     BLR_STRUC           : TYPE (BLR_STRUC_T) : the main structure
!
!     unit                : The unit of the file to be written or read
!
!     mode                : the type of operation to be performed by 
!                           the routine memory_save  = compute the 
!                           size of the save file and of the structure
!                           save         = save the instace
!                           restore      = restore the instace
!
!     TOTAL_FILE_SIZE     : size of the file to be written or read
!
!     TOTAL_STRUC_SIZE    : size of the structure to be saved or restored
!
!     SIZE_INT            : size of an integer      
!
!     INFO1/INFO2         : copies of of INFO(1) and INFO(2) to allow 
!                           save/restore of failled instaces
!
TYPE(FDM_STRUC_T) :: FDM_STRUC
INTEGER,intent(IN)::unit,MYID
INTEGER,intent(IN) :: mode
INTEGER,INTENT(OUT) :: Local_SIZE_GEST
INTEGER(8),intent(OUT) :: Local_SIZE_VARIABLES
INTEGER,intent(INOUT):: INFO(2)
INTEGER,intent(IN):: SIZE_INT
INTEGER(8),intent(IN) :: TOTAL_FILE_SIZE,TOTAL_STRUC_SIZE
INTEGER(8),intent(INOUT):: size_read,size_allocated,size_written
!     Done as for mumps_save_restore main structure but here by hand:
INTEGER, PARAMETER :: S_NB_FREE_IDX = 1
INTEGER, PARAMETER :: S_STACK_FREE_IDX = 2
INTEGER, PARAMETER :: S_COUNT_ACCESS = 3
INTEGER, PARAMETER :: NBVARIABLES_FDM_STRUC_T = 3
INTEGER(8),dimension(NBVARIABLES_FDM_STRUC_T):: &
  &     SIZE_VARIABLES_FDM_STRUC_T
INTEGER,dimension(NBVARIABLES_FDM_STRUC_T)::SIZE_GEST_FDM_STRUC_T
INTEGER,dimension(NBVARIABLES_FDM_STRUC_T)::NbRecords_FDM_STRUC_T
INTEGER:: size_array1,dummy,allocok
INTEGER:: err,i1,NbSubRecords
#if defined(MUMPS_NOF2003)
INTEGER:: Local_NbRecords
#endif
INTEGER(4) :: I4
SIZE_VARIABLES_FDM_STRUC_T(:)=0_8
SIZE_GEST_FDM_STRUC_T(:)=0
NbRecords_FDM_STRUC_T(:)=0
!     
!     BEGINNING OF THE MAIN LOOP ON ALL VARIABLES OF THE STRUCTURE
!
DO i1=1,NBVARIABLES_FDM_STRUC_T
   SELECT CASE(i1)
   CASE(S_NB_FREE_IDX)
      NbRecords_FDM_STRUC_T(i1)=1
      if(mode.EQ.memory_save_mode) then
         SIZE_VARIABLES_FDM_STRUC_T(i1)=SIZE_INT
      elseif(mode.EQ.save_mode) then
         SIZE_VARIABLES_FDM_STRUC_T(i1)=SIZE_INT
         write(unit,iostat=err) FDM_STRUC%NB_FREE_IDX
         if(err.ne.0) then
            INFO(1) = -72
            CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_written, &
  &                 INFO(2))
         endif
         IF ( INFO(1) .LT. 0 ) GOTO 100
      elseif(mode.EQ.restore_mode) then
         SIZE_VARIABLES_FDM_STRUC_T(i1)=SIZE_INT
         read(unit,iostat=err) FDM_STRUC%NB_FREE_IDX
         if(err.ne.0) THEN
            INFO(1) = -75
            CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_read &
  &                 ,INFO(2))
         endif
         IF ( INFO(1) .LT. 0 ) GOTO 100
      endif
   CASE(S_STACK_FREE_IDX)
      NbRecords_FDM_STRUC_T(i1)=2
      if(mode.EQ.memory_save_mode) then
         IF(associated(FDM_STRUC%STACK_FREE_IDX)) THEN
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT
            SIZE_VARIABLES_FDM_STRUC_T(i1)= &
  &                 size(FDM_STRUC%STACK_FREE_IDX,1)*SIZE_INT
         ELSE
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT*2
            SIZE_VARIABLES_FDM_STRUC_T(i1)=0_8
         ENDIF
      elseif(mode.EQ.save_mode) then
         IF(associated(FDM_STRUC%STACK_FREE_IDX)) THEN
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT
            SIZE_VARIABLES_FDM_STRUC_T(i1)= &
  &                 size(FDM_STRUC%STACK_FREE_IDX,1)*SIZE_INT
            write(unit,iostat=err) &
  &                 size(FDM_STRUC%STACK_FREE_IDX,1)
            if(err.ne.0) then
               INFO(1) = -72
               CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_written, &
  &                    INFO(2))
            endif
            IF ( INFO(1) .LT. 0 ) GOTO 100
            write(unit,iostat=err) FDM_STRUC%STACK_FREE_IDX
         ELSE
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT*2
            SIZE_VARIABLES_FDM_STRUC_T(i1)=0_8
            write(unit,iostat=err) -999
            if(err.ne.0) then
               INFO(1) = -72
               CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_written, &
  &                    INFO(2))
            endif
            IF ( INFO(1) .LT. 0 ) GOTO 100
            write(unit,iostat=err) -999
         ENDIF
         if(err.ne.0) then
            INFO(1) = -72
            CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_written, &
  &                 INFO(2))
         endif
         IF ( INFO(1) .LT. 0 ) GOTO 100
      elseif(mode.EQ.restore_mode) then
         nullify(FDM_STRUC%STACK_FREE_IDX)
         read(unit,iostat=err) size_array1
         if(err.ne.0) THEN
            INFO(1) = -75
            CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_read &
  &                 ,INFO(2))
         endif
         IF ( INFO(1) .LT. 0 ) GOTO 100
         if(size_array1.EQ.-999) then
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT*2
            SIZE_VARIABLES_FDM_STRUC_T(i1)=0_8
            read(unit,iostat=err) dummy
         else
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT
            SIZE_VARIABLES_FDM_STRUC_T(i1)=size_array1*SIZE_INT
            allocate(FDM_STRUC%STACK_FREE_IDX(size_array1), &
  &                 stat=allocok)
            if (allocok .GT. 0) THEN
               INFO(1) = -78
               CALL MUMPS_SETI8TOI4( &
  &                    TOTAL_STRUC_SIZE-size_allocated &
  &                    ,INFO(2))
            endif
            read(unit,iostat=err) FDM_STRUC%STACK_FREE_IDX
         endif
         IF ( INFO(1) .LT. 0 ) GOTO 100
         if(err.ne.0) THEN
            INFO(1) = -75
            CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_read &
  &                 ,INFO(2))
         endif
         IF ( INFO(1) .LT. 0 ) GOTO 100
      endif
   CASE(S_COUNT_ACCESS)
      NbRecords_FDM_STRUC_T(i1)=2
      if(mode.EQ.memory_save_mode) then
         IF(associated(FDM_STRUC%COUNT_ACCESS)) THEN
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT
            SIZE_VARIABLES_FDM_STRUC_T(i1)= &
  &                 size(FDM_STRUC%COUNT_ACCESS,1)*SIZE_INT
         ELSE
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT*2
            SIZE_VARIABLES_FDM_STRUC_T(i1)=0_8
         ENDIF
      elseif(mode.EQ.save_mode) then
         IF(associated(FDM_STRUC%COUNT_ACCESS)) THEN
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT
            SIZE_VARIABLES_FDM_STRUC_T(i1)= &
  &                 size(FDM_STRUC%COUNT_ACCESS,1)*SIZE_INT
            write(unit,iostat=err) &
  &                 size(FDM_STRUC%COUNT_ACCESS,1)
            if(err.ne.0) then
               INFO(1) = -72
               CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_written, &
  &                    INFO(2))
            endif
            IF ( INFO(1) .LT. 0 ) GOTO 100
            write(unit,iostat=err) FDM_STRUC%COUNT_ACCESS
         ELSE
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT*2
            SIZE_VARIABLES_FDM_STRUC_T(i1)=0_8
            write(unit,iostat=err) -999
            if(err.ne.0) then
               INFO(1) = -72
               CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_written, &
  &                    INFO(2))
            endif
            IF ( INFO(1) .LT. 0 ) GOTO 100
            write(unit,iostat=err) -999
         ENDIF
         if(err.ne.0) then
            INFO(1) = -72
            CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_written, &
  &                 INFO(2))
         endif
         IF ( INFO(1) .LT. 0 ) GOTO 100
      elseif(mode.EQ.restore_mode) then
         nullify(FDM_STRUC%COUNT_ACCESS)
         read(unit,iostat=err) size_array1
         if(err.ne.0) THEN
            INFO(1) = -75
            CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_read &
  &                 ,INFO(2))
         endif
         IF ( INFO(1) .LT. 0 ) GOTO 100
         if(size_array1.EQ.-999) then
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT*2
            SIZE_VARIABLES_FDM_STRUC_T(i1)=0_8
            read(unit,iostat=err) dummy
         else
            SIZE_GEST_FDM_STRUC_T(i1)=SIZE_INT
            SIZE_VARIABLES_FDM_STRUC_T(i1)=size_array1*SIZE_INT
            allocate(FDM_STRUC%COUNT_ACCESS(size_array1), &
  &                 stat=allocok)
            if (allocok .GT. 0) THEN
               INFO(1) = -78
               CALL MUMPS_SETI8TOI4( &
  &                    TOTAL_STRUC_SIZE-size_allocated &
  &                    ,INFO(2))
            endif
            read(unit,iostat=err) FDM_STRUC%COUNT_ACCESS
         endif
         IF ( INFO(1) .LT. 0 ) GOTO 100
         if(err.ne.0) THEN
            INFO(1) = -75
            CALL MUMPS_SETI8TOI4(TOTAL_FILE_SIZE-size_read &
  &                 ,INFO(2))
         endif
         IF ( INFO(1) .LT. 0 ) GOTO 100
      endif
   CASE DEFAULT
   END SELECT
   if(mode.EQ.memory_save_mode) then
!     If the size to write (SIZE_VARIABLES_FDM_STRUC_T(i1)) is greater than 2^31
!     Subrecords are created which need to be taken into account in
!     the file size computation
      NbSubRecords=int(SIZE_VARIABLES_FDM_STRUC_T(i1)/huge(I4))
      IF(NbSubRecords.GT.0) then
         NbRecords_FDM_STRUC_T(i1)=NbRecords_FDM_STRUC_T(i1) &
  &              +NbSubRecords
      ENDIF
   elseif(mode.EQ.save_mode) then
      size_written=size_written+SIZE_VARIABLES_FDM_STRUC_T(i1) &
  &           +int(SIZE_GEST_FDM_STRUC_T(i1),kind=8)
#if defined(MUMPS_NOF2003)
      size_written=size_written &
  &           +int(2*SIZE_INT*NbRecords_FDM_STRUC_T(i1),kind=8)
#endif
   elseif(mode.EQ.restore_mode) then
      size_allocated=size_allocated+ &
  &           SIZE_VARIABLES_FDM_STRUC_T(i1)
      size_read=size_read+SIZE_VARIABLES_FDM_STRUC_T(i1) &
  &           +int(SIZE_GEST_FDM_STRUC_T(i1),kind=8)
#if defined(MUMPS_NOF2003)
      size_read=size_read &
  &           +int(2*SIZE_INT*NbRecords_FDM_STRUC_T(i1),kind=8)
#endif
   endif
ENDDO
if(mode.EQ.memory_save_mode) then
   Local_SIZE_VARIABLES=sum(SIZE_VARIABLES_FDM_STRUC_T)
   Local_SIZE_GEST=sum(SIZE_GEST_FDM_STRUC_T)
#if defined(MUMPS_NOF2003)
   Local_NbRecords=sum(NbRecords_FDM_STRUC_T)
   Local_SIZE_GEST=Local_SIZE_GEST+2*SIZE_INT*Local_NbRecords
#endif
endif
100 continue
RETURN
END SUBROUTINE MUMPS_SAVE_RESTORE_FDM_STRUC
#endif       
END MODULE MUMPS_FRONT_DATA_MGT_M
