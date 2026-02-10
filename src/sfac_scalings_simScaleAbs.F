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
SUBROUTINE SMUMPS_SIMSCALEABS(IRN_loc, JCN_loc, A_loc, NZ_loc, &
  &     M, N, NUMPROCS, MYID, COMM, &
  &     RPARTVEC, CPARTVEC, &
  &     RSNDRCVSZ, CSNDRCVSZ, REGISTRE, &
  &     IWRK, IWRKSZ, &
  &     INTSZ, RESZ, OP, &
  &     ROWSCA, COLSCA, WRKRC, ISZWRKRC, &
  &     WRKR_TH, LWRKR_TH, &
  &     WRKC_TH, LWRKC_TH, &
  &     NOMP_MAX, SYM, NB1, NB2, NB3, EPS, &
  &     ONENORMERR,INFNORMERR)
!----------------------------------------------------------------------
!    IF SYM=0 CALLs unsymmetric variant SMUMPS_SIMSCALEABSUNS.
!    IF SYM=2 CALLS symmetric variant where only one of a_ij and a_ji 
!         is stored. SMUMPS_SIMSCALEABSSYM
!---------------------------------------------------------------------
!    For details, see the two subroutines below
!         SMUMPS_SIMSCALEABSUNS and SMUMPS_SIMSCALEABSSYM
! ---------------------------------------------------------------------
!
!$    USE OMP_LIB
IMPLICIT NONE
INCLUDE 'mpif.h'
INTEGER(8) NZ_loc
INTEGER(8) :: IWRKSZ
INTEGER(8) :: ISZWRKRC, LWRKR_TH, LWRKC_TH
INTEGER    :: NOMP_MAX
INTEGER M, N, OP
INTEGER NUMPROCS, MYID, COMM
INTEGER(8) :: INTSZ, RESZ
INTEGER IRN_loc(NZ_loc)
INTEGER JCN_loc(NZ_loc)
REAL A_loc(NZ_loc)
INTEGER RPARTVEC(M)
INTEGER RSNDRCVSZ(2*NUMPROCS)
INTEGER CPARTVEC(N)
INTEGER CSNDRCVSZ(2*NUMPROCS)
INTEGER IWRK(IWRKSZ)
INTEGER(8) :: REGISTRE(12)
REAL ROWSCA(M)
REAL COLSCA(N)
REAL WRKRC(ISZWRKRC)
REAL WRKR_TH(LWRKR_TH,max(NOMP_MAX,1))
REAL WRKC_TH(LWRKC_TH,max(NOMP_MAX,1))
REAL ONENORMERR,INFNORMERR
!     LOCALS
!     FOR the scaling phase
INTEGER SYM, NB1, NB2, NB3
REAL EPS
!     EXTERNALS
EXTERNAL SMUMPS_SIMSCALEABSUNS,SMUMPS_SIMSCALEABSSYM, &
  &         SMUMPS_INITREAL
!     MUST HAVE IT
INTEGER I
INTEGER, PARAMETER :: K361 = 2048
!$    INTEGER    :: NOMP 
!$    INTEGER    :: CHUNK
!$    IF (NOMP_MAX.GT.0) THEN
!$     NOMP  = omp_get_max_threads()
!$     CHUNK= max(K361/2, (N+NOMP-1) / NOMP )
!$    ENDIF
IF(SYM.EQ.0) THEN
   CALL SMUMPS_SIMSCALEABSUNS(IRN_loc, JCN_loc, A_loc, &
  &        NZ_loc, &
  &        M, N, NUMPROCS, MYID, COMM, &
  &        RPARTVEC, CPARTVEC, &
  &        RSNDRCVSZ, CSNDRCVSZ, REGISTRE, &
  &        IWRK, IWRKSZ, &
  &        INTSZ, RESZ, OP, &
  &        ROWSCA, COLSCA, WRKRC, ISZWRKRC, &
  &        WRKR_TH, LWRKR_TH, &
  &        WRKC_TH, LWRKC_TH, NOMP_MAX, &
  &        NB1, NB2, NB3, EPS, &
  &        ONENORMERR, INFNORMERR)
ELSE
   CALL SMUMPS_SIMSCALEABSSYM(IRN_loc, JCN_loc, A_loc, &
  &        NZ_loc, &
  &        N, NUMPROCS, MYID, COMM, &
  &        RPARTVEC, &
  &        RSNDRCVSZ, REGISTRE, &
  &        IWRK, IWRKSZ, &
  &        INTSZ, RESZ, OP, &
  &        ROWSCA, WRKRC, ISZWRKRC, &
  &        WRKR_TH, LWRKR_TH, NOMP_MAX, &
  &        NB1, NB2, NB3, EPS, &
  &        ONENORMERR, INFNORMERR)
   IF (OP.EQ.2) THEN
    IF (NOMP_MAX.LE.0) THEN
      DO I=1,N
       COLSCA(I) = ROWSCA(I)
      ENDDO
    ELSE
!$OMP      PARALLEL DO PRIVATE(I)
!$OMP&     SCHEDULE(STATIC,CHUNK)
!$OMP&     IF ( N > K361 .AND. NOMP .GT. 1)
      DO I=1,N
       COLSCA(I) = ROWSCA(I)
      ENDDO
    ENDIF
   ENDIF
ENDIF
RETURN
END SUBROUTINE SMUMPS_SIMSCALEABS
SUBROUTINE SMUMPS_SIMSCALEABSUNS(IRN_loc, JCN_loc, A_loc, NZ_loc, &
  &     M, N, NUMPROCS, MYID, COMM, &
  &     RPARTVEC, CPARTVEC, &
  &     RSNDRCVSZ, CSNDRCVSZ, REGISTRE, &
  &     IWRK, IWRKSZ, &
  &     INTSZ, RESZ, OP, &
  &     ROWSCA, COLSCA, WRKRC, ISZWRKRC, &
  &     WRKR_TH, LWRKR_TH, &
  &     WRKC_TH, LWRKC_TH, NOMP_MAX, &
  &     NB1, NB2, NB3, EPS, &
  &     ONENORMERR, INFNORMERR)
!----------------------------------------------------------------------
! Input parameters:
!      M, N: size of matrix (in general M=N, but the algorithm 
!            works for rectangular matrices as well (norms other than
!            inf-norm are not possible mathematically in this case).
!      NUMPROCS, MYID, COMM: guess what are those
!      RPARTVEC:  row partvec to be filled when OP=1
!      CPARTVEC:  col partvec to be filled when OP=1
!      RSNDRCVSZ: send recv sizes for row operations. 
!                 to be filled when OP=1
!      CSNDRCVSZ: send recv sizes for col operations. 
!                 to be filled when OP=1
!      REGISTRE:  to store some pointers (size etc)
!      IWRK: working space. when OP=1 IWRKSZ.GE.4*MAXMN
!            when OP=2 INTSZ portion is used. Thus, IWRKSZ>INTSZ 
!            when OP=2
!      IWRKSZ: size
!      INTSZ: to be computed when OP=1, necessary integer space to run 
!             scaling algo when OP=2
!      RESZ:  to be computed when OP=1, necessary real space to run 
!             scaling algo when OP=2
!      OP: 
!          =1 estimation of memory and construction of partvecs
!           writes into RPARTVEC,CPARTVEC,RSNDRCVSZ,CSNDRCVSZ,REGISTRE
!           does not access WRKRC, uses IWRK as workspace
!           computes INTSZ and RESZ.
!          =2 Compute scalings 
!           restores pointers from REGISTRE, 
!           stores communication structure in IWRK (from the start). 
!
!      ROWSCA: space for row scaling factor; has size M
!      COLSCA: space for col scaling factor; has size N
!      WRKRC: real working space. when OP=1, is not accessed. Thus, it
!             can be declared to be of size 1 at OP=1 call.
!      ISZWRKRC: size
!      SYM: is matrix symmetric
!      NB1, NB2, NB3: algo runs 
!                     NB1 iters of inf-norm (default  1/1), 
!                     NB2 iters of 1-norm   (default  3/10),
!                     NB3 iters of inf-norm (default  3/10).
!          in succession.
!      EPS: tolerance for concergence. 
!           IF EPS < 0.R0 then does not test convergence.
!           If convergence occured during the first set of inf-norm
!           iterations, we start performing one-norm iterations.
!           If convergence occured during the one-norm iterations,
!           we start performing the second set of inf-norm iterations.
!           If convergence occured during the second set of inf-norm,
!           we prepare to return.
!     ONENORMERR : error in one norm scaling (associated with the scaling 
!                  arrays of the previous iterations), 
!     INFNORMERR : error in inf norm scaling (associated with the scaling 
!                  arrays of the previous iterations).
!---------------------------------------------------------------------
! On input:
!      OP=1==>Requirements
!             IWRKSZ.GE.4*MAXMN
!             RPARTVEC  of size M
!             CPARTVEC  of size N
!             RSNDRCVSZ of size 2*NUMPROCS
!             CSNDRCVSZ of size 2*NUMPROCS
!             REGISTRE  of size 12
!             
!      OP=2==>Requirements
!             INTSZ .GE. REGISTRE(11)
!             RESZ  .GE. REGISTRE(12) 
!---------------------------------------------------------------------
! On output:
!     ROWSCA and COLSCA 
!            at processor 0 of COMM: complete factors.
!            at other processors   : only the ROWSCA(i) or COLSCA(j) 
!            for which there is a nonzero a_i* or a_*j are useful.
!     ONENORMERR : error in one norm scaling 
!                = -1.0 if iter2=0.
!     INFNORMERR : error in inf norm scaling 
!                = inf norm error at iter3 if iter3 > 0
!                = inf norm error at iter1 if iter1 > 0, iter3=0
!                = -1.0 if iter1=iter3=0
! ---------------------------------------------------------------------
! References:
!     The scaling algorithms are based on those discussed in
!     [1] D. Ruiz, "A scaling algorithm to equilibrate both rows and 
!         columns norms in matrices", Tech. Rep. Rutherford 
!         Appleton Laboratory, Oxon, UK and ENSEEIHT-IRIT, 
!         Toulouse, France, RAL-TR-2001-034 and RT/APO/01/4, 2001.
!     [2] D. Ruiz and B. Ucar, "A symmetry preserving algorithm for
!         matrix scaling", in preparation as of Jan'08.
!
!     The parallelization approach is discussed in
!     [3] P. R. Amestoy, I. S. Duff, D. Ruiz, and B. Ucar,
!         "A parallel matrix scaling algorithm".
!         In proceedings of VECPAR'08-International Meeting-High 
!         Performance Computing for Computational Science, Jan'08.
!     and was supported by ANR-SOLSTICE project (ANR-06-CIS6-010)
! ---------------------------------------------------------------------
!$    USE OMP_LIB
IMPLICIT NONE
INCLUDE 'mpif.h'
INTEGER(8) :: NZ_loc
INTEGER(8) :: IWRKSZ, INTSZ
INTEGER(8) :: ISZWRKRC, LWRKR_TH, LWRKC_TH
INTEGER    :: M, N, OP
INTEGER    :: NUMPROCS, MYID, COMM, NOMP_MAX
INTEGER(8) :: RESZ
INTEGER IRN_loc(NZ_loc)
INTEGER JCN_loc(NZ_loc)
REAL A_loc(NZ_loc)
INTEGER RPARTVEC(M)
INTEGER CPARTVEC(N)
INTEGER RSNDRCVSZ(2*NUMPROCS)
INTEGER CSNDRCVSZ(2*NUMPROCS)
INTEGER(8) :: REGISTRE(12)
INTEGER IWRK(IWRKSZ)
REAL ROWSCA(M)
REAL COLSCA(N)
REAL WRKRC(ISZWRKRC)
REAL WRKR_TH(LWRKR_TH,max(NOMP_MAX,1))
REAL WRKC_TH(LWRKC_TH,max(NOMP_MAX,1))
REAL ONENORMERR,INFNORMERR
!     LOCALS
INTEGER IRSNDRCVNUM, ORSNDRCVNUM
INTEGER ICSNDRCVNUM, OCSNDRCVNUM
INTEGER IRSNDRCVVOL, ORSNDRCVVOL
INTEGER ICSNDRCVVOL, OCSNDRCVVOL
INTEGER  INUMMYR, INUMMYC
! IMPORTANT POINTERS
INTEGER(8) :: IMYRPTR,IMYCPTR
INTEGER(8) :: IRNGHBPRCS, IRSNDRCVIA,IRSNDRCVJA
INTEGER(8) :: ORNGHBPRCS, ORSNDRCVIA,ORSNDRCVJA
INTEGER(8) :: ICNGHBPRCS, ICSNDRCVIA,ICSNDRCVJA
INTEGER(8) :: OCNGHBPRCS, OCSNDRCVIA,OCSNDRCVJA
INTEGER(8) :: ISTATUS, REQUESTS, TMPWORK
INTEGER(8) :: ITDRPTR, ITDCPTR, ISRRPTR
INTEGER(8) :: OSRRPTR, ISRCPTR, OSRCPTR
!     FOR the scaling phase
INTEGER NB1, NB2, NB3
REAL EPS
!     Iteration vars 
INTEGER ITER, IR, IC
INTEGER(8) :: NZIND
REAL ELM
!     COMM TAGS....
INTEGER TAG_COMM_COL
PARAMETER(TAG_COMM_COL=100)
INTEGER TAG_COMM_ROW
PARAMETER(TAG_COMM_ROW=101)
INTEGER TAG_ITERS
PARAMETER(TAG_ITERS=102)
!     FUNCTIONS
EXTERNAL SMUMPS_CREATEPARTVEC, &
  &     SMUMPS_NUMVOLSNDRCV, &
  &     SMUMPS_SETUPCOMMS, &
  &     SMUMPS_FILLMYROWCOLINDICES, &
  &     SMUMPS_INITREAL, &
  &     SMUMPS_INITREALLST, &
  &     SMUMPS_DOCOMMINF, &
  &     SMUMPS_DOCOMM1N
REAL SMUMPS_ERRSCALOC
REAL SMUMPS_ERRSCA1
INTRINSIC abs
REAL RONE, RZERO
PARAMETER(RONE=1.0E0,RZERO=0.0E0)
!     TMP VARS
INTEGER(8) :: RESZR, RESZC
INTEGER(8) :: INTSZR, INTSZC
INTEGER MAXMN
INTEGER I, IERROR, IOMP
REAL ONEERRROW, ONEERRCOL, ONEERRL, ONEERRG
REAL INFERRROW, INFERRCOL, INFERRL, INFERRG
LOGICAL OORANGEIND
INTEGER, PARAMETER :: K361 = 2048
!$    INTEGER    :: NOMP
!$    INTEGER    :: CHUNK, CHUNK_NZ
!$    ! Too large => pb with cache L3 ?
!$    ! INTEGER(8) :: CHUNK8 
!$    IF (NOMP_MAX.GT.0) THEN
!$     NOMP     = omp_get_max_threads()
!$     CHUNK    = max(K361/2, (N+NOMP-1) / NOMP )
!$    ! CHUNK8= (NZ_loc+int(NOMP-1,8) / int(NOMP,8) ) )
!$     CHUNK_NZ = max(K361/2, (N+NOMP_MAX-1) / NOMP_MAX )
!$    ENDIF
!
OORANGEIND = .FALSE.
INFERRG = -RONE
ONEERRG = -RONE
MAXMN = M
IF(MAXMN < N) MAXMN = N
!     Create row partvec and col partvec
IF(OP == 1) THEN
   IF(NUMPROCS > 1) THEN
!     Check done outside
!     IF(IWRKSZ.LT.4*MAXMN) THEN   ERROR.... 
      CALL SMUMPS_CREATEPARTVEC(MYID, NUMPROCS, COMM, &
  &           IRN_loc, JCN_loc, NZ_loc, &
  &           RPARTVEC, M, N, &
  &           IWRK, IWRKSZ, INUMMYR, NOMP_MAX)
      CALL SMUMPS_CREATEPARTVEC(MYID, NUMPROCS, COMM, &
  &           JCN_loc, IRN_loc,  NZ_loc, &
  &           CPARTVEC, N, M, &
  &           IWRK, IWRKSZ, INUMMYC, NOMP_MAX)
!     Compute sndrcv sizes, store them for later use           
      CALL SMUMPS_NUMVOLSNDRCV(MYID, NUMPROCS, M, RPARTVEC, &
  &           NZ_loc, IRN_loc, N, JCN_loc, &
  &           IRSNDRCVNUM,IRSNDRCVVOL, &
  &           ORSNDRCVNUM,ORSNDRCVVOL, &
  &           IWRK,IWRKSZ, &
  &           RSNDRCVSZ(1), RSNDRCVSZ(1+NUMPROCS), COMM)
      CALL SMUMPS_NUMVOLSNDRCV(MYID, NUMPROCS, N, CPARTVEC, &
  &           NZ_loc, JCN_loc, M, IRN_loc, &
  &           ICSNDRCVNUM,ICSNDRCVVOL, &
  &           OCSNDRCVNUM,OCSNDRCVVOL, &
  &           IWRK,IWRKSZ, &
  &           CSNDRCVSZ(1), CSNDRCVSZ(1+NUMPROCS), COMM)
      INTSZR =  int(IRSNDRCVNUM,8) + int(ORSNDRCVNUM,8) + &
  &           int(IRSNDRCVVOL,8) + int(ORSNDRCVVOL,8) + &
  &           2_8*int(NUMPROCS+1,8) + int(INUMMYR,8)
      INTSZC = int(ICSNDRCVNUM,8) + int(OCSNDRCVNUM,8) + &
  &           int(ICSNDRCVVOL,8) + int(OCSNDRCVVOL,8) + &
  &           2_8*int(NUMPROCS+1,8) + int(INUMMYC,8)
      INTSZ = INTSZR + INTSZC + int(MAXMN,8) + &
  &           int(MPI_STATUS_SIZE +1,8) * int(NUMPROCS,8)
   ELSE
!     NUMPROCS IS 1
       IRSNDRCVNUM = 0
       ORSNDRCVNUM = 0
       IRSNDRCVVOL = 0
       ORSNDRCVVOL = 0
       INUMMYR = 0
       ICSNDRCVNUM  = 0
       OCSNDRCVNUM  = 0
       ICSNDRCVVOL = 0
       OCSNDRCVVOL  = 0
       INUMMYC = 0
       INTSZ = 0_8
    ENDIF
!     CALCULATE NECESSARY REAL SPACE
    RESZR = int(M,8) + int(IRSNDRCVVOL,8) + int(ORSNDRCVVOL,8)
    RESZC = int(N,8) + int(ICSNDRCVVOL,8) + int(OCSNDRCVVOL,8)
    RESZ = RESZR  + RESZC
!     CALCULATE NECESSARY INT SPACE
!     The last maxmn is tmpwork for setup comm and fillmyrowcol
    REGISTRE(1)  = int(IRSNDRCVNUM,8)
    REGISTRE(2)  = int(ORSNDRCVNUM,8)
    REGISTRE(3)  = int(IRSNDRCVVOL,8)
    REGISTRE(4)  = int(ORSNDRCVVOL,8)
    REGISTRE(5)  = int(ICSNDRCVNUM,8)
    REGISTRE(6)  = int(OCSNDRCVNUM,8)
    REGISTRE(7)  = int(ICSNDRCVVOL,8)
    REGISTRE(8)  = int(OCSNDRCVVOL,8)
    REGISTRE(9)  = int(INUMMYR,8)
    REGISTRE(10) = int(INUMMYC,8)
    REGISTRE(11) = INTSZ
    REGISTRE(12) = RESZ
 ELSE
!     else of op=1. That is op=2 now.
!     restore the numbers
    IRSNDRCVNUM = int(REGISTRE(1))
    ORSNDRCVNUM = int(REGISTRE(2))
    IRSNDRCVVOL = int(REGISTRE(3))
    ORSNDRCVVOL = int(REGISTRE(4))
    ICSNDRCVNUM = int(REGISTRE(5))
    OCSNDRCVNUM = int(REGISTRE(6))
    ICSNDRCVVOL = int(REGISTRE(7))
    OCSNDRCVVOL = int(REGISTRE(8))
    INUMMYR     = int(REGISTRE(9))
    INUMMYC     = int(REGISTRE(10))
    IF(NUMPROCS > 1) THEN
!     Check done outsize
!             IF(INTSZ < REGISTRE(11)) THEN ERROR
!             IF(RESZ < REGISTRE(12)) THEN ERROR
!     Fill up myrows and my colsX
       CALL SMUMPS_FILLMYROWCOLINDICES(MYID, NUMPROCS,COMM, &
  &            IRN_loc, JCN_loc, NZ_loc, &
  &            RPARTVEC, CPARTVEC, M, N, &
  &            IWRK(1_8), INUMMYR, &
  &            IWRK(1_8+int(INUMMYR,8)), INUMMYC, &
  &            IWRK(1_8+int(INUMMYR,8)+int(INUMMYC,8)), &
  &            IWRKSZ-int(INUMMYR,8)-int(INUMMYC,8), NOMP_MAX )
       IMYRPTR = 1_8
       IMYCPTR = IMYRPTR + int(INUMMYR,8)
!     Set up comm and run.
!     set pointers in iwrk (4 parts)
!     
!     ROWS    [---------------------------------------------]
       IRNGHBPRCS = IMYCPTR    + int(INUMMYC    ,8)
       IRSNDRCVIA = IRNGHBPRCS + int(IRSNDRCVNUM,8)
       IRSNDRCVJA = IRSNDRCVIA + int(NUMPROCS+1 ,8)
       ORNGHBPRCS = IRSNDRCVJA + int(IRSNDRCVVOL,8)
       ORSNDRCVIA = ORNGHBPRCS + int(ORSNDRCVNUM,8)
       ORSNDRCVJA = ORSNDRCVIA + int(NUMPROCS+1 ,8)
!     COLS    [---------------------------------------------]
       ICNGHBPRCS = ORSNDRCVJA + int(ORSNDRCVVOL,8)
       ICSNDRCVIA = ICNGHBPRCS + int(ICSNDRCVNUM,8)
       ICSNDRCVJA = ICSNDRCVIA + int(NUMPROCS+1 ,8)
       OCNGHBPRCS = ICSNDRCVJA + int(ICSNDRCVVOL,8)
       OCSNDRCVIA = OCNGHBPRCS + int(OCSNDRCVNUM,8)
       OCSNDRCVJA = OCSNDRCVIA + int(NUMPROCS+1 ,8)
!     
!     MPI     [-----------------]
       REQUESTS = OCSNDRCVJA + int(OCSNDRCVVOL,8)
       ISTATUS = REQUESTS + int(NUMPROCS,8)
!     
!     TMPWRK  [-----------------]
       TMPWORK = ISTATUS + int(MPI_STATUS_SIZE,8)*int(NUMPROCS,8)
       CALL SMUMPS_SETUPCOMMS(MYID, NUMPROCS, M, RPARTVEC, &
  &            NZ_loc, IRN_loc,N, JCN_loc, &
  &            IRSNDRCVNUM, IRSNDRCVVOL, &
  &            IWRK(IRNGHBPRCS),IWRK(IRSNDRCVIA),IWRK(IRSNDRCVJA), &
  &            ORSNDRCVNUM, ORSNDRCVVOL, &
  &            IWRK(ORNGHBPRCS),IWRK(ORSNDRCVIA),IWRK(ORSNDRCVJA), &
  &            RSNDRCVSZ(1), RSNDRCVSZ(1+NUMPROCS), &
  &            IWRK(TMPWORK), &
  &            IWRK(ISTATUS), IWRK(REQUESTS), &
  &            TAG_COMM_ROW, COMM)
       CALL SMUMPS_SETUPCOMMS(MYID, NUMPROCS, N, CPARTVEC, &
  &            NZ_loc, JCN_loc, M, IRN_loc, &
  &            ICSNDRCVNUM, ICSNDRCVVOL, &
  &            IWRK(ICNGHBPRCS), &
  &            IWRK(ICSNDRCVIA), &
  &            IWRK(ICSNDRCVJA), &
  &            OCSNDRCVNUM, OCSNDRCVVOL, &
  &            IWRK(OCNGHBPRCS),IWRK(OCSNDRCVIA),IWRK(OCSNDRCVJA), &
  &            CSNDRCVSZ(1), CSNDRCVSZ(1+NUMPROCS), &
  &            IWRK(TMPWORK), &
  &            IWRK(ISTATUS),  IWRK(REQUESTS), &
  &            TAG_COMM_COL, COMM)
       CALL SMUMPS_INITREAL(ROWSCA, M, RZERO, NOMP_MAX)
       CALL SMUMPS_INITREAL(COLSCA, N, RZERO, NOMP_MAX)
       CALL SMUMPS_INITREALLST(ROWSCA, M, &
  &            IWRK(IMYRPTR),INUMMYR, RONE, NOMP_MAX)
       CALL SMUMPS_INITREALLST(COLSCA, N, &
  &            IWRK(IMYCPTR),INUMMYC, RONE, NOMP_MAX)
    ELSE
       CALL SMUMPS_INITREAL(ROWSCA, M, RONE, NOMP_MAX)
       CALL SMUMPS_INITREAL(COLSCA, N, RONE, NOMP_MAX)
    ENDIF
    ITDRPTR = 1_8
    ITDCPTR = ITDRPTR + int(M,8)
!     
    ISRRPTR = ITDCPTR + int(N,8)
    OSRRPTR = ISRRPTR + int(IRSNDRCVVOL,8)
!     
    ISRCPTR = OSRRPTR + int(ORSNDRCVVOL,8)
    OSRCPTR = ISRCPTR + int(ICSNDRCVVOL,8)
!     To avoid bound check errors...
    IF(NUMPROCS == 1)THEN
       OSRCPTR = OSRCPTR - 1_8
       ISRCPTR = ISRCPTR - 1_8
       OSRRPTR = OSRRPTR - 1_8
       ISRRPTR = ISRRPTR - 1_8
    ELSE
       IF(IRSNDRCVVOL == 0) ISRRPTR = ISRRPTR - 1_8
       IF(ORSNDRCVVOL == 0) OSRRPTR = OSRRPTR - 1_8
       IF(ICSNDRCVVOL == 0) ISRCPTR = ISRCPTR - 1_8
       IF(OCSNDRCVVOL == 0) OSRCPTR = OSRCPTR - 1_8
    ENDIF
    ITER = 1
    DO WHILE (ITER.LE.NB1+NB2+NB3)
!{
!           -------------------------
!           CLEAR temporary Dr and Dc
!           -------------------------
      IF (NOMP_MAX.GT.1 .AND. &
  &        (ITER.GT.NB1 .AND.ITER.LE.NB1+NB2) &
  &      ) THEN
!{
!$OMP         PARALLEL
!$OMP&         PRIVATE(IOMP)
!$OMP&         NUM_THREADS(NOMP_MAX)
         IOMP  = 1
!$             IOMP = OMP_GET_THREAD_NUM() + 1
         IF(NUMPROCS > 1) THEN
           CALL SMUMPS_ZEROOUT(WRKR_TH(1,IOMP),N, &
  &                 IWRK(IMYRPTR),INUMMYR, 0)
           CALL SMUMPS_ZEROOUT(WRKC_TH(1,IOMP),N, &
  &                 IWRK(IMYCPTR),INUMMYC, 0)
         ELSE
           CALL SMUMPS_INITREAL(WRKR_TH(1,IOMP),N, RZERO, &
  &                                0)
           CALL SMUMPS_INITREAL(WRKC_TH(1,IOMP),N, RZERO, &
  &                                0)
         ENDIF
!$OMP         END PARALLEL
!}
      ELSE
!{
         IF(NUMPROCS > 1) THEN
            CALL SMUMPS_ZEROOUT(WRKRC(ITDRPTR),M, &
  &                 IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
            CALL SMUMPS_ZEROOUT(WRKRC(ITDCPTR),N, &
  &                 IWRK(IMYCPTR),INUMMYC, NOMP_MAX)
         ELSE
            CALL SMUMPS_INITREAL(WRKRC(ITDRPTR),M, RZERO, &
  &                                NOMP_MAX)
            CALL SMUMPS_INITREAL(WRKRC(ITDCPTR),N, RZERO, &
  &                                NOMP_MAX)
         ENDIF
!}
       ENDIF
       IF((ITER.LE.NB1).OR.(ITER > NB1+NB2)) THEN
!     ------------------
!     INF-NORM ITERATION
!     ------------------
         IF (NOMP_MAX.LE.0) THEN
          IF((ITER.EQ.1).OR.(OORANGEIND)) THEN
             DO NZIND=1_8,NZ_loc
                IR = IRN_loc(NZIND)
                IC = JCN_loc(NZIND)
                IF((IR.GE.1).AND.(IR.LE.M).AND. &
  &                     (IC.GE.1).AND.(IC.LE.N)) THEN
                   ELM = abs(A_loc(NZIND))*ROWSCA(IR)*COLSCA(IC)
                   IF(WRKRC(IR)<ELM) WRKRC(IR)= ELM
                   IF(WRKRC(ITDCPTR-1_8+int(IC,8))<ELM) &
  &                      WRKRC(ITDCPTR-1_8+int(IC,8))= ELM
                ELSE
                   OORANGEIND = .TRUE.
                ENDIF
             ENDDO
          ELSEIF(.NOT.OORANGEIND) THEN
             DO NZIND=1,NZ_loc
                IR = IRN_loc(NZIND)
                IC = JCN_loc(NZIND)
                ELM = abs(A_loc(NZIND))*ROWSCA(IR)*COLSCA(IC)
                IF(WRKRC(IR)<ELM) WRKRC(IR)= ELM
                IF(WRKRC(ITDCPTR-1_8+int(IC,8))<ELM) &
  &                   WRKRC(ITDCPTR-1_8+int(IC,8))= ELM
             ENDDO
          ENDIF
         ELSE
          IF((ITER.EQ.1).OR.(OORANGEIND)) THEN
!$OMP   PARALLEL DO PRIVATE(NZIND,IR,IC,ELM) 
!$OMP&  SCHEDULE(STATIC,CHUNK_NZ)
!$OMP&  IF ( NZ_loc > int(K361,8) .AND. NOMP .GT. 1)
!$OMP&  REDUCTION(.OR.:OORANGEIND)
             DO NZIND=1_8,NZ_loc
                IR = IRN_loc(NZIND)
                IC = JCN_loc(NZIND)
                IF((IR.GE.1).AND.(IR.LE.M).AND. &
  &                     (IC.GE.1).AND.(IC.LE.N)) THEN
                   ELM = abs(A_loc(NZIND))*ROWSCA(IR)*COLSCA(IC)
!$OMP                    ATOMIC UPDATE
                   WRKRC(IR)= max (ELM, WRKRC(IR))
!$OMP                    END ATOMIC
!                         IF(WRKRC(ITDRPTR-1_8+int(IR,8))<ELM) THEN
!                            WRKRC(ITDRPTR-1_8+int(IR,8))= ELM
!                         ENDIF
!                        IF(WRKRC(ITDCPTR-1_8+int(IC,8))<ELM) THEN
!                           WRKRC(ITDCPTR-1_8+int(IC,8))= ELM
!                         ENDIF
!                        ITDCPTR+IC might be > int4
!$OMP                    ATOMIC UPDATE
                   WRKRC(ITDCPTR-1_8+int(IC,8)) = &
  &                   max (ELM,WRKRC(ITDCPTR-1_8+int(IC,8)))
!$OMP                    END ATOMIC
                ELSE
                   OORANGEIND = .TRUE.
                ENDIF
             ENDDO
!$OMP   END PARALLEL DO
          ELSEIF(.NOT.OORANGEIND) THEN
!$OMP   PARALLEL DO PRIVATE(NZIND,IR,IC,ELM) 
!$OMP&  SCHEDULE(STATIC,CHUNK_NZ)
!$OMP&  IF ( NZ_loc > int(K361,8) .AND. NOMP .GT. 1)
             DO NZIND=1,NZ_loc
                IR = IRN_loc(NZIND)
                IC = JCN_loc(NZIND)
                ELM = abs(A_loc(NZIND))*ROWSCA(IR)*COLSCA(IC)
!                      IF(WRKRC(ITDRPTR-1_8+int(IR,8))<ELM) THEN
!                         WRKRC(ITDRPTR-1_8+int(IR,8))= ELM
!                      ENDIF
!$OMP                 ATOMIC UPDATE
                WRKRC(IR)= max (ELM, WRKRC(IR))
!$OMP                 END ATOMIC
!                      IF(WRKRC(ITDCPTR-1_8+int(IC,8))<ELM) THEN
!                         WRKRC(ITDCPTR-1_8+int(IC,8))= ELM
!                      ENDIF
!$OMP                 ATOMIC UPDATE
                WRKRC(ITDCPTR-1_8+int(IC,8)) = &
  &                   max (ELM,WRKRC(ITDCPTR-1_8+int(IC,8)))
!$OMP                 END ATOMIC
             ENDDO
!$OMP   END PARALLEL DO
          ENDIF
!}
         ENDIF
          IF(NUMPROCS > 1) THEN
             CALL SMUMPS_DOCOMMINF(MYID, NUMPROCS, &
  &                  WRKRC(ITDCPTR), N, TAG_ITERS+ITER, &
  &                  ICSNDRCVNUM,IWRK(ICNGHBPRCS), &
  &                  ICSNDRCVVOL,IWRK(ICSNDRCVIA), IWRK(ICSNDRCVJA), &
  &                  WRKRC(ISRCPTR), &
  &                  OCSNDRCVNUM,IWRK(OCNGHBPRCS), &
  &                  OCSNDRCVVOL,IWRK(OCSNDRCVIA), IWRK(OCSNDRCVJA), &
  &                  WRKRC( OSRCPTR), &
  &                  IWRK(ISTATUS),IWRK(REQUESTS), &
  &                  COMM)
!     
            CALL SMUMPS_DOCOMMINF(MYID, NUMPROCS, &
  &                  WRKRC(ITDRPTR), M, TAG_ITERS+2+ITER, &
  &                  IRSNDRCVNUM,IWRK(IRNGHBPRCS), &
  &                  IRSNDRCVVOL,IWRK(IRSNDRCVIA), IWRK(IRSNDRCVJA), &
  &                  WRKRC(ISRRPTR), &
  &                  ORSNDRCVNUM,IWRK(ORNGHBPRCS), &
  &                  ORSNDRCVVOL,IWRK(ORSNDRCVIA), IWRK(ORSNDRCVJA), &
  &                  WRKRC( OSRRPTR), &
  &                  IWRK(ISTATUS),IWRK(REQUESTS), &
  &                  COMM)
            IF((EPS .GT. RZERO) .OR. &
  &                 (ITER.EQ.NB1).OR. &
  &                 ((ITER.EQ.NB1+NB2+NB3).AND. &
  &                 (NB1+NB3.GT.0))) THEN
               INFERRROW = SMUMPS_ERRSCALOC(ROWSCA, &
  &                    WRKRC(ITDRPTR), M, &
  &                    IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
!     find error for the cols
               INFERRCOL = SMUMPS_ERRSCALOC(COLSCA, &
  &                    WRKRC(ITDCPTR), N, &
  &                    IWRK(IMYCPTR),INUMMYC, NOMP_MAX)
!     get max of those two errors
               INFERRL = INFERRCOL
               IF(INFERRROW > INFERRL ) THEN
                  INFERRL = INFERRROW
               ENDIF
!     
               CALL MPI_ALLREDUCE(INFERRL, INFERRG, &
  &                    1, MPI_REAL, &
  &                    MPI_MAX, COMM, IERROR)
               IF(INFERRG.LE.EPS) THEN
                  CALL SMUMPS_UPDATESCALE(COLSCA, &
  &                       WRKRC(ITDCPTR),N, &
  &                       IWRK(IMYCPTR),INUMMYC, NOMP_MAX)
                  CALL SMUMPS_UPDATESCALE(ROWSCA, &
  &                       WRKRC(ITDRPTR),M, &
  &                       IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
                  IF(ITER .LE. NB1) THEN
                     ITER = NB1+1
                     CYCLE
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
         ELSE
!     SINGLE PROCESSOR CASE: INF-NORM ERROR COMPUTATION
            IF((EPS .GT. RZERO) .OR. &
  &                 (ITER.EQ.NB1).OR. &
  &                 ((ITER.EQ.NB1+NB2+NB3).AND. &
  &                 (NB1+NB3.GT.0))) THEN
               INFERRROW = SMUMPS_ERRSCA1(ROWSCA, &
  &                    WRKRC(ITDRPTR), M, NOMP_MAX)
!     find error for the cols
               INFERRCOL = SMUMPS_ERRSCA1(COLSCA, &
  &                    WRKRC(ITDCPTR), N, NOMP_MAX)
!     get max of those two errors
               INFERRL = INFERRCOL
               IF(INFERRROW > INFERRL) THEN
                  INFERRL = INFERRROW
               ENDIF
               INFERRG = INFERRL
               IF(INFERRG.LE.EPS) THEN
                  CALL SMUMPS_UPSCALE1(COLSCA, WRKRC(ITDCPTR), N, &
  &                                       NOMP_MAX)
                  CALL SMUMPS_UPSCALE1(ROWSCA, WRKRC(ITDRPTR), M, &
  &                                       NOMP_MAX)
                  IF(ITER .LE. NB1) THEN
                     ITER = NB1+1
                     CYCLE
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ELSE
!     ----------------------------------------
!     WE HAVE ITER.GT.NB1 AND ITER.LE.NB1+NB2. 
!     ONE-NORM ITERATION
!     ----------------------------------------
       IF (NOMP_MAX.LE.1) THEN
         IF((ITER .EQ.1).OR.(OORANGEIND))THEN
            DO NZIND=1_8,NZ_loc
               IR = IRN_loc(NZIND)
               IC = JCN_loc(NZIND)
               IF((IR.GE.1).AND.(IR.LE.M).AND. &
  &                    (IC.GE.1).AND.(IC.LE.N)) THEN
                  ELM = abs(A_loc(NZIND))*ROWSCA(IR)*COLSCA(IC)
!                       WRKRC(ITDRPTR-1_8+int(IR,8)) =
!     &                 WRKRC(ITDRPTR-1_8+int(IR,8)) + ELM
                  WRKRC(IR) =  WRKRC(IR) + ELM
                  WRKRC(ITDCPTR-1_8+int(IC,8)) = &
  &                  WRKRC(ITDCPTR-1_8+int(IC,8)) + ELM
               ELSE
                  OORANGEIND = .TRUE.
               ENDIF
            ENDDO
         ELSEIF(.NOT.OORANGEIND) THEN
            DO NZIND=1,NZ_loc
               IR = IRN_loc(NZIND)
               IC = JCN_loc(NZIND)
               ELM = abs(A_loc(NZIND))*ROWSCA(IR)*COLSCA(IC)
!                    WRKRC(ITDRPTR-1_8+int(IR,8)) =
!     &              WRKRC(ITDRPTR-1_8+int(IR,8)) + ELM
               WRKRC(IR) =  WRKRC(IR) + ELM
               WRKRC(ITDCPTR-1_8+int(IC,8)) = &
  &               WRKRC(ITDCPTR-1_8+int(IC,8)) + ELM
            ENDDO
         ENDIF
!}
       ELSE  ! NOMP_MAX>1
         IF((ITER .EQ.1).OR.(OORANGEIND))THEN
!$OMP   PARALLEL PRIVATE(IOMP)
!$OMP&  NUM_THREADS(NOMP_MAX)
!$OMP&    IF ( NZ_loc > int(K361,8) .AND. NOMP .GT. 1)
              IOMP  = 1
!$                  IOMP = OMP_GET_THREAD_NUM() + 1
!$OMP     DO PRIVATE(NZIND,IR,IC,ELM)
!$OMP&    SCHEDULE(STATIC,CHUNK_NZ)
!$OMP&    REDUCTION(.OR.:OORANGEIND)
            DO NZIND=1_8,NZ_loc
               IR = IRN_loc(NZIND)
               IC = JCN_loc(NZIND)
               IF((IR.GE.1).AND.(IR.LE.M).AND. &
  &                    (IC.GE.1).AND.(IC.LE.N)) THEN
                  ELM = abs(A_loc(NZIND))*ROWSCA(IR)*COLSCA(IC)
                  WRKR_TH(IR,IOMP) = WRKR_TH(IR,IOMP) + ELM
                  WRKC_TH(IC,IOMP) = WRKC_TH(IC,IOMP) + ELM
               ELSE
                  OORANGEIND = .TRUE.
               ENDIF
            ENDDO
!$OMP     END DO
!$OMP   END PARALLEL
         ELSEIF(.NOT.OORANGEIND) THEN
!$OMP   PARALLEL PRIVATE(IOMP)
!$OMP&  NUM_THREADS(NOMP_MAX)
!$OMP&    IF ( NZ_loc > int(K361,8) .AND. NOMP .GT. 1)
              IOMP  = 1
!$                  IOMP = OMP_GET_THREAD_NUM() + 1
!$OMP     DO PRIVATE(NZIND,IR,IC,ELM)
!$OMP&    SCHEDULE(STATIC,CHUNK_NZ)
            DO NZIND=1,NZ_loc
               IR = IRN_loc(NZIND)
               IC = JCN_loc(NZIND)
               ELM = abs(A_loc(NZIND))*ROWSCA(IR)*COLSCA(IC)
               WRKR_TH(IR,IOMP) = WRKR_TH(IR,IOMP) + ELM
               WRKC_TH(IC,IOMP) = WRKC_TH(IC,IOMP) + ELM
            ENDDO
!$OMP     END DO
!$OMP   END PARALLEL
         ENDIF
!
!              For all i on MYID:
!              Build WRKRC(i) = Sum       (WRKR_TH(i,IOMP)
!                              IOMP \in [1:NOMP_MAX]
         IF(NUMPROCS > 1) THEN
           CALL SMUMPS_REDUCE_WRK_MPI (WRKRC, N, WRKR_TH, &
  &                 NOMP_MAX, &
  &                 IWRK(IMYRPTR),INUMMYR)
           CALL SMUMPS_REDUCE_WRK_MPI (WRKRC(ITDCPTR), &
  &                 N, WRKC_TH, NOMP_MAX, &
  &                 IWRK(IMYCPTR),INUMMYC)
         ELSE
           CALL SMUMPS_REDUCE_WRK (WRKRC, N, WRKR_TH, NOMP_MAX)
           CALL SMUMPS_REDUCE_WRK (WRKRC(ITDCPTR), &
  &                 N, WRKC_TH, NOMP_MAX)
         ENDIF
!}
       ENDIF
         IF(NUMPROCS > 1) THEN
            CALL SMUMPS_DOCOMM1N(MYID, NUMPROCS, &
  &                 WRKRC(ITDCPTR), N, TAG_ITERS+ITER, &
  &                 ICSNDRCVNUM, IWRK(ICNGHBPRCS), &
  &                 ICSNDRCVVOL, IWRK(ICSNDRCVIA), IWRK(ICSNDRCVJA), &
  &                 WRKRC(ISRCPTR), &
  &                 OCSNDRCVNUM, IWRK(OCNGHBPRCS), &
  &                 OCSNDRCVVOL, IWRK(OCSNDRCVIA), IWRK(OCSNDRCVJA), &
  &                 WRKRC( OSRCPTR), &
  &                 IWRK(ISTATUS), IWRK(REQUESTS), &
  &                 COMM)
!     
            CALL SMUMPS_DOCOMM1N(MYID, NUMPROCS, &
  &                 WRKRC(ITDRPTR), M, TAG_ITERS+2+ITER, &
  &                 IRSNDRCVNUM, IWRK(IRNGHBPRCS), &
  &                 IRSNDRCVVOL, IWRK(IRSNDRCVIA), IWRK(IRSNDRCVJA), &
  &                 WRKRC(ISRRPTR), &
  &                 ORSNDRCVNUM, IWRK(ORNGHBPRCS), &
  &                 ORSNDRCVVOL, IWRK(ORSNDRCVIA), IWRK(ORSNDRCVJA), &
  &                 WRKRC( OSRRPTR), &
  &                 IWRK(ISTATUS), IWRK(REQUESTS), &
  &                 COMM)
            IF((EPS .GT. RZERO) .OR. &
  &                 ((ITER.EQ.NB1+NB2).AND. &
  &                 (NB2.GT.0))) THEN
               ONEERRROW = SMUMPS_ERRSCALOC(ROWSCA, &
  &                    WRKRC(ITDRPTR), M, &
  &                    IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
!     find error for the cols
               ONEERRCOL = SMUMPS_ERRSCALOC(COLSCA, &
  &                    WRKRC(ITDCPTR), N, &
  &                    IWRK(IMYCPTR),INUMMYC, NOMP_MAX)
!     get max of those two errors
               ONEERRL = ONEERRCOL
               IF(ONEERRROW > ONEERRL ) THEN
                  ONEERRL = ONEERRROW
               ENDIF
!     
               CALL MPI_ALLREDUCE(ONEERRL, ONEERRG, &
  &                    1, MPI_REAL, &
  &                    MPI_MAX, COMM, IERROR)
               IF(ONEERRG.LE.EPS) THEN
                  CALL SMUMPS_UPDATESCALE(COLSCA, &
  &                       WRKRC(ITDCPTR),N, &
  &                       IWRK(IMYCPTR),INUMMYC, NOMP_MAX)
                  CALL SMUMPS_UPDATESCALE(ROWSCA, &
  &                       WRKRC(ITDRPTR),M, &
  &                       IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
                  ITER = NB1+NB2+1
                  CYCLE
               ENDIF
            ENDIF
         ELSE
!     SINGLE-PROCESSOR CASE: ONE-NORM ERROR COMPUTATION
            IF((EPS .GT. RZERO) .OR. &
  &                 ((ITER.EQ.NB1+NB2).AND. &
  &                 (NB2.GT.0))) THEN
               ONEERRROW = SMUMPS_ERRSCA1(ROWSCA, &
  &                    WRKRC(ITDRPTR), M, NOMP_MAX)
!     find error for the cols
               ONEERRCOL = SMUMPS_ERRSCA1(COLSCA, &
  &                    WRKRC(ITDCPTR), N, NOMP_MAX)
!     get max of those two errors
               ONEERRL = ONEERRCOL
               IF(ONEERRROW > ONEERRL) THEN
                  ONEERRL = ONEERRROW
               ENDIF
               ONEERRG = ONEERRL
               IF(ONEERRG.LE.EPS) THEN
                 CALL SMUMPS_UPSCALE1(COLSCA, WRKRC(ITDCPTR), N, &
  &                                      NOMP_MAX)
                 CALL SMUMPS_UPSCALE1(ROWSCA, WRKRC(ITDRPTR), M, &
  &                                      NOMP_MAX)
                 ITER = NB1+NB2+1
                 CYCLE
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF(NUMPROCS > 1) THEN
         CALL SMUMPS_UPDATESCALE(COLSCA,  WRKRC(ITDCPTR), N, &
  &              IWRK(IMYCPTR),INUMMYC, NOMP_MAX)
         CALL SMUMPS_UPDATESCALE(ROWSCA,  WRKRC(ITDRPTR), M, &
  &              IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
!   
      ELSE
!     SINGLE PROCESSOR CASE: Conv check and update of sca arrays
         CALL SMUMPS_UPSCALE1(COLSCA, WRKRC(ITDCPTR), N, &
  &                              NOMP_MAX)
         CALL SMUMPS_UPSCALE1(ROWSCA, WRKRC(ITDRPTR), M, &
  &                              NOMP_MAX)
      ENDIF
      ITER = ITER + 1
!}
   ENDDO
   ONENORMERR = ONEERRG
   INFNORMERR = INFERRG
   IF(NUMPROCS > 1) THEN
!{
      CALL MPI_REDUCE(ROWSCA, WRKRC(1), M, MPI_REAL, &
  &           MPI_MAX, 0, &
  &           COMM, IERROR)
      IF(MYID.EQ.0) THEN
         DO I=1, M
            ROWSCA(I) = WRKRC(I)
         ENDDO
      ENDIF
!     Scaling factors are printed
!     WRITE (6,*) MYID, 'ROWSCA=',ROWSCA
!     WRITE (6,*) MYID, 'COLSCA=',COLSCA
!     CALL FLUSH(6)
!     REduce the whole scaling factors to processor 0 of COMM
      CALL MPI_REDUCE(COLSCA, WRKRC(1_8+int(M,8)), N, &
  &           MPI_REAL, &
  &           MPI_MAX, 0, &
  &           COMM, IERROR)
      IF(MYID.EQ.0) THEN
!{
        IF (NOMP_MAX.LE.0) THEN
         DO I=1, N
            COLSCA(I) = WRKRC(int(I,8)+int(M,8))
         ENDDO
        ELSE
!$OMP       PARALLEL DO PRIVATE(I)
!$OMP&      SCHEDULE(STATIC,CHUNK)
!$OMP&      IF ( N > K361 .AND. NOMP .GT. 1)
         DO I=1, N
            COLSCA(I) = WRKRC(int(I,8)+int(M,8))
         ENDDO
!$OMP       END PARALLEL DO
        ENDIF
!}
      ENDIF
!}
   ENDIF
ENDIF
RETURN
END SUBROUTINE SMUMPS_SIMSCALEABSUNS
!
! 
!     SEPARATOR: Another function begins
!
! 
SUBROUTINE SMUMPS_SIMSCALEABSSYM(IRN_loc, JCN_loc, A_loc, NZ_loc, &
  &     N, NUMPROCS, MYID, COMM, &
  &     PARTVEC, &
  &     RSNDRCVSZ, &
  &     REGISTRE, &
  &     IWRK, IWRKSZ, &
  &     INTSZ, RESZ, OP, &
  &     SCA, WRKRC, ISZWRKRC, &
  &     WRKR_TH, LWRKR_TH, NOMP_MAX, &
  &     NB1, NB2, NB3, EPS, &
  &     ONENORMERR, INFNORMERR)
!----------------------------------------------------------------------
! Input parameters:
!     N: size of matrix (sym matrix, square).
!     NUMPROCS, MYID, COMM: guess what are those
!     PARTVEC:  row/col partvec to be filled when OP=1
!     RSNDRCVSZ:send recv sizes for row/col operations. 
!               to be filled when OP=1
!     REGISTRE: to store some pointers (size etc). Its size is 12,
!               but we do not use all in this routine.
!     IWRK: working space. when OP=1 IWRKSZ.GE.2*MAXMN
!           when OP=2 INTSZ portion is used. Donc, IWRKSZ>INTSZ 
!           when OP=2
!      IWRKSZ: size
!      INTSZ: to be computed when OP=1, necessary integer space to run 
!             scaling algo when OP=2
!      RESZ:  to be computed when OP=1, necessary real space to run 
!             scaling algo when OP=2
!      OP: 
!          =1 estimation of memory and construction of partvecs
!           writes into PARTVEC,RSNDRCVSZ,REGISTRE
!           does not access WRKRC, uses IWRK as workspace
!           computes INTSZ and RESZ.
!          =2 Compute scalings 
!           restores pointers from REGISTRE, 
!           stores communication structure in IWRK (from the start). 
!
!      SCA: space for row/col scaling factor; has size M
!      WRKRC: real working space. when OP=1, is not accessed. Donc, it
!             can be declared to be of size 1 at OP=1 call.
!      ISZWRKRC: size
!      SYM: is matrix symmetric
!      NB1, NB2, NB3: algo runs 
!                     NB1 iters of inf-norm (default  1/1), 
!                     NB2 iters of 1-norm   (default  3/10),
!                     NB3 iters of inf-norm (default  3/10).
!          in succession.
!      EPS: tolerance for concergence. 
!           IF EPS < 0.R0 then does not test convergence.
!           See comments for the uns case above.      
!     ONENORMERR : error in one norm scaling (see comments for the 
!                  uns case above), 
!     INFNORMERR : error in inf norm scaling (see comments for the 
!                  uns case above).
!---------------------------------------------------------------------
! On input:
!      OP=1==>Requirements
!             IWRKSZ.GE.2*MAXMN   XXXX compare with uns variant.
!             PARTVEC  of size N
!             SNDRCVSZ of size 2*NUMPROCS
!             REGISTRE  of size 12
!             
!      OP=2==>Requirements
!             INTSZ .GE. REGISTRE(11)
!             RESZ  .GE. REGISTRE(12) 
!---------------------------------------------------------------------
! On output:
!     SCA
!            at processor 0 of COMM: complete factors.
!            at other processors   : only the SCA(i) and SCA(j)
!            for which there is a nonzero a_ij.
!     ONENORMERR : error in one norm scaling 
!                = -1.0 if iter2=0.
!     INFNORMERR : error in inf norm scaling 
!                = inf norm error at iter3 if iter3 > 0
!                = inf norm error at iter1 if iter1 > 0, iter3=0
!                = -1.0 if iter1=iter3=0
! ---------------------------------------------------------------------
! NOTE: some variables are named in such a way that they correspond
!       to the row variables in unsym case. They are used for both 
!       row and col communications.
! ---------------------------------------------------------------------
! References:
!     The scaling algorithms are based on those discussed in
!     [1] D. Ruiz, "A scaling algorithm to equilibrate both rows and 
!         columns norms in matrices", Tech. Rep. Rutherford 
!         Appleton Laboratory, Oxon, UK and ENSEEIHT-IRIT, 
!         Toulouse, France, RAL-TR-2001-034 and RT/APO/01/4, 2001.
!     [2] D. Ruiz and B. Ucar, "A symmetry preserving algorithm for
!         matrix scaling", in preparation as of Jan'08.
!
!     The parallelization approach is based on discussion in
!     [3] P. R. Amestoy, I. S. Duff, D. Ruiz, and B. Ucar, "A parallel
!         matrix scaling algorithm", accepted for publication, 
!         In proceedings of VECPAR'08-International Meeting-High 
!         Performance Computing for Computational Science, Jan'08.
!     and was supported by ANR-SOLSTICE project (ANR-06-CIS6-010)
! ---------------------------------------------------------------------
!$    USE OMP_LIB
IMPLICIT NONE
INCLUDE 'mpif.h'
INTEGER(8) :: NZ_loc
INTEGER N, OP
INTEGER(8) :: IWRKSZ, LWRKR_TH
INTEGER NUMPROCS, MYID, COMM, NOMP_MAX
INTEGER(8) :: INTSZ, RESZ
INTEGER IRN_loc(NZ_loc)
INTEGER JCN_loc(NZ_loc)
REAL A_loc(NZ_loc)
INTEGER PARTVEC(N), RSNDRCVSZ(2*NUMPROCS)
INTEGER IWRK(IWRKSZ)
INTEGER(8) :: REGISTRE(12)
REAL SCA(N)
INTEGER(8) :: ISZWRKRC
REAL WRKRC(ISZWRKRC), &
  &     WRKR_TH(LWRKR_TH, max(NOMP_MAX,1))
!     LOCALS
INTEGER IRSNDRCVNUM, ORSNDRCVNUM
INTEGER IRSNDRCVVOL, ORSNDRCVVOL
INTEGER  INUMMYR
! IMPORTANT POINTERS
INTEGER(8) :: IMYRPTR,IMYCPTR
INTEGER(8) :: IRNGHBPRCS, IRSNDRCVIA,IRSNDRCVJA
INTEGER(8) :: ORNGHBPRCS, ORSNDRCVIA,ORSNDRCVJA
INTEGER(8) :: ISTATUS, REQUESTS, TMPWORK
INTEGER(8) :: ITDRPTR, ISRRPTR, OSRRPTR
REAL ONENORMERR,INFNORMERR
!     FOR the scaling phase  
INTEGER NB1, NB2, NB3
REAL EPS
!     Iteration vars 
INTEGER ITER, IR, IC
INTEGER(8) :: NZIND
REAL ELM
!     COMM TAGS....
INTEGER TAG_COMM_ROW
PARAMETER(TAG_COMM_ROW=101)
INTEGER TAG_ITERS
PARAMETER(TAG_ITERS=102)
!     FUNCTIONS
EXTERNAL SMUMPS_CREATEPARTVECSYM, &
  &     SMUMPS_NUMVOLSNDRCVSYM, &
  &     SMUMPS_SETUPCOMMSSYM, &
  &     SMUMPS_FILLMYROWCOLINDICESSYM, &
  &     SMUMPS_DOCOMMINF, &
  &     SMUMPS_DOCOMM1N, &
  &     SMUMPS_INITREAL, &
  &     SMUMPS_INITREALLST
REAL SMUMPS_ERRSCALOC
REAL SMUMPS_ERRSCA1
INTRINSIC abs
REAL RONE, RZERO
PARAMETER(RONE=1.0E0,RZERO=0.0E0)
!     TMP VARS
INTEGER(8) :: INTSZR
INTEGER MAXMN
INTEGER I, IERROR
REAL ONEERRL, ONEERRG
REAL INFERRL, INFERRG
LOGICAL OORANGEIND
INTEGER, PARAMETER :: K361 = 2048
INTEGER            :: IOMP
!$    INTEGER    :: NOMP
!$    INTEGER    :: CHUNK, CHUNK_NZ
!$    ! Too large => pb with cache L3 ?
!$    ! INTEGER(8) :: CHUNK8 
!$    ! CHUNK8= max(int(K361/2,8), 
!$    !  &   (NZ_loc+int(NOMP-1,8) / int(NOMP,8) ) )
!$    ! CHUNK8   = min(CHUNK8, huge(CHUNK)-1_8)
!$    NOMP  = omp_get_max_threads()
!$    CHUNK= max(K361/2, (N+NOMP-1) / NOMP )
!$    IF (NOMP_MAX.GT.0) THEN
!$      CHUNK_NZ = max(K361/2, (N+NOMP_MAX-1) / NOMP_MAX )
!$    ENDIF
!
OORANGEIND = .FALSE.
INFERRG = -RONE
ONEERRG = -RONE
MAXMN = N
IF(OP == 1) THEN
!{
   IF(NUMPROCS > 1) THEN
!     Check done outside
!     IF(IWRKSZ.LT.2*MAXMN) THEN   ERROR.... 
      CALL SMUMPS_CREATEPARTVECSYM(MYID, NUMPROCS, COMM, &
  &           IRN_loc, JCN_loc, NZ_loc, &
  &           PARTVEC, N, &
  &           IWRK, IWRKSZ, INUMMYR )
!
!     Check done outside
      CALL SMUMPS_NUMVOLSNDRCVSYM(MYID, NUMPROCS, N, PARTVEC, &
  &           NZ_loc, IRN_loc, JCN_loc, IRSNDRCVNUM,IRSNDRCVVOL, &
  &           ORSNDRCVNUM, ORSNDRCVVOL, &
  &           IWRK,IWRKSZ, &
  &           RSNDRCVSZ(1), RSNDRCVSZ(1+NUMPROCS), COMM)
!     
!     
      INTSZR =  int(IRSNDRCVNUM,8) + int(ORSNDRCVNUM,8) + &
  &           int(IRSNDRCVVOL,8) + int(ORSNDRCVVOL,8) + &
  &           2_8*int(NUMPROCS+1,8) + int(INUMMYR,8)
      INTSZ = INTSZR + int(N,8) + &
  &           int(MPI_STATUS_SIZE +1,8) * int(NUMPROCS,8)
   ELSE
!     NUMPROCS IS 1
      IRSNDRCVNUM = 0
      ORSNDRCVNUM = 0
      IRSNDRCVVOL = 0
      ORSNDRCVVOL = 0
      INUMMYR = 0
      INTSZ = 0_8
   ENDIF
!     CALCULATE NECESSARY REAL SPACE
   RESZ = int(N,8) + int(IRSNDRCVVOL,8) + int(ORSNDRCVVOL,8)
   REGISTRE(1)  = int(IRSNDRCVNUM,8)
   REGISTRE(2)  = int(ORSNDRCVNUM,8)
   REGISTRE(3)  = int(IRSNDRCVVOL,8)
   REGISTRE(4)  = int(ORSNDRCVVOL,8)
   REGISTRE(9)  = int(INUMMYR,8)
   REGISTRE(11) = INTSZ
   REGISTRE(12) = RESZ
!}
ELSE
!{
!     else of op=1. That is op=2 now.
!     restore the numbers
   IRSNDRCVNUM = int(REGISTRE(1))
   ORSNDRCVNUM = int(REGISTRE(2))
   IRSNDRCVVOL = int(REGISTRE(3))
   ORSNDRCVVOL = int(REGISTRE(4))
   INUMMYR     = int(REGISTRE(9))
    IF(NUMPROCS > 1) THEN
!     Check done outsize
!             IF(INTSZ < REGISTRE(11)) THEN ERROR
!             IF(RESZ < REGISTRE(12)) THEN ERROR
!     Fill up myrows and my colsX
       CALL SMUMPS_FILLMYROWCOLINDICESSYM(MYID, NUMPROCS,COMM, &
  &            IRN_loc, JCN_loc, NZ_loc, &
  &            PARTVEC, N, &
  &            IWRK(1), INUMMYR, &
  &            IWRK(1+INUMMYR), IWRKSZ-int(INUMMYR,8), NOMP_MAX)
       IMYRPTR = 1_8
       IMYCPTR = IMYRPTR + int(INUMMYR,8)
!     Set up comm and run.
!     set pointers in iwrk (3 parts)
!     
!     ROWS    [---------------------------------------------]
       IRNGHBPRCS = IMYCPTR
       IRSNDRCVIA = IRNGHBPRCS + int(IRSNDRCVNUM,8)
       IRSNDRCVJA = IRSNDRCVIA + int(NUMPROCS+1,8)
       ORNGHBPRCS = IRSNDRCVJA + int(IRSNDRCVVOL,8)
       ORSNDRCVIA = ORNGHBPRCS + int(ORSNDRCVNUM,8)
       ORSNDRCVJA = ORSNDRCVIA + int(NUMPROCS + 1,8)
!     MPI     [-----------------]
       REQUESTS = ORSNDRCVJA + int(ORSNDRCVVOL,8)
       ISTATUS = REQUESTS + int(NUMPROCS,8)
!     TMPWRK  [-----------------]
       TMPWORK = ISTATUS + int(MPI_STATUS_SIZE,8)*int(NUMPROCS,8)
       CALL SMUMPS_SETUPCOMMSSYM(MYID, NUMPROCS, N, PARTVEC, &
  &            NZ_loc, IRN_loc, JCN_loc, &
  &            IRSNDRCVNUM, IRSNDRCVVOL, &
  &            IWRK(IRNGHBPRCS),IWRK(IRSNDRCVIA),IWRK(IRSNDRCVJA), &
  &            ORSNDRCVNUM, ORSNDRCVVOL, &
  &            IWRK(ORNGHBPRCS),IWRK(ORSNDRCVIA),IWRK(ORSNDRCVJA), &
  &            RSNDRCVSZ(1), RSNDRCVSZ(1+NUMPROCS), &
  &            IWRK(TMPWORK), &
  &            IWRK(ISTATUS), IWRK(REQUESTS), &
  &            TAG_COMM_ROW, COMM)
       CALL SMUMPS_INITREAL(SCA, N, RZERO, NOMP_MAX)
       CALL SMUMPS_INITREALLST(SCA, N, &
  &            IWRK(IMYRPTR),INUMMYR, RONE, NOMP_MAX)
    ELSE
       CALL SMUMPS_INITREAL(SCA, N, RONE, NOMP_MAX)
    ENDIF
    ITDRPTR = 1_8
    ISRRPTR = ITDRPTR + int(N,8)
    OSRRPTR = ISRRPTR + int(IRSNDRCVVOL,8)
!     
!     To avoid bound check errors...
    IF(NUMPROCS == 1)THEN
       OSRRPTR = OSRRPTR - 1
       ISRRPTR = ISRRPTR - 1
    ELSE
       IF(IRSNDRCVVOL == 0) ISRRPTR = ISRRPTR - 1
       IF(ORSNDRCVVOL == 0) OSRRPTR = OSRRPTR - 1
    ENDIF
!     computation starts
    ITER = 1
    DO WHILE(ITER.LE.NB1+NB2+NB3)
!{
!            -------------------------
!            CLEAR temporary Dr and Dc 
!            -------------------------
       IF (NOMP_MAX.GT.1 .AND. &
  &         (ITER.GT.NB1 .AND.ITER.LE.NB1+NB2) &
  &       ) THEN
!             if one norm iteration and multithreading activated
!             WRKR_TH need be initialized and 
!             WRKRC will be set by reduction of WRKR_TH
!$OMP         PARALLEL
!$OMP&         PRIVATE(IOMP)
!$OMP&         NUM_THREADS(NOMP_MAX)
         IOMP  = 1
!$             IOMP = OMP_GET_THREAD_NUM() + 1
         IF(NUMPROCS > 1) THEN
           CALL SMUMPS_ZEROOUT(WRKR_TH(1,IOMP),N, &
  &                 IWRK(IMYRPTR),INUMMYR, 0)
         ELSE
           CALL SMUMPS_INITREAL(WRKR_TH(1,IOMP),N, RZERO, &
  &                                0)
         ENDIF
!$OMP         END PARALLEL
       ELSE
        IF(NUMPROCS > 1) THEN
!FIXME         Size N should be adjusted to effective size
           CALL SMUMPS_ZEROOUT(WRKRC(ITDRPTR),N, &
  &                IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
        ELSE
           CALL SMUMPS_INITREAL(WRKRC(ITDRPTR),N, RZERO, &
  &                                NOMP_MAX)
        ENDIF
       ENDIF
!
       IF((ITER.LE.NB1).OR.(ITER > NB1+NB2)) THEN
!            ------------------
!{           INF-NORM ITERATION
!            ------------------
         IF (NOMP_MAX.LE.0) THEN
           IF((ITER .EQ.1).OR.(OORANGEIND))THEN
             DO NZIND=1_8,NZ_loc
                IR = IRN_loc(NZIND)
                IC = JCN_loc(NZIND)
                IF((IR.GE.1).AND.(IR.LE.N).AND. &
  &                     (IC.GE.1).AND.(IC.LE.N)) THEN
                   ELM = abs(A_loc(NZIND))*SCA(IR)*SCA(IC)
                   IF (WRKRC(IR)<ELM) WRKRC(IR)= ELM
                   IF (WRKRC(IC)<ELM) WRKRC(IC)= ELM
                ELSE
                   OORANGEIND = .TRUE.
                ENDIF
             ENDDO
           ELSEIF(.NOT.OORANGEIND) THEN
             DO NZIND=1_8,NZ_loc
                IR = IRN_loc(NZIND)
                IC = JCN_loc(NZIND)
                ELM = abs(A_loc(NZIND))*SCA(IR)*SCA(IC)
                IF (WRKRC(IR)<ELM) WRKRC(IR)= ELM
                IF (WRKRC(IC)<ELM) WRKRC(IC)= ELM
             ENDDO
           ENDIF
         ELSE
          IF((ITER.EQ.1).OR.(OORANGEIND)) THEN
!$OMP   PARALLEL DO PRIVATE(NZIND,IR,IC,ELM) 
!$OMP&  SCHEDULE(STATIC,CHUNK_NZ)
!$OMP&  IF ( NZ_loc > int(K361,8) .AND. NOMP .GT. 1)
!$OMP&  REDUCTION(.OR.:OORANGEIND)
             DO NZIND=1_8,NZ_loc
                IR = IRN_loc(NZIND)
                IC = JCN_loc(NZIND)
                IF((IR.GE.1).AND.(IR.LE.N).AND. &
  &                     (IC.GE.1).AND.(IC.LE.N)) THEN
                   ELM = abs(A_loc(NZIND))*SCA(IR)*SCA(IC)
!$OMP                    ATOMIC UPDATE
                   WRKRC(IR)= max (ELM, WRKRC(IR))
!$OMP                    END ATOMIC
!                         IF(WRKRC(ITDRPTR-1_8+int(IR,8))<ELM) THEN
!                            WRKRC(ITDRPTR-1_8+int(IR,8))= ELM
!                         ENDIF
!$OMP                    ATOMIC UPDATE
                   WRKRC(IC) = max (ELM,WRKRC(IC))
!$OMP                    END ATOMIC
!                         IF(WRKRC(ITDRPTR-1_8+int(IC,8))<ELM) THEN
!                            WRKRC(ITDRPTR-1_8+int(IC,8))= ELM
!                         ENDIF
                ELSE
                   OORANGEIND = .TRUE.
                ENDIF
             ENDDO
!$OMP   END PARALLEL DO
          ELSEIF(.NOT.OORANGEIND) THEN
!$OMP   PARALLEL DO PRIVATE(NZIND,IR,IC,ELM) 
!$OMP&  SCHEDULE(STATIC,CHUNK_NZ)
!$OMP&  IF ( NZ_loc > int(K361,8) .AND. NOMP .GT. 1)
             DO NZIND=1_8,NZ_loc
                IR = IRN_loc(NZIND)
                IC = JCN_loc(NZIND)
                ELM = abs(A_loc(NZIND))*SCA(IR)*SCA(IC)
!$OMP                 ATOMIC UPDATE
                WRKRC(IR)= max (ELM, WRKRC(IR))
!$OMP                 END ATOMIC
!                      IF(WRKRC(ITDRPTR-1_8+int(IR,8))<ELM) THEN
!                         WRKRC(ITDRPTR-1_8+int(IR,8))= ELM
!                      ENDIF
!$OMP                 ATOMIC UPDATE
                WRKRC(IC) = max (ELM, WRKRC(IC))
!$OMP                 END ATOMIC
!                      IF(WRKRC(ITDRPTR-1_8+int(IC,8))<ELM) THEN
!                         WRKRC(ITDRPTR-1_8+int(IC,8))= ELM
!                      ENDIF
             ENDDO
!$OMP   END PARALLEL DO
          ENDIF
!}
         ENDIF
         IF(NUMPROCS > 1) THEN
!{
            CALL SMUMPS_DOCOMMINF(MYID, NUMPROCS, &
  &                  WRKRC(ITDRPTR), N, TAG_ITERS+2+ITER, &
  &                  IRSNDRCVNUM,IWRK(IRNGHBPRCS), &
  &                  IRSNDRCVVOL,IWRK(IRSNDRCVIA), IWRK(IRSNDRCVJA), &
  &                  WRKRC(ISRRPTR), &
  &                  ORSNDRCVNUM,IWRK(ORNGHBPRCS), &
  &                  ORSNDRCVVOL,IWRK(ORSNDRCVIA), IWRK(ORSNDRCVJA), &
  &                  WRKRC( OSRRPTR), &
  &                  IWRK(ISTATUS),IWRK(REQUESTS), &
  &                  COMM)
!CCC FIXME #if defined(dev_version)
            IF((EPS .GT. RZERO) .OR. &
  &                 (ITER.EQ.NB1).OR. &
  &                 ((ITER.EQ.NB1+NB2+NB3).AND. &
  &                 (NB1+NB3.GT.0))) THEN
               INFERRL = SMUMPS_ERRSCALOC(SCA, &
  &                    WRKRC(ITDRPTR), N, &
  &                    IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
               CALL MPI_ALLREDUCE(INFERRL, INFERRG, &
  &                    1, MPI_REAL, &
  &                    MPI_MAX, COMM, IERROR)
               IF(INFERRG.LE.EPS) THEN
                  CALL SMUMPS_UPDATESCALE(SCA,  WRKRC(ITDRPTR), N, &
  &                       IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
                  IF(ITER .LE. NB1) THEN
                     ITER = NB1+1
                     CYCLE
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
!CC #endif
!}
         ELSE
!{             SINGLE PROCESSOR CASE: INF-NORM ERROR COMPUTATION
            IF((EPS .GT. RZERO) .OR. &
  &                 (ITER.EQ.NB1).OR. &
  &                 ((ITER.EQ.NB1+NB2+NB3).AND. &
  &                 (NB1+NB3.GT.0))) THEN
               INFERRL = SMUMPS_ERRSCA1(SCA, &
  &                    WRKRC(ITDRPTR), N, NOMP_MAX)
               INFERRG = INFERRL
               IF(INFERRG.LE.EPS) THEN
                  CALL SMUMPS_UPSCALE1(SCA, WRKRC(ITDRPTR), N, &
  &                                       NOMP_MAX)
                  IF(ITER .LE. NB1) THEN
                     ITER = NB1+1
                     CYCLE
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
!}
         ENDIF
!}
      ELSE
!           ----------------------------------------
!{          WE HAVE ITER.GT.NB1 AND ITER.LE.NB1+NB2. 
!           ONE-NORM ITERATION
!           ----------------------------------------
       IF (NOMP_MAX.LE.1) THEN
         IF((ITER.EQ.1).OR.(OORANGEIND))THEN
            DO NZIND=1_8,NZ_loc
               IR = IRN_loc(NZIND)
               IC = JCN_loc(NZIND)
               IF((IR.GE.1).AND.(IR.LE.N).AND. &
  &                    (IC.GE.1).AND.(IC.LE.N)) THEN
                  ELM = abs(A_loc(NZIND))*SCA(IR)*SCA(IC)
                  WRKRC(IR) = WRKRC(IR) + ELM
                  IF(IR.NE.IC) THEN
                     WRKRC(IC) = WRKRC(IC) + ELM
                  ENDIF
               ELSE
                  OORANGEIND = .TRUE.
               ENDIF
            ENDDO
         ELSEIF(.NOT.OORANGEIND)THEN
            DO NZIND=1_8,NZ_loc
               IR = IRN_loc(NZIND)
               IC = JCN_loc(NZIND)
               ELM = abs(A_loc(NZIND))*SCA(IR)*SCA(IC)
               WRKRC(IR) = WRKRC(IR) + ELM
               IF(IR.NE.IC) THEN
                  WRKRC(IC) = WRKRC(IC) + ELM
               ENDIF
            ENDDO
         ENDIF
       ELSE ! NOMP_MAX>1
         IF((ITER.EQ.1).OR.(OORANGEIND))THEN
!$OMP   PARALLEL PRIVATE(IOMP)
!$OMP&  NUM_THREADS(NOMP_MAX)
!$OMP&    IF ( NZ_loc > int(K361,8) .AND. NOMP .GT. 1)
              IOMP  = 1
!$                  IOMP = OMP_GET_THREAD_NUM() + 1
!$OMP     DO PRIVATE(NZIND,IR,IC,ELM)
!$OMP&    SCHEDULE(STATIC,CHUNK_NZ)
!$OMP&    REDUCTION(.OR.:OORANGEIND)
            DO NZIND=1_8,NZ_loc
               IR = IRN_loc(NZIND)
               IC = JCN_loc(NZIND)
               IF((IR.GE.1).AND.(IR.LE.N).AND. &
  &                    (IC.GE.1).AND.(IC.LE.N)) THEN
                  ELM = abs(A_loc(NZIND))*SCA(IR)*SCA(IC)
                  WRKR_TH(IR,IOMP) = WRKR_TH(IR,IOMP) + ELM
                  IF(IR.NE.IC) THEN
                    WRKR_TH(IC,IOMP) = WRKR_TH(IC,IOMP) + ELM
                  ENDIF
               ELSE
                  OORANGEIND = .TRUE.
               ENDIF
            ENDDO
!$OMP     END DO
!$OMP   END PARALLEL
         ELSEIF(.NOT.OORANGEIND)THEN
!$OMP   PARALLEL PRIVATE(IOMP)
!$OMP&  NUM_THREADS(NOMP_MAX)
!$OMP&    IF ( NZ_loc > int(K361,8) .AND. NOMP .GT. 1)
              IOMP  = 1
!$                  IOMP = OMP_GET_THREAD_NUM() + 1
!$OMP     DO PRIVATE(NZIND,IR,IC,ELM)
!$OMP&    SCHEDULE(STATIC,CHUNK_NZ)
            DO NZIND=1_8,NZ_loc
               IR = IRN_loc(NZIND)
               IC = JCN_loc(NZIND)
               ELM = abs(A_loc(NZIND))*SCA(IR)*SCA(IC)
               WRKR_TH(IR,IOMP) = WRKR_TH(IR,IOMP) + ELM
               IF(IR.NE.IC) THEN
                 WRKR_TH(IC,IOMP) = WRKR_TH(IC,IOMP) + ELM
               ENDIF
            ENDDO
!$OMP     END DO
!$OMP   END PARALLEL
!}
         ENDIF
!
!              For all i on MYID:
!              Build WRKRC(i) = Sum       (WRKR_TH(i,IOMP)
!                              IOMP \in [1:NOMP_MAX]
         IF(NUMPROCS > 1) THEN
           CALL SMUMPS_REDUCE_WRK_MPI (WRKRC, N, WRKR_TH, &
  &                 NOMP_MAX, &
  &                 IWRK(IMYRPTR),INUMMYR)
         ELSE
           CALL SMUMPS_REDUCE_WRK (WRKRC, N, WRKR_TH, NOMP_MAX)
         ENDIF
       ENDIF
         IF(NUMPROCS > 1) THEN
!{
            CALL SMUMPS_DOCOMM1N(MYID, NUMPROCS, &
  &                 WRKRC(ITDRPTR), N, TAG_ITERS+2+ITER, &
  &                 IRSNDRCVNUM, IWRK(IRNGHBPRCS), &
  &                 IRSNDRCVVOL, IWRK(IRSNDRCVIA), IWRK(IRSNDRCVJA), &
  &                 WRKRC(ISRRPTR), &
  &                 ORSNDRCVNUM, IWRK(ORNGHBPRCS), &
  &                 ORSNDRCVVOL, IWRK(ORSNDRCVIA), IWRK(ORSNDRCVJA), &
  &                 WRKRC( OSRRPTR), &
  &                 IWRK(ISTATUS), IWRK(REQUESTS), &
  &                 COMM)
            IF((EPS .GT. RZERO) .OR. &
  &                 ((ITER.EQ.NB1+NB2).AND. &
  &                 (NB2.GT.0))) THEN
               ONEERRL = SMUMPS_ERRSCALOC(SCA, &
  &                    WRKRC(ITDRPTR), N, &
  &                    IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
!     mpi allreduce.
               CALL MPI_ALLREDUCE(ONEERRL, ONEERRG, &
  &                    1, MPI_REAL, &
  &                    MPI_MAX, COMM, IERROR)
               IF(ONEERRG.LE.EPS) THEN
                  CALL SMUMPS_UPDATESCALE(SCA,  WRKRC(ITDRPTR), N, &
  &                       IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
                  ITER = NB1+NB2+1
                  CYCLE
               ENDIF
            ENDIF
!}
         ELSE
!{             SINGLE-PROCESSOR CASE: ONE-NORM ERROR COMPUTATION
            IF((EPS .GT. RZERO) .OR. &
  &                 ((ITER.EQ.NB1+NB2).AND. &
  &                 (NB2.GT.0))) THEN
               ONEERRL = SMUMPS_ERRSCA1(SCA, &
  &                    WRKRC(ITDRPTR), N, NOMP_MAX)
               ONEERRG = ONEERRL
               IF(ONEERRG.LE.EPS) THEN
                  CALL SMUMPS_UPSCALE1(SCA, WRKRC(ITDRPTR), N, &
  &                                       NOMP_MAX)
                  ITER = NB1+NB2+1
                  CYCLE
               ENDIF
            ENDIF
         ENDIF
!}
      ENDIF
      IF(NUMPROCS > 1) THEN
         CALL SMUMPS_UPDATESCALE(SCA, WRKRC(ITDRPTR), N, &
  &              IWRK(IMYRPTR),INUMMYR, NOMP_MAX)
      ELSE
         CALL SMUMPS_UPSCALE1(SCA, WRKRC(ITDRPTR), N, &
  &                              NOMP_MAX)
      ENDIF
      ITER = ITER + 1
!}
   ENDDO
   ONENORMERR = ONEERRG
   INFNORMERR = INFERRG
   IF(NUMPROCS > 1) THEN
!{
      CALL MPI_REDUCE(SCA, WRKRC(1), N, MPI_REAL, &
  &           MPI_MAX, 0, &
  &           COMM, IERROR)
      IF(MYID.EQ.0) THEN
        IF (NOMP_MAX.LE.0) THEN
         DO I=1, N
            SCA(I) = WRKRC(I)
         ENDDO
        ELSE
!$OMP       PARALLEL DO PRIVATE(I)
!$OMP&      SCHEDULE(STATIC,CHUNK)
!$OMP&      IF ( N > K361 .AND. NOMP .GT. 1)
         DO I=1, N
            SCA(I) = WRKRC(I)
         ENDDO
!$OMP       END PARALLEL DO
        ENDIF
      ENDIF
!}
   ENDIF
!}
ENDIF
RETURN
END SUBROUTINE SMUMPS_SIMSCALEABSSYM
