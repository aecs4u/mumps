! =========================================================
! 
!  This file includes various modifications of an original 
!  routine MUMPS_ANA_H. The main reference for the approach 
!  used in this routine is
!   Patrick Amestoy, Timothy A. Davis, and Iain S. Duff,
!    "An approximate minimum degree ordering algorithm,"
!    SIAM J. Matrix Analysis  vol 17, pages=886--905 (1996)
!    MUMPS_ANA_H is based on the original AMD code:
!
!    AMD, Copyright (c), 1996-2016, Timothy A. Davis,
!    Patrick R. Amestoy, and Iain S. Duff.  All Rights Reserved.
!    Used in MUMPS under the BSD 3-clause license.
!
! All other routines are modifications of this original routine
! done by MUMPS developers over the years (1996-2020) and are 
! used in MUMPS under the BSD 3-clause license.
!
! BSD 3-clause licence:
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions 
! are met:
!  * Redistributions of source code must retain the above copyright
!    notice, this list of conditions and the following disclaimer.
!  * Redistributions in binary form must reproduce the above 
!    copyright notice, this list of conditions and the following 
!    disclaimer in the documentation and/or other materials provided 
!    with the distribution.
!  * Neither the name of the University of California, Berkeley nor 
!    the names of its contributors may be used to endorse or promote 
!    products derived from this software without specific prior 
!    written permission.
!    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
!    CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
!    INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
!    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
!    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
!    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
!    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
!    NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
!    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
!    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
!    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
!    OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
!    EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!   MUMPS_AMD_ELT is a modification 
!   designed to handle amalgamated and compressed
!   graphs and was developed in 1999 by Patrick Amestoy 
!   in the context of the PARASOL project (1997-1999).
! 
!   MUMPS_HAMD is a modification 
!   designed to take into account a halo in the graph. 
!   The graph is composed is partitioned in two types of nodes
!   the so called internal nodes and the so called halo nodes.
!   Halo nodes cannot be selected the both the initial degrees 
!   and updated degrees of internal node should be taken 
!   into account.  
!   This routine also referred to as HALOAMD in MUMPS comments
!   is used for both Schur functionality and in the coupling with 
!   partitioners such as SCOTCH.
!   This code was developed for MUMPS platform 
!   by Patrick Amestoy between 1997 and 1999.
!
!   MUMPS_HAMF4 is a major modification of MUMPS_HAMD 
!   since metric used to select pivots in not anymore the 
!   degree but an approximation of the fill-in.
!   In this approximation 
!   all cliques of elements adjacent to the variable are deducted.
!   Written by Patrick Amestoy between 1999 and 2000.
!   It is also used by F. Pellegrini in SCOTCH since 2000.
!
!   MUMPS_QAMD: modified version of reference AMD routine MUMPS_ANA_H 
!   designed to automatically detect and exploit dense or quasi dense
!   rows in the reduced matrix at any step of the minimum degree.
!   Written in 1997 by Patrick Amestoy.
!   References:
!    P.R. AMESTOY, Recent progress in parallel multifrontal solvers
!      for unsymmetric sparse matrices,
!      Proceedings of the 15th World Congress on Scientific Computation,
!      Modelling and Applied Mathematics, IMACS, Berlin (1997).
!    P.R. AMESTOY (1999), Methodes directes paralleles de
!      resolution des systemes creux de grande taille.
!      Rapport de these d'habilitation de l'INPT.
!
!   MUMPS_CST_AMF: modified version of MUMPS_HAMF4 routine 
!   implementing constraint minimum fill-in based ordering.
!   Written by Stephane Pralet for MUMPS platform 
!   during his post-doctorate at INPT-IRIT (Oct. 2004- Oct. 2005)
!
!  ----------------------------------------
!  To suppress aggressive absorption in ...
!      MUMPS_ANA_H   : Historical AMD
!        define NOAGG1
!      MUMPS_AMD_ELT  : (work on compressed graphs)
!        define NOAGG2
!      MUMPS_HAMD     : AMD with Halo and used for Schur
!        define NOAGG3
!      MUMPS_HAMF4 : Halo AMF version
!        define NOAGG4
!      MUMPS_QAMD     : Quasi dense
!        define NOAGG5
!      MUMPS_SYMQAMD  : Symbolic facto based on quasi dense
!        In the case of MUMPS_SYMQAMD, the aggressive absorption
!        is controlled by a parameter, AGG6.
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! MUMPS_ANA_H:  Approximate Minimum Degree AMD approach.
!
! Description of MUMPS_ANA_H
!   Given a representation of the nonzero pattern of a symmetric matrix,
!   A, (excluding the diagonal) perform an approximate minimum
!   degree ordering to compute a pivot order
!   such that fill-in in the Cholesky factors A = LL^T is kept low. 
!   Aggressive absorption might be used to
!   tighten the bound on the degree.  This can result a
!   significant improvement in the quality of the ordering for
!   some matrices.
!
!     References and definitions:  
!     [1] Timothy A. Davis and Iain Duff, "An unsymmetric-pattern
!              multifrontal method for sparse LU factorization",
!              SIAM J. Matrix Analysis and Applications, 
!              volume=18, pages=140-158 (1997)
!     [2] Patrick R. Amestoy, Timothy A. Davis, and Iain S. Duff,
!              "An approximate minimum degree ordering algorithm,"
!              SIAM J. Matrix Analysis  vol 17, pages=886--905 (1996)
!     [3] Alan George and Joseph Liu, "The evolution of the
!              minimum degree ordering algorithm," SIAM Review, vol.
!              31, no. 1, pp. 1-19, March 1989.  We list below the
!              features mentioned in that paper that this code
!              includes:
!       mass elimination:
!               Yes.  supervariable detection for mass elimination.
!       indistinguishable nodes:
!               Yes (we call these "supervariables").  
!               We modified the approach used by Duff and Reid to 
!               detect them (the previous hash was the true degree,
!               which we no longer keep track of).  A supervariable is
!               a set of rows with identical nonzero pattern.  All
!               variables in a supervariable are eliminated together.
!               Each supervariable has as its numerical name that of
!               one of its variables (its principal variable).
!       quotient graph representation:
!               Yes.  We use the term "element" for the cliques formed
!               during elimination.  
!               The algorithm can operate in place, but it will work
!               more efficiently if given some "elbow room."
!       element absorption:
!               Yes. Similar to Duff,Reid and  George,Liu approaches
!       external degree:
!               Yes. Similar to Duff, Reid and  George, Liu approaches
!       incomplete degree update and multiple elimination:
!               No implemented.  Our method of
!               degree update within MUMPS_ANA_H is element-based, not
!               variable-based.  It is thus not well-suited for use
!               with incomplete degree update or multiple elimination.
!
!-----------------------------------------------------------------------
SUBROUTINE MUMPS_ANA_H(TOTEL, COMPUTE_PERM, &
  &                   N, IWLEN, PE, PFREE, LEN, IW, NV, ELEN, &
  &                   LAST, NCMPA, DEGREE, HEAD, NEXT, W, PARENT)
! 
!    Restrictive integer 64 bit variant :
!    it is assumed that IW array size can exceed 32-bit integer
!
!     Input not modified
INTEGER, INTENT(IN)     :: TOTEL, N
INTEGER(8), INTENT(IN)  :: IWLEN
LOGICAL, INTENT(IN)     :: COMPUTE_PERM
!     Input undefined on output 
INTEGER, INTENT(INOUT)  :: LEN(N), IW(IWLEN)
! 
!     Output only 
INTEGER, INTENT(OUT)   :: NCMPA
INTEGER, INTENT(OUT)   :: ELEN(N), LAST(N), PARENT(N)
! 
!     Input/output
INTEGER(8), INTENT(INOUT) :: PFREE
INTEGER(8), INTENT(INOUT) :: PE(N)
!     NV also meaningful as input to encode compressed graphs
INTEGER, INTENT(INOUT) :: NV(N)
! 
!     Internal Workspace only
INTEGER         :: NEXT(N), DEGREE(N), HEAD(TOTEL), W(N)
! ---------------------
! Interface Description
! ---------------------
! INPUT ARGUMENTS (unaltered):
!-----------------------------
! n     : The matrix order.
!         number of supervariables if compress/blocked format
!         Restriction:  n .ge. 1
! totel : Number of variables to eliminate
!         In case of blocked format:
!         each variable i is a supervariable of size nv(i)
!         totel is computed as the sum(nv(i)) for i \in [1:n]
!         the algorithm stops when totel variables are
!         eliminated.
! compute_perm : indicates if permutations should be computed 
!         on output in last/elen 
! iwlen:        The length of iw (1..iwlen).  On input, the matrix is
!       stored in iw (1..pfree-1).  However, iw (1..iwlen) should be
!       slightly larger than what is required to hold the matrix, at
!       least iwlen .ge. pfree + n is recommended.  Otherwise,
!       excessive compressions will take place.
!       *** We do not recommend running this algorithm with ***
!       ***      iwlen .lt. pfree + n.                      ***
!       *** Better performance will be obtained if          ***
!       ***      iwlen .ge. pfree + n                       ***
!       *** or better yet                                   ***
!       ***      iwlen .gt. 1.2 * pfree                     ***
!       *** (where pfree is its value on input).            ***
!       The algorithm will not run at all if iwlen .lt. pfree-1.
!
!       Restriction: iwlen .ge. pfree-1
!-----------------------------------------------------------------------
! INPUT/OUPUT ARGUMENTS:
!-----------------------------------------------------------------------
! pe:   On input, pe (i) is the index in iw of the start of row i, or
!       zero if row i has no off-diagonal non-zeros.
!
!       During execution, it is used for both supervariables and
!       elements:
!
!       * Principal supervariable i:  index into iw of the
!               description of supervariable i.  A supervariable
!               represents one or more rows of the matrix
!               with identical nonzero pattern.
!       * Non-principal supervariable i:  if i has been absorbed
!               into another supervariable j, then pe (i) = -j.
!               That is, j has the same pattern as i.
!               Note that j might later be absorbed into another
!               supervariable j2, in which case pe (i) is still -j,
!               and pe (j) = -j2.
!       * Unabsorbed element e:  the index into iw of the description
!               of element e, if e has not yet been absorbed by a
!               subsequent element.  Element e is created when
!               the supervariable of the same name is selected as
!               the pivot.
!       * Absorbed element e:  if element e is absorbed into element
!               e2, then pe (e) = -e2.  This occurs when the pattern of
!               e (that is, Le) is found to be a subset of the pattern
!               of e2 (that is, Le2).  If element e is "null" (it has
!               no nonzeros outside its pivot block), then pe (e) = 0.
!
!       On output, pe holds the assembly tree/forest, which implicitly
!       represents a pivot order with identical fill-in as the actual
!       order (via a depth-first search of the tree).
!
!       On output:  (PE is copied on output into PARENT array)
!       If nv (i) .gt. 0, then i represents a node in the assembly tree,
!       and the parent of i is -pe (i), or zero if i is a root.
!       If nv (i) = 0, then (i,-pe (i)) represents an edge in a
!       subtree, the root of which is a node in the assembly tree.
! pfree:On input, the matrix is stored in iw (1..pfree-1) and
!       the rest of the array iw is free.
!       During execution, additional data is placed in iw, and pfree
!       is modified so that components  of iw from pfree are free.
!       On output, pfree is set equal to the size of iw that
!       would have been needed for no compressions to occur.  If
!       ncmpa is zero, then pfree (on output) is less than or equal to
!       iwlen, and the space iw (pfree+1 ... iwlen) was not used.
!       Otherwise, pfree (on output) is greater than iwlen, and all the
!       memory in iw was used.
! nv:   On input, encoding of compressed graph:
!       if nv(1) = -1 then graph is not compressed otherwise
!       nv(I) holds the weight of node I. 
!       During execution, abs (nv (i)) is equal to the number of rows
!       that are represented by the principal supervariable i.  If i is
!       a nonprincipal variable, then nv (i) = 0.  
!       nv (i) .lt. 0 signifies that i is a
!       principal variable in the pattern Lme of the current pivot
!       element me.  
!       On output, nv (e) holds the true degree of element
!       e at the time it was created (including the diagonal part).
!-----------------------------------------------------------------------
! INPUT/MODIFIED (undefined on output):
!-----------------------------------------------------------------------
! len:  On input, len (i) holds the number of entries in row i of the
!       matrix, excluding the diagonal.  The contents of len (1..n)
!       are undefined on output.
! iw:   On input, iw (1..pfree-1) holds the description of each row i
!       in the matrix.  The matrix must be symmetric, and both upper
!       and lower triangular parts must be present.  The diagonal must
!       not be present.  
!       Row i is held as follows:
!               len (i):  the length of the row i data structure
!               iw (pe (i) ... pe (i) + len (i) - 1):
!                       the list of column indices for nonzeros
!                       in row i (simple supervariables), excluding
!                       the diagonal.  All supervariables start with
!                       one row/column each (supervariable i is just
!                       row i).
!               if len (i) is zero on input, then pe (i) is ignored
!               on input.
!
!               Note that the rows need not be in any particular order,
!               and there may be empty space between the rows.
!
!       During execution, the supervariable i experiences fill-in.
!       This is represented by placing in i a list of the elements
!       that cause fill-in in supervariable i:
!
!               len (i):  the length of supervariable i
!               iw (pe (i) ... pe (i) + elen (i) - 1):
!                       the list of elements that contain i.  This list
!                       is kept short by removing absorbed elements.
!               iw (pe (i) + elen (i) ... pe (i) + len (i) - 1):
!                       the list of supervariables in i.  This list
!                       is kept short by removing nonprincipal
!                       variables, and any entry j that is also
!                       contained in at least one of the elements
!                       (j in Le) in the list for i (e in row i).
!
!       When supervariable i is selected as pivot, we create an
!       element e of the same name (e=i):
!
!               len (e):  the length of element e
!               iw (pe (e) ... pe (e) + len (e) - 1):
!                       the list of supervariables in element e.
!
!       An element represents the fill-in that occurs when supervariable
!       i is selected as pivot (which represents the selection of row i
!       and all non-principal variables whose principal variable is i).
!       We use the term Le to denote the set of all supervariables
!       in element e.  Absorbed supervariables and elements are pruned
!       from these lists when computationally convenient.
!
!       CAUTION:  THE INPUT MATRIX IS OVERWRITTEN DURING COMPUTATION.
!       The contents of iw are undefined on output.
!-----------------------------------------------------------------------
! OUTPUT (need not be set on input):
!-----------------------------------------------------------------------
! elen: 
!       See the description of iw above.  At the start of execution,
!       elen (i) is set to zero.  During execution, elen (i) is the
!       number of elements in the list for supervariable i.  When e
!       becomes an element, elen (e) = -nel is set, where nel is the
!       current step of factorization.  elen (i) = 0 is done when i
!       becomes nonprincipal.
!
!       For variables, elen (i) .ge. 0 holds until just before the
!       permutation vectors are computed.  For elements,
!       elen (e) .lt. 0 holds.
!
!       On output elen (1..n) holds the inverse permutation (the same
!       as the 'INVP' argument in Sparspak).  That is, if k = elen (i),
!       then row i is the kth pivot row.  Row i of A appears as the
!       (elen(i))-th row in the permuted matrix, PAP^T.
! last: 
!       In a degree list, last (i) is the supervariable preceding i,
!       or zero if i is the head of the list.  In a hash bucket,
!       last (i) is the hash key for i.  last (head (hash)) is also
!       used as the head of a hash bucket if head (hash) contains a
!       degree list (see head, below).
!
!       On output, last (1..n) holds the permutation (the same as the
!       'PERM' argument in Sparspak).  That is, if i = last (k), then
!       row i is the kth pivot row.  Row last (k) of A is the k-th row
!       in the permuted matrix, PAP^T.
! ncmpa:        The number of times iw was compressed.  If this is
!       excessive, then the execution took longer than what could have
!       been.  To reduce ncmpa, try increasing iwlen to be 10% or 20%
!       larger than the value of pfree on input (or at least
!       iwlen .ge. pfree + n).  The fastest performance will be
!       obtained when ncmpa is returned as zero.  If iwlen is set to
!       the value returned by pfree on *output*, then no compressions
!       will occur.
!-----------------------------------------------------------------------
! LOCAL (not input or output - used only during execution):
!-----------------------------------------------------------------------
! degree:       If i is a supervariable, then degree (i) holds the
!       current approximation of the external degree of row i (an upper
!       bound).  The external degree is the number of nonzeros in row i,
!       minus abs (nv (i)) (the diagonal part).  The bound is equal to
!       the external degree if elen (i) is less than or equal to two.
!
!       We also use the term "external degree" for elements e to refer
!       to |Le \ Lme|.  If e is an element, then degree (e) holds |Le|,
!       which is the degree of the off-diagonal part of the element e
!       (not including the diagonal part).
! head: head is used for degree lists.  head (deg) is the first
!       supervariable in a degree list (all supervariables i in a
!       degree list deg have the same approximate degree, namely,
!       deg = degree (i)).  If the list deg is empty then
!       head (deg) = 0.
!
!       During supervariable detection head (hash) also serves as a
!       pointer to a hash bucket.
!       If head (hash) .gt. 0, there is a degree list of degree hash.
!               The hash bucket head pointer is last (head (hash)).
!       If head (hash) = 0, then the degree list and hash bucket are
!               both empty.
!       If head (hash) .lt. 0, then the degree list is empty, and
!               -head (hash) is the head of the hash bucket.
!       After supervariable detection is complete, all hash buckets
!       are empty, and the (last (head (hash)) = 0) condition is
!       restored for the non-empty degree lists.
! next: next (i) is the supervariable following i in a link list, or
!       zero if i is the last in the list.  Used for two kinds of
!       lists:  degree lists and hash buckets (a supervariable can be
!       in only one kind of list at a time).
! w:    The flag array w determines the status of elements and
!       variables, and the external degree of elements.
!
!       for elements:
!          if w (e) = 0, then the element e is absorbed
!          if w (e) .ge. wflg, then w (e) - wflg is the size of
!               the set |Le \ Lme|, in terms of nonzeros (the
!               sum of abs (nv (i)) for each principal variable i that
!               is both in the pattern of element e and NOT in the
!               pattern of the current pivot element, me).
!          if wflg .gt. w (e) .gt. 0, then e is not absorbed and has
!               not yet been seen in the scan of the element lists in
!               the computation of |Le\Lme| in loop 150 below.
!
!       for variables:
!          during supervariable detection, if w (j) .ne. wflg then j is
!          not in the pattern of variable i
!
!       The w array is initialized by setting w (i) = 1 for all i,
!       and by setting wflg = 2.  It is reinitialized if wflg becomes
!       too large (to ensure that wflg+n does not cause integer
!       overflow).
!-----------------------------------------------------------------------
! LOCAL INTEGERS:
!-----------------------------------------------------------------------
INTEGER   :: DEG, DEGME, DEXT, DMAX, E, ELENME, ELN, I, &
  &        ILAST, INEXT, J, JLAST, JNEXT, K, KNT1, KNT2, KNT3, &
  &        LENJ, LN,  ME,  MINDEG, NEL, &
  &        NLEFT, NVI, NVJ, NVPIV, SLENME, WE, WFLG, WNVI, X
INTEGER KNT1_UPDATED, KNT2_UPDATED
INTEGER(8) :: MAXMEM, MEM, NEWMEM
INTEGER    :: MAXINT_N
INTEGER(8) :: HASH, HMOD
! deg:        the degree of a variable or element
! degme:      size, |Lme|, of the current element, me (= degree (me))
! dext:       external degree, |Le \ Lme|, of some element e
! dmax:       largest |Le| seen so far
! e:          an element
! elenme:     the length, elen (me), of element list of pivotal var.
! eln:        the length, elen (...), of an element list
! hash:       the computed value of the hash function
! hmod:       the hash function is computed modulo hmod = max (1,n-1)
! i:          a supervariable
! ilast:      the entry in a link list preceding i
! inext:      the entry in a link list following i
! j:          a supervariable
! jlast:      the entry in a link list preceding j
! jnext:      the entry in a link list, or path, following j
! k:          the pivot order of an element or variable
! knt1:       loop counter used during element construction
! knt2:       loop counter used during element construction
! knt3:       loop counter used during compression
! lenj:       len (j)
! ln:         length of a supervariable list
! maxint_n    large integer to test risk of overflow on wflg
! maxmem:     amount of memory needed for no compressions
! me:         current supervariable being eliminated, and the
!                     current element created by eliminating that
!                     supervariable
! mem:        memory in use assuming no compressions have occurred
! mindeg:     current minimum degree
! nel:        number of pivots selected so far
! newmem:     amount of new memory needed for current pivot element
! nleft:      n - nel, the number of nonpivotal rows/columns remaining
! nvi:        the number of variables in a supervariable i (= nv (i))
! nvj:        the number of variables in a supervariable j (= nv (j))
! nvpiv:      number of pivots in current element
! slenme:     number of variables in variable list of pivotal variable
! we:         w (e)
! wflg:       used for flagging the w array.  See description of iw.
! wnvi:       wflg - nv (i)
! x:          either a supervariable or an element
!-----------------------------------------------------------------------
! LOCAL POINTERS:
!-----------------------------------------------------------------------
INTEGER(8) P, P1, P2, P3, PDST, PEND, PJ, PME, &
  &           PME1, PME2, PN, PSRC
!             Any parameter (pe (...) or pfree) or local variable
!             starting with "p" (for Pointer) is an index into iw,
!             and all indices into iw use variables starting with
!             "p."  The only exception to this rule is the iwlen
!             input argument.
! p:          pointer into lots of things
! p1:         pe (i) for some variable i (start of element list)
! p2:         pe (i) + elen (i) -  1 for some var. i (end of el. list)
! p3:         index of first supervariable in clean list
! pdst:       destination pointer, for compression
! pend:       end of memory to compress
! pj:         pointer into an element or variable
! pme:        pointer into the current element (pme1...pme2)
! pme1:       the current element, me, is stored in iw (pme1...pme2)
! pme2:       the end of the current element
! pn:         pointer into a "clean" variable, also used to compress
! psrc:       source pointer, for compression
LOGICAL COMPRESS
!-----------------------------------------------------------------------
!  FUNCTIONS CALLED:
!-----------------------------------------------------------------------
INTRINSIC max, min, mod
!=======================================================================
!  INITIALIZATIONS
!=======================================================================
WFLG = 2
MAXINT_N=huge(WFLG)-N
MINDEG = 1
NCMPA = 0
NEL = 0
HMOD = int(max (1, N-1),kind=8)
DMAX = 0
MEM = PFREE - 1
MAXMEM = MEM
DO I = 1, N
  LAST (I) = 0
  HEAD (I) = 0
  W (I) = 1
  ELEN (I) = 0
ENDDO
DO I = 1, TOTEL
  HEAD(I) = 0
ENDDO
IF(NV(1) .LT. 0) THEN
   COMPRESS = .FALSE.
ELSE
   COMPRESS = .TRUE.
ENDIF
IF (COMPRESS) THEN
   DO I=1,N
      DEGREE(I) = 0
      DO P= PE(I) , PE(I)+int(LEN(I)-1,8)
         DEGREE(I) = DEGREE(I) + NV(IW(P))
      ENDDO
   ENDDO
ELSE
   DO I=1,N
      NV(I) = 1
      DEGREE (I) = LEN (I)
   ENDDO
ENDIF
!
!     ----------------------------------------------------------------
!     initialize degree lists and eliminate rows with no off-diag. nz.
!     ----------------------------------------------------------------
DO 20 I = 1, N
  DEG = DEGREE (I)
  IF (DEG .GT. 0) THEN
!         ----------------------------------------------------------
!         place i in the degree list corresponding to its degree
!         ----------------------------------------------------------
    INEXT = HEAD (DEG)
    IF (INEXT .NE. 0) LAST (INEXT) = I
    NEXT (I) = INEXT
    HEAD (DEG) = I
  ELSE
!         ----------------------------------------------------------
!         we have a variable that can be eliminated at once because
!         there is no off-diagonal non-zero in its row.
!         ----------------------------------------------------------
      NEL = NEL + NV(I)
    ELEN (I) = -NEL
    PE (I) = 0
    W (I) = 0
  ENDIF
20 CONTINUE
! =====================================================================
!  WHILE (selecting pivots) DO
! =====================================================================
30 IF (NEL .LT. TOTEL) THEN
! =====================================================================
!  GET PIVOT OF MINIMUM DEGREE
! ======================================================================
!       -------------------------------------------------------------
!       find next supervariable for elimination
!       -------------------------------------------------------------
  DO 40 DEG = MINDEG, TOTEL
    ME = HEAD (DEG)
    IF (ME .GT. 0) GO TO 50
40   CONTINUE
50   MINDEG = DEG
!       -------------------------------------------------------------
!       remove chosen variable from link list
!       -------------------------------------------------------------
  INEXT = NEXT (ME)
  IF (INEXT .NE. 0) LAST (INEXT) = 0
  HEAD (DEG) = INEXT
!       -------------------------------------------------------------
!       me represents the elimination of pivots nel+1 to nel+nv(me).
!       place me itself as the first in this set.  It will be moved
!       to the nel+nv(me) position when the permutation vectors are
!       computed.
!       -------------------------------------------------------------
  ELENME = ELEN (ME)
  ELEN (ME) = - (NEL + 1)
  NVPIV = NV (ME)
  NEL = NEL + NVPIV
!=======================================================================
!  CONSTRUCT NEW ELEMENT
!=======================================================================
!       -------------------------------------------------------------
!       At this point, me is the pivotal supervariable.  It will be
!       converted into the current element.  Scan list of the
!       pivotal supervariable, me, setting tree pointers and
!       constructing new list of supervariables for the new element,
!       me.  p is a pointer to the current position in the old list.
!       -------------------------------------------------------------
!       flag the variable "me" as being in Lme by negating nv (me)
  NV (ME) = -NVPIV
  DEGME = 0
  IF (ELENME .EQ. 0) THEN
!         ----------------------------------------------------------
!         construct the new element in place
!         ----------------------------------------------------------
    PME1 = PE (ME)
    PME2 = PME1 - 1
    DO 60 P = PME1, PME1 + LEN (ME) - 1
      I = IW (P)
      NVI = NV (I)
      IF (NVI .GT. 0) THEN
!             ----------------------------------------------------
!             i is a principal variable not yet placed in Lme.
!             store i in new list
!             ----------------------------------------------------
        DEGME = DEGME + NVI
!             flag i as being in Lme by negating nv (i)
        NV (I) = -NVI
        PME2 = PME2 + 1
        IW (PME2) = I
!             ----------------------------------------------------
!             remove variable i from degree list.
!             ----------------------------------------------------
        ILAST = LAST (I)
        INEXT = NEXT (I)
        IF (INEXT .NE. 0) LAST (INEXT) = ILAST
        IF (ILAST .NE. 0) THEN
          NEXT (ILAST) = INEXT
        ELSE
!               i is at the head of the degree list
          HEAD (DEGREE (I)) = INEXT
        ENDIF
      ENDIF
60     CONTINUE
!         this element takes no new memory in iw:
    NEWMEM = 0
  ELSE
!         ----------------------------------------------------------
!         construct the new element in empty space, iw (pfree ...)
!         ----------------------------------------------------------
    P = PE (ME)
    PME1 = PFREE
    SLENME = LEN (ME) - ELENME
    KNT1_UPDATED = 0
    DO 120 KNT1 = 1, ELENME + 1
      KNT1_UPDATED = KNT1_UPDATED +1
      IF (KNT1 .GT. ELENME) THEN
!             search the supervariables in me.
        E = ME
        PJ = P
        LN = SLENME
      ELSE
!             search the elements in me.
        E = IW (P)
        P = P + 1
        PJ = PE (E)
        LN = LEN (E)
      ENDIF
!           -------------------------------------------------------
!           search for different supervariables and add them to the
!           new list, compressing when necessary. this loop is
!           executed once for each element in the list and once for
!           all the supervariables in the list.
!           -------------------------------------------------------
      KNT2_UPDATED = 0
      DO 110 KNT2 = 1, LN
        KNT2_UPDATED = KNT2_UPDATED+1
        I = IW (PJ)
        PJ = PJ + 1
        NVI = NV (I)
        IF (NVI .GT. 0) THEN
!               -------------------------------------------------
!               compress iw, if necessary
!               -------------------------------------------------
          IF (PFREE .GT. IWLEN) THEN
!                 prepare for compressing iw by adjusting
!                 pointers and lengths so that the lists being
!                 searched in the inner and outer loops contain
!                 only the remaining entries.
            PE (ME) = P
            LEN (ME) = LEN (ME) - KNT1_UPDATED
!                 Reset KNT1_UPDATED in case of recompress 
!                 at same iteration of the loop 120
            KNT1_UPDATED = 0
!                 Check if anything left in supervariable ME
            IF (LEN (ME) .EQ. 0) PE (ME) = 0
            PE (E) = PJ
            LEN (E) = LN - KNT2_UPDATED
!                 Reset KNT2_UPDATED in case of recompress 
!                 at same iteration of the loop 110
            KNT2_UPDATED = 0
!                 Check if anything left in element E
            IF (LEN (E) .EQ. 0) PE (E) = 0
            NCMPA = NCMPA + 1
!                 store first item in pe
!                 set first entry to -item
            DO 70 J = 1, N
              PN = PE (J)
              IF (PN .GT. 0) THEN
                PE (J) = int(IW (PN), 8)
                IW (PN) = -J
              ENDIF
70             CONTINUE
!                 psrc/pdst point to source/destination
            PDST = 1
            PSRC = 1
            PEND = PME1 - 1
!                 while loop:
80             CONTINUE
            IF (PSRC .LE. PEND) THEN
!                   search for next negative entry
              J = -IW (PSRC)
              PSRC = PSRC + 1
              IF (J .GT. 0) THEN
                IW (PDST) = int(PE (J))
                PE (J) = PDST
                PDST = PDST + 1
!                     copy from source to destination
                LENJ = LEN (J)
                DO 90 KNT3 = 0, LENJ - 2
                  IW (PDST + KNT3) = IW (PSRC + KNT3)
90                 CONTINUE
                PDST = PDST + LENJ - 1
                PSRC = PSRC + LENJ - 1
              ENDIF
              GO TO 80
            ENDIF
!                 move the new partially-constructed element
            P1 = PDST
            DO 100 PSRC = PME1, PFREE - 1
              IW (PDST) = IW (PSRC)
              PDST = PDST + 1
100             CONTINUE
            PME1 = P1
            PFREE = PDST
            PJ = PE (E)
            P = PE (ME)
          ENDIF
!               -------------------------------------------------
!               i is a principal variable not yet placed in Lme
!               store i in new list
!               -------------------------------------------------
          DEGME = DEGME + NVI
!               flag i as being in Lme by negating nv (i)
          NV (I) = -NVI
          IW (PFREE) = I
          PFREE = PFREE + 1
!               -------------------------------------------------
!               remove variable i from degree link list
!               -------------------------------------------------
          ILAST = LAST (I)
          INEXT = NEXT (I)
          IF (INEXT .NE. 0) LAST (INEXT) = ILAST
          IF (ILAST .NE. 0) THEN
            NEXT (ILAST) = INEXT
          ELSE
!                 i is at the head of the degree list
            HEAD (DEGREE (I)) = INEXT
          ENDIF
        ENDIF
110       CONTINUE
      IF (E .NE. ME) THEN
!             set tree pointer and flag to indicate element e is
!             absorbed into new element me (the parent of e is me)
        PE (E) = int(-ME,8)
        W (E) = 0
      ENDIF
120     CONTINUE
    PME2 = PFREE - 1
!         this element takes newmem new memory in iw (possibly zero)
    NEWMEM = PFREE - PME1
    MEM = MEM + NEWMEM
    MAXMEM = max (MAXMEM, MEM)
  ENDIF
!       -------------------------------------------------------------
!       me has now been converted into an element in iw (pme1..pme2)
!       -------------------------------------------------------------
!       degme holds the external degree of new element
  DEGREE (ME) = DEGME
  PE (ME) = PME1
  LEN (ME) = int(PME2 - PME1 + 1)
!       -------------------------------------------------------------
!       make sure that wflg is not too large.  With the current
!       value of wflg, wflg+n must not cause integer overflow
!       -------------------------------------------------------------
  IF (WFLG .GT. MAXINT_N) THEN
    DO 130 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
130     CONTINUE
    WFLG = 2
  ENDIF
!=======================================================================
!  COMPUTE (w (e) - wflg) = |Le\Lme| FOR ALL ELEMENTS
!=======================================================================
!       -------------------------------------------------------------
!       Scan 1:  compute the external degrees of previous elements
!       with respect to the current element.  That is:
!            (w (e) - wflg) = |Le \ Lme|
!       for each element e that appears in any supervariable in Lme.
!       The notation Le refers to the pattern (list of
!       supervariables) of a previous element e, where e is not yet
!       absorbed, stored in iw (pe (e) + 1 ... pe (e) + iw (pe (e))).
!       The notation Lme refers to the pattern of the current element
!       (stored in iw (pme1..pme2)).   If (w (e) - wflg) becomes
!       zero, then the element e will be absorbed in scan 2.
!       -------------------------------------------------------------
  DO 150 PME = PME1, PME2
    I = IW (PME)
    ELN = ELEN (I)
    IF (ELN .GT. 0) THEN
!           note that nv (i) has been negated to denote i in Lme:
      NVI = -NV (I)
      WNVI = WFLG - NVI
      DO 140 P = PE (I), PE (I) + ELN - 1
        E = IW (P)
        WE = W (E)
        IF (WE .GE. WFLG) THEN
!               unabsorbed element e has been seen in this loop
          WE = WE - NVI
        ELSE IF (WE .NE. 0) THEN
!               e is an unabsorbed element
!               this is the first we have seen e in all of Scan 1
          WE = DEGREE (E) + WNVI
        ENDIF
        W (E) = WE
140       CONTINUE
    ENDIF
150   CONTINUE
!=======================================================================
!  DEGREE UPDATE AND ELEMENT ABSORPTION
!=======================================================================
!       -------------------------------------------------------------
!       Scan 2:  for each i in Lme, sum up the degree of Lme 
!       (which is degme), 
!       plus the sum of the external degrees of each Le
!       for the elements e appearing within i, plus the
!       supervariables in i.  Place i in hash list.
!       -------------------------------------------------------------
  DO 180 PME = PME1, PME2
    I = IW (PME)
    P1 = PE (I)
    P2 = P1 + ELEN (I) - 1
    PN = P1
    HASH = 0_8
    DEG = 0
!         ----------------------------------------------------------
!         scan the element list associated with supervariable i
!         ----------------------------------------------------------
    DO 160 P = P1, P2
      E = IW (P)
!           dext = | Le \ Lme |
      DEXT = W (E) - WFLG
      IF (DEXT .GT. 0) THEN
        DEG = DEG + DEXT
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + int(E,kind=8)
      ELSE IF (DEXT .EQ. 0) THEN
#if  defined (NOAGG1)
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + int(E,kind=8)
#else
!             aggressive absorption: e is not adjacent to me, but
!             the |Le \ Lme| is 0, so absorb it into me
        PE (E) = int(-ME,8)
        W (E) = 0
#endif
      ENDIF
160     CONTINUE
!         count the number of elements in i (including me):
    ELEN (I) = int(PN - P1 + 1)
!         ----------------------------------------------------------
!         scan the supervariables in the list associated with i
!         ----------------------------------------------------------
    P3 = PN
    DO 170 P = P2 + 1, P1 + int(LEN (I) - 1,8)
      J = IW (P)
      NVJ = NV (J)
      IF (NVJ .GT. 0) THEN
!             j is unabsorbed, and not in Lme.
!             add to degree and add to new list
        DEG = DEG + NVJ
        IW (PN) = J
        PN = PN + 1
        HASH = HASH + int(J,kind=8)
      ENDIF
170     CONTINUE
!         ----------------------------------------------------------
!         update the degree and check for mass elimination
!         ----------------------------------------------------------
#if  defined (NOAGG1)
    IF (DEG.EQ.0.AND.(ELEN(I).GT.1)) THEN
!         When DEG is zero we need to 
!         absorb in ME all elements adjacent to I 
             P1 = PE (I)
!                  exclude ME --> -2 
             P2 = P1 + int(ELEN (I),8) - 2_8
             DO P =P1,P2
               E      = IW(P)
               PE (E) = int(-ME,8)
               W (E)  = 0
             ENDDO
    ENDIF
!              .... Ready for mass elimination
#endif
    IF (DEG .EQ. 0) THEN
!           -------------------------------------------------------
!           mass elimination
!           -------------------------------------------------------
!           There is nothing left of this node except for an
!           edge to the current pivot element.  elen (i) is 1,
!           and there are no variables adjacent to node i.
!           Absorb i into the current pivot element, me.
      PE (I) = int(-ME,8)
      NVI = -NV (I)
      DEGME = DEGME - NVI
      NVPIV = NVPIV + NVI
      NEL = NEL + NVI
      NV (I) = 0
      ELEN (I) = 0
    ELSE
!           -------------------------------------------------------
!           update the upper-bound degree of i
!           -------------------------------------------------------
!           the following degree does not yet include the size
!           of the current element, which is added later:
      DEGREE (I) = min (DEGREE (I), DEG)
!           -------------------------------------------------------
!           add me to the list for i
!           -------------------------------------------------------
!           move first supervariable to end of list
      IW (PN) = IW (P3)
!           move first element to end of element part of list
      IW (P3) = IW (P1)
!           add new element to front of list.
      IW (P1) = ME
!           store the new length of the list in len (i)
      LEN (I) = int(PN - P1 + 1)
!           -------------------------------------------------------
!           place in hash bucket.  Save hash key of i in last (i).
!           -------------------------------------------------------
      HASH = mod (HASH, HMOD) + 1_8
      J = HEAD (HASH)
      IF (J .LE. 0) THEN
!             the degree list is empty, hash head is -j
        NEXT (I) = -J
        HEAD (HASH) = -I
      ELSE
!             degree list is not empty
!             use last (head (hash)) as hash head
        NEXT (I) = LAST (J)
        LAST (J) = I
      ENDIF
      LAST (I) = int(HASH,kind=kind(LAST))
    ENDIF
180   CONTINUE
  DEGREE (ME) = DEGME
!       -------------------------------------------------------------
!       Clear the counter array, w (...), by incrementing wflg.
!       -------------------------------------------------------------
  DMAX = max (DMAX, DEGME)
  WFLG = WFLG + DMAX
!       make sure that wflg+n does not cause integer overflow
  IF (WFLG .GT. MAXINT_N) THEN
    DO 190 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
190     CONTINUE
    WFLG = 2
  ENDIF
!       at this point, w (1..n) .lt. wflg holds
!=======================================================================
!  SUPERVARIABLE DETECTION
!=======================================================================
  DO 250 PME = PME1, PME2
    I = IW (PME)
    IF (NV (I) .LT. 0) THEN
!           i is a principal variable in Lme
!           -------------------------------------------------------
!           examine all hash buckets with 2 or more variables.  We
!           do this by examing all unique hash keys for super-
!           variables in the pattern Lme of the current element, me
!           -------------------------------------------------------
      HASH = int(LAST (I),kind=8)
!           let i = head of hash bucket, and empty the hash bucket
      J = HEAD (HASH)
      IF (J .EQ. 0) GO TO 250
      IF (J .LT. 0) THEN
!             degree list is empty
        I = -J
        HEAD (HASH) = 0
      ELSE
!             degree list is not empty, restore last () of head
        I = LAST (J)
        LAST (J) = 0
      ENDIF
      IF (I .EQ. 0) GO TO 250
!           while loop:
200       CONTINUE
      IF (NEXT (I) .NE. 0) THEN
!             ----------------------------------------------------
!             this bucket has one or more variables following i.
!             scan all of them to see if i can absorb any entries
!             that follow i in hash bucket.  Scatter i into w.
!             ----------------------------------------------------
        LN = LEN (I)
        ELN = ELEN (I)
!             do not flag the first element in the list (me)
        DO 210 P = PE (I) + 1, PE (I) + LN - 1
          W (IW (P)) = WFLG
210         CONTINUE
!             ----------------------------------------------------
!             scan every other entry j following i in bucket
!             ----------------------------------------------------
        JLAST = I
        J = NEXT (I)
!             while loop:
220         CONTINUE
        IF (J .NE. 0) THEN
!               -------------------------------------------------
!               check if j and i have identical nonzero pattern
!               -------------------------------------------------
!               jump if i and j do not have same size data structure
          IF (LEN (J) .NE. LN) GO TO 240
!               jump if i and j do not have same number adj elts
          IF (ELEN (J) .NE. ELN) GO TO 240
!               do not flag the first element in the list (me)
          DO 230 P = PE (J) + 1, PE (J) + LN - 1
!                 jump if an entry (iw(p)) is in j but not in i
            IF (W (IW (P)) .NE. WFLG) GO TO 240
230           CONTINUE
!               -------------------------------------------------
!               found it!  j can be absorbed into i
!               -------------------------------------------------
          PE (J) = int(-I,8)
!               both nv (i) and nv (j) are negated since they
!               are in Lme, and the absolute values of each
!               are the number of variables in i and j:
          NV (I) = NV (I) + NV (J)
          NV (J) = 0
          ELEN (J) = 0
!               delete j from hash bucket
          J = NEXT (J)
          NEXT (JLAST) = J
          GO TO 220
!               -------------------------------------------------
240           CONTINUE
!               j cannot be absorbed into i
!               -------------------------------------------------
          JLAST = J
          J = NEXT (J)
        GO TO 220
        ENDIF
!             ----------------------------------------------------
!             no more variables can be absorbed into i
!             go to next i in bucket and clear flag array
!             ----------------------------------------------------
        WFLG = WFLG + 1
        I = NEXT (I)
        IF (I .NE. 0) GO TO 200
      ENDIF
    ENDIF
250   CONTINUE
!=======================================================================
!  RESTORE DEGREE LISTS AND REMOVE NONPRINCIPAL SUPERVAR. FROM ELEMENT
!=======================================================================
  P = PME1
  NLEFT = TOTEL - NEL
  DO 260 PME = PME1, PME2
    I = IW (PME)
    NVI = -NV (I)
    IF (NVI .GT. 0) THEN
!           i is a principal variable in Lme
!           restore nv (i) to signify that i is principal
      NV (I) = NVI
!           -------------------------------------------------------
!           compute the external degree (add size of current elem)
!           -------------------------------------------------------
      DEG = min (DEGREE (I) + DEGME - NVI, NLEFT - NVI)
!           -------------------------------------------------------
!           place the supervariable at the head of the degree list
!           -------------------------------------------------------
      INEXT = HEAD (DEG)
      IF (INEXT .NE. 0) LAST (INEXT) = I
      NEXT (I) = INEXT
      LAST (I) = 0
      HEAD (DEG) = I
!           -------------------------------------------------------
!           save the new degree, and find the minimum degree
!           -------------------------------------------------------
      MINDEG = min (MINDEG, DEG)
      DEGREE (I) = DEG
!           -------------------------------------------------------
!           place the supervariable in the element pattern
!           -------------------------------------------------------
      IW (P) = I
      P = P + 1
    ENDIF
260   CONTINUE
!=======================================================================
!  FINALIZE THE NEW ELEMENT
!=======================================================================
  NV (ME) = NVPIV + DEGME
!       nv (me) is now the degree of pivot (including diagonal part)
!       save the length of the list for the new element me
  LEN (ME) = int(P - PME1)
  IF (LEN (ME) .EQ. 0) THEN
!         there is nothing left of the current pivot element
    PE (ME) = 0_8
    W (ME) = 0
  ENDIF
  IF (NEWMEM .NE. 0) THEN
!         element was not constructed in place: deallocate part
!         of it (final size is less than or equal to newmem,
!         since newly nonprincipal variables have been removed).
    PFREE = P
    MEM = MEM - NEWMEM + LEN (ME)
  ENDIF
!=======================================================================
!       END WHILE (selecting pivots)
GO TO 30
ENDIF
!=======================================================================
!=======================================================================
!     COMPUTE THE PERMUTATION VECTORS and update TREE
!=======================================================================
!     ----------------------------------------------------------------
!     The time taken by the following code is O(n).  At this
!     point, elen (e) = -k has been done for all elements e,
!     and elen (i) = 0 has been done for all nonprincipal
!     variables i.  At this point, there are no principal
!     supervariables left, and all elements are absorbed.
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!     compute the ordering of unordered nonprincipal variables
!     ----------------------------------------------------------------
DO 290 I = 1, N
  IF (ELEN (I) .EQ. 0) THEN
!         ----------------------------------------------------------
!         i is an un-ordered row.  Traverse the tree from i until
!         reaching an element, e.  The element, e, was the
!         principal supervariable of i and all nodes in the path
!         from i to when e was selected as pivot.
!         ----------------------------------------------------------
    J = int(-PE (I))
!         while (j is a variable) do:
270     CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        J = int(-PE (J))
        GO TO 270
      ENDIF
      E = J
!           ----------------------------------------------------------
!           get the current pivot ordering of e
!           ----------------------------------------------------------
      K = -ELEN (E)
!           ----------------------------------------------------------
!           traverse the path again from i to e, and compress the
!           path (all nodes point to e).  Path compression allows
!           this code to compute in O(n) time.  Order the unordered
!           nodes in the path, and place the element e at the end.
!           ----------------------------------------------------------
      J = I
!           while (j is a variable) do:
280       CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        JNEXT = int(-PE (J))
        PE (J) = int(-E,8)
        IF (ELEN (J) .EQ. 0) THEN
!               j is an unordered row
          ELEN (J) = K
          K = K + 1
        ENDIF
        J = JNEXT
      GO TO 280
      ENDIF
!         leave elen (e) negative, so we know it is an element
    ELEN (E) = -K
  ENDIF
290 CONTINUE
!
IF (COMPUTE_PERM) THEN
!     ----------------------------------------------------------------
!     reset the inverse permutation (elen (1..n)) to be positive,
!     and compute the permutation (last (1..n)).
!     ----------------------------------------------------------------
IF(COMPRESS) THEN
  LAST(1:N) = 0
  HEAD(1:TOTEL-N)=0
  DO I = 1, N
    K = abs (ELEN (I))
    IF ( K <= N ) THEN
      LAST (K) = I
    ELSE
      HEAD(K-N)=I
    ENDIF
  ENDDO
  I = 1
  DO K = 1, N
    IF(LAST (K) .NE. 0) THEN
      LAST(I) = LAST(K)
      ELEN(LAST(K)) = I
      I = I + 1
    ENDIF
  ENDDO
  DO K = N+1, TOTEL
    IF (HEAD(K-N) .NE. 0) THEN
      LAST(I)=HEAD(K-N)
      ELEN(HEAD(K-N)) = I
      I = I + 1
    ENDIF
  END DO
ELSE
  DO 300 I = 1, N
    K = abs (ELEN (I))
    LAST (K) = I
    ELEN (I) = K
300   CONTINUE
 ENDIF
!=======================================================================
!      END OF COMPUTING PERMUTATIONS
!=======================================================================
 ENDIF
!=======================================================================
!  RETURN THE MEMORY USAGE IN IW
!=======================================================================
!     If maxmem is less than or equal to iwlen, then no compressions
!     occurred, and iw (maxmem+1 ... iwlen) was unused.  Otherwise
!     compressions did occur, and iwlen would have had to have been
!     greater than or equal to maxmem for no compressions to occur.
!     Return the value of maxmem in the pfree argument.
PFREE = MAXMEM
!===============================
!     Save IPE in PARENT array
DO I=1,N
 PARENT(I) = int(PE(I))
ENDDO
!===============================
RETURN
END SUBROUTINE MUMPS_ANA_H
!-----------------------------------------------------------------------
! MUMPS_AMD_ELT: modified version of reference AMD routine MUMPS_ANA_H
! capable of processing already amalgamated or compressed graph. 
! Used within MUMPS process for the elemental input format of matrices
! Input data is in this context modified to be a graph of supervariables.
!   
! Modifications of the interface : 
! ------------------------------
!  INPUT:
!  -----
!     1/ LEN(I) < 0   <=> i is a secondary variable whose principal
!                         variable is -LEN(I)
!     2/ For all secondary variables the adj list MUST not be provided. 
!        THAT is:
!        -------
!           if pe(isecondary) = 0 then 
!                 adjacency list of isecondary is not provided
!           else
!             pe(isecondary) >0 
!             len(isecondary) must be equal to len(iprincipal_associated)
!             then the corresponding space wil not be used and 
!             will be freed by amd if necessary.
!           endif
! REMARK:
! ------
! 1/ N must be still set to the order of the matrix 
!    (not of the amalgamated gragh)
! 2/ For each supervariable S only supervariables adjacent to S are provided
!    len(S) is then the number of such supervariables
!    NV(S) is however updated during the initialisation phase to represent 
!    the size of the supervariable 
!    ( increment nv(s) for each i / len(i) =-s )
! 3/ If (len(i) >=0  for all i ) then we get the classical AMD code
! ------------------
SUBROUTINE MUMPS_AMD_ELT(N,IWLEN, PE, PFREE, LEN, IW, NV, ELEN, &
  &                   LAST, NCMPA, DEGREE, HEAD, NEXT, W, PARENT)
! 
!    Restrictive integer 64 bit variant :
!    it is assumed that IW array size can exceed 32-bit integer
! 
!     Input not modified
INTEGER, INTENT(IN)    :: N
INTEGER(8), INTENT(IN) :: IWLEN
!     Input undefined on output 
INTEGER, INTENT(INOUT)  :: LEN(N), IW(IWLEN)
! 
!     Output only 
INTEGER, INTENT(OUT)   :: NCMPA
INTEGER, INTENT(OUT)   :: NV(N), ELEN(N), LAST(N), PARENT(N)
! 
!     Input/output
INTEGER(8), INTENT(INOUT) :: PFREE
INTEGER(8), INTENT(INOUT) :: PE(N)
!
!     Internal Workspace only
INTEGER NEXT(N), DEGREE(N), HEAD(N), W(N)
!
! Description:
!   Given a representation of the nonzero pattern of a symmetric matrix,
!   A, (excluding the diagonal) perform an approximate minimum
!   degree ordering to compute a pivot order
!   such that fill-in in the Cholesky factors A = LL^T is kept low. 
! ---------------------
! Interface Description
! ---------------------
! INPUT ARGUMENTS (unaltered):
!-----------------------------
! n:    The matrix order.
!
!       Restriction:  n .ge. 1
! iwlen:        The length of iw (1..iwlen).  On input, the matrix is
!       stored in iw (1..pfree-1).  However, iw (1..iwlen) should be
!       slightly larger than what is required to hold the matrix, at
!       least iwlen .ge. pfree + n is recommended.  Otherwise,
!       excessive compressions will take place.
!       *** We do not recommend running this algorithm with ***
!       ***      iwlen .lt. pfree + n.                      ***
!       *** Better performance will be obtained if          ***
!       ***      iwlen .ge. pfree + n                       ***
!       *** or better yet                                   ***
!       ***      iwlen .gt. 1.2 * pfree                     ***
!       *** (where pfree is its value on input).            ***
!       The algorithm will not run at all if iwlen .lt. pfree-1.
!
!       Restriction: iwlen .ge. pfree-1
!-----------------------------------------------------------------------
! INPUT/OUPUT ARGUMENTS:
!-----------------------------------------------------------------------
! pe:   On input, pe (i) is the index in iw of the start of row i, or
!       zero if row i has no off-diagonal non-zeros.
!
!       During execution, it is used for both supervariables and
!       elements:
!
!       * Principal supervariable i:  index into iw of the
!               description of supervariable i.  A supervariable
!               represents one or more rows of the matrix
!               with identical nonzero pattern.
!       * Non-principal supervariable i:  if i has been absorbed
!               into another supervariable j, then pe (i) = -j.
!               That is, j has the same pattern as i.
!               Note that j might later be absorbed into another
!               supervariable j2, in which case pe (i) is still -j,
!               and pe (j) = -j2.
!       * Unabsorbed element e:  the index into iw of the description
!               of element e, if e has not yet been absorbed by a
!               subsequent element.  Element e is created when
!               the supervariable of the same name is selected as
!               the pivot.
!       * Absorbed element e:  if element e is absorbed into element
!               e2, then pe (e) = -e2.  This occurs when the pattern of
!               e (that is, Le) is found to be a subset of the pattern
!               of e2 (that is, Le2).  If element e is "null" (it has
!               no nonzeros outside its pivot block), then pe (e) = 0.
!
!       On output, pe holds the assembly tree/forest, which implicitly
!       represents a pivot order with identical fill-in as the actual
!       order (via a depth-first search of the tree).
!
!       On output:
!       If nv (i) .gt. 0, then i represents a node in the assembly tree,
!       and the parent of i is -pe (i), or zero if i is a root.
!       If nv (i) = 0, then (i,-pe (i)) represents an edge in a
!       subtree, the root of which is a node in the assembly tree.
! 
!       On output:  (PE is copied on output into PARENT array)
! pfree:        On input, the matrix is stored in iw (1..pfree-1) and
!       the rest of the array iw is free.
!       During execution, additional data is placed in iw, and pfree
!       is modified so that components  of iw from pfree are free.
!       On output, pfree is set equal to the size of iw that
!       would have been needed for no compressions to occur.  If
!       ncmpa is zero, then pfree (on output) is less than or equal to
!       iwlen, and the space iw (pfree+1 ... iwlen) was not used.
!       Otherwise, pfree (on output) is greater than iwlen, and all the
!       memory in iw was used.
!-----------------------------------------------------------------------
! INPUT/MODIFIED (undefined on output):
!-----------------------------------------------------------------------
! len:  On input, len (i) holds the number of entries in row i of the
!       matrix, excluding the diagonal.  The contents of len (1..n)
!       are undefined on output.
! iw:   On input, iw (1..pfree-1) holds the description of each row i
!       in the matrix.  The matrix must be symmetric, and both upper
!       and lower triangular parts must be present.  The diagonal must
!       not be present.  Row i is held as follows:
!
!               len (i):  the length of the row i data structure
!               iw (pe (i) ... pe (i) + len (i) - 1):
!                       the list of column indices for nonzeros
!                       in row i (simple supervariables), excluding
!                       the diagonal.  All supervariables start with
!                       one row/column each (supervariable i is just
!                       row i).
!               if len (i) is zero on input, then pe (i) is ignored
!               on input.
!
!               Note that the rows need not be in any particular order,
!               and there may be empty space between the rows.
!
!       During execution, the supervariable i experiences fill-in.
!       This is represented by placing in i a list of the elements
!       that cause fill-in in supervariable i:
!
!               len (i):  the length of supervariable i
!               iw (pe (i) ... pe (i) + elen (i) - 1):
!                       the list of elements that contain i.  This list
!                       is kept short by removing absorbed elements.
!               iw (pe (i) + elen (i) ... pe (i) + len (i) - 1):
!                       the list of supervariables in i.  This list
!                       is kept short by removing nonprincipal
!                       variables, and any entry j that is also
!                       contained in at least one of the elements
!                       (j in Le) in the list for i (e in row i).
!
!       When supervariable i is selected as pivot, we create an
!       element e of the same name (e=i):
!
!               len (e):  the length of element e
!               iw (pe (e) ... pe (e) + len (e) - 1):
!                       the list of supervariables in element e.
!
!       An element represents the fill-in that occurs when supervariable
!       i is selected as pivot (which represents the selection of row i
!       and all non-principal variables whose principal variable is i).
!       We use the term Le to denote the set of all supervariables
!       in element e.  Absorbed supervariables and elements are pruned
!       from these lists when computationally convenient.
!
!       CAUTION:  THE INPUT MATRIX IS OVERWRITTEN DURING COMPUTATION.
!       The contents of iw are undefined on output.
!-----------------------------------------------------------------------
! OUTPUT (need not be set on input):
!-----------------------------------------------------------------------
! nv:   During execution, abs (nv (i)) is equal to the number of rows
!       that are represented by the principal supervariable i.  If i is
!       a nonprincipal variable, then nv (i) = 0.  Initially,
!       nv (i) = 1 for all i.  nv (i) .lt. 0 signifies that i is a
!       principal variable in the pattern Lme of the current pivot
!       element me.  On output, nv (e) holds the true degree of element
!       e at the time it was created (including the diagonal part).
! elen: See the description of iw above.  At the start of execution,
!       elen (i) is set to zero.  During execution, elen (i) is the
!       number of elements in the list for supervariable i.  When e
!       becomes an element, elen (e) = -nel is set, where nel is the
!       current step of factorization.  elen (i) = 0 is done when i
!       becomes nonprincipal.
!
!       For variables, elen (i) .ge. 0 holds until just before the
!       permutation vectors are computed.  For elements,
!       elen (e) .lt. 0 holds.
!
!       On output elen (1..n) holds the inverse permutation (the same
!       as the 'INVP' argument in Sparspak).  That is, if k = elen (i),
!       then row i is the kth pivot row.  Row i of A appears as the
!       (elen(i))-th row in the permuted matrix, PAP^T.
! last: In a degree list, last (i) is the supervariable preceding i,
!       or zero if i is the head of the list.  In a hash bucket,
!       last (i) is the hash key for i.  last (head (hash)) is also
!       used as the head of a hash bucket if head (hash) contains a
!       degree list (see head, below).
!
!       On output, last (1..n) holds the permutation (the same as the
!       'PERM' argument in Sparspak).  That is, if i = last (k), then
!       row i is the kth pivot row.  Row last (k) of A is the k-th row
!       in the permuted matrix, PAP^T.
! ncmpa:        The number of times iw was compressed.  If this is
!       excessive, then the execution took longer than what could have
!       been.  To reduce ncmpa, try increasing iwlen to be 10% or 20%
!       larger than the value of pfree on input (or at least
!       iwlen .ge. pfree + n).  The fastest performance will be
!       obtained when ncmpa is returned as zero.  If iwlen is set to
!       the value returned by pfree on *output*, then no compressions
!       will occur.
!-----------------------------------------------------------------------
! LOCAL (not input or output - used only during execution):
!-----------------------------------------------------------------------
! degree:       If i is a supervariable, then degree (i) holds the
!       current approximation of the external degree of row i (an upper
!       bound).  The external degree is the number of nonzeros in row i,
!       minus abs (nv (i)) (the diagonal part).  The bound is equal to
!       the external degree if elen (i) is less than or equal to two.
!
!       We also use the term "external degree" for elements e to refer
!       to |Le \ Lme|.  If e is an element, then degree (e) holds |Le|,
!       which is the degree of the off-diagonal part of the element e
!       (not including the diagonal part).
! head: head is used for degree lists.  head (deg) is the first
!       supervariable in a degree list (all supervariables i in a
!       degree list deg have the same approximate degree, namely,
!       deg = degree (i)).  If the list deg is empty then
!       head (deg) = 0.
!
!       During supervariable detection head (hash) also serves as a
!       pointer to a hash bucket.
!       If head (hash) .gt. 0, there is a degree list of degree hash.
!               The hash bucket head pointer is last (head (hash)).
!       If head (hash) = 0, then the degree list and hash bucket are
!               both empty.
!       If head (hash) .lt. 0, then the degree list is empty, and
!               -head (hash) is the head of the hash bucket.
!       After supervariable detection is complete, all hash buckets
!       are empty, and the (last (head (hash)) = 0) condition is
!       restored for the non-empty degree lists.
! next: next (i) is the supervariable following i in a link list, or
!       zero if i is the last in the list.  Used for two kinds of
!       lists:  degree lists and hash buckets (a supervariable can be
!       in only one kind of list at a time).
! w:    The flag array w determines the status of elements and
!       variables, and the external degree of elements.
!
!       for elements:
!          if w (e) = 0, then the element e is absorbed
!          if w (e) .ge. wflg, then w (e) - wflg is the size of
!               the set |Le \ Lme|, in terms of nonzeros (the
!               sum of abs (nv (i)) for each principal variable i that
!               is both in the pattern of element e and NOT in the
!               pattern of the current pivot element, me).
!          if wflg .gt. w (e) .gt. 0, then e is not absorbed and has
!               not yet been seen in the scan of the element lists in
!               the computation of |Le\Lme| in loop 150 below.
!
!       for variables:
!          during supervariable detection, if w (j) .ne. wflg then j is
!          not in the pattern of variable i
!
!       The w array is initialized by setting w (i) = 1 for all i,
!       and by setting wflg = 2.  It is reinitialized if wflg becomes
!       too large (to ensure that wflg+n does not cause integer
!       overflow).
!-----------------------------------------------------------------------
! LOCAL INTEGERS:
!-----------------------------------------------------------------------
INTEGER :: DEG, DEGME, DEXT, DMAX, E, ELENME, ELN, I, &
  &        ILAST, INEXT, J, JLAST, JNEXT, K, KNT1, KNT2, KNT3, &
  &        LENJ, LN, ME, MINDEG, NEL, &
  &        NLEFT, NVI, NVJ, NVPIV, SLENME, WE, WFLG, WNVI, X, &
  &        NPRINC
INTEGER KNT1_UPDATED, KNT2_UPDATED
INTEGER(8) :: MAXMEM, MEM, NEWMEM
INTEGER    :: MAXINT_N
INTEGER(8) :: HASH, HMOD
! deg:        the degree of a variable or element
! degme:      size, |Lme|, of the current element, me (= degree (me))
! dext:       external degree, |Le \ Lme|, of some element e
! dmax:       largest |Le| seen so far
! e:          an element
! elenme:     the length, elen (me), of element list of pivotal var.
! eln:        the length, elen (...), of an element list
! hash:       the computed value of the hash function
! hmod:       the hash function is computed modulo hmod = max (1,n-1)
! i:          a supervariable
! ilast:      the entry in a link list preceding i
! inext:      the entry in a link list following i
! j:          a supervariable
! jlast:      the entry in a link list preceding j
! jnext:      the entry in a link list, or path, following j
! k:          the pivot order of an element or variable
! knt1:       loop counter used during element construction
! knt2:       loop counter used during element construction
! knt3:       loop counter used during compression
! lenj:       len (j)
! ln:         length of a supervariable list
! maxint_n    large integer to test risk of overflow on wflg
! maxmem:     amount of memory needed for no compressions
! me:         current supervariable being eliminated, and the
!                     current element created by eliminating that
!                     supervariable
! mem:        memory in use assuming no compressions have occurred
! mindeg:     current minimum degree
! nel:        number of pivots selected so far
! newmem:     amount of new memory needed for current pivot element
! nleft:      n - nel, the number of nonpivotal rows/columns remaining
! nvi:        the number of variables in a supervariable i (= nv (i))
! nvj:        the number of variables in a supervariable j (= nv (j))
! nvpiv:      number of pivots in current element
! slenme:     number of variables in variable list of pivotal variable
! we:         w (e)
! wflg:       used for flagging the w array.  See description of iw.
! wnvi:       wflg - nv (i)
! x:          either a supervariable or an element
! nprinc :    number of principal variables = number of varialbles
!             of the compressed graph.
!             (if the graph is not compressed then nprinc = n)
!-----------------------------------------------------------------------
! LOCAL POINTERS:
!-----------------------------------------------------------------------
INTEGER(8) :: P, P1, P2, P3, PDST, PEND, PJ, PME, PME1, PME2, &
  &              PN, PSRC
!             Any parameter (pe (...) or pfree) or local variable
!             starting with "p" (for Pointer) is an index into iw,
!             and all indices into iw use variables starting with
!             "p."  The only exception to this rule is the iwlen
!             input argument.
! p:          pointer into lots of things
! p1:         pe (i) for some variable i (start of element list)
! p2:         pe (i) + elen (i) -  1 for some var. i (end of el. list)
! p3:         index of first supervariable in clean list
! pdst:       destination pointer, for compression
! pend:       end of memory to compress
! pj:         pointer into an element or variable
! pme:        pointer into the current element (pme1...pme2)
! pme1:       the current element, me, is stored in iw (pme1...pme2)
! pme2:       the end of the current element
! pn:         pointer into a "clean" variable, also used to compress
! psrc:       source pointer, for compression
!-----------------------------------------------------------------------
!  FUNCTIONS CALLED:
!-----------------------------------------------------------------------
INTRINSIC max, min, mod
!=======================================================================
!  INITIALIZATIONS
!=======================================================================
WFLG = 2
MAXINT_N=huge(WFLG)-N
MINDEG = 1
NCMPA = 0
NEL = 0
HMOD = int(max (1, N-1),kind=8)
DMAX = 0
MEM = PFREE - 1
MAXMEM = MEM
NPRINC = 0
DO I = 1, N
  LAST (I) = 0
  HEAD (I) = 0
  NV (I) = 1
  W (I) = 1
  ELEN (I) = 0
ENDDO
DO I=1, N
  IF (LEN (I).GE.0) THEN
     DEGREE (I) = LEN (I)
     NPRINC = NPRINC + 1
  ELSE
!          i is a secondary variable belonging 
!          to supervariable j=-len (i)
     J        = -LEN (I)
!          used only to skip secondary variables in loop 20
     DEGREE (I) = - 1
     IF ( PE(I) .NE. 0_8 ) THEN
!            adjacency list of secondary variable was 
!            provided by the user, 
!            the space will be compressed if necessary
       LEN (I) = LEN(J)
     ELSE
       LEN (I) = 0
     ENDIF
     PE (I)   = int(-J,8)
     NV (J)   = NV (J) + NV (I)
     NV (I)   = 0
     ELEN (I) = 0
  ENDIF
ENDDO
!     ----------------------------------------------------------------
!     initialize degree lists and eliminate rows with no off-diag. nz.
!     ----------------------------------------------------------------
DO 20 I = 1, N
  DEG = DEGREE (I)
!       degree(i) < 0 corresponds to secondary variables
!       that need be skipped.
  IF (DEG .GT. 0) THEN
!         ----------------------------------------------------------
!         place i in the degree list corresponding to its degree
!         ----------------------------------------------------------
    INEXT = HEAD (DEG)
    IF (INEXT .NE. 0) LAST (INEXT) = I
    NEXT (I) = INEXT
    HEAD (DEG) = I
  ELSE IF ( DEG.EQ. 0) THEN
!         ----------------------------------------------------------
!         we have a variable that can be eliminated at once because
!         there is no off-diagonal non-zero in its row.
!         ----------------------------------------------------------
!
!         We have a graph of supervariable and thus need to update 
!         singleton that might already be supervariables with nv(i)
!         When a supervariable is eliminated its 
!        principal variable must be set to the current step
!         (NEL+1) which must be stored (negated) in ELEN  
!         ONLY THEN (current step) NEL should be incremented.
!         This will be exploited when computing the global ordering
!         of all (secondary and principal) variables at the end of the AMD routine.
    ELEN (I) = - (NEL + 1)
    NEL = NEL + NV(I)
    PE (I) = 0_8
    W (I) = 0
  ENDIF
20 CONTINUE
!=======================================================================
!  WHILE (selecting pivots) DO
!=======================================================================
!
!  Note that we do want to loop until NEL = N since
!  we update NEL with the size of the eliminated supervariable
!  
30 IF (NEL .LT. N) THEN
!=======================================================================
!  GET PIVOT OF MINIMUM DEGREE
!=======================================================================
!       -------------------------------------------------------------
!       find next supervariable for elimination
!       -------------------------------------------------------------
  DO 40 DEG = MINDEG, N
    ME = HEAD (DEG)
    IF (ME .GT. 0) GO TO 50
40   CONTINUE
50   MINDEG = DEG
!       -------------------------------------------------------------
!       remove chosen variable from link list
!       -------------------------------------------------------------
  INEXT = NEXT (ME)
  IF (INEXT .NE. 0) LAST (INEXT) = 0
  HEAD (DEG) = INEXT
!       -------------------------------------------------------------
!       me represents the elimination of pivots nel+1 to nel+nv(me).
!       place me itself as the first in this set.  It will be moved
!       to the nel+nv(me) position when the permutation vectors are
!       computed.
!       -------------------------------------------------------------
  ELENME = ELEN (ME)
  ELEN (ME) = - (NEL + 1)
  NVPIV = NV (ME)
  NEL = NEL + NVPIV
!=======================================================================
!  CONSTRUCT NEW ELEMENT
!=======================================================================
!       -------------------------------------------------------------
!       At this point, me is the pivotal supervariable.  It will be
!       converted into the current element.  Scan list of the
!       pivotal supervariable, me, setting tree pointers and
!       constructing new list of supervariables for the new element,
!       me.  p is a pointer to the current position in the old list.
!       -------------------------------------------------------------
!       flag the variable "me" as being in Lme by negating nv (me)
  NV (ME) = -NVPIV
  DEGME = 0
  IF (ELENME .EQ. 0) THEN
!         ----------------------------------------------------------
!         construct the new element in place
!         ----------------------------------------------------------
    PME1 = PE (ME)
    PME2 = PME1 - 1
    DO 60 P = PME1, PME1 + int(LEN (ME) - 1,8)
      I = IW (P)
      NVI = NV (I)
      IF (NVI .GT. 0) THEN
!             ----------------------------------------------------
!             i is a principal variable not yet placed in Lme.
!             store i in new list
!             ----------------------------------------------------
        DEGME = DEGME + NVI
!             flag i as being in Lme by negating nv (i)
        NV (I) = -NVI
        PME2 = PME2 + 1_8
        IW (PME2) = I
!             ----------------------------------------------------
!             remove variable i from degree list.
!             ----------------------------------------------------
        ILAST = LAST (I)
        INEXT = NEXT (I)
        IF (INEXT .NE. 0) LAST (INEXT) = ILAST
        IF (ILAST .NE. 0) THEN
          NEXT (ILAST) = INEXT
        ELSE
!               i is at the head of the degree list
          HEAD (DEGREE (I)) = INEXT
        ENDIF
      ENDIF
60     CONTINUE
!         this element takes no new memory in iw:
    NEWMEM = 0
  ELSE
!         ----------------------------------------------------------
!         construct the new element in empty space, iw (pfree ...)
!         ----------------------------------------------------------
    P = PE (ME)
    PME1 = PFREE
    SLENME = LEN (ME) - ELENME
    KNT1_UPDATED = 0
    DO 120 KNT1 = 1, ELENME + 1
      KNT1_UPDATED = KNT1_UPDATED +1
      IF (KNT1 .GT. ELENME) THEN
!             search the supervariables in me.
        E = ME
        PJ = P
        LN = SLENME
      ELSE
!             search the elements in me.
        E = IW (P)
        P = P + 1
        PJ = PE (E)
        LN = LEN (E)
      ENDIF
!           -------------------------------------------------------
!           search for different supervariables and add them to the
!           new list, compressing when necessary. this loop is
!           executed once for each element in the list and once for
!           all the supervariables in the list.
!           -------------------------------------------------------
      KNT2_UPDATED = 0
      DO 110 KNT2 = 1, LN
        KNT2_UPDATED = KNT2_UPDATED+1
        I = IW (PJ)
        PJ = PJ + 1
        NVI = NV (I)
        IF (NVI .GT. 0) THEN
!               -------------------------------------------------
!               compress iw, if necessary
!               -------------------------------------------------
          IF (PFREE .GT. IWLEN) THEN
!                 prepare for compressing iw by adjusting
!                 pointers and lengths so that the lists being
!                 searched in the inner and outer loops contain
!                 only the remaining entries.
            PE (ME) = P
            LEN (ME) = LEN (ME) - KNT1_UPDATED
!                 Reset KNT1_UPDATED in case of recompress 
!                 at same iteration of the loop 120
            KNT1_UPDATED = 0
!                 Check if anything left in supervariable ME
            IF (LEN (ME) .EQ. 0) PE (ME) = 0_8
            PE (E) = PJ
            LEN (E) = LN - KNT2_UPDATED
!                 Reset KNT2_UPDATED in case of recompress 
!                 at same iteration of the loop 110
            KNT2_UPDATED = 0
!                 Check if anything left in element E
            IF (LEN (E) .EQ. 0) PE (E) = 0_8
            NCMPA = NCMPA + 1
!                 store first item in pe
!                 set first entry to -item
            DO 70 J = 1, N
              PN = PE (J)
              IF (PN .GT. 0_8) THEN
                PE (J) = int(IW (PN),8)
                IW (PN) = -J
              ENDIF
70             CONTINUE
!                 psrc/pdst point to source/destination
            PDST = 1
            PSRC = 1
            PEND = PME1 - 1
!                 while loop:
80             CONTINUE
            IF (PSRC .LE. PEND) THEN
!                   search for next negative entry
              J = -IW (PSRC)
              PSRC = PSRC + 1
              IF (J .GT. 0) THEN
                IW (PDST) = int(PE (J))
                PE (J) = PDST
                PDST = PDST + 1_8
!                     copy from source to destination
                LENJ = LEN (J)
                DO 90 KNT3 = 0, LENJ - 2
                  IW (PDST + KNT3) = IW (PSRC + KNT3)
90                 CONTINUE
                PDST = PDST + int(LENJ - 1,8)
                PSRC = PSRC + int(LENJ - 1,8)
              ENDIF
              GO TO 80
            ENDIF
!                 move the new partially-constructed element
            P1 = PDST
            DO 100 PSRC = PME1, PFREE - 1
              IW (PDST) = IW (PSRC)
              PDST = PDST + 1
100             CONTINUE
            PME1 = P1
            PFREE = PDST
            PJ = PE (E)
            P = PE (ME)
          ENDIF
!               -------------------------------------------------
!               i is a principal variable not yet placed in Lme
!               store i in new list
!               -------------------------------------------------
          DEGME = DEGME + NVI
!               flag i as being in Lme by negating nv (i)
          NV (I) = -NVI
          IW (PFREE) = I
          PFREE = PFREE + 1
!               -------------------------------------------------
!               remove variable i from degree link list
!               -------------------------------------------------
          ILAST = LAST (I)
          INEXT = NEXT (I)
          IF (INEXT .NE. 0) LAST (INEXT) = ILAST
          IF (ILAST .NE. 0) THEN
            NEXT (ILAST) = INEXT
          ELSE
!                 i is at the head of the degree list
            HEAD (DEGREE (I)) = INEXT
          ENDIF
        ENDIF
110       CONTINUE
      IF (E .NE. ME) THEN
!             set tree pointer and flag to indicate element e is
!             absorbed into new element me (the parent of e is me)
        PE (E) = int(-ME,8)
        W (E) = 0
      ENDIF
120     CONTINUE
    PME2 = PFREE - 1
!         this element takes newmem new memory in iw (possibly zero)
    NEWMEM = PFREE - PME1
    MEM = MEM + NEWMEM
    MAXMEM = max (MAXMEM, MEM)
  ENDIF
!       -------------------------------------------------------------
!       me has now been converted into an element in iw (pme1..pme2)
!       -------------------------------------------------------------
!       degme holds the external degree of new element
  DEGREE (ME) = DEGME
  PE (ME) = PME1
  LEN (ME) = int(PME2 - PME1 + 1)
!       -------------------------------------------------------------
!       make sure that wflg is not too large.  With the current
!       value of wflg, wflg+n must not cause integer overflow
!       -------------------------------------------------------------
  IF (WFLG .GT. MAXINT_N) THEN
    DO 130 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
130     CONTINUE
    WFLG = 2
  ENDIF
!=======================================================================
!  COMPUTE (w (e) - wflg) = |Le\Lme| FOR ALL ELEMENTS
!=======================================================================
!       -------------------------------------------------------------
!       Scan 1:  compute the external degrees of previous elements
!       with respect to the current element.  That is:
!            (w (e) - wflg) = |Le \ Lme|
!       for each element e that appears in any supervariable in Lme.
!       The notation Le refers to the pattern (list of
!       supervariables) of a previous element e, where e is not yet
!       absorbed, stored in iw (pe (e) + 1 ... pe (e) + iw (pe (e))).
!       The notation Lme refers to the pattern of the current element
!       (stored in iw (pme1..pme2)).   If (w (e) - wflg) becomes
!       zero, then the element e will be absorbed in scan 2.
!       -------------------------------------------------------------
  DO 150 PME = PME1, PME2
    I = IW (PME)
    ELN = ELEN (I)
    IF (ELN .GT. 0) THEN
!           note that nv (i) has been negated to denote i in Lme:
      NVI = -NV (I)
      WNVI = WFLG - NVI
      DO 140 P = PE (I), PE (I) + int(ELN - 1,8)
        E = IW (P)
        WE = W (E)
        IF (WE .GE. WFLG) THEN
!               unabsorbed element e has been seen in this loop
          WE = WE - NVI
        ELSE IF (WE .NE. 0) THEN
!               e is an unabsorbed element
!               this is the first we have seen e in all of Scan 1
          WE = DEGREE (E) + WNVI
        ENDIF
        W (E) = WE
140       CONTINUE
    ENDIF
150   CONTINUE
!=======================================================================
!  DEGREE UPDATE AND ELEMENT ABSORPTION
!=======================================================================
!       -------------------------------------------------------------
!       Scan 2:  for each i in Lme, sum up the degree of Lme (which
!       is degme), plus the sum of the external degrees of each Le
!       for the elements e appearing within i, plus the
!       supervariables in i.  Place i in hash list.
!       -------------------------------------------------------------
  DO 180 PME = PME1, PME2
    I = IW (PME)
    P1 = PE (I)
    P2 = P1 + int(ELEN (I) - 1,8)
    PN = P1
    HASH = 0_8
    DEG = 0
!         ----------------------------------------------------------
!         scan the element list associated with supervariable i
!         ----------------------------------------------------------
    DO 160 P = P1, P2
      E = IW (P)
!           dext = | Le \ Lme |
      DEXT = W (E) - WFLG
      IF (DEXT .GT. 0) THEN
        DEG = DEG + DEXT
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + int(E,kind=8)
      ELSE IF (DEXT .EQ. 0) THEN
#if defined (NOAGG2)
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + int(E,kind=8)
#else
!             aggressive absorption: e is not adjacent to me, but
!             the |Le \ Lme| is 0, so absorb it into me
        PE (E) = int(-ME,8)
        W (E) = 0
#endif
      ENDIF
160     CONTINUE
!         count the number of elements in i (including me):
    ELEN (I) = int(PN - P1 + 1_8)
!         ----------------------------------------------------------
!         scan the supervariables in the list associated with i
!         ----------------------------------------------------------
    P3 = PN
    DO 170 P = P2 + 1, P1 + int(LEN (I) - 1,8)
      J = IW (P)
      NVJ = NV (J)
      IF (NVJ .GT. 0) THEN
!             j is unabsorbed, and not in Lme.
!             add to degree and add to new list
        DEG = DEG + NVJ
        IW (PN) = J
        PN = PN + 1
        HASH = HASH + int(J,kind=8)
      ENDIF
170     CONTINUE
!         ----------------------------------------------------------
!         update the degree and check for mass elimination
!         ----------------------------------------------------------
#if defined (NOAGG2)
    IF (DEG.EQ.0.AND.(ELEN(I).GT.1)) THEN
!         When DEG is zero we need to 
!         absorb in ME all elements adjacent to I 
             P1 = PE (I)
!                  exclude ME --> -2 
             P2 = P1 + int(ELEN (I),8) - 2_8
             DO P =P1,P2
               E      = IW(P)
               PE (E) = int(-ME,8)
               W (E)  = 0
             ENDDO
    ENDIF
!              .... Ready for mass elimination
#endif
    IF (DEG .EQ. 0) THEN
!           -------------------------------------------------------
!           mass elimination
!           -------------------------------------------------------
!           There is nothing left of this node except for an
!           edge to the current pivot element.  elen (i) is 1,
!           and there are no variables adjacent to node i.
!           Absorb i into the current pivot element, me.
      PE (I) = int(-ME,8)
      NVI = -NV (I)
      DEGME = DEGME - NVI
      NVPIV = NVPIV + NVI
      NEL = NEL + NVI
      NV (I) = 0
      ELEN (I) = 0
    ELSE
!           -------------------------------------------------------
!           update the upper-bound degree of i
!           -------------------------------------------------------
!           the following degree does not yet include the size
!           of the current element, which is added later:
      DEGREE (I) = min (DEGREE (I), DEG)
!           -------------------------------------------------------
!           add me to the list for i
!           -------------------------------------------------------
!           move first supervariable to end of list
      IW (PN) = IW (P3)
!           move first element to end of element part of list
      IW (P3) = IW (P1)
!           add new element to front of list.
      IW (P1) = ME
!           store the new length of the list in len (i)
      LEN (I) = int(PN - P1 + 1_8)
!           -------------------------------------------------------
!           place in hash bucket.  Save hash key of i in last (i).
!           -------------------------------------------------------
      HASH = mod (HASH, HMOD) + 1_8
      J = HEAD (HASH)
      IF (J .LE. 0) THEN
!             the degree list is empty, hash head is -j
        NEXT (I) = -J
        HEAD (HASH) = -I
      ELSE
!             degree list is not empty
!             use last (head (hash)) as hash head
        NEXT (I) = LAST (J)
        LAST (J) = I
      ENDIF
      LAST (I) = int(HASH,kind=kind(LAST))
    ENDIF
180   CONTINUE
  DEGREE (ME) = DEGME
!       -------------------------------------------------------------
!       Clear the counter array, w (...), by incrementing wflg.
!       -------------------------------------------------------------
  DMAX = max (DMAX, DEGME)
  WFLG = WFLG + DMAX
!       make sure that wflg+n does not cause integer overflow
  IF (WFLG .GT. MAXINT_N) THEN
    DO 190 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
190     CONTINUE
    WFLG = 2
  ENDIF
!       at this point, w (1..n) .lt. wflg holds
!=======================================================================
!  SUPERVARIABLE DETECTION
!=======================================================================
  DO 250 PME = PME1, PME2
    I = IW (PME)
    IF (NV (I) .LT. 0) THEN
!           i is a principal variable in Lme
!           -------------------------------------------------------
!           examine all hash buckets with 2 or more variables.  We
!           do this by examing all unique hash keys for super-
!           variables in the pattern Lme of the current element, me
!           -------------------------------------------------------
      HASH = int(LAST (I),kind=8)
!           let i = head of hash bucket, and empty the hash bucket
      J = HEAD (HASH)
      IF (J .EQ. 0) GO TO 250
      IF (J .LT. 0) THEN
!             degree list is empty
        I = -J
        HEAD (HASH) = 0
      ELSE
!             degree list is not empty, restore last () of head
        I = LAST (J)
        LAST (J) = 0
      ENDIF
      IF (I .EQ. 0) GO TO 250
!           while loop:
200       CONTINUE
      IF (NEXT (I) .NE. 0) THEN
!             ----------------------------------------------------
!             this bucket has one or more variables following i.
!             scan all of them to see if i can absorb any entries
!             that follow i in hash bucket.  Scatter i into w.
!             ----------------------------------------------------
        LN = LEN (I)
        ELN = ELEN (I)
!             do not flag the first element in the list (me)
        DO 210 P = PE (I) + 1, PE (I) + int(LN - 1,8)
          W (IW (P)) = WFLG
210         CONTINUE
!             ----------------------------------------------------
!             scan every other entry j following i in bucket
!             ----------------------------------------------------
        JLAST = I
        J = NEXT (I)
!             while loop:
220         CONTINUE
        IF (J .NE. 0) THEN
!               -------------------------------------------------
!               check if j and i have identical nonzero pattern
!               -------------------------------------------------
!               jump if i and j do not have same size data structure
          IF (LEN (J) .NE. LN) GO TO 240
!               jump if i and j do not have same number adj elts
          IF (ELEN (J) .NE. ELN) GO TO 240
!               do not flag the first element in the list (me)
          DO 230 P = PE (J) + 1, PE (J) + int(LN - 1,8)
!                 jump if an entry (iw(p)) is in j but not in i
            IF (W (IW (P)) .NE. WFLG) GO TO 240
230           CONTINUE
!               -------------------------------------------------
!               found it!  j can be absorbed into i
!               -------------------------------------------------
          PE (J) = int(-I,8)
!               both nv (i) and nv (j) are negated since they
!               are in Lme, and the absolute values of each
!               are the number of variables in i and j:
          NV (I) = NV (I) + NV (J)
          NV (J) = 0
          ELEN (J) = 0
!               delete j from hash bucket
          J = NEXT (J)
          NEXT (JLAST) = J
          GO TO 220
!               -------------------------------------------------
240           CONTINUE
!               j cannot be absorbed into i
!               -------------------------------------------------
          JLAST = J
          J = NEXT (J)
        GO TO 220
        ENDIF
!             ----------------------------------------------------
!             no more variables can be absorbed into i
!             go to next i in bucket and clear flag array
!             ----------------------------------------------------
        WFLG = WFLG + 1
        I = NEXT (I)
        IF (I .NE. 0) GO TO 200
      ENDIF
    ENDIF
250   CONTINUE
!=======================================================================
!  RESTORE DEGREE LISTS AND REMOVE NONPRINCIPAL SUPERVAR. FROM ELEMENT
!=======================================================================
  P = PME1
  NLEFT = N - NEL
  DO 260 PME = PME1, PME2
    I = IW (PME)
    NVI = -NV (I)
    IF (NVI .GT. 0) THEN
!           i is a principal variable in Lme
!           restore nv (i) to signify that i is principal
      NV (I) = NVI
!           -------------------------------------------------------
!           compute the external degree (add size of current elem)
!           -------------------------------------------------------
      DEG = min (DEGREE (I) + DEGME - NVI, NLEFT - NVI)
!           -------------------------------------------------------
!           place the supervariable at the head of the degree list
!           -------------------------------------------------------
      INEXT = HEAD (DEG)
      IF (INEXT .NE. 0) LAST (INEXT) = I
      NEXT (I) = INEXT
      LAST (I) = 0
      HEAD (DEG) = I
!           -------------------------------------------------------
!           save the new degree, and find the minimum degree
!           -------------------------------------------------------
      MINDEG = min (MINDEG, DEG)
      DEGREE (I) = DEG
!           -------------------------------------------------------
!           place the supervariable in the element pattern
!           -------------------------------------------------------
      IW (P) = I
      P = P + 1
    ENDIF
260   CONTINUE
!=======================================================================
!  FINALIZE THE NEW ELEMENT
!=======================================================================
  NV (ME) = NVPIV + DEGME
!       nv (me) is now the degree of pivot (including diagonal part)
!       save the length of the list for the new element me
  LEN (ME) = int(P - PME1)
  IF (LEN (ME) .EQ. 0) THEN
!         there is nothing left of the current pivot element
    PE (ME) = 0_8
    W (ME) = 0
  ENDIF
  IF (NEWMEM .NE. 0) THEN
!         element was not constructed in place: deallocate part
!         of it (final size is less than or equal to newmem,
!         since newly nonprincipal variables have been removed).
    PFREE = P
    MEM = MEM - NEWMEM + int(LEN (ME),8)
  ENDIF
!=======================================================================
!       END WHILE (selecting pivots)
GO TO 30
ENDIF
!=======================================================================
!=======================================================================
!  COMPUTE THE PERMUTATION VECTORS
!=======================================================================
!     ----------------------------------------------------------------
!     The time taken by the following code is O(n).  At this
!     point, elen (e) = -k has been done for all elements e,
!     and elen (i) = 0 has been done for all nonprincipal
!     variables i.  At this point, there are no principal
!     supervariables left, and all elements are absorbed.
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!     compute the ordering of unordered nonprincipal variables
!     ----------------------------------------------------------------
DO 290 I = 1, N
  IF (ELEN (I) .EQ. 0) THEN
!         ----------------------------------------------------------
!         i is an un-ordered row.  Traverse the tree from i until
!         reaching an element, e.  The element, e, was the
!         principal supervariable of i and all nodes in the path
!         from i to when e was selected as pivot.
!         ----------------------------------------------------------
    J = int(-PE (I))
!         while (j is a variable) do:
270     CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        J = int(-PE (J))
        GO TO 270
      ENDIF
      E = J
!           ----------------------------------------------------------
!           get the current pivot ordering of e
!           ----------------------------------------------------------
      K = -ELEN (E)
!           ----------------------------------------------------------
!           traverse the path again from i to e, and compress the
!           path (all nodes point to e).  Path compression allows
!           this code to compute in O(n) time.  Order the unordered
!           nodes in the path, and place the element e at the end.
!           ----------------------------------------------------------
      J = I
!           while (j is a variable) do:
280       CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        JNEXT = int(-PE (J))
        PE (J) = int(-E,8)
        IF (ELEN (J) .EQ. 0) THEN
!               j is an unordered row
          ELEN (J) = K
          K = K + 1
        ENDIF
        J = JNEXT
      GO TO 280
      ENDIF
!         leave elen (e) negative, so we know it is an element
    ELEN (E) = -K
  ENDIF
290 CONTINUE
!     ----------------------------------------------------------------
!     reset the inverse permutation (elen (1..n)) to be positive,
!     and compute the permutation (last (1..n)).
!     ----------------------------------------------------------------
DO 300 I = 1, N
  K = abs (ELEN (I))
  LAST (K) = I
  ELEN (I) = K
300 CONTINUE
!=======================================================================
!  RETURN THE MEMORY USAGE IN IW
!=======================================================================
!     If maxmem is less than or equal to iwlen, then no compressions
!     occurred, and iw (maxmem+1 ... iwlen) was unused.  Otherwise
!     compressions did occur, and iwlen would have had to have been
!     greater than or equal to maxmem for no compressions to occur.
!     Return the value of maxmem in the pfree argument.
PFREE = MAXMEM
!===============================
!     Save PE in PARENT array
DO I=1,N
 PARENT(I) = int(PE(I))
ENDDO
!===============================
RETURN
END SUBROUTINE MUMPS_AMD_ELT
! ----------------------------------------------------------------------
! Description of MUMPS_HAMD:
!   MUMPS_HAMD is a modification of AMD reference code (MUMPS_ANA_H) 
!   designed to take into account a halo in the graph. 
!   The graph is composed is partitioned in two types of nodes
!   the so called internal nodes and the so called halo nodes.
!   Halo nodes cannot be selected the both the inital degrees 
!   and updated degrees of internal node should be taken 
!   into account.  
!   This routine also referred to as HALOAMD in MUMPS comments
!   is used for both Schur functionality and in the coupling with 
!   partitioners such as SCOTCH.
!
!   Restrictive integer 64 bit variant :
!   it is assumed that IW array size can exceed 32-bit integer
!
!      
SUBROUTINE MUMPS_HAMD(N, IWLEN, PE, PFREE, LEN, IW, NV, ELEN, &
  &                   LAST, NCMPA, DEGREE, HEAD, NEXT, W, PARENT, &
  &                   LISTVAR_SCHUR, SIZE_SCHUR)
!
! Parameters
!    Input not modified
INTEGER, intent(in) :: SIZE_SCHUR
INTEGER, intent(in) :: LISTVAR_SCHUR(SIZE_SCHUR)
INTEGER, INTENT(IN)    :: N
INTEGER(8), INTENT(IN) :: IWLEN
!     Input undefined on output 
INTEGER, INTENT(INOUT)  :: LEN(N), IW(IWLEN)
! 
!     Output only 
INTEGER, INTENT(OUT)   :: NCMPA
INTEGER, INTENT(OUT)   :: NV(N), ELEN(N), LAST(N), PARENT(N)
! 
!     Input/output
INTEGER(8), INTENT(INOUT) :: PFREE
INTEGER(8), INTENT(INOUT) :: PE(N)
! 
!     Internal Workspace only
INTEGER     :: NEXT(N), DEGREE(N), HEAD(N), W(N)
!
! ---------------------
! Interface Description
! ---------------------
!    HAMD (short for HALOAMD)
!    The initial version (so called HALOAMD_V1, developped in September 1997)
!    is designed to experiment the numerical (fill-in) impact 
!    of taking into account the halo. This code should be able 
!    to experiment no-halo, partial halo, complete halo.
!    DATE: September 17th 1997
!
!    HALOAMD is designed to process a gragh composed of two types
!            of nodes, V0 and V1, extracted from a larger gragh. 
!            V0^V1 = {}, 
!       
!            We used Min. degree heuristic to order only 
!            nodes in V0, but the adjacency to nodes
!            in V1 is taken into account during ordering.
!            Nodes in V1 are odered at last.
!            Adjacency between nodes of V1 need not be provided,
!            however |len(i)| must always corresponds to the number of 
!            edges effectively provided in the adjacency list of i.
!          On input :
!          ********
!            Nodes INODE in V1 are flagged with len(INODE) = -degree 
!            modif version HALO V3 (August 1998): 
!                                 if len(i) =0 and i \in V1 then 
!                                 len(i) must be set on input to -N-1
!          ERROR return (negative values in ncmpa)
!          ************
!            negative value in ncmpa indicates an error detected 
!               by HALOAMD.
!
!            The graph provided MUST follow the rule:
!             if (i,j) is an edge in the gragh then 
!             j must be in the adjacency list of i AND 
!             i must be in the adjacency list of j.
!    REMARKS:
!        1/  Providing edges between nodes of V1 should not 
!            affect the final ordering, only the amount of edges 
!            of the halo should effectively affect the solution.
!            This code should work in the following cases:
!              1/ halo not provided
!              2/ halo partially provided
!              3/ complete halo
!              4/ complete halo+interconnection between nodes of V1.
!
!              1/ should run and provide identical results (w.r.t to current 
!               implementation of AMD in SCOTCH).
!             3/ and 4 should provide identical results.
!
!        2/ All modifications of the AMD initial code are indicated
!           with begin HALO .. end HALO
!
!            
!   Ordering of nodes in V0 is based on 
!   Approximate Minimum Degree ordering algorithm, 
!   with aggressive absorption:
!   Given a representation of the nonzero pattern of a symmetric matrix,
!   A, (excluding the diagonal) perform an approximate minimum
!   degree ordering to compute a pivot order
!   such that fill-in in the Cholesky factors A = LL^T is kept low. 
!
! ------------------------------
! Modification history:
! ---------------------
! Date:  September, 1997 (V1)
!        April, 1998 (V2)
!        August, 1998 (V3)
!        Octobre, 1998 (V4)
!        December, 1998 (V5)
!        January, 1999 (V6)
!   HALOAMD_V6: 
!   ----------
!    1/ ERROR 2 detection followed by stop statement suppressed 
!  . 2/ pb 1  identified in V5 was not correctly solved
!
!   HALOAMD_V5:
!   ----------
!    1/ Pb with matrix psmigr 1, because upper bound 
!      degree DEG  >N was considered as a node in V1 
!
!   HALOAMD_V4: 
!   ---------- 
!    Only UnsymetrizedMultifrontal interface 
!    (ok for both scotch and UnsymetricMultifrontal) is 
!    included in this file
!
!   HALOAMD_V3: 
!   ----------
!    Problem in version 2 : variables of V1 with len(i) =0 
!                        are not well processed. 
!                        See modification of the 
!                        input to characterize those variables.
! 
!    Problem detected by Jacko Koster while experimenting with 
!    version 2 of haloAMD in the context of multiple front method :
!    "if for an interface variable i, row i in the matrix has only a
!    nonzero entry on the diagonal, we first remove this entry and len(i)  
!    is set to zero on input to HALOAMD. However, this means that HALOAMD 
!    will treat variable i as an interior variable (in V0) instead as an 
!    interface variable (in V1). (It is indeed a bit strange to have such 
!    interface variables but we encountered some in our debugging 
!    experiments with some random partitionings.)
!
!    Solution : 
!     IF on input i \in V1 and len(i) =0 (that is adjlist(i)={}) THEN
!      len(i) must be set on input to -N-1. 
!     ENDIF
!     therefore all variables i / len(i) < 0 an only those are in V1
!     variable with len(i) = -N-1 are then processed differently at 
!     the beginning of the code
!
!   HALOAMD_V2:
!   ----------
!    The end of the tree (including links to block of flagged indices
!                       is built) . The list of flagged indices is 
!                       considered as a dense amalgamated node.
!
!    Comments on the OUTPUT:
!    ----------------------
!    Let V= V0 U V1 the nodes of the initial graph (|V|=n). 
!    The assembly tree corresponds to the tree 
!    of the supernodes (or supervariables). Each node of the 
!    assembly tree is then composed of one principal variable 
!    and a list of secondary variables. The list of 
!    variable of a node (principal + secondary variables) then 
!    describes the structure of the diagonal bloc of the 
!    supernode. 
!    The elimination tree denotes the tree of all the variables(=node) and 
!    is therefore of order n.
!
!    The arrays NV(N) and PE(N) give a description of the 
!    assembly tree. 
!     1/ Description of array nv(N) (on OUPUT)
!        nv(i)=0 i is a secondary variable 
!        N+1> nv(i) >0 i is a principal variable, nv(i) holds the 
!                the number of elements in column i of L (true degree of i)
!     2/ Description of array PE(N) (on OUPUT)
!       pe(i) = -(father of variable/node i) in the elimination tree:
!       If nv (i) .gt. 0, then i represents a node in the assembly tree,
!       and the parent of i is -pe (i), or zero if i is a root.
!       If nv (i) = 0, then (i,-pe (i)) represents an edge in a
!       subtree, the root of which is a node in the assembly tree.
!     3/ Example:
!       Let If be a root node father of Is in the assembly tree. 
!       If is the principal 
!       variable of the node If and let If1, If2, If3 be the 
!       secondary variables of node If.
!       Is is the principal 
!       variable of the node Is and let Is1, Is2 be the secondary variables
!       of node Is.
!      
!       THEN: 
!        NV(If1)=NV(If2)=NV(If3) = 0  (secondary variables)
!        NV(Is1)=NV(Is2) = 0  (secondary variables)
!        NV(If) > 0  ( principal variable)
!        NV(Is) > 0  ( principal variable)
!        PE(If)  = 0 (root node)
!        PE(Is)  = -If (If is the father of Is in the assembly tree)
!        PE(If1)=PE(If2)=PE(If3)= -If  ( If is the principal variable)
!        PE(Is1)=PE(Is2)= -Is  ( Is is the principal variable)
!-----------------------------------------------------------------------
! INPUT ARGUMENTS (unaltered):
!-----------------------------------------------------------------------
! n:    The matrix order.
!
!       Restriction:  n .ge. 1
! iwlen:        The length of iw (1..iwlen).  On input, the matrix is
!       stored in iw (1..pfree-1).  However, iw (1..iwlen) should be
!       slightly larger than what is required to hold the matrix, at
!       least iwlen .ge. pfree + n is recommended.  Otherwise,
!       excessive compressions will take place.
!       *** We do not recommend running this algorithm with ***
!       ***      iwlen .lt. pfree + n.                      ***
!       *** Better performance will be obtained if          ***
!       ***      iwlen .ge. pfree + n                       ***
!       *** or better yet                                   ***
!       ***      iwlen .gt. 1.2 * pfree                     ***
!       *** (where pfree is its value on input).            ***
!       The algorithm will not run at all if iwlen .lt. pfree-1.
!
!       Restriction: iwlen .ge. pfree-1
!-----------------------------------------------------------------------
! INPUT/OUPUT ARGUMENTS:
!-----------------------------------------------------------------------
! pe:   On input, pe (i) is the index in iw of the start of row i, or
!       zero if row i has no off-diagonal non-zeros.
!
!       During execution, it is used for both supervariables and
!       elements:
!
!       * Principal supervariable i:  index into iw of the
!               description of supervariable i.  A supervariable
!               represents one or more rows of the matrix
!               with identical nonzero pattern.
!       * Non-principal supervariable i:  if i has been absorbed
!               into another supervariable j, then pe (i) = -j.
!               That is, j has the same pattern as i.
!               Note that j might later be absorbed into another
!               supervariable j2, in which case pe (i) is still -j,
!               and pe (j) = -j2.
!       * Unabsorbed element e:  the index into iw of the description
!               of element e, if e has not yet been absorbed by a
!               subsequent element.  Element e is created when
!               the supervariable of the same name is selected as
!               the pivot.
!       * Absorbed element e:  if element e is absorbed into element
!               e2, then pe (e) = -e2.  This occurs when the pattern of
!               e (that is, Le) is found to be a subset of the pattern
!               of e2 (that is, Le2).  If element e is "null" (it has
!               no nonzeros outside its pivot block), then pe (e) = 0.
!
!       On output, pe holds the assembly tree/forest, which implicitly
!       represents a pivot order with identical fill-in as the actual
!       order (via a depth-first search of the tree).
!
!       On output:
!       If nv (i) .gt. 0, then i represents a node in the assembly tree,
!       and the parent of i is -pe (i), or zero if i is a root.
!       If nv (i) = 0, then (i,-pe (i)) represents an edge in a
!       subtree, the root of which is a node in the assembly tree.
!       On output:  (PE is copied on output into PARENT array)
!
! pfree:        On input, the matrix is stored in iw (1..pfree-1) and
!       the rest of the array iw is free.
!       During execution, additional data is placed in iw, and pfree
!       is modified so that components  of iw from pfree are free.
!       On output, pfree is set equal to the size of iw that
!       would have been needed for no compressions to occur.  If
!       ncmpa is zero, then pfree (on output) is less than or equal to
!       iwlen, and the space iw (pfree+1 ... iwlen) was not used.
!       Otherwise, pfree (on output) is greater than iwlen, and all the
!       memory in iw was used.
!-----------------------------------------------------------------------
! INPUT/MODIFIED (undefined on output):
!-----------------------------------------------------------------------
! len:  On input, len (i) 
!           positive or null (>=0) : i \in V0 and 
!                     len(i) holds the number of entries in row i of the
!                     matrix, excluding the diagonal.  
!           negative (<0) : i \in V1, and 
!                     -len(i) hold the number of entries in row i of the
!                     matrix, excluding the diagonal.
!                     len(i) = - | Adj(i) | if i \in V1                    
!                              or -N -1 if  | Adj(i) | = 0 and i \in V1 
!       The contents of len (1..n)
!       are undefined on output.
! iw:   On input, iw (1..pfree-1) holds the description of each row i
!       in the matrix.  The matrix must be symmetric, and both upper
!       and lower triangular parts must be present.  The diagonal must
!       not be present.  Row i is held as follows:
!
!               len (i):  the length of the row i data structure
!               iw (pe (i) ... pe (i) + len (i) - 1):
!                       the list of column indices for nonzeros
!                       in row i (simple supervariables), excluding
!                       the diagonal.  All supervariables start with
!                       one row/column each (supervariable i is just
!                       row i).
!               if len (i) is zero on input, then pe (i) is ignored
!               on input.
!
!               Note that the rows need not be in any particular order,
!               and there may be empty space between the rows.
!
!       During execution, the supervariable i experiences fill-in.
!       This is represented by placing in i a list of the elements
!       that cause fill-in in supervariable i:
!
!               len (i):  the length of supervariable i
!               iw (pe (i) ... pe (i) + elen (i) - 1):
!                       the list of elements that contain i.  This list
!                       is kept short by removing absorbed elements.
!               iw (pe (i) + elen (i) ... pe (i) + len (i) - 1):
!                       the list of supervariables in i.  This list
!                       is kept short by removing nonprincipal
!                       variables, and any entry j that is also
!                       contained in at least one of the elements
!                       (j in Le) in the list for i (e in row i).
!
!       When supervariable i is selected as pivot, we create an
!       element e of the same name (e=i):
!
!               len (e):  the length of element e
!               iw (pe (e) ... pe (e) + len (e) - 1):
!                       the list of supervariables in element e.
!
!       An element represents the fill-in that occurs when supervariable
!       i is selected as pivot (which represents the selection of row i
!       and all non-principal variables whose principal variable is i).
!       We use the term Le to denote the set of all supervariables
!       in element e.  Absorbed supervariables and elements are pruned
!       from these lists when computationally convenient.
!
!       CAUTION:  THE INPUT MATRIX IS OVERWRITTEN DURING COMPUTATION.
!       The contents of iw are undefined on output.
!-----------------------------------------------------------------------
! OUTPUT (need not be set on input):
!-----------------------------------------------------------------------
! nv:   During execution, abs (nv (i)) is equal to the number of rows
!       that are represented by the principal supervariable i.  If i is
!       a nonprincipal variable, then nv (i) = 0.  Initially,
!       nv (i) = 1 for all i.  nv (i) .lt. 0 signifies that i is a
!       principal variable in the pattern Lme of the current pivot
!       element me.  On output, nv (e) holds the true degree of element
!       e at the time it was created (including the diagonal part).
! begin HALO
!       On output, nv(I) can be used to find node in set V1.
!       nv(I) = N+1 characterizes nodes in V1. 
! end HALO
! elen: See the description of iw above.  At the start of execution,
!       elen (i) is set to zero.  During execution, elen (i) is the
!       number of elements in the list for supervariable i.  When e
!       becomes an element, elen (e) = -nel is set, where nel is the
!       current step of factorization.  elen (i) = 0 is done when i
!       becomes nonprincipal.
!
!       For variables, elen (i) .ge. 0 holds until just before the
!       permutation vectors are computed.  For elements,
!       elen (e) .lt. 0 holds.
!
!       On output elen (1..n) holds the inverse permutation (the same
!       as the 'INVP' argument in Sparspak).  That is, if k = elen (i),
!       then row i is the kth pivot row.  Row i of A appears as the
!       (elen(i))-th row in the permuted matrix, PAP^T.
! last: In a degree list, last (i) is the supervariable preceding i,
!       or zero if i is the head of the list.  In a hash bucket,
!       last (i) is the hash key for i.  last (head (hash)) is also
!       used as the head of a hash bucket if head (hash) contains a
!       degree list (see head, below).
!
!       On output, last (1..n) holds the permutation (the same as the
!       'PERM' argument in Sparspak).  That is, if i = last (k), then
!       row i is the kth pivot row.  Row last (k) of A is the k-th row
!       in the permuted matrix, PAP^T.
! ncmpa:        The number of times iw was compressed.  If this is
!       excessive, then the execution took longer than what could have
!       been.  To reduce ncmpa, try increasing iwlen to be 10% or 20%
!       larger than the value of pfree on input (or at least
!       iwlen .ge. pfree + n).  The fastest performance will be
!       obtained when ncmpa is returned as zero.  If iwlen is set to
!       the value returned by pfree on *output*, then no compressions
!       will occur.
! begin HALO
!        on output ncmpa <0 --> error detected during HALO_AMD:
!           error 1: ncmpa = -N , ordering was stopped.
! end HALO
!
!-----------------------------------------------------------------------
! LOCAL (not input or output - used only during execution):
!-----------------------------------------------------------------------
! degree:       If i is a supervariable, then degree (i) holds the
!       current approximation of the external degree of row i (an upper
!       bound).  The external degree is the number of nonzeros in row i,
!       minus abs (nv (i)) (the diagonal part).  The bound is equal to
!       the external degree if elen (i) is less than or equal to two.
!       We also use the term "external degree" for elements e to refer
!       to |Le \ Lme|.  If e is an element, then degree (e) holds |Le|,
!       which is the degree of the off-diagonal part of the element e
!       (not including the diagonal part).
! begin HALO
!       degree(I) = n+1 indicates that i belongs to V1
! end HALO
!
! head: head is used for degree lists.  head (deg) is the first
!       supervariable in a degree list (all supervariables i in a
!       degree list deg have the same approximate degree, namely,
!       deg = degree (i)).  If the list deg is empty then
!       head (deg) = 0.
!
!       During supervariable detection head (hash) also serves as a
!       pointer to a hash bucket.
!       If head (hash) .gt. 0, there is a degree list of degree hash.
!               The hash bucket head pointer is last (head (hash)).
!       If head (hash) = 0, then the degree list and hash bucket are
!               both empty.
!       If head (hash) .lt. 0, then the degree list is empty, and
!               -head (hash) is the head of the hash bucket.
!       After supervariable detection is complete, all hash buckets
!       are empty, and the (last (head (hash)) = 0) condition is
!       restored for the non-empty degree lists.
! next: next (i) is the supervariable following i in a link list, or
!       zero if i is the last in the list.  Used for two kinds of
!       lists:  degree lists and hash buckets (a supervariable can be
!       in only one kind of list at a time).
! w:    The flag array w determines the status of elements and
!       variables, and the external degree of elements.
!
!       for elements:
!          if w (e) = 0, then the element e is absorbed
!          if w (e) .ge. wflg, then w (e) - wflg is the size of
!               the set |Le \ Lme|, in terms of nonzeros (the
!               sum of abs (nv (i)) for each principal variable i that
!               is both in the pattern of element e and NOT in the
!               pattern of the current pivot element, me).
!          if wflg .gt. w (e) .gt. 0, then e is not absorbed and has
!               not yet been seen in the scan of the element lists in
!               the computation of |Le\Lme| in loop 150 below.
!
!       for variables:
!          during supervariable detection, if w (j) .ne. wflg then j is
!          not in the pattern of variable i
!
!       The w array is initialized by setting w (i) = 1 for all i,
!       and by setting wflg = 2.  It is reinitialized if wflg becomes
!       too large (to ensure that wflg+n does not cause integer
!       overflow).
!-----------------------------------------------------------------------
! LOCAL INTEGERS:
!-----------------------------------------------------------------------
INTEGER    :: DEG, DEGME, DEXT, DMAX, E, ELENME, ELN, I, &
  &        ILAST, INEXT, J, JLAST, JNEXT, K, KNT1, KNT2, KNT3, &
  &        LENJ, LN, ME, MINDEG, NEL, &
  &        NLEFT, NVI, NVJ, NVPIV, SLENME, WE, WFLG, WNVI, X, &
  &        NBFLAG, NREAL, LASTD, NELME
INTEGER KNT1_UPDATED, KNT2_UPDATED
INTEGER(8) ::  MAXMEM, MEM, NEWMEM
INTEGER    :: MAXINT_N
INTEGER(8) :: HASH, HMOD
! deg:        the degree of a variable or element
! degme:      size, |Lme|, of the current element, me (= degree (me))
! dext:       external degree, |Le \ Lme|, of some element e
! dmax:       largest |Le| seen so far
! e:          an element
! elenme:     the length, elen (me), of element list of pivotal var.
! eln:        the length, elen (...), of an element list
! hash:       the computed value of the hash function
! hmod:       the hash function is computed modulo hmod = max (1,n-1)
! i:          a supervariable
! ilast:      the entry in a link list preceding i
! inext:      the entry in a link list following i
! j:          a supervariable
! jlast:      the entry in a link list preceding j
! jnext:      the entry in a link list, or path, following j
! k:          the pivot order of an element or variable
! knt1:       loop counter used during element construction
! knt2:       loop counter used during element construction
! knt3:       loop counter used during compression
! lenj:       len (j)
! ln:         length of a supervariable list
! maxint_n:   large integer to test risk of overflow on wflg
! maxmem:     amount of memory needed for no compressions
! me:         current supervariable being eliminated, and the
!                     current element created by eliminating that
!                     supervariable
! mem:        memory in use assuming no compressions have occurred
! mindeg:     current minimum degree
! nel:        number of pivots selected so far
! newmem:     amount of new memory needed for current pivot element
! nleft:      n - nel, the number of nonpivotal rows/columns remaining
! nvi:        the number of variables in a supervariable i (= nv (i))
! nvj:        the number of variables in a supervariable j (= nv (j))
! nvpiv:      number of pivots in current element
! slenme:     number of variables in variable list of pivotal variable
! we:         w (e)
! wflg:       used for flagging the w array.  See description of iw.
! wnvi:       wflg - nv (i)
! x:          either a supervariable or an element
! begin HALO
! nbflag:     number of flagged entries in the initial gragh.
! nreal :     number of entries on which ordering must be perfomed
!             (nreel = N- nbflag)
! nelme number of pivots selected when reaching the root
! lastd index of the last row in the list of dense rows
! end HALO
!-----------------------------------------------------------------------
! LOCAL POINTERS:
!-----------------------------------------------------------------------
INTEGER(8) :: P, P1, P2, P3, PDST, PEND, PJ, PME, PME1, PME2, &
  &              PN, PSRC
!             Any parameter (pe (...) or pfree) or local variable
!             starting with "p" (for Pointer) is an index into iw,
!             and all indices into iw use variables starting with
!             "p."  The only exception to this rule is the iwlen
!             input argument.
! p:          pointer into lots of things
! p1:         pe (i) for some variable i (start of element list)
! p2:         pe (i) + elen (i) -  1 for some var. i (end of el. list)
! p3:         index of first supervariable in clean list
! pdst:       destination pointer, for compression
! pend:       end of memory to compress
! pj:         pointer into an element or variable
! pme:        pointer into the current element (pme1...pme2)
! pme1:       the current element, me, is stored in iw (pme1...pme2)
! pme2:       the end of the current element
! pn:         pointer into a "clean" variable, also used to compress
! psrc:       source pointer, for compression
!-----------------------------------------------------------------------
!  FUNCTIONS CALLED:
!-----------------------------------------------------------------------
INTRINSIC max, min, mod
!=======================================================================
!  INITIALIZATIONS
!=======================================================================
WFLG = 2
MAXINT_N=huge(WFLG)-N
MINDEG = 1
NCMPA = 0
NEL = 0
HMOD = int(max (1, N-1),kind=8)
DMAX = 0
MEM = PFREE - 1
MAXMEM = MEM
! begin HALO 
NBFLAG = 0
LASTD  = 0
! end HALO 
DO 10 I = 1, N
  LAST (I) = 0
  HEAD (I) = 0
  NV (I) = 1
  W (I) = 1
  ELEN (I) = 0
  DEGREE(I) = LEN(I)
10 CONTINUE
!
! begin HALO-SCHUR
NBFLAG = SIZE_SCHUR
!
DO K=1,SIZE_SCHUR
!
 I = LISTVAR_SCHUR(K)
 DEGREE(I) = N+1
 IF ((LEN(I) .EQ.0).OR.(LEN(I).EQ.-N-1)) THEN
!      Both ways of characterizing i \in Schur with Adj(I) = 0
!        Because of compress, we force skipping this
!        entry which is anyway empty
   PE (I)     = 0_8
   LEN(I)     = 0
 ENDIF
!      insert I at the end of degree list of n
!                  (safe: because max external degree is N-1)
 DEG = N
 IF (LASTD.EQ.0) THEN
!              degree list is empty
         LASTD     = I
         HEAD(DEG) = I
         NEXT(I)   = 0
         LAST(I)   = 0
 ELSE
         NEXT(LASTD) = I
         LAST(I)     = LASTD
         LASTD       = I
         NEXT(I)     = 0
 ENDIF
!
ENDDO
!     number of entries to be ordered.
NREAL = N - NBFLAG
! end HALO-SCHUR
!     ----------------------------------------------------------------
!     initialize degree lists and eliminate rows with no off-diag. nz.
!     ----------------------------------------------------------------
DO 20 I = 1, N
  DEG = DEGREE (I)
! begin HALO-SCHUR
  IF (DEG.EQ.N+1)  GOTO 20
! end HALO-SCHUR
!
  IF (DEG .GT. 0) THEN
!         ----------------------------------------------------------
!         place i in the degree list corresponding to its degree
!         ----------------------------------------------------------
    INEXT = HEAD (DEG)
    IF (INEXT .NE. 0) LAST (INEXT) = I
    NEXT (I) = INEXT
    HEAD (DEG) = I
  ELSE
!         ----------------------------------------------------------
!         we have a variable that can be eliminated at once because
!         there is no off-diagonal non-zero in its row.
!         ----------------------------------------------------------
    NEL = NEL + NV(I)
    ELEN (I) = -NEL
    PE (I) = 0_8
    W (I) = 0
  ENDIF
20 CONTINUE
!=======================================================================
!  WHILE (selecting pivots) DO
!=======================================================================
! begin HALO V5
NLEFT = N-NEL
! end HALO V5
! begin HALO
! AMD test:   30 IF (NEL .LT. N) THEN
30 IF (NEL .LT. NREAL) THEN
! end HALO
!=======================================================================
!  GET PIVOT OF MINIMUM DEGREE
!=======================================================================
!       -------------------------------------------------------------
!       find next supervariable for elimination
!       -------------------------------------------------------------
  DO 40 DEG = MINDEG, N
    ME = HEAD (DEG)
    IF (ME .GT. 0) GO TO 50
40   CONTINUE
50   MINDEG = DEG
! begin HALO
  IF (ME.LE.0) THEN
    write (*,*) ' ERROR 1 in HALO_AMD '
!         return to calling program with error return
    NCMPA = -N
    GOTO 500
  ENDIF
! end HALO
!         -------------------------------------------------------------
!         remove chosen variable from link list
!         -------------------------------------------------------------
    INEXT = NEXT (ME)
    IF (INEXT .NE. 0) LAST (INEXT) = 0
    HEAD (DEG) = INEXT
!       -------------------------------------------------------------
!       me represents the elimination of pivots nel+1 to nel+nv(me).
!       place me itself as the first in this set.  It will be moved
!       to the nel+nv(me) position when the permutation vectors are
!       computed.
!       -------------------------------------------------------------
  ELENME = ELEN (ME)
  ELEN (ME) = - (NEL + 1)
  NVPIV = NV (ME)
  NEL = NEL + NVPIV
!=======================================================================
!  CONSTRUCT NEW ELEMENT
!=======================================================================
!       -------------------------------------------------------------
!       At this point, me is the pivotal supervariable.  It will be
!       converted into the current element.  Scan list of the
!       pivotal supervariable, me, setting tree pointers and
!       constructing new list of supervariables for the new element,
!       me.  p is a pointer to the current position in the old list.
!       -------------------------------------------------------------
!       flag the variable "me" as being in Lme by negating nv (me)
  NV (ME) = -NVPIV
  DEGME = 0
  IF (ELENME .EQ. 0) THEN
!         ----------------------------------------------------------
!         construct the new element in place
!         ----------------------------------------------------------
    PME1 = PE (ME)
    PME2 = PME1 - 1
    DO 60 P = PME1, PME1 + LEN (ME) - 1
      I = IW (P)
      NVI = NV (I)
      IF (NVI .GT. 0) THEN
!             ----------------------------------------------------
!             i is a principal variable not yet placed in Lme.
!             store i in new list
!             ----------------------------------------------------
        DEGME = DEGME + NVI
!             flag i as being in Lme by negating nv (i)
        NV (I) = -NVI
        PME2 = PME2 + 1
        IW (PME2) = I
! begin HALO
        IF (DEGREE(I).LE.N) THEN
! end HALO
!             ----------------------------------------------------
!             remove variable i from degree list. (only if i \in V0)
!             ----------------------------------------------------
        ILAST = LAST (I)
        INEXT = NEXT (I)
        IF (INEXT .NE. 0) LAST (INEXT) = ILAST
        IF (ILAST .NE. 0) THEN
          NEXT (ILAST) = INEXT
        ELSE
!               i is at the head of the degree list
          HEAD (DEGREE (I)) = INEXT
        ENDIF
! begin HALO
        ENDIF
! end HALO
      ENDIF
60     CONTINUE
!         this element takes no new memory in iw:
    NEWMEM = 0
  ELSE
!         ----------------------------------------------------------
!         construct the new element in empty space, iw (pfree ...)
!         ----------------------------------------------------------
    P = PE (ME)
    PME1 = PFREE
    SLENME = LEN (ME) - ELENME
    KNT1_UPDATED = 0
    DO 120 KNT1 = 1, ELENME + 1
      KNT1_UPDATED = KNT1_UPDATED +1
      IF (KNT1 .GT. ELENME) THEN
!             search the supervariables in me.
        E = ME
        PJ = P
        LN = SLENME
      ELSE
!             search the elements in me.
        E = IW (P)
        P = P + 1
        PJ = PE (E)
        LN = LEN (E)
      ENDIF
!           -------------------------------------------------------
!           search for different supervariables and add them to the
!           new list, compressing when necessary. this loop is
!           executed once for each element in the list and once for
!           all the supervariables in the list.
!           -------------------------------------------------------
      KNT2_UPDATED = 0
      DO 110 KNT2 = 1, LN
        KNT2_UPDATED = KNT2_UPDATED+1
        I = IW (PJ)
        PJ = PJ + 1
        NVI = NV (I)
        IF (NVI .GT. 0) THEN
!               -------------------------------------------------
!               compress iw, if necessary
!               -------------------------------------------------
          IF (PFREE .GT. IWLEN) THEN
!                 prepare for compressing iw by adjusting
!                 pointers and lengths so that the lists being
!                 searched in the inner and outer loops contain
!                 only the remaining entries.
            PE (ME) = P
            LEN (ME) = LEN (ME) - KNT1_UPDATED
!                 Reset KNT1_UPDATED in case of recompress 
!                 at same iteration of the loop 120
            KNT1_UPDATED = 0
!                 Check if anything left in supervariable ME
            IF (LEN (ME) .EQ. 0) PE (ME) = 0_8
            PE (E) = PJ
            LEN (E) = LN - KNT2_UPDATED
!                 Reset KNT2_UPDATED in case of recompress 
!                 at same iteration of the loop 110
            KNT2_UPDATED = 0
!                 Check if anything left in element E
            IF (LEN (E) .EQ. 0) PE (E) = 0
            NCMPA = NCMPA + 1
!                 store first item in pe
!                 set first entry to -item
            DO 70 J = 1, N
              PN = PE (J)
              IF (PN .GT. 0) THEN
                PE (J) = int(IW (PN),8)
                IW (PN) = -J
              ENDIF
70             CONTINUE
!                 psrc/pdst point to source/destination
            PDST = 1
            PSRC = 1
            PEND = PME1 - 1
!                 while loop:
80             CONTINUE
            IF (PSRC .LE. PEND) THEN
!                   search for next negative entry
              J = -IW (PSRC)
              PSRC = PSRC + 1
              IF (J .GT. 0) THEN
                IW (PDST) = int(PE (J))
                PE (J) = PDST
                PDST = PDST + 1
!                     copy from source to destination
                LENJ = LEN (J)
                DO 90 KNT3 = 0, LENJ - 2
                  IW (PDST + KNT3) = IW (PSRC + KNT3)
90                 CONTINUE
                PDST = PDST + LENJ - 1
                PSRC = PSRC + LENJ - 1
              ENDIF
              GO TO 80
            ENDIF
!                 move the new partially-constructed element
            P1 = PDST
            DO 100 PSRC = PME1, PFREE - 1
              IW (PDST) = IW (PSRC)
              PDST = PDST + 1
100             CONTINUE
            PME1 = P1
            PFREE = PDST
            PJ = PE (E)
            P = PE (ME)
          ENDIF
!               -------------------------------------------------
!               i is a principal variable not yet placed in Lme
!               store i in new list
!               -------------------------------------------------
          DEGME = DEGME + NVI
!               flag i as being in Lme by negating nv (i)
          NV (I) = -NVI
          IW (PFREE) = I
          PFREE = PFREE + 1
! begin HALO
        IF (DEGREE(I).LE.N) THEN
! end HALO
!               -------------------------------------------------
!               remove variable i from degree link list 
!                            (only if i in V0)
!               -------------------------------------------------
          ILAST = LAST (I)
          INEXT = NEXT (I)
          IF (INEXT .NE. 0) LAST (INEXT) = ILAST
          IF (ILAST .NE. 0) THEN
            NEXT (ILAST) = INEXT
          ELSE
!                 i is at the head of the degree list
            HEAD (DEGREE (I)) = INEXT
          ENDIF
! begin HALO
        ENDIF
! end HALO
        ENDIF
110       CONTINUE
      IF (E .NE. ME) THEN
!             set tree pointer and flag to indicate element e is
!             absorbed into new element me (the parent of e is me)
        PE (E) = int(-ME,8)
        W (E) = 0
      ENDIF
120     CONTINUE
    PME2 = PFREE - 1
!         this element takes newmem new memory in iw (possibly zero)
    NEWMEM = PFREE - PME1
    MEM = MEM + NEWMEM
    MAXMEM = max (MAXMEM, MEM)
  ENDIF
!       -------------------------------------------------------------
!       me has now been converted into an element in iw (pme1..pme2)
!       -------------------------------------------------------------
!       degme holds the external degree of new element
  DEGREE (ME) = DEGME
  PE (ME) = PME1
  LEN (ME) = int(PME2 - PME1 + 1_8)
!       -------------------------------------------------------------
!       make sure that wflg is not too large.  With the current
!       value of wflg, wflg+n must not cause integer overflow
!       -------------------------------------------------------------
  IF (WFLG .GT. MAXINT_N) THEN
    DO 130 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
130     CONTINUE
    WFLG = 2
  ENDIF
!=======================================================================
!  COMPUTE (w (e) - wflg) = |Le\Lme| FOR ALL ELEMENTS
!=======================================================================
!       -------------------------------------------------------------
!       Scan 1:  compute the external degrees of previous elements
!       with respect to the current element.  That is:
!            (w (e) - wflg) = |Le \ Lme|
!       for each element e that appears in any supervariable in Lme.
!       The notation Le refers to the pattern (list of
!       supervariables) of a previous element e, where e is not yet
!       absorbed, stored in iw (pe (e) + 1 ... pe (e) + iw (pe (e))).
!       The notation Lme refers to the pattern of the current element
!       (stored in iw (pme1..pme2)).   If (w (e) - wflg) becomes
!       zero, then the element e will be absorbed in scan 2.
!       -------------------------------------------------------------
  DO 150 PME = PME1, PME2
    I = IW (PME)
    ELN = ELEN (I)
    IF (ELN .GT. 0) THEN
!           note that nv (i) has been negated to denote i in Lme:
      NVI = -NV (I)
      WNVI = WFLG - NVI
      DO 140 P = PE (I), PE (I) + int(ELN - 1,8)
        E = IW (P)
        WE = W (E)
        IF (WE .GE. WFLG) THEN
!               unabsorbed element e has been seen in this loop
          WE = WE - NVI
        ELSE IF (WE .NE. 0) THEN
!               e is an unabsorbed element
!               this is the first we have seen e in all of Scan 1
          WE = DEGREE (E) + WNVI
        ENDIF
        W (E) = WE
140       CONTINUE
    ENDIF
150   CONTINUE
!=======================================================================
!  DEGREE UPDATE AND ELEMENT ABSORPTION
!=======================================================================
!       -------------------------------------------------------------
!       Scan 2:  for each i in Lme, sum up the degree of Lme (which
!       is degme), plus the sum of the external degrees of each Le
!       for the elements e appearing within i, plus the
!       supervariables in i.  Place i in hash list.
!       -------------------------------------------------------------
  DO 180 PME = PME1, PME2
    I = IW (PME)
    P1 = PE (I)
    P2 = P1 + ELEN (I) - 1
    PN = P1
    HASH = 0_8
    DEG = 0
!         ----------------------------------------------------------
!         scan the element list associated with supervariable i
!         ----------------------------------------------------------
    DO 160 P = P1, P2
      E = IW (P)
!           dext = | Le \ Lme |
      DEXT = W (E) - WFLG
      IF (DEXT .GT. 0) THEN
        DEG = DEG + DEXT
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + int(E,kind=8)
      ELSE IF (DEXT .EQ. 0) THEN
#if defined (NOAGG3)
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + E
#else
!             aggressive absorption: e is not adjacent to me, but
!             the |Le \ Lme| is 0, so absorb it into me
        PE (E) = int(-ME,8)
        W (E) = 0
#endif
      ENDIF
160     CONTINUE
!         count the number of elements in i (including me):
    ELEN (I) = int(PN - P1 + 1_8)
!         ----------------------------------------------------------
!         scan the supervariables in the list associated with i
!         ----------------------------------------------------------
    P3 = PN
    DO 170 P = P2 + 1, P1 + int(LEN (I) - 1,8)
      J = IW (P)
      NVJ = NV (J)
      IF (NVJ .GT. 0) THEN
!             j is unabsorbed, and not in Lme.
!             add to degree and add to new list
        DEG = DEG + NVJ
        IW (PN) = J
        PN = PN + 1
        HASH = HASH + int(J,kind=8)
      ENDIF
170     CONTINUE
! begin HALO
    IF (DEGREE(I).EQ.N+1) DEG = N+1
! end HALO
!         ----------------------------------------------------------
!         update the degree and check for mass elimination
!         ----------------------------------------------------------
#if defined (NOAGG3)
    IF (ELEN(I).EQ.1 .AND. P3.EQ.PN) THEN
#else
    IF (DEG .EQ. 0) THEN
#endif
!           -------------------------------------------------------
!           mass elimination
!           -------------------------------------------------------
!           There is nothing left of this node except for an
!           edge to the current pivot element.  elen (i) is 1,
!           and there are no variables adjacent to node i.
!           Absorb i into the current pivot element, me.
      PE (I) = int(-ME,8)
      NVI = -NV (I)
      DEGME = DEGME - NVI
      NVPIV = NVPIV + NVI
      NEL = NEL + NVI
      NV (I) = 0
      ELEN (I) = 0
    ELSE
!           -------------------------------------------------------
!           update the upper-bound degree of i
!           -------------------------------------------------------
!           the following degree does not yet include the size
!           of the current element, which is added later:
! begin HALO V6
      IF (DEGREE(I).NE.N+1) THEN
!                I does not belong to halo
           DEG        = min (DEG, NLEFT)
           DEGREE (I) = min (DEGREE (I), DEG)
      ENDIF
! end HALO V6
!           -------------------------------------------------------
!           add me to the list for i
!           -------------------------------------------------------
!           move first supervariable to end of list
      IW (PN) = IW (P3)
!           move first element to end of element part of list
      IW (P3) = IW (P1)
!           add new element to front of list.
      IW (P1) = ME
!           store the new length of the list in len (i)
      LEN (I) = int(PN - P1 + 1)
! begin HALO
      IF (DEG.LE.N) THEN
! end HALO
!           -------------------------------------------------------
!           place in hash bucket.  Save hash key of i in last (i).
!           -------------------------------------------------------
      HASH = mod (HASH, HMOD) + 1_8
      J = HEAD (HASH)
      IF (J .LE. 0) THEN
!             the degree list is empty, hash head is -j
        NEXT (I) = -J
        HEAD (HASH) = -I
      ELSE
!             degree list is not empty
!             use last (head (hash)) as hash head
        NEXT (I) = LAST (J)
        LAST (J) = I
      ENDIF
      LAST (I) = int(HASH, kind=kind(LAST))
! begin HALO
      ENDIF
! end HALO
    ENDIF
180   CONTINUE
  DEGREE (ME) = DEGME
!       -------------------------------------------------------------
!       Clear the counter array, w (...), by incrementing wflg.
!       -------------------------------------------------------------
  DMAX = max (DMAX, DEGME)
  WFLG = WFLG + DMAX
!       make sure that wflg+n does not cause integer overflow
  IF (WFLG .GT. MAXINT_N) THEN
    DO 190 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
190     CONTINUE
    WFLG = 2
  ENDIF
!       at this point, w (1..n) .lt. wflg holds
!=======================================================================
!  SUPERVARIABLE DETECTION
!=======================================================================
  DO 250 PME = PME1, PME2
    I = IW (PME)
! begin HALO
! old AMD          IF (NV (I) .LT. 0) THEN
    IF ( (NV (I) .LT. 0) .AND. (DEGREE(I) .LE. N) ) THEN
! end HALO
!           i is a principal variable in Lme
!           -------------------------------------------------------
!           examine all hash buckets with 2 or more variables.  We
!           do this by examing all unique hash keys for super-
!           variables in the pattern Lme of the current element, me
!           -------------------------------------------------------
      HASH = int(LAST (I),kind=8)
!           let i = head of hash bucket, and empty the hash bucket
      J = HEAD (HASH)
      IF (J .EQ. 0) GO TO 250
      IF (J .LT. 0) THEN
!             degree list is empty
        I = -J
        HEAD (HASH) = 0
      ELSE
!             degree list is not empty, restore last () of head
        I = LAST (J)
        LAST (J) = 0
      ENDIF
      IF (I .EQ. 0) GO TO 250
!           while loop:
200       CONTINUE
      IF (NEXT (I) .NE. 0) THEN
!             ----------------------------------------------------
!             this bucket has one or more variables following i.
!             scan all of them to see if i can absorb any entries
!             that follow i in hash bucket.  Scatter i into w.
!             ----------------------------------------------------
        LN = LEN (I)
        ELN = ELEN (I)
!             do not flag the first element in the list (me)
        DO 210 P = PE (I) + 1, PE (I) + LN - 1
          W (IW (P)) = WFLG
210         CONTINUE
!             ----------------------------------------------------
!             scan every other entry j following i in bucket
!             ----------------------------------------------------
        JLAST = I
        J = NEXT (I)
!             while loop:
220         CONTINUE
        IF (J .NE. 0) THEN
!               -------------------------------------------------
!               check if j and i have identical nonzero pattern
!               -------------------------------------------------
!               jump if i and j do not have same size data structure
          IF (LEN (J) .NE. LN) GO TO 240
!               jump if i and j do not have same number adj elts
          IF (ELEN (J) .NE. ELN) GO TO 240
!               do not flag the first element in the list (me)
          DO 230 P = PE (J) + 1, PE (J) + LN - 1
!                 jump if an entry (iw(p)) is in j but not in i
            IF (W (IW (P)) .NE. WFLG) GO TO 240
230           CONTINUE
!               -------------------------------------------------
!               found it!  j can be absorbed into i
!               -------------------------------------------------
          PE (J) = int(-I,8)
!               both nv (i) and nv (j) are negated since they
!               are in Lme, and the absolute values of each
!               are the number of variables in i and j:
          NV (I) = NV (I) + NV (J)
          NV (J) = 0
          ELEN (J) = 0
!               delete j from hash bucket
          J = NEXT (J)
          NEXT (JLAST) = J
          GO TO 220
!               -------------------------------------------------
240           CONTINUE
!               j cannot be absorbed into i
!               -------------------------------------------------
          JLAST = J
          J = NEXT (J)
        GO TO 220
        ENDIF
!             ----------------------------------------------------
!             no more variables can be absorbed into i
!             go to next i in bucket and clear flag array
!             ----------------------------------------------------
        WFLG = WFLG + 1
        I = NEXT (I)
        IF (I .NE. 0) GO TO 200
      ENDIF
    ENDIF
250   CONTINUE
!=======================================================================
!  RESTORE DEGREE LISTS AND REMOVE NONPRINCIPAL SUPERVAR. FROM ELEMENT
!=======================================================================
  P = PME1
  NLEFT = N - NEL
  DO 260 PME = PME1, PME2
    I = IW (PME)
    NVI = -NV (I)
    IF (NVI .GT. 0) THEN
!           i is a principal variable in Lme
!           restore nv (i) to signify that i is principal
      NV (I) = NVI
! begin HALO
      IF (DEGREE(I).LE.N) THEN
! end HALO
!           -------------------------------------------------------
!           compute the external degree (add size of current elem)
!           -------------------------------------------------------
      DEG = min (DEGREE (I) + DEGME - NVI, NLEFT - NVI)
!           -------------------------------------------------------
!           place the supervariable at the head of the degree list
!           -------------------------------------------------------
      INEXT = HEAD (DEG)
      IF (INEXT .NE. 0) LAST (INEXT) = I
      NEXT (I) = INEXT
      LAST (I) = 0
      HEAD (DEG) = I
!           -------------------------------------------------------
!           save the new degree, and find the minimum degree
!           -------------------------------------------------------
      MINDEG = min (MINDEG, DEG)
      DEGREE (I) = DEG
! begin HALO
        ENDIF
! end HALO
!           -------------------------------------------------------
!           place the supervariable in the element pattern
!           -------------------------------------------------------
      IW (P) = I
      P = P + 1
    ENDIF
260   CONTINUE
!=======================================================================
!  FINALIZE THE NEW ELEMENT
!=======================================================================
  NV (ME) = NVPIV + DEGME
!       nv (me) is now the degree of pivot (including diagonal part)
!       save the length of the list for the new element me
  LEN (ME) = int(P - PME1)
  IF (LEN (ME) .EQ. 0) THEN
!         there is nothing left of the current pivot element
    PE (ME) = 0_8
    W (ME) = 0
  ENDIF
  IF (NEWMEM .NE. 0) THEN
!         element was not constructed in place: deallocate part
!         of it (final size is less than or equal to newmem,
!         since newly nonprincipal variables have been removed).
    PFREE = P
    MEM = MEM - NEWMEM + LEN (ME)
  ENDIF
!=======================================================================
!       END WHILE (selecting pivots)
GO TO 30
ENDIF
!=======================================================================
! begin HALO V2
IF (NEL.LT.N) THEN
!
!     All possible pivots (not flagged have been eliminated).
!     We amalgamate all flagged variables at the root and 
!     we finish the elimination tree.
!          1/ Go through all
!          non absorbed elements (root of the subgraph)
!          and absorb in ME
!          2/ perform mass elimination of all dense rows
     DO DEG = MINDEG, N
       ME = HEAD (DEG)
       IF (ME .GT. 0) GO TO 51
     ENDDO
51      MINDEG = DEG
!
     IF (ME.NE.LISTVAR_SCHUR(1)) THEN
       write(6,*) ' ERROR 2 in MUMPS_HAMD '
       write(6,*) ' wrong principal var for Schur !!'
       NCMPA = -N - 2
       CALL MUMPS_ABORT()
     ENDIF
!
     NELME    = -(NEL+1)
     DO X=1,N
      IF ((PE(X).GT.0) .AND. (ELEN(X).LT.0)) THEN
!            X is an unabsorbed element
       PE(X) = int(-ME,8)
!            W(X) = 0 could be suppressed ?? check it
      ELSEIF (DEGREE(X).EQ.N+1) THEN
!            X is a dense row, absorb it in ME (mass elimination)
       NEL   = NEL + NV(X)
       PE(X) = int(-ME,8)
       ELEN(X) = 0
!            Correct value of NV is (secondary variable)
       NV(X) = 0
      ENDIF
     ENDDO
!          ME is the root node
     ELEN(ME) = NELME
!          Correct value of NV is (principal variable)
     NV(ME)   = N-NREAL
     PE(ME)   = 0
! end HALO V2
!
! begin HALO
     IF (NEL.NE.N) THEN
   write(*,*) ' ERROR 2 in MUMPS_HAMD NEL, N=', NEL,N
   NCMPA = -N - 1
  ENDIF
ENDIF
! end HALO
!=======================================================================
!  COMPUTE THE PERMUTATION VECTORS
!=======================================================================
!     ----------------------------------------------------------------
!     The time taken by the following code is O(n).  At this
!     point, elen (e) = -k has been done for all elements e,
!     and elen (i) = 0 has been done for all nonprincipal
!     variables i.  At this point, there are no principal
!     supervariables left, and all elements are absorbed.
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!     compute the ordering of unordered nonprincipal variables
!     ----------------------------------------------------------------
DO 290 I = 1, N
  IF (ELEN (I) .EQ. 0) THEN
!         ----------------------------------------------------------
!         i is an un-ordered row.  Traverse the tree from i until
!         reaching an element, e.  The element, e, was the
!         principal supervariable of i and all nodes in the path
!         from i to when e was selected as pivot.
!         ----------------------------------------------------------
    J = int(-PE (I))
!         while (j is a variable) do:
270     CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        J = int(-PE (J))
        GO TO 270
      ENDIF
      E = J
!           ----------------------------------------------------------
!           get the current pivot ordering of e
!           ----------------------------------------------------------
      K = -ELEN (E)
!           ----------------------------------------------------------
!           traverse the path again from i to e, and compress the
!           path (all nodes point to e).  Path compression allows
!           this code to compute in O(n) time.  Order the unordered
!           nodes in the path, and place the element e at the end.
!           ----------------------------------------------------------
      J = I
!           while (j is a variable) do:
280       CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        JNEXT = int(-PE (J))
        PE (J) = int(-E,8)
        IF (ELEN (J) .EQ. 0) THEN
!               j is an unordered row
          ELEN (J) = K
          K = K + 1
        ENDIF
        J = JNEXT
      GO TO 280
      ENDIF
!         leave elen (e) negative, so we know it is an element
    ELEN (E) = -K
  ENDIF
290 CONTINUE
!     ----------------------------------------------------------------
!     reset the inverse permutation (elen (1..n)) to be positive,
!     and compute the permutation (last (1..n)).
!     ----------------------------------------------------------------
DO 300 I = 1, N
  K = abs (ELEN (I))
  LAST (K) = I
  ELEN (I) = K
300 CONTINUE
!=======================================================================
!  RETURN THE MEMORY USAGE IN IW
!=======================================================================
!     If maxmem is less than or equal to iwlen, then no compressions
!     occurred, and iw (maxmem+1 ... iwlen) was unused.  Otherwise
!     compressions did occur, and iwlen would have had to have been
!     greater than or equal to maxmem for no compressions to occur.
!     Return the value of maxmem in the pfree argument.
500 PFREE = MAXMEM
!===============================
!     Save IPE in PARENT array
DO I=1,N
 PARENT(I) = int(PE(I))
ENDDO
!===============================
RETURN
END SUBROUTINE MUMPS_HAMD
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Description of MUMPS_HAMF4: 
! MUMPS_HAMF4 is a modified version of halo AMD routine MUMPS_HAMD
! implementing an approximate minimum fill-in heuritic.
! Version provided to F. Pellegrini on Nov 2000 to be used in SCOTCH.
! Approximation of level4 of the minimum fill heuristic
!
!   Restrictive integer 64 bit variant :
!   it is assumed that IW array size can exceed 32-bit integer
!
SUBROUTINE MUMPS_HAMF4 &
  &           (NORIG, N, COMPUTE_PERM, NBBUCK, &
  &                   IWLEN, PE, PFREE, LEN, IW, NV, ELEN, &
  &                   LAST, NCMPA, DEGREE, WF, NEXT, W, HEAD &
  &                   , PARENT &
  &                   )
IMPLICIT NONE
!
! Parameters
!    Input not modified
!    N : number of nodes in the complete graph including halo
!    NORIG :
!       if compressed graph (nv(1).ne-1) then
!         NORIG is the sum(nv(i)) for i \in [1:N]
!       else NORIG = N 
INTEGER, INTENT(IN)    :: NORIG, N, NBBUCK
LOGICAL, INTENT(IN)     :: COMPUTE_PERM
INTEGER(8), INTENT(IN) :: IWLEN
!     Input undefined on output 
INTEGER, INTENT(INOUT)  :: LEN(N), IW(IWLEN)
!     NV also meaningful as input to encode compressed graphs
INTEGER, INTENT(INOUT)  :: NV(N)
! 
!     Output only 
INTEGER, INTENT(OUT)   :: NCMPA
INTEGER, INTENT(OUT)   :: ELEN(N), LAST(N)
INTEGER, INTENT(OUT)   :: PARENT(N)
! 
!     Input/output
INTEGER(8), INTENT(INOUT) :: PFREE
INTEGER(8), INTENT(INOUT) :: PE(N)
! 
!     Internal Workspace only
!       Min fill approximation one extra array of size NBBUCK+2 
!       is also needed
INTEGER     :: NEXT(N), DEGREE(N), W(N)
INTEGER     :: HEAD(0:NBBUCK+1), WF(N)
!
!  Comments on the OUTPUT:
!  ----------------------
!  Let V= V0 U V1 the nodes of the initial graph (|V|=n). 
!  The assembly tree corresponds to the tree 
!    of the supernodes (or supervariables). Each node of the 
!    assembly tree is then composed of one principal variable 
!    and a list of secondary variables. The list of 
!    variable of a node (principal + secondary variables) then 
!    describes the structure of the diagonal bloc of the 
!    supernode. 
!  The elimination tree denotes the tree of all the variables(=node) and 
!    is therefore of order n.
!
!  The arrays NV(N) and PE(N) give a description of the 
!  assembly tree. 
!  Note that on output 
!   INTEGER(8) PE array is copied on output into
!   INTEGER PARENT array
!  
!   1/ Description of array nv(N) (on OUTPUT)
!    nv(i)=0 i is a secondary variable 
!    nv(i) >0 i is a principal variable, nv(i) holds the 
!                  the number of elements in column i of L (true degree of i)
!    With compressed graph (nv(1).ne.-1 on input), 
!    nv(i) can be greater than N since degree can be as large as NORIG
!
!   2/ Description of array PE(N) (on OUTPUT)
!       Note that on 
!       pe(i) = -(father of variable/node i) in the elimination tree:
!       If nv (i) .gt. 0, then i represents a node in the assembly tree,
!       and the parent of i is -pe (i), or zero if i is a root.
!       If nv (i) = 0, then (i,-pe (i)) represents an edge in a
!       subtree, the root of which is a node in the assembly tree.
!   
!   3/ Example:
!      Let If be a root node father of Is in the assembly tree. 
!      If is the principal 
!      variable of the node If and let If1, If2, If3 be the secondary variables
!      of node If.
!      Is is the principal 
!      variable of the node Is and let Is1, Is2 be the secondary variables
!      of node Is.
!      
!      THEN: 
!        NV(If1)=NV(If2)=NV(If3) = 0  (secondary variables)
!        NV(Is1)=NV(Is2) = 0  (secondary variables)
!        NV(If) > 0  ( principal variable)
!        NV(Is) > 0  ( principal variable)
!        PE(If)  = 0 (root node)
!        PE(Is)  = -If (If is the father of Is in the assembly tree)
!        PE(If1)=PE(If2)=PE(If3)= -If  ( If is the principal variable)
!        PE(Is1)=PE(Is2)= -Is  ( Is is the principal variable)
!      
!
!
! HALOAMD_V1: (September 1997)
! **********
! Initial version designed to experiment the numerical (fill-in) impact 
! of taking into account the halo. This code should be able 
! to experiment no-halo, partial halo, complete halo.
! DATE: September 17th 1997
!
! HALOAMD is designed to process a gragh composed of two types
!            of nodes, V0 and V1, extracted from a larger gragh. 
!            V0^V1 = {}, 
!       
!            We used Min. degree heuristic to order only 
!            nodes in V0, but the adjacency to nodes
!            in V1 is taken into account during ordering.
!            Nodes in V1 are odered at last.
!            Adjacency between nodes of V1 need not be provided,
!            however |len(i)| must always corresponds to the number of 
!            edges effectively provided in the adjacency list of i.
!          On input :
!          ********
!            Nodes INODE in V1 are flagged with len(INODE) = -degree 
!                           if len(i) =0 and i \in V1 then 
!                           len(i) must be set on input to -NORIG-1
!          ERROR return (negative values in ncmpa)
!          ************
!            negative value in ncmpa indicates an error detected 
!               by HALOAMD.
!
!            The graph provided MUST follow the rule:
!             if (i,j) is an edge in the gragh then 
!             j must be in the adjacency list of i AND 
!             i must be in the adjacency list of j.
!    REMARKS
!    -------
!        
!        1/  Providing edges between nodes of V1 should not 
!            affect the final ordering, only the amount of edges 
!            of the halo should effectively affect the solution.
!            This code should work in the following cases:
!              1/ halo not provided
!              2/ halo partially provided
!              3/ complete halo
!              4/ complete halo+interconnection between nodes of V1.
!
!              1/ should run and provide identical results (w.r.t to current 
!               implementation of AMD in SCOTCH).
!             3/ and 4 should provide identical results.
!
!        2/ All modifications of the AMD initial code are indicated
!           with begin HALO .. end HALO
!
!            
!   Given a representation of the nonzero pattern of a symmetric matrix,
!       A, (excluding the diagonal) perform an approximate minimum
!       fill-in heuristic. Aggresive absorption is
!       used to tighten the bound on the degree.  This can result an
!       significant improvement in the quality of the ordering for
!       some matrices.
!-----------------------------------------------------------------------
! INPUT ARGUMENTS (unaltered):
!-----------------------------------------------------------------------
! n:    The matrix order.
!       Restriction:  n .ge. 1
! compute_perm : indicates if permutations should be computed 
!         on output in last/elen 
! iwlen:        The length of iw (1..iwlen).  On input, the matrix is
!       stored in iw (1..pfree-1).  However, iw (1..iwlen) should be
!       slightly larger than what is required to hold the matrix, at
!       least iwlen .ge. pfree + n is recommended.  Otherwise,
!       excessive compressions will take place.
!       *** We do not recommend running this algorithm with ***
!       ***      iwlen .lt. pfree + n.                      ***
!       *** Better performance will be obtained if          ***
!       ***      iwlen .ge. pfree + n                       ***
!       *** or better yet                                   ***
!       ***      iwlen .gt. 1.2 * pfree                     ***
!       *** (where pfree is its value on input).            ***
!       The algorithm will not run at all if iwlen .lt. pfree-1.
!
!       Restriction: iwlen .ge. pfree-1
!-----------------------------------------------------------------------
! INPUT/OUPUT ARGUMENTS:
!-----------------------------------------------------------------------
! pe:   On input, pe (i) is the index in iw of the start of row i, or
!       zero if row i has no off-diagonal non-zeros.
!
!       During execution, it is used for both supervariables and
!       elements:
!
!       * Principal supervariable i:  index into iw of the
!               description of supervariable i.  A supervariable
!               represents one or more rows of the matrix
!               with identical nonzero pattern.
!       * Non-principal supervariable i:  if i has been absorbed
!               into another supervariable j, then pe (i) = -j.
!               That is, j has the same pattern as i.
!               Note that j might later be absorbed into another
!               supervariable j2, in which case pe (i) is still -j,
!               and pe (j) = -j2.
!       * Unabsorbed element e:  the index into iw of the description
!               of element e, if e has not yet been absorbed by a
!               subsequent element.  Element e is created when
!               the supervariable of the same name is selected as
!               the pivot.
!       * Absorbed element e:  if element e is absorbed into element
!               e2, then pe (e) = -e2.  This occurs when the pattern of
!               e (that is, Le) is found to be a subset of the pattern
!               of e2 (that is, Le2).  If element e is "null" (it has
!               no nonzeros outside its pivot block), then pe (e) = 0.
!
!       On output, pe holds the assembly tree/forest, which implicitly
!       represents a pivot order with identical fill-in as the actual
!       order (via a depth-first search of the tree).
!
!       On output:
!       If nv (i) .gt. 0, then i represents a node in the assembly tree,
!       and the parent of i is -pe (i), or zero if i is a root.
!       If nv (i) = 0, then (i,-pe (i)) represents an edge in a
!       subtree, the root of which is a node in the assembly tree.
!       On output:  (PE is copied on output into PARENT array)
!
! pfree:        On input, the matrix is stored in iw (1..pfree-1) and
!       the rest of the array iw is free.
!       During execution, additional data is placed in iw, and pfree
!       is modified so that components  of iw from pfree are free.
!       On output, pfree is set equal to the size of iw that
!       would have been needed for no compressions to occur.  If
!       ncmpa is zero, then pfree (on output) is less than or equal to
!       iwlen, and the space iw (pfree+1 ... iwlen) was not used.
!       Otherwise, pfree (on output) is greater than iwlen, and all the
!       memory in iw was used.
!
! nv:   On input, encoding of compressed graph:
!        if NV(1) = -1 then graph is not compressed otherwise
!        NV(I) holds the weight of node I. 
!       During execution, abs (nv (i)) is equal to the number of rows
!       that are represented by the principal supervariable i.  If i is
!       a nonprincipal variable, then nv (i) = 0.  Initially,
!       nv (i) = 1 for all i.  nv (i) .lt. 0 signifies that i is a
!       principal variable in the pattern Lme of the current pivot
!       element me.  On output, nv (e) holds the true degree of element
!       e at the time it was created (including the diagonal part).
! begin HALO
!       On output, nv(I) can be used to find node in set V1.
!       Not true anymore : ( nv(I) = N+1 characterizes nodes in V1
!                 instead nodes in V1 are considered as a dense root node )
! end HALO
!-----------------------------------------------------------------------
! INPUT/MODIFIED (undefined on output):
!-----------------------------------------------------------------------
! len:  On input, len (i) 
!           positive or null (>=0) : i \in V0 and 
!                     len(i) holds the number of entries in row i of the
!                     matrix, excluding the diagonal.  
!           negative (<0) : i \in V1, and 
!                     -len(i) hold the number of entries in row i of the
!                     matrix, excluding the diagonal.
!       The contents of len (1..n)
!       are undefined on output.
! iw:   On input, iw (1..pfree-1) holds the description of each row i
!       in the matrix.  The matrix must be symmetric, and both upper
!       and lower triangular parts must be present.  The diagonal must
!       not be present.  Row i is held as follows:
!
!               len (i):  the length of the row i data structure
!               iw (pe (i) ... pe (i) + len (i) - 1):
!                       the list of column indices for nonzeros
!                       in row i (simple supervariables), excluding
!                       the diagonal.  All supervariables start with
!                       one row/column each (supervariable i is just
!                       row i).
!               if len (i) is zero on input, then pe (i) is ignored
!               on input.
!
!               Note that the rows need not be in any particular order,
!               and there may be empty space between the rows.
!
!       During execution, the supervariable i experiences fill-in.
!       This is represented by placing in i a list of the elements
!       that cause fill-in in supervariable i:
!
!               len (i):  the length of supervariable i
!               iw (pe (i) ... pe (i) + elen (i) - 1):
!                       the list of elements that contain i.  This list
!                       is kept short by removing absorbed elements.
!               iw (pe (i) + elen (i) ... pe (i) + len (i) - 1):
!                       the list of supervariables in i.  This list
!                       is kept short by removing nonprincipal
!                       variables, and any entry j that is also
!                       contained in at least one of the elements
!                       (j in Le) in the list for i (e in row i).
!
!       When supervariable i is selected as pivot, we create an
!       element e of the same name (e=i):
!
!               len (e):  the length of element e
!               iw (pe (e) ... pe (e) + len (e) - 1):
!                       the list of supervariables in element e.
!
!       An element represents the fill-in that occurs when supervariable
!       i is selected as pivot (which represents the selection of row i
!       and all non-principal variables whose principal variable is i).
!       We use the term Le to denote the set of all supervariables
!       in element e.  Absorbed supervariables and elements are pruned
!       from these lists when computationally convenient.
!
!       CAUTION:  THE INPUT MATRIX IS OVERWRITTEN DURING COMPUTATION.
!       The contents of iw are undefined on output.
!
!-----------------------------------------------------------------------
! OUTPUT (need not be set on input):
!-----------------------------------------------------------------------
! elen: See the description of iw above.  At the start of execution,
!       elen (i) is set to zero.  During execution, elen (i) is the
!       number of elements in the list for supervariable i.  When e
!       becomes an element, elen (e) = -nel is set, where nel is the
!       current step of factorization.  elen (i) = 0 is done when i
!       becomes nonprincipal.
!
!       For variables, elen (i) .ge. 0 holds 
!       until just before the permutation vectors are computed.  
!       For elements, elen (e) .lt. 0 holds.
!
!       On output elen (1..n) holds the inverse permutation (the same
!       as the 'INVP' argument in Sparspak).  That is, if k = elen (i),
!       then row i is the kth pivot row.  Row i of A appears as the
!       (elen(i))-th row in the permuted matrix, PAP^T.
! last: In a degree list, last (i) is the supervariable preceding i,
!       or zero if i is the head of the list.  In a hash bucket,
!       last (i) is the hash key for i.  last (head (hash)) is also
!       used as the head of a hash bucket if head (hash) contains a
!       degree list (see head, below).
!
!       On output, last (1..n) holds the permutation (the same as the
!       'PERM' argument in Sparspak).  That is, if i = last (k), then
!       row i is the kth pivot row.  Row last (k) of A is the k-th row
!       in the permuted matrix, PAP^T.
! ncmpa:        The number of times iw was compressed.  If this is
!       excessive, then the execution took longer than what could have
!       been.  To reduce ncmpa, try increasing iwlen to be 10% or 20%
!       larger than the value of pfree on input (or at least
!       iwlen .ge. pfree + n).  The fastest performance will be
!       obtained when ncmpa is returned as zero.  If iwlen is set to
!       the value returned by pfree on *output*, then no compressions
!       will occur.
! begin HALO
!        on output ncmpa <0 --> error detected during HALO_AMD:
!           error 1: ncmpa = -N , ordering was stopped.
! end HALO
!
!-----------------------------------------------------------------------
! LOCAL (not input or output - used only during execution):
!-----------------------------------------------------------------------
! degree:       If i is a supervariable, then degree (i) holds the
!       current approximation of the external degree of row i (an upper
!       bound).  The external degree is the number of nonzeros in row i,
!       minus abs (nv (i)) (the diagonal part).  The bound is equal to
!       the external degree if elen (i) is less than or equal to two.
!       We also use the term "external degree" for elements e to refer
!       to |Le \ Lme|.  If e is an element, then degree (e) holds |Le|,
!       which is the degree of the off-diagonal part of the element e
!       (not including the diagonal part).
! begin HALO
!       while processing variables degree(I) =   -NBBUCK-1 (=N2) 
!                                  indicates that i belongs to V1
! end HALO
!
! head: head is used for degree lists.  head (deg) is the first
!       supervariable in a degree list (all supervariables i in a
!       degree list deg have the same approximate degree, namely,
!       deg = degree (i)).  If the list deg is empty then
!       head (deg) = 0.
!
!       During supervariable detection head (hash) also serves as a
!       pointer to a hash bucket.
!       If head (hash) .gt. 0, there is a degree list of degree hash.
!               The hash bucket head pointer is last (head (hash)).
!       If head (hash) = 0, then the degree list and hash bucket are
!               both empty.
!       If head (hash) .lt. 0, then the degree list is empty, and
!               -head (hash) is the head of the hash bucket.
!       After supervariable detection is complete, all hash buckets
!       are empty, and the (last (head (hash)) = 0) condition is
!       restored for the non-empty degree lists.
! next: next (i) is the supervariable following i in a link list, or
!       zero if i is the last in the list.  Used for two kinds of
!       lists:  degree lists and hash buckets (a supervariable can be
!       in only one kind of list at a time).
! w:    The flag array w determines the status of elements and
!       variables, and the external degree of elements.
!
!       for elements:
!          if w (e) = 0, then the element e is absorbed
!          if w (e) .ge. wflg, then w (e) - wflg is the size of
!               the set |Le \ Lme|, in terms of nonzeros (the
!               sum of abs (nv (i)) for each principal variable i that
!               is both in the pattern of element e and NOT in the
!               pattern of the current pivot element, me).
!          if wflg .gt. w (e) .gt. 0, then e is not absorbed and has
!               not yet been seen in the scan of the element lists in
!               the computation of |Le\Lme| in loop 150 below.
!
!       for variables:
!          during supervariable detection, if w (j) .ne. wflg then j is
!          not in the pattern of variable i
!
!       The w array is initialized by setting w (i) = 1 for all i,
!       and by setting wflg = 2.  It is reinitialized if wflg becomes
!       too large (to ensure that wflg+n does not cause integer
!       overflow).
!
! wf : integer array  used to store the already filled area of 
!      the variables adajcent to current pivot. 
!      wf is then used to update the score of variable i.
!
!-----------------------------------------------------------------------
! LOCAL INTEGERS:
!-----------------------------------------------------------------------
INTEGER :: DEG, DEGME, DEXT, DMAX, E, ELENME, ELN, I, &
  &        ILAST, INEXT, J, JLAST, JNEXT, K, KNT1, KNT2, KNT3, &
  &        LENJ, LN, ME, MINDEG, NEL, &
  &        NLEFT, NVI, NVJ, NVPIV, SLENME, WE, WFLG, WNVI, X, &
  &        NBFLAG, LASTD, NELME, WF3, WF4, N2, PAS
INTEGER :: NLEFT_V1
 INTEGER KNT1_UPDATED, KNT2_UPDATED
 INTEGER(8) :: MAXMEM, MEM, NEWMEM
 INTEGER    :: MAXINT_N
 INTEGER(8) :: HASH, HMOD
 DOUBLE PRECISION RMF, RMF1
 DOUBLE PRECISION dummy
 INTEGER idummy
! deg:        the degree of a variable or element
! degme:      size, |Lme|, of the current element, me (= degree (me))
! dext:       external degree, |Le \ Lme|, of some element e
! dmax:       largest |Le| seen so far
! e:          an element
! elenme:     the length, elen (me), of element list of pivotal var.
! eln:        the length, elen (...), of an element list
! hash:       the computed value of the hash function
! hmod:       the hash function is computed modulo hmod = max (1,n-1)
! i:          a supervariable
! ilast:      the entry in a link list preceding i
! inext:      the entry in a link list following i
! j:          a supervariable
! jlast:      the entry in a link list preceding j
! jnext:      the entry in a link list, or path, following j
! k:          the pivot order of an element or variable
! knt1:       loop counter used during element construction
! knt2:       loop counter used during element construction
! knt3:       loop counter used during compression
! lenj:       len (j)
! ln:         length of a supervariable list
! maxint_n:   large integer to test risk of overflow on wflg
! maxmem:     amount of memory needed for no compressions
! me:         current supervariable being eliminated, and the
!                     current element created by eliminating that
!                     supervariable
! mem:        memory in use assuming no compressions have occurred
! mindeg:     current minimum degree
! nel:        number of pivots selected so far
! newmem:     amount of new memory needed for current pivot element
! nleft:      n - nel, the number of nonpivotal rows/columns remaining
! nvi:        the number of variables in a supervariable i (= nv (i))
! nvj:        the number of variables in a supervariable j (= nv (j))
! nvpiv:      number of pivots in current element
! slenme:     number of variables in variable list of pivotal variable
! we:         w (e)
! wflg:       used for flagging the w array.  See description of iw.
! wnvi:       wflg - nv (i)
! x:          either a supervariable or an element
! wf3:  off diagoanl block area
! wf4:  diagonal block area
! mf : Minimum fill
! begin HALO
! nbflag:     number of flagged entries in the initial gragh.
! nreal :     number of entries on which ordering must be perfomed
!             (nreel = N- nbflag)
! nelme number of pivots selected when reaching the root
! lastd index of the last row in the list of dense rows
! end HALO
!-----------------------------------------------------------------------
! LOCAL POINTERS:
!-----------------------------------------------------------------------
INTEGER(8) :: P, P1, P2, P3, PDST, PEND, PJ, PME, PME1, PME2, &
  &              PN, PSRC
!             Any parameter (pe (...) or pfree) or local variable
!             starting with "p" (for Pointer) is an index into iw,
!             and all indices into iw use variables starting with
!             "p."  The only exception to this rule is the iwlen
!             input argument.
! p:          pointer into lots of things
! p1:         pe (i) for some variable i (start of element list)
! p2:         pe (i) + elen (i) -  1 for some var. i (end of el. list)
! p3:         index of first supervariable in clean list
! pdst:       destination pointer, for compression
! pend:       end of memory to compress
! pj:         pointer into an element or variable
! pme:        pointer into the current element (pme1...pme2)
! pme1:       the current element, me, is stored in iw (pme1...pme2)
! pme2:       the end of the current element
! pn:         pointer into a "clean" variable, also used to compress
! psrc:       source pointer, for compression
!-----------------------------------------------------------------------
!  FUNCTIONS CALLED:
!-----------------------------------------------------------------------
INTRINSIC max, min, mod, huge
INTEGER TOTEL
LOGICAL COMPRESS
!=======================================================================
!  INITIALIZATIONS
!=======================================================================
!     HEAD (0:NBBUCK+1)
!
! idummy holds the largest integer - 1
! dummy  = dble (idummy)
idummy = huge(idummy) - 1
dummy = dble(idummy)
!     variable with degree equal to N2 are in halo
!     bucket NBBUCK+1 used for HALO variables
N2 = -NBBUCK-1
! Distance betweeen elements of the N, ..., NBBUCK entries of HEAD
!
PAS = max((N/8), 1)
WFLG = 2
MAXINT_N=huge(WFLG)-N
NCMPA = 0
NEL = 0
HMOD = int(max (1, NBBUCK-1),kind=8)
DMAX = 0
MEM = PFREE - 1
MAXMEM = MEM
MINDEG = 0
NLEFT_V1 = 0
!
NBFLAG = 0
LASTD  = 0
HEAD(0:NBBUCK+1) = 0
DO 10 I = 1, N
  LAST(I) = 0
!        NV(I) = 1
  W(I) = 1
  ELEN (I) = 0
10 CONTINUE
IF(NV(1) .LT. 0) THEN
   COMPRESS = .FALSE.
ELSE
   COMPRESS = .TRUE.
ENDIF
IF(COMPRESS) THEN
   TOTEL = 0
   DO I=1,N
      IF (LEN(I).LT.0) THEN
         DEGREE (I) = N2
         NBFLAG     = NBFLAG +1
         NLEFT_V1   = NLEFT_V1 + NV(I)
         IF (LEN(I).EQ.-NORIG-1) THEN
!     variable in V1 with empty adj list 
            LEN (I)    = 0
!     Because of compress, we force skipping this
!     entry which is anyway empty
            PE (I)     = 0_8
         ELSE
            LEN (I)    = - LEN(I)
         ENDIF
!       end HALO V3
      ELSE
         TOTEL = TOTEL + NV(I)
         DEGREE(I) = 0
         DO P= PE(I) , PE(I)+int(LEN(I)-1,8)
            DEGREE(I) = DEGREE(I) + NV(IW(P))
         ENDDO
!     DEGREE (I) = LEN (I)
      ENDIF
   ENDDO
ELSE
   DO I=1,N
      NV(I) = 1
      IF (LEN(I).LT.0) THEN
         DEGREE (I) = N2
         NBFLAG     = NBFLAG +1
         NLEFT_V1   = NLEFT_V1 + NV(I)
         IF (LEN(I).EQ.-N-1) THEN
            LEN (I)    = 0
!     Because of compress, we force skipping this
!     entry which is anyway empty
            PE (I)     = 0_8
         ELSE
            LEN (I)    = - LEN(I)
         ENDIF
!     end HALO V3
      ELSE
         DEGREE (I) = LEN (I)
      ENDIF
   ENDDO
   TOTEL = N - NBFLAG
ENDIF
!
!
!     ----------------------------------------------------------------
!     initialize degree lists and eliminate rows with no off-diag. nz.
!     ----------------------------------------------------------------
DO 20 I = 1, N
  DEG = DEGREE (I)
  IF (DEG.EQ.N2) THEN
!            DEG = N2 (flagged variables are stored 
!                  in the degree list of NBBUCK + 1
!                  (safe: because max 
!                         max value of degree is NBBUCK)
!
       DEG = NBBUCK + 1
       IF (LASTD.EQ.0) THEN
!              degree list is empty
         LASTD     = I
         HEAD(DEG) = I
         NEXT(I)   = 0
         LAST(I)   = 0
       ELSE
         NEXT(LASTD) = I
         LAST(I)     = LASTD
         LASTD       = I
         NEXT(I)     = 0
       ENDIF
   GOTO 20
  ENDIF
!
!
  IF (DEG .GT. 0) THEN
    WF(I) = DEG
!   version 1
     IF (DEG.GT.NORIG) THEN
      DEG = min(((DEG-NORIG)/PAS) + NORIG, NBBUCK)
     ENDIF
!           Note that if deg=0 then 
!           No fill-in will occur, 
!           but one variable is adjacent to I
!          ----------------------------------------------------------
!          place i in the degree list corresponding to its degree
!          ----------------------------------------------------------
     INEXT = HEAD (DEG)
     IF (INEXT .NE. 0) LAST (INEXT) = I
     NEXT (I) = INEXT
     HEAD (DEG) = I
  ELSE
!         ----------------------------------------------------------
!         we have a variable that can be eliminated at once because
!         there is no off-diagonal non-zero in its row.
!         ----------------------------------------------------------
    NEL = NEL + NV(I)
    ELEN (I) = -NEL
    PE (I) = 0_8
    W (I) = 0
  ENDIF
!=======================================================================
!
20 CONTINUE
!=======================================================================
!  WHILE (selecting pivots) DO
!=======================================================================
NLEFT = TOTEL-NEL + NLEFT_V1
!=======================================================================
! =====================================================================
30 IF (NEL .LT. TOTEL) THEN
! =====================================================================
!  GET PIVOT OF MINIMUM DEGREE
!=======================================================================
!       -------------------------------------------------------------
!       find next supervariable for elimination
!       -------------------------------------------------------------
  DO 40 DEG = MINDEG, NBBUCK
    ME = HEAD (DEG)
    IF (ME .GT. 0) GO TO 50
40   CONTINUE
50   MINDEG = DEG
  IF (ME.LE.0) THEN
    NCMPA = -N
    CALL MUMPS_ABORT()
  ENDIF
 IF (DEG.GT.NORIG) THEN
!        -------------------------------
!        Linear search to find variable 
!        with best score in the list
!        -------------------------------
!        While end of list list not reached
!         NEXT(J) = 0
   J = NEXT(ME)
   K = WF(ME)
55    CONTINUE
   IF (J.GT.0) THEN
    IF (WF(J).LT.K) THEN
     ME = J
     K  = WF(ME)
    ENDIF
    J= NEXT(J)
    GOTO 55
   ENDIF
   ILAST = LAST(ME)
   INEXT = NEXT(ME)
   IF (INEXT .NE. 0) LAST (INEXT) = ILAST
   IF (ILAST .NE. 0) THEN
     NEXT (ILAST) = INEXT
   ELSE
!          me is at the head of the degree list
     HEAD (DEG) = INEXT
   ENDIF
!
  ELSE
!         -------------------------------------------------------------
!         remove chosen variable from link list
!         -------------------------------------------------------------
    INEXT = NEXT (ME)
    IF (INEXT .NE. 0) LAST (INEXT) = 0
    HEAD (DEG) = INEXT
  ENDIF
!       -------------------------------------------------------------
!       me represents the elimination of pivots nel+1 to nel+nv(me).
!       place me itself as the first in this set.  It will be moved
!       to the nel+nv(me) position when the permutation vectors are
!       computed.
!       -------------------------------------------------------------
  ELENME = ELEN (ME)
  ELEN (ME) = - (NEL + 1)
  NVPIV = NV (ME)
  NEL = NEL + NVPIV
!=======================================================================
!  CONSTRUCT NEW ELEMENT
!=======================================================================
!       -------------------------------------------------------------
!       At this point, me is the pivotal supervariable.  It will be
!       converted into the current element.  Scan list of the
!       pivotal supervariable, me, setting tree pointers and
!       constructing new list of supervariables for the new element,
!       me.  p is a pointer to the current position in the old list.
!       -------------------------------------------------------------
!       flag the variable "me" as being in Lme by negating nv (me)
  NV (ME) = -NVPIV
  DEGME = 0
  IF (ELENME .EQ. 0) THEN
!         ----------------------------------------------------------
!         construct the new element in place
!         ----------------------------------------------------------
    PME1 = PE (ME)
    PME2 = PME1 - 1
    DO 60 P = PME1, PME1 + LEN (ME) - 1
      I = IW (P)
      NVI = NV (I)
      IF (NVI .GT. 0) THEN
!             ----------------------------------------------------
!             i is a principal variable not yet placed in Lme.
!             store i in new list
!             ----------------------------------------------------
        DEGME = DEGME + NVI
!             flag i as being in Lme by negating nv (i)
        NV (I) = -NVI
        PME2 = PME2 + 1
        IW (PME2) = I
        IF (DEGREE(I).NE.N2) THEN
!             ----------------------------------------------------
!             remove variable i from degree list. (only if i \in V0)
!             ----------------------------------------------------
        ILAST = LAST (I)
        INEXT = NEXT (I)
        IF (INEXT .NE. 0) LAST (INEXT) = ILAST
        IF (ILAST .NE. 0) THEN
          NEXT (ILAST) = INEXT
        ELSE
!               i is at the head of the degree list
          IF (WF(I).GT.NORIG) THEN
           DEG = min(((WF(I)-NORIG)/PAS) + NORIG, NBBUCK)
          ELSE
           DEG = WF(I)
          ENDIF
          HEAD (DEG) = INEXT
        ENDIF
        ENDIF
      ENDIF
60     CONTINUE
!         this element takes no new memory in iw:
    NEWMEM = 0
  ELSE
!         ----------------------------------------------------------
!         construct the new element in empty space, iw (pfree ...)
!         ----------------------------------------------------------
    P = PE (ME)
    PME1 = PFREE
    SLENME = LEN (ME) - ELENME
    KNT1_UPDATED = 0
    DO 120 KNT1 = 1, ELENME + 1
      KNT1_UPDATED = KNT1_UPDATED +1
      IF (KNT1 .GT. ELENME) THEN
!             search the supervariables in me.
        E = ME
        PJ = P
        LN = SLENME
      ELSE
!             search the elements in me.
        E = IW (P)
        P = P + 1
        PJ = PE (E)
        LN = LEN (E)
      ENDIF
!           -------------------------------------------------------
!           search for different supervariables and add them to the
!           new list, compressing when necessary. this loop is
!           executed once for each element in the list and once for
!           all the supervariables in the list.
!           -------------------------------------------------------
      KNT2_UPDATED = 0
      DO 110 KNT2 = 1, LN
        KNT2_UPDATED = KNT2_UPDATED+1
        I = IW (PJ)
        PJ = PJ + 1
        NVI = NV (I)
        IF (NVI .GT. 0) THEN
!               -------------------------------------------------
!               compress iw, if necessary
!               -------------------------------------------------
          IF (PFREE .GT. IWLEN) THEN
!                 prepare for compressing iw by adjusting
!                 pointers and lengths so that the lists being
!                 searched in the inner and outer loops contain
!                 only the remaining entries.
            PE (ME) = P
            LEN (ME) = LEN (ME) - KNT1_UPDATED
!                 Reset KNT1_UPDATED in case of recompress 
!                 at same iteration of the loop 120
            KNT1_UPDATED = 0
!                 Check if anything left in supervariable ME
            IF (LEN (ME) .EQ. 0) PE (ME) = 0_8
            PE (E) = PJ
            LEN (E) = LN - KNT2_UPDATED
!                 Reset KNT2_UPDATED in case of recompress 
!                 at same iteration of the loop 110
            KNT2_UPDATED = 0
!                 Check if anything left in element E
            IF (LEN (E) .EQ. 0) PE (E) = 0_8
            NCMPA = NCMPA + 1
!                 store first item in pe
!                 set first entry to -item
            DO 70 J = 1, N
              PN = PE (J)
              IF (PN .GT. 0) THEN
                PE (J) = int(IW (PN),8)
                IW (PN) = -J
              ENDIF
70             CONTINUE
!                 psrc/pdst point to source/destination
            PDST = 1
            PSRC = 1
            PEND = PME1 - 1
!                 while loop:
80             CONTINUE
            IF (PSRC .LE. PEND) THEN
!                   search for next negative entry
              J = -IW (PSRC)
              PSRC = PSRC + 1
              IF (J .GT. 0) THEN
                IW (PDST) = int(PE (J))
                PE (J) = PDST
                PDST = PDST + 1_8
!                     copy from source to destination
                LENJ = LEN (J)
                DO 90 KNT3 = 0, LENJ - 2
                  IW (PDST + KNT3) = IW (PSRC + KNT3)
90                 CONTINUE
                PDST = PDST + LENJ - 1
                PSRC = PSRC + LENJ - 1
              ENDIF
              GO TO 80
            ENDIF
!                 move the new partially-constructed element
            P1 = PDST
            DO 100 PSRC = PME1, PFREE - 1
              IW (PDST) = IW (PSRC)
              PDST = PDST + 1
100             CONTINUE
            PME1 = P1
            PFREE = PDST
            PJ = PE (E)
            P = PE (ME)
          ENDIF
!               -------------------------------------------------
!               i is a principal variable not yet placed in Lme
!               store i in new list
!               -------------------------------------------------
          DEGME = DEGME + NVI
!               flag i as being in Lme by negating nv (i)
          NV (I) = -NVI
          IW (PFREE) = I
          PFREE = PFREE + 1
        IF (DEGREE(I).NE.N2) THEN
!               -------------------------------------------------
!               remove variable i from degree link list 
!                            (only if i in V0)
!               -------------------------------------------------
          ILAST = LAST (I)
          INEXT = NEXT (I)
          IF (INEXT .NE. 0) LAST (INEXT) = ILAST
          IF (ILAST .NE. 0) THEN
            NEXT (ILAST) = INEXT
          ELSE
            IF (WF(I).GT.NORIG) THEN
             DEG = min(((WF(I)-NORIG)/PAS) + NORIG , NBBUCK)
            ELSE
             DEG = WF(I)
            ENDIF
!                 i is at the head of the degree list
            HEAD (DEG) = INEXT
          ENDIF
        ENDIF
        ENDIF
110       CONTINUE
      IF (E .NE. ME) THEN
!             set tree pointer and flag to indicate element e is
!             absorbed into new element me (the parent of e is me)
        PE (E) = int(-ME,8)
        W (E) = 0
      ENDIF
120     CONTINUE
    PME2 = PFREE - 1
!         this element takes newmem new memory in iw (possibly zero)
    NEWMEM = PFREE - PME1
    MEM = MEM + NEWMEM
    MAXMEM = max (MAXMEM, MEM)
  ENDIF
!       -------------------------------------------------------------
!       me has now been converted into an element in iw (pme1..pme2)
!       -------------------------------------------------------------
!       degme holds the external degree of new element
  DEGREE (ME) = DEGME
  PE (ME) = PME1
  LEN (ME) = int(PME2 - PME1 + 1_8)
!       -------------------------------------------------------------
!       make sure that wflg is not too large.  With the current
!       value of wflg, wflg+n must not cause integer overflow
!       -------------------------------------------------------------
  IF (WFLG .GT. MAXINT_N) THEN
    DO 130 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
130     CONTINUE
    WFLG = 2
  ENDIF
!=======================================================================
!  COMPUTE (w (e) - wflg) = |Le\Lme| FOR ALL ELEMENTS
!=======================================================================
!       -------------------------------------------------------------
!       Scan 1:  compute the external degrees of previous elements
!       with respect to the current element.  That is:
!            (w (e) - wflg) = |Le \ Lme|
!       for each element e that appears in any supervariable in Lme.
!       The notation Le refers to the pattern (list of
!       supervariables) of a previous element e, where e is not yet
!       absorbed, stored in iw (pe (e) + 1 ... pe (e) + iw (pe (e))).
!       The notation Lme refers to the pattern of the current element
!       (stored in iw (pme1..pme2)).   If (w (e) - wflg) becomes
!       zero, then the element e will be absorbed in scan 2.
!       -------------------------------------------------------------
  DO 150 PME = PME1, PME2
    I = IW (PME)
    ELN = ELEN (I)
    IF (ELN .GT. 0) THEN
!           note that nv (i) has been negated to denote i in Lme:
      NVI = -NV (I)
      WNVI = WFLG - NVI
      DO 140 P = PE (I), PE (I) + int(ELN - 1,8)
        E = IW (P)
        WE = W (E)
        IF (WE .GE. WFLG) THEN
!               unabsorbed element e has been seen in this loop
          WE = WE - NVI
        ELSE IF (WE .NE. 0) THEN
!               e is an unabsorbed element
!               this is the first we have seen e in all of Scan 1
          WE = DEGREE (E) + WNVI
          WF(E) = 0
        ENDIF
        W (E) = WE
140       CONTINUE
    ENDIF
150   CONTINUE
!=======================================================================
!  DEGREE UPDATE AND ELEMENT ABSORPTION
!=======================================================================
!       -------------------------------------------------------------
!       Scan 2:  for each i in Lme, sum up the degree of Lme (which
!       is degme), plus the sum of the external degrees of each Le
!       for the elements e appearing within i, plus the
!       supervariables in i.  Place i in hash list.
!       -------------------------------------------------------------
  DO 180 PME = PME1, PME2
    I = IW (PME)
    P1 = PE (I)
    P2 = P1 + ELEN (I) - 1
    PN = P1
    HASH = 0_8
    DEG  = 0
    WF3  = 0
    WF4  = 0
    NVI  = -NV(I)
!         ----------------------------------------------------------
!         scan the element list associated with supervariable i
!         ----------------------------------------------------------
    DO 160 P = P1, P2
      E = IW (P)
!           dext = | Le \ Lme |
      DEXT = W (E) - WFLG
      IF (DEXT .GT. 0) THEN
        IF ( WF(E) .EQ. 0 ) THEN
!              First time we meet e : compute wf(e) 
!              which holds the surface associated to element e 
!              it will later be deducted from fill-in 
!              area of all variables adjacent to e
         WF(E) = DEXT * ( (2 * DEGREE(E))  -  DEXT - 1)
        ENDIF
        WF4 = WF4 + WF(E)
        DEG = DEG + DEXT
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + int(E, kind=8)
      ELSE IF (DEXT .EQ. 0) THEN
#if defined (NOAGG4)
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + int(E,kind=8)
#else
!             aggressive absorption: e is not adjacent to me, but
!             the |Le \ Lme| is 0, so absorb it into me
        PE (E) = int(-ME,8)
        W (E) = 0
#endif
      ENDIF
160     CONTINUE
!         count the number of elements in i (including me):
    ELEN (I) = int(PN - P1 + 1_8)
!         ----------------------------------------------------------
!         scan the supervariables in the list associated with i
!         ----------------------------------------------------------
    P3 = PN
    DO 170 P = P2 + 1_8, P1 + int(LEN (I) - 1,8)
      J = IW (P)
      NVJ = NV (J)
      IF (NVJ .GT. 0) THEN
!             j is unabsorbed, and not in Lme.
!             add to degree and add to new list
        DEG = DEG + NVJ
        WF3 = WF3 + NVJ
        IW (PN) = J
        PN = PN + 1
        HASH = HASH + int(J,kind=8)
      ENDIF
170     CONTINUE
!
    IF (DEGREE(I).EQ.N2) DEG = N2
!         ----------------------------------------------------------
!         update the degree and check for mass elimination
!         ----------------------------------------------------------
#if defined (NOAGG4)
    IF (ELEN(I).EQ.1 .AND. P3.EQ.PN) THEN
#else
    IF (DEG .EQ. 0) THEN
#endif
!           -------------------------------------------------------
!           mass elimination
!           -------------------------------------------------------
!           There is nothing left of this node except for an
!           edge to the current pivot element.  elen (i) is 1,
!           and there are no variables adjacent to node i.
!           Absorb i into the current pivot element, me.
      PE (I) = int(-ME,8)
      NVI = -NV (I)
      DEGME = DEGME - NVI
      NVPIV = NVPIV + NVI
      NEL = NEL + NVI
      NV (I) = 0
      ELEN (I) = 0
    ELSE
!           -------------------------------------------------------
!           update the upper-bound degree of i
!           -------------------------------------------------------
!           the following degree does not yet include the size
!           of the current element, which is added later:
      IF (DEGREE(I).NE.N2) THEN
!                I does not belong to halo
           IF ( DEGREE (I).LT.DEG ) THEN
!                  Our appox degree is loose.
!                  we keep old value. Note that in 
!                  this case we cannot substract WF(I)
!                  for min-fill score.
             WF4 = 0
             WF3 = 0
           ELSE
             DEGREE(I)  = DEG
           ENDIF
      ENDIF
!
!           compute WF(I) taking into account size of block 3.0
      WF(I)      = WF4 + 2*NVI*WF3
!           -------------------------------------------------------
!           add me to the list for i
!           -------------------------------------------------------
!           move first supervariable to end of list
      IW (PN) = IW (P3)
!           move first element to end of element part of list
      IW (P3) = IW (P1)
!           add new element to front of list.
      IW (P1) = ME
!           store the new length of the list in len (i)
      LEN (I) = int(PN - P1 + 1)
      IF (DEG.NE.N2) THEN
!           -------------------------------------------------------
!           place in hash bucket.  Save hash key of i in last (i).
!           -------------------------------------------------------
      HASH = mod (HASH, HMOD) + 1_8
      J = HEAD (HASH)
      IF (J .LE. 0) THEN
!             the degree list is empty, hash head is -j
        NEXT (I) = -J
        HEAD (HASH) = -I
      ELSE
!             degree list is not empty
!             use last (head (hash)) as hash head
        NEXT (I) = LAST (J)
        LAST (J) = I
      ENDIF
      LAST (I) = int(HASH,kind=kind(LAST))
      ENDIF
    ENDIF
180   CONTINUE
  DEGREE (ME) = DEGME
!       -------------------------------------------------------------
!       Clear the counter array, w (...), by incrementing wflg.
!       -------------------------------------------------------------
  DMAX = max (DMAX, DEGME)
  WFLG = WFLG + DMAX
!       make sure that wflg+n does not cause integer overflow
  IF (WFLG .GT. MAXINT_N) THEN
    DO 190 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
190     CONTINUE
    WFLG = 2
  ENDIF
!       at this point, w (1..n) .lt. wflg holds
!=======================================================================
!  SUPERVARIABLE DETECTION
!=======================================================================
  DO 250 PME = PME1, PME2
    I = IW (PME)
    IF ( (NV (I) .LT. 0) .AND. (DEGREE(I).NE.N2) ) THEN
!           i is a principal variable in Lme
!           -------------------------------------------------------
!           examine all hash buckets with 2 or more variables.  We
!           do this by examing all unique hash keys for super-
!           variables in the pattern Lme of the current element, me
!           -------------------------------------------------------
      HASH = int(LAST (I),kind=8)
!           let i = head of hash bucket, and empty the hash bucket
      J = HEAD (HASH)
      IF (J .EQ. 0) GO TO 250
      IF (J .LT. 0) THEN
!             degree list is empty
        I = -J
        HEAD (HASH) = 0
      ELSE
!             degree list is not empty, restore last () of head
        I = LAST (J)
        LAST (J) = 0
      ENDIF
      IF (I .EQ. 0) GO TO 250
!           while loop:
200       CONTINUE
      IF (NEXT (I) .NE. 0) THEN
!             ----------------------------------------------------
!             this bucket has one or more variables following i.
!             scan all of them to see if i can absorb any entries
!             that follow i in hash bucket.  Scatter i into w.
!             ----------------------------------------------------
        LN = LEN (I)
        ELN = ELEN (I)
!             do not flag the first element in the list (me)
        DO 210 P = PE (I) + 1_8, PE (I) + int(LN - 1,8)
          W (IW (P)) = WFLG
210         CONTINUE
!             ----------------------------------------------------
!             scan every other entry j following i in bucket
!             ----------------------------------------------------
        JLAST = I
        J = NEXT (I)
!             while loop:
220         CONTINUE
        IF (J .NE. 0) THEN
!               -------------------------------------------------
!               check if j and i have identical nonzero pattern
!               -------------------------------------------------
!               jump if i and j do not have same size data structure
          IF (LEN (J) .NE. LN) GO TO 240
!               jump if i and j do not have same number adj elts
          IF (ELEN (J) .NE. ELN) GO TO 240
!               do not flag the first element in the list (me)
          DO 230 P = PE (J) + 1_8, PE (J) + int(LN - 1,8)
!                 jump if an entry (iw(p)) is in j but not in i
            IF (W (IW (P)) .NE. WFLG) GO TO 240
230           CONTINUE
!               -------------------------------------------------
!               found it!  j can be absorbed into i
!               -------------------------------------------------
          PE (J) = int(-I,8)
          WF(I)  = max(WF(I),WF(J))
!               both nv (i) and nv (j) are negated since they
!               are in Lme, and the absolute values of each
!               are the number of variables in i and j:
          NV (I) = NV (I) + NV (J)
          NV (J) = 0
          ELEN (J) = 0
!               delete j from hash bucket
          J = NEXT (J)
          NEXT (JLAST) = J
          GO TO 220
!               -------------------------------------------------
240           CONTINUE
!               j cannot be absorbed into i
!               -------------------------------------------------
          JLAST = J
          J = NEXT (J)
        GO TO 220
        ENDIF
!             ----------------------------------------------------
!             no more variables can be absorbed into i
!             go to next i in bucket and clear flag array
!             ----------------------------------------------------
        WFLG = WFLG + 1
        I = NEXT (I)
        IF (I .NE. 0) GO TO 200
      ENDIF
    ENDIF
250   CONTINUE
!=======================================================================
!  RESTORE DEGREE LISTS AND REMOVE NONPRINCIPAL SUPERVAR. FROM ELEMENT
!=======================================================================
  P = PME1
  NLEFT = TOTEL - NEL + NLEFT_V1
  DO 260 PME = PME1, PME2
    I = IW (PME)
    NVI = -NV (I)
    IF (NVI .GT. 0) THEN
!           i is a principal variable in Lme
!           restore nv (i) to signify that i is principal
      NV (I) = NVI
      IF (DEGREE(I).NE.N2) THEN
!           -------------------------------------------------------
!           compute the external degree (add size of current elem)
!           -------------------------------------------------------
!--------------------------
!--------------------------
      IF (DEGREE (I) + DEGME .GT. NLEFT ) THEN
!
        DEG = DEGREE(I)
        RMF1  = dble(DEG)*dble( (DEG-1) + 2*DEGME ) &
  &              - dble(WF(I))
        DEGREE(I) = NLEFT - NVI
        DEG       = DEGREE(I)
        RMF = dble(DEG)*dble(DEG-1) &
  &         -  dble(DEGME-NVI)*dble(DEGME-NVI-1)
        RMF = min(RMF, RMF1)
      ELSE
        DEG = DEGREE(I)
        DEGREE(I) = DEGREE (I) + DEGME - NVI
!             All previous cliques taken into account (AMF4)
        RMF  = dble(DEG)*dble( (DEG-1) + 2*DEGME ) &
  &              - dble(WF(I))
      ENDIF
!
      RMF =  RMF / dble(NVI+1)
!
      IF (RMF.LT.dummy) THEN
       WF(I) = int ( anint( RMF ))
      ELSEIF (RMF / dble(N) .LT. dummy) THEN
       WF(I) = int ( anint( RMF/dble(N) ))
      ELSE
       WF(I) = idummy
      ENDIF
      WF(I) = max(1,WF(I))
      DEG = WF(I)
      IF (DEG.GT.NORIG) THEN
        DEG = min(((DEG-NORIG)/PAS) + NORIG, NBBUCK)
      ENDIF
      INEXT = HEAD (DEG)
      IF (INEXT .NE. 0) LAST (INEXT) = I
      NEXT (I) = INEXT
      LAST (I) = 0
      HEAD (DEG) = I
!           -------------------------------------------------------
!           save the new degree, and find the minimum degree
!           -------------------------------------------------------
      MINDEG = min (MINDEG, DEG)
! begin HALO
        ENDIF
! end HALO
!           -------------------------------------------------------
!           place the supervariable in the element pattern
!           -------------------------------------------------------
      IW (P) = I
      P = P + 1
    ENDIF
260   CONTINUE
!=======================================================================
!  FINALIZE THE NEW ELEMENT
!=======================================================================
  NV (ME) = NVPIV + DEGME
!       fill_est = fill_est + nvpiv * (nvpiv + 2 * degme)
!       nv (me) is now the degree of pivot (including diagonal part)
!       save the length of the list for the new element me
  LEN (ME) = int(P - PME1)
  IF (LEN (ME) .EQ. 0) THEN
!         there is nothing left of the current pivot element
    PE (ME) = 0_8
    W (ME) = 0
  ENDIF
  IF (NEWMEM .NE. 0) THEN
!         element was not constructed in place: deallocate part
!         of it (final size is less than or equal to newmem,
!         since newly nonprincipal variables have been removed).
    PFREE = P
    MEM = MEM - NEWMEM + int(LEN (ME),8)
  ENDIF
!=======================================================================
!       END WHILE (selecting pivots)
GO TO 30
ENDIF
!=======================================================================
! begin HALO V2
IF (NEL.LT.NORIG) THEN
!
!     All possible pivots (not flagged have been eliminated).
!     We amalgamate all flagged variables at the root and 
!     we finish the elimination tree.
!          1/ Go through all
!          non absorbed elements (root of the subgraph)
!          and absorb in ME
!          2/ perform mass elimination of all dense rows
     DO DEG = MINDEG, NBBUCK+1
       ME = HEAD (DEG)
       IF (ME .GT. 0) GO TO 51
     ENDDO
51      MINDEG = DEG
     NELME    = -(NEL+1)
     DO X=1,N
      IF ((PE(X).GT.0) .AND. (ELEN(X).LT.0)) THEN
!            X is an unabsorbed element
       PE(X) = int(-ME,8)
!            W(X) = 0 could be suppressed ?? check it
      ELSEIF (DEGREE(X).EQ.N2) THEN
!            X is a dense row, absorb it in ME (mass elimination)
       NEL   = NEL + NV(X)
       PE(X) = int(-ME,8)
       ELEN(X) = 0
!            Correct value of NV is (secondary variable)
       NV(X) = 0
      ENDIF
     ENDDO
!          ME is the root node
     ELEN(ME) = NELME
!          Correct value of NV is (principal variable)
     NV(ME)   = NBFLAG
     PE(ME)   = 0_8
  IF (NEL.NE.NORIG) THEN
   NCMPA = -NORIG - 1
   GOTO 500
  ENDIF
ENDIF
! end HALO
!=======================================================================
!  COMPUTE THE PERMUTATION VECTORS and update TREE
!=======================================================================
!     ----------------------------------------------------------------
!     The time taken by the following code is O(n).  At this
!     point, elen (e) = -k has been done for all elements e,
!     and elen (i) = 0 has been done for all nonprincipal
!     variables i.  At this point, there are no principal
!     supervariables left, and all elements are absorbed.
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!     compute the ordering of unordered nonprincipal variables
!     ----------------------------------------------------------------
DO 290 I = 1, N
  IF (ELEN (I) .EQ. 0) THEN
!         ----------------------------------------------------------
!         i is an un-ordered row.  Traverse the tree from i until
!         reaching an element, e.  The element, e, was the
!         principal supervariable of i and all nodes in the path
!         from i to when e was selected as pivot.
!         ----------------------------------------------------------
    J = int(-PE (I))
!         while (j is a variable) do:
270     CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        J = int(-PE (J))
        GO TO 270
      ENDIF
      E = J
!           ----------------------------------------------------------
!           get the current pivot ordering of e
!           ----------------------------------------------------------
      K = -ELEN (E)
!           ----------------------------------------------------------
!           traverse the path again from i to e, and compress the
!           path (all nodes point to e).  Path compression allows
!           this code to compute in O(n) time.  Order the unordered
!           nodes in the path, and place the element e at the end.
!           ----------------------------------------------------------
      J = I
!           while (j is a variable) do:
280       CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        JNEXT = int(-PE (J))
        PE (J) = int(-E,8)
        IF (ELEN (J) .EQ. 0) THEN
!               j is an unordered row
          ELEN (J) = K
          K = K + 1
        ENDIF
        J = JNEXT
      GO TO 280
      ENDIF
!         leave elen (e) negative, so we know it is an element
    ELEN (E) = -K
  ENDIF
290 CONTINUE
IF (COMPUTE_PERM) THEN
!     ----------------------------------------------------------------
!     reset the inverse permutation (elen (1..n)) to be positive,
!     and compute the pivot order (last (1..n)).
!     ----------------------------------------------------------------
! begin COMPRESS
IF(COMPRESS) THEN
!       N is the size of the compressed graph.
!       If the graph was compressed on input then
!       indices in ELEN are in [1,TOTEL]
!       We build the inverse of ELEN in LAST (similar to
!       the pivot order but has zeros in it) and then compress
!       it. Since LAST is assumed to be of size N at the
!       interface level, we need another array to store
!       the inverse of ELEN for entries greater than N
!       We use DEGREE.
  LAST(1:N) = 0
  HEAD(1:TOTEL-N)=0
  DO I = 1, N
    K = abs (ELEN (I))
    IF ( K <= N ) THEN
      LAST (K) = I
    ELSE
      HEAD(K-N)=I
    ENDIF
  ENDDO
  I = 1
  DO K = 1, N
    IF(LAST (K) .NE. 0) THEN
      LAST(I) = LAST(K)
      ELEN(LAST(K)) = I
      I = I + 1
    ENDIF
  ENDDO
  DO K = N+1, TOTEL
    IF (HEAD(K-N) .NE. 0) THEN
      LAST(I)=HEAD(K-N)
      ELEN(HEAD(K-N)) = I
      I = I + 1
    ENDIF
  END DO
ELSE
  DO 300 I = 1, N
     K = abs (ELEN (I))
     LAST (K) = I
     ELEN (I) = K
300   CONTINUE
ENDIF
! end COMPRESS
ENDIF
!=======================================================================
!  RETURN THE MEMORY USAGE IN IW
!=======================================================================
!     If maxmem is less than or equal to iwlen, then no compressions
!     occurred, and iw (maxmem+1 ... iwlen) was unused.  Otherwise
!     compressions did occur, and iwlen would have had to have been
!     greater than or equal to maxmem for no compressions to occur.
!     Return the value of maxmem in the pfree argument.
500 PFREE = MAXMEM
!===============================
!     Save IPE in PARENT array
DO I=1,N
 PARENT(I) = int(PE(I))
ENDDO
!===============================
RETURN
END SUBROUTINE MUMPS_HAMF4
!
!-----------------------------------------------------------------------
! MUMPS_QAMD: modified version of reference AMD routine MUMPS_ANA_H 
! designed to automatically detect and exploit dense or quasi dense
! rows in the reduced matrix at any step of the minimum degree.
!
! References:
!    P.R. AMESTOY, Recent progress in parallel multifrontal solvers
!      for unsymmetric sparse matrices,
!      Proceedings of the 15th World Congress on Scientific Computation,
!      Modelling and Applied Mathematics, IMACS, Berlin (1997).
!    P.R. AMESTOY (1999), Methodes directes paralleles de
!      resolution des systemes creux de grande taille.
!      Rapport de these d'habilitation de l'INPT.
!
! Date 1997
! ---------
!
SUBROUTINE MUMPS_QAMD &
  &                (TOTEL, COMPUTE_PERM, IVersion, THRESH, NDENSE, &
  &                 N, IWLEN, PE, PFREE, LEN, IW, NV, &
  &                 ELEN, LAST, NCMPA, DEGREE, HEAD, NEXT, W, &
  &                 PARENT)
!    Input not modified
INTEGER, INTENT(IN)    :: TOTEL, N
LOGICAL, INTENT(IN)    :: COMPUTE_PERM
INTEGER, INTENT(IN)    :: IVersion, THRESH
INTEGER(8), INTENT(IN) :: IWLEN
INTEGER, INTENT(INOUT)  :: LEN(N), IW(IWLEN)
INTEGER, INTENT(OUT)   :: NCMPA
INTEGER, INTENT(OUT)   :: ELEN(N), PARENT(N)
INTEGER, INTENT(OUT)   :: LAST(N)
INTEGER(8), INTENT(INOUT) :: PFREE
INTEGER(8), INTENT(INOUT) :: PE(N)
!     NV also meaningful as input to encode compressed graphs
INTEGER, INTENT(INOUT)  :: NV(N)
INTEGER, INTENT(OUT) :: NEXT(N), DEGREE(N), HEAD(TOTEL), W(N)
INTEGER, INTENT(OUT) :: NDENSE(N)
! The input integer parameter THRESH defines the quasi density:
! THRESH : input parameter (not modified) 
!  THRESH is used to compute THRESM 
!   <=0 or N Only exactly dense rows in the reduced matrix are selected.
!   >1 and <=N THRESH correspond to the munimum density requirement.
!
!       IVersion =
!                   1 : No dense row detection during elimination
!                       Suppressing dense row selection after 1st
!                       and final restrart (Using initial degree of 
!                         quasi dense
!                         rows when restarting and suppress
!                         dense row selection)
!                   else  : All functionalities enabled
! Additionnal parameters/variables due to dense row manipulation:
! PARAMETERS:
! ----------
!          
! Local variables:
! ---------------
INTEGER THRESM, MINDEN, MAXDEN, NDME
INTEGER NBD,NBED, NBDM, LASTD, NELME
!      INTEGER DEG1
LOGICAL IDENSE
DOUBLE PRECISION RELDEN
! 
! THRESM : Local Integer holding a 
!          potentially modified value of THRESH.
!          When quasi dense rows are reintegrated in the 
!          graph to be processed then THRESM is modified.
!   Note that if one sets THRESM to negative value then
!       <0 Classical AMD algorithm (no dense row detection)
! RELDEN : holds average density to set THRESM automatically
! MINDEN: min degree of quasi-dense rows when restarting
! MAXDEN: max degree of quasi-dense rows when restarting
! NDME  : number of dense row adjacent to me
! NELME number of pivots selected when reching the root
! LASTD index of the last row in the list of dense rows
! NBD is the total number of dense rows selected 
! NBED is the total number of exactly dense rows detected. 
! NBDM is the maximum number of dense rows selected 
! IDENSE is used to indicate that the supervariable I is a dense or
!        quasi-dense row.
!-----------------------------------------------------------------------
!   Given a representation of the nonzero pattern of a symmetric matrix,
!   A, (excluding the diagonal) perform an approximate minimum
!   degree ordering to compute a pivot order
!   such that fill-in in the Cholesky factors A = LL^T is kept low. 
!   Aggressive absorption might be used to
!   tighten the bound on the degree.  This can result a
!   significant improvement in the quality of the ordering for
!   some matrices.
!-----------------------------------------------------------------------
! INPUT ARGUMENTS (unaltered):
!-----------------------------------------------------------------------
! n     : The matrix order.
!         number of supervariables if compress/blocked format
!         Restriction:  n .ge. 1
! totel : Number of variables to eliminate
!         In case of blocked format:
!         each variable i is a supervariable of size nv(i)
!         totel is computed as the sum(nv(i)) for i \in [1:n]
!         the algorithm stops when totel variables are
!         eliminated.
! compute_perm : indicates if permutations should be computed 
!         on output in last/elen 
! iwlen:        The length of iw (1..iwlen).  On input, the matrix is
!       stored in iw (1..pfree-1).  However, iw (1..iwlen) should be
!       slightly larger than what is required to hold the matrix, at
!       least iwlen .ge. pfree + n is recommended.  Otherwise,
!       excessive compressions will take place.
!       *** We do not recommend running this algorithm with ***
!       ***      iwlen .lt. pfree + n.                      ***
!       *** Better performance will be obtained if          ***
!       ***      iwlen .ge. pfree + n                       ***
!       *** or better yet                                   ***
!       ***      iwlen .gt. 1.2 * pfree                     ***
!       *** (where pfree is its value on input).            ***
!       The algorithm will not run at all if iwlen .lt. pfree-1.
!
!       Restriction: iwlen .ge. pfree-1
!-----------------------------------------------------------------------
! INPUT/OUPUT ARGUMENTS:
!-----------------------------------------------------------------------
! pe:   On input, pe (i) is the index in iw of the start of row i, or
!       zero if row i has no off-diagonal non-zeros.
!
!       During execution, it is used for both supervariables and
!       elements:
!
!       * Principal supervariable i:  index into iw of the
!               description of supervariable i.  A supervariable
!               represents one or more rows of the matrix
!               with identical nonzero pattern.
!       * Non-principal supervariable i:  if i has been absorbed
!               into another supervariable j, then pe (i) = -j.
!               That is, j has the same pattern as i.
!               Note that j might later be absorbed into another
!               supervariable j2, in which case pe (i) is still -j,
!               and pe (j) = -j2.
!       * Unabsorbed element e:  the index into iw of the description
!               of element e, if e has not yet been absorbed by a
!               subsequent element.  Element e is created when
!               the supervariable of the same name is selected as
!               the pivot.
!       * Absorbed element e:  if element e is absorbed into element
!               e2, then pe (e) = -e2.  This occurs when the pattern of
!               e (that is, Le) is found to be a subset of the pattern
!               of e2 (that is, Le2).  If element e is "null" (it has
!               no nonzeros outside its pivot block), then pe (e) = 0.
!
!       On output, pe holds the assembly tree/forest, which implicitly
!       represents a pivot order with identical fill-in as the actual
!       order (via a depth-first search of the tree).
!
!       On output:
!       If nv (i) .gt. 0, then i represents a node in the assembly tree,
!       and the parent of i is -pe (i), or zero if i is a root.
!       If nv (i) = 0, then (i,-pe (i)) represents an edge in a
!       subtree, the root of which is a node in the assembly tree.
!
!       On output:  (PE is copied on output into PARENT array)
!
! pfree:        On input, the matrix is stored in iw (1..pfree-1) and
!       the rest of the array iw is free.
!       During execution, additional data is placed in iw, and pfree
!       is modified so that components  of iw from pfree are free.
!       On output, pfree is set equal to the size of iw that
!       would have been needed for no compressions to occur.  If
!       ncmpa is zero, then pfree (on output) is less than or equal to
!       iwlen, and the space iw (pfree+1 ... iwlen) was not used.
!       Otherwise, pfree (on output) is greater than iwlen, and all the
!       memory in iw was used.
!
! nv:   On input, encoding of compressed graph:
!       if nv(1) = -1 then graph is not compressed otherwise
!       nv(I) holds the weight of node I. 
!       During execution, abs (nv (i)) is equal to the number of rows
!       that are represented by the principal supervariable i.  If i is
!       a nonprincipal variable, then nv (i) = 0.  
!       nv (i) .lt. 0 signifies that i is a
!       principal variable in the pattern Lme of the current pivot
!       element me.  
!       On output, nv (e) holds the true degree of element
!       e at the time it was created (including the diagonal part).
! begin HALO
!       On output, nv(I) can be used to find node in set V1.
!       Not true anymore : ( nv(I) = N+1 characterizes nodes in V1. 
!                 instead nodes in V1 are considered as a dense root node )
! end HALO
!-----------------------------------------------------------------------
! INPUT/MODIFIED (undefined on output):
!-----------------------------------------------------------------------
! len:  On input, len (i) holds the number of entries in row i of the
!       matrix, excluding the diagonal.  The contents of len (1..n)
!       are undefined on output.
! iw:   On input, iw (1..pfree-1) holds the description of each row i
!       in the matrix.  The matrix must be symmetric, and both upper
!       and lower triangular parts must be present.  The diagonal must
!       not be present.  Row i is held as follows:
!
!               len (i):  the length of the row i data structure
!               iw (pe (i) ... pe (i) + len (i) - 1):
!                       the list of column indices for nonzeros
!                       in row i (simple supervariables), excluding
!                       the diagonal.  All supervariables start with
!                       one row/column each (supervariable i is just
!                       row i).
!               if len (i) is zero on input, then pe (i) is ignored
!               on input.
!
!               Note that the rows need not be in any particular order,
!               and there may be empty space between the rows.
!
!       During execution, the supervariable i experiences fill-in.
!       This is represented by placing in i a list of the elements
!       that cause fill-in in supervariable i:
!
!               len (i):  the length of supervariable i
!               iw (pe (i) ... pe (i) + elen (i) - 1):
!                       the list of elements that contain i.  This list
!                       is kept short by removing absorbed elements.
!               iw (pe (i) + elen (i) ... pe (i) + len (i) - 1):
!                       the list of supervariables in i.  This list
!                       is kept short by removing nonprincipal
!                       variables, and any entry j that is also
!                       contained in at least one of the elements
!                       (j in Le) in the list for i (e in row i).
!
!       When supervariable i is selected as pivot, we create an
!       element e of the same name (e=i):
!
!               len (e):  the length of element e
!               iw (pe (e) ... pe (e) + len (e) - 1):
!                       the list of supervariables in element e.
!
!       An element represents the fill-in that occurs when supervariable
!       i is selected as pivot (which represents the selection of row i
!       and all non-principal variables whose principal variable is i).
!       We use the term Le to denote the set of all supervariables
!       in element e.  Absorbed supervariables and elements are pruned
!       from these lists when computationally convenient.
!
!       CAUTION:  THE INPUT MATRIX IS OVERWRITTEN DURING COMPUTATION.
!       The contents of iw are undefined on output.
!-----------------------------------------------------------------------
! OUTPUT (need not be set on input):
!-----------------------------------------------------------------------
! elen: See the description of iw above.  At the start of execution,
!       elen (i) is set to zero.  During execution, elen (i) is the
!       number of elements in the list for supervariable i.  When e
!       becomes an element, elen (e) = -nel is set, where nel is the
!       current step of factorization.  elen (i) = 0 is done when i
!       becomes nonprincipal.
!
!       For variables, elen (i) .ge. 0 holds until just before the
!       permutation vectors are computed.  For elements,
!       elen (e) .lt. 0 holds.
!
!       On output elen (1..n) holds the inverse permutation (the same
!       as the 'INVP' argument in Sparspak).  That is, if k = elen (i),
!       then row i is the kth pivot row.  Row i of A appears as the
!       (elen(i))-th row in the permuted matrix, PAP^T.
! last: In a degree list, last (i) is the supervariable preceding i,
!       or zero if i is the head of the list.  In a hash bucket,
!       last (i) is the hash key for i.  last (head (hash)) is also
!       used as the head of a hash bucket if head (hash) contains a
!       degree list (see head, below).
!
!       On output, last (1..n) holds the permutation (the same as the
!       'PERM' argument in Sparspak).  That is, if i = last (k), then
!       row i is the kth pivot row.  Row last (k) of A is the k-th row
!       in the permuted matrix, PAP^T.
! ncmpa:        The number of times iw was compressed.  If this is
!       excessive, then the execution took longer than what could have
!       been.  To reduce ncmpa, try increasing iwlen to be 10% or 20%
!       larger than the value of pfree on input (or at least
!       iwlen .ge. pfree + n).  The fastest performance will be
!       obtained when ncmpa is returned as zero.  If iwlen is set to
!       the value returned by pfree on *output*, then no compressions
!       will occur.
!-----------------------------------------------------------------------
! LOCAL (not input or output - used only during execution):
!-----------------------------------------------------------------------
! degree:       If i is a supervariable, then degree (i) holds the
!       current approximation of the external degree of row i (an upper
!       bound).  The external degree is the number of nonzeros in row i,
!       minus abs (nv (i)) (the diagonal part).  The bound is equal to
!       the external degree if elen (i) is less than or equal to two.
!
!       We also use the term "external degree" for elements e to refer
!       to |Le \ Lme|.  If e is an element, then degree (e) holds |Le|,
!       which is the degree of the off-diagonal part of the element e
!       (not including the diagonal part).
!dense
! degree (I) =N+1 if I is an exactly dense row in reduced matrix.
!            =N+1+LAST_approximate_external_deg of I   
!                      if I is a quasi dense row in reduced matrix.
! All dense or quasi dense rows are stored in the list pointed 
!       by head(n). Quasi-dense rows (degree(I)=n) are stored first, 
!       and are followed by exactly dense rows in the reduced matrix.
!       LASTD holds the last row in this list of dense rows or is zero
!       if the list is empty.
!dense
! head: head is used for degree lists.  head (deg) is the first
!       supervariable in a degree list (all supervariables i in a
!       degree list deg have the same approximate degree, namely,
!       deg = degree (i)).  If the list deg is empty then
!       head (deg) = 0.
!
!       During supervariable detection head (hash) also serves as a
!       pointer to a hash bucket.
!       If head (hash) .gt. 0, there is a degree list of degree hash.
!               The hash bucket head pointer is last (head (hash)).
!       If head (hash) = 0, then the degree list and hash bucket are
!               both empty.
!       If head (hash) .lt. 0, then the degree list is empty, and
!               -head (hash) is the head of the hash bucket.
!       After supervariable detection is complete, all hash buckets
!       are empty, and the (last (head (hash)) = 0) condition is
!       restored for the non-empty degree lists.
! next: next (i) is the supervariable following i in a link list, or
!       zero if i is the last in the list.  Used for two kinds of
!       lists:  degree lists and hash buckets (a supervariable can be
!       in only one kind of list at a time).
! w:    The flag array w determines the status of elements and
!       variables, and the external degree of elements.
!
!       for elements:
!          if w (e) = 0, then the element e is absorbed
!          if w (e) .ge. wflg, then w (e) - wflg is the size of
!               the set |Le \ Lme|, in terms of nonzeros (the
!               sum of abs (nv (i)) for each principal variable i that
!               is both in the pattern of element e and NOT in the
!               pattern of the current pivot element, me).
!          if wflg .gt. w (e) .gt. 0, then e is not absorbed and has
!               not yet been seen in the scan of the element lists in
!               the computation of |Le\Lme| in loop 150 below.
!
!       for variables:
!          during supervariable detection, if w (j) .ne. wflg then j is
!          not in the pattern of variable i
!
!       The w array is initialized by setting w (i) = 1 for all i,
!       and by setting wflg = 2.  It is reinitialized if wflg becomes
!       too large (to ensure that wflg+n does not cause integer
!       overflow).
!-----------------------------------------------------------------------
! LOCAL INTEGERS:
!-----------------------------------------------------------------------
INTEGER :: DEG, DEGME, DEXT, DMAX, E, ELENME, ELN, I, &
  &        ILAST, INEXT, J, JLAST, JNEXT, K, KNT1, KNT2, KNT3, &
  &        LENJ, LN, ME, MINDEG, NEL, &
  &        NLEFT, NVI, NVJ, NVPIV, SLENME, WE, WFLG, WNVI, X
INTEGER KNT1_UPDATED, KNT2_UPDATED
INTEGER(8) MAXMEM, MEM, NEWMEM
INTEGER   :: MAXINT_N
INTEGER(8):: HASH, HMOD
! deg:        the degree of a variable or element
! degme:      size, |Lme|, of the current element, me (= degree (me))
! dext:       external degree, |Le \ Lme|, of some element e
! dmax:       largest |Le| seen so far
! e:          an element
! elenme:     the length, elen (me), of element list of pivotal var.
! eln:        the length, elen (...), of an element list
! hash:       the computed value of the hash function
! hmod:       the hash function is computed modulo hmod = max (1,n-1)
! i:          a supervariable
! ilast:      the entry in a link list preceding i
! inext:      the entry in a link list following i
! j:          a supervariable
! jlast:      the entry in a link list preceding j
! jnext:      the entry in a link list, or path, following j
! k:          the pivot order of an element or variable
! knt1:       loop counter used during element construction
! knt2:       loop counter used during element construction
! knt3:       loop counter used during compression
! lenj:       len (j)
! ln:         length of a supervariable list
! maxint_n:   large integer to test risk of overflow on wflg
! maxmem:     amount of memory needed for no compressions
! me:         current supervariable being eliminated, and the
!                     current element created by eliminating that
!                     supervariable
! mem:        memory in use assuming no compressions have occurred
! mindeg:     current minimum degree
! nel:        number of pivots selected so far
! newmem:     amount of new memory needed for current pivot element
! nleft:      n - nel, the number of nonpivotal rows/columns remaining
! nvi:        the number of variables in a supervariable i (= nv (i))
! nvj:        the number of variables in a supervariable j (= nv (j))
! nvpiv:      number of pivots in current element
! slenme:     number of variables in variable list of pivotal variable
! we:         w (e)
! wflg:       used for flagging the w array.  See description of iw.
! wnvi:       wflg - nv (i)
! x:          either a supervariable or an element
!-----------------------------------------------------------------------
! LOCAL POINTERS:
!-----------------------------------------------------------------------
INTEGER(8) P, P1, P2, P3, PDST, PEND, PJ, PME, PME1, PME2, &
  &           PN, PSRC, PLN, PELN
!             Any parameter (pe (...) or pfree) or local variable
!             starting with "p" (for Pointer) is an index into iw,
!             and all indices into iw use variables starting with
!             "p."  The only exception to this rule is the iwlen
!             input argument.
! p:          pointer into lots of things
! p1:         pe (i) for some variable i (start of element list)
! p2:         pe (i) + elen (i) -  1 for some var. i (end of el. list)
! p3:         index of first supervariable in clean list
! pdst:       destination pointer, for compression
! pend:       end of memory to compress
! pj:         pointer into an element or variable
! pme:        pointer into the current element (pme1...pme2)
! pme1:       the current element, me, is stored in iw (pme1...pme2)
! pme2:       the end of the current element
! pn:         pointer into a "clean" variable, also used to compress
! psrc:       source pointer, for compression
LOGICAL COMPRESS
!-----------------------------------------------------------------------
!  FUNCTIONS CALLED:
!-----------------------------------------------------------------------
INTRINSIC max, min, mod
!=======================================================================
!  INITIALIZATIONS
!=======================================================================
!     ------------------------------------------------------
!     Experiments with automatic setting of parameter THRESH.
!     ------------------------------------------------------
IF (THRESH.GT.0) THEN
   THRESM  = min(N,THRESH)
   DO I=1,N
       THRESM = max(THRESM, LEN(I))
    ENDDO
     RELDEN = dble(PFREE-1)/dble(N)
!      RELDEN holds the average density, THRESM the maximum density
   THRESM =  int(RELDEN)*10 + (THRESM-int(RELDEN))/10 + 1
!     ------------------------------------------------------
!     end automatic setting of THRESM
!     ------------------------------------------------------
ELSE
!        only exactly dense row will be selected
   THRESM = TOTEL
ENDIF
IF (THRESM.GE.0) THEN
 IF ((THRESM.GT.TOTEL).OR.(THRESM.LT.2)) THEN
!      exactly dense rows only
    THRESM = TOTEL
 ENDIF
ENDIF
LASTD = 0
NBD   = 0
NBED  = 0
NBDM  = 0
WFLG = 2
MAXINT_N=huge(WFLG)-N
MINDEG = 1
NCMPA = 0
NEL = 0
HMOD = int(max (1, N-1),kind=8)
DMAX = 0
MEM = PFREE - 1
MAXMEM = MEM
DO I = 1, N
  NDENSE(I)= 0
  W (I) = 1
  ELEN (I) = 0
  LAST(I) = 0
ENDDO
DO I = 1, TOTEL
  HEAD(I) = 0
ENDDO
IF(NV(1) .LT. 0) THEN
   COMPRESS = .FALSE.
ELSE
   COMPRESS = .TRUE.
ENDIF
IF (COMPRESS) THEN
   DO I=1,N
      DEGREE(I) = 0
      DO P= PE(I) , PE(I)+int(LEN(I)-1,8)
         DEGREE(I) = DEGREE(I) + NV(IW(P))
      ENDDO
   ENDDO
ELSE
   DO I=1,N
      NV(I) = 1
      DEGREE (I) = LEN (I)
   ENDDO
ENDIF
!     ----------------------------------------------------------------
!     initialize degree lists and eliminate rows with no off-diag. nz.
!     ----------------------------------------------------------------
!         NEXT = 0
DO 20 I = 1, N
   DEG = DEGREE (I)
   IF (DEG .GT. 0) THEN
!         ----------------------------------------------------------
!         place i in the degree list corresponding to its degree
!         or in the dense row list if i is dense or quasi dense.
!         ----------------------------------------------------------
!         test for row density
      IF ( (THRESM.GE.0) .AND. &
  &           (DEG+NV(I).GE.THRESM) ) THEN
!           I will be inserted in the degree list of N
         NBD = NBD+NV(I)
         IF (DEG+NV(I).NE.TOTEL-NEL) THEN
            DEGREE(I) = DEGREE(I)+TOTEL+1
!            insert I at the beginning of degree list of n
            DEG = TOTEL
            INEXT = HEAD (DEG)
            IF (INEXT .NE. 0) LAST (INEXT) = I
            NEXT (I) = INEXT
            HEAD (DEG) = I
            LAST(I)  = 0
            IF (LASTD.EQ.0) LASTD=I
         ELSE
            NBED = NBED+NV(I)
            DEGREE(I) = TOTEL+1
!            insert I at the end of degree list of n
            DEG = TOTEL
            IF (LASTD.EQ.0) THEN
!              degree list is empty
               LASTD     = I
               HEAD(DEG) = I
               NEXT(I)   = 0
               LAST(I)   = 0
            ELSE
               NEXT(LASTD) = I
               LAST(I)     = LASTD
               LASTD       = I
               NEXT(I)     = 0
            ENDIF
         ENDIF
      ELSE
!           place i in the degree list corresponding to its degree
         INEXT = HEAD (DEG)
         IF (INEXT .NE. 0) LAST (INEXT) = I
         NEXT (I) = INEXT
         HEAD (DEG) = I
      ENDIF
   ELSE
!         ----------------------------------------------------------
!         we have a variable that can be eliminated at once because
!         there is no off-diagonal non-zero in its row.
!         ----------------------------------------------------------
      NEL = NEL + NV(I)
!          NEL = NEL + 1
      ELEN (I) = -NEL
      PE (I) = 0_8
      W (I) = 0
   ENDIF
20 CONTINUE
!         We suppress dense row selection if none of them was found in A 
!         in the 1st pass
    IF (NBD.EQ.0) THRESM = TOTEL
!
!=======================================================================
!  WHILE (selecting pivots) DO
!=======================================================================
30     IF (NEL .LT. TOTEL) THEN
!=======================================================================
!  GET PIVOT OF MINIMUM DEGREE
!=======================================================================
!       -------------------------------------------------------------
!       find next supervariable for elimination
!       -------------------------------------------------------------
  DO 40 DEG = MINDEG, TOTEL
    ME = HEAD (DEG)
    IF (ME .GT. 0) GO TO 50
40   CONTINUE
50   MINDEG = DEG
  IF (DEG.LT.TOTEL)  THEN
!       -------------------------------------------------------------
!       remove chosen variable from link list
!       -------------------------------------------------------------
    INEXT = NEXT (ME)
    IF (INEXT .NE. 0) LAST (INEXT) = 0
    HEAD (DEG) = INEXT
  ELSE
    NBDM = max(NBDM,NBD)
    IF (DEGREE(ME).GT.TOTEL+1) THEN
      MINDEN = NBD
      MAXDEN = 0
      IF (WFLG .GT. MAXINT_N) THEN
       DO  52 X = 1, N
        IF (W (X) .NE. 0) W (X) = 1
52        CONTINUE
       WFLG = 2
      ENDIF
      WFLG = WFLG + 1
51       CONTINUE
!           ---------------------------------------------------------
!           remove chosen variable from link list
!           ---------------------------------------------------------
      INEXT = NEXT (ME)
      IF (INEXT .NE. 0) THEN
         LAST (INEXT) = 0
      ELSE
         LASTD = 0
      ENDIF
!           ----------------------------------------------------------
!           build adjacency list of ME in quotient gragh
!           and calculate its external degree  in ndense(me)
!           ----------------------------------------------------------
      NDENSE(ME) = 0
      W(ME)      = WFLG
      P1 = PE(ME)
      P2 = P1 + int(LEN(ME) -1,8)
!           PLN-1 holds the pointer in IW to the last elet/var in adj list
!              of ME.  LEN(ME) will then be set to PLN-P1
!           PELN-1 hold the pointer in IW to the last elet in adj list
!              of ME.  ELEN(ME) will then be set to PELN-P1
!           element adjacent to ME
      PLN       = P1
      PELN      = P1
      DO 55 P=P1,P2
        E= IW(P)
        IF (W(E).EQ.WFLG) GOTO 55
        W(E) = WFLG
        IF (PE(E).LT.0_8) THEN
!              E is a nonprincipal variable or absorbed element
          X = E
53           X = int(-PE(X))
          IF (W(X) .EQ.WFLG) GOTO 55
          W(X) = WFLG
          IF ( PE(X) .LT. 0_8 ) GOTO 53
          E = X
        ENDIF
!             -------------------------------------------
!             E is an unabsorbed element or a "dense" row
!                 (NOT already flagged)
!             -------------------------------------------
        IF (ELEN(E).LT.0) then
!              E is a new element in adj(ME)
         NDENSE(E) = NDENSE(E) - NV(ME)
         IW(PLN) = IW(PELN)
         IW(PELN) = E
         PLN  = PLN+1_8
         PELN = PELN + 1_8
!              update ndense of ME with all unflagged dense
!              rows in E
         PME1 = PE(E)
         DO 54 PME = PME1, PME1+int(LEN(E)-1,8)
          X = IW(PME)
          IF ((ELEN(X).GE.0).AND.(W(X).NE.WFLG)) THEN
!                X is a dense row 
           NDENSE(ME) = NDENSE(ME) + NV(X)
           W(X) = WFLG
          ENDIF
54          CONTINUE
        ELSE
!              E is a dense row 
         NDENSE(ME) = NDENSE(ME) + NV(E)
         IW(PLN)=E
         PLN = PLN+1_8
        ENDIF
55       CONTINUE
!           ----------------------------------------------
!           DEGREE(ME)-(N+1) holds last external degree computed
!           when Me was detected as dense
!           NDENSE(ME) is the exact external degree of ME
!           ----------------------------------------------
      WFLG     = WFLG + 1
      LEN(ME)  = int(PLN-P1)
      ELEN(ME) = int(PELN-P1)
      NDME = NDENSE(ME)+NV(ME)
      MINDEN = min (MINDEN, NDME)
      MAXDEN = max (MAXDEN, NDME)
!            If we want to select ME as exactly dense (NDME.EQ.NBD)
!            of quasi dense NDME.GE.THRESMupdated then 
!            ndense(of elements adjacent to ME) sould be updated
      IF (NDENSE(ME).EQ.0) NDENSE(ME) =1
      IF (IVersion.EQ.1) THEN
!              ------------------------------------------------
!              place ME in the degree list of  DEGREE(ME)-(N+1)
!              NDENSE is not used in this case (simulate of 
!                      preprocessing )
!              ------------------------------------------------
        DEG = max (DEGREE(ME)-(TOTEL+1), 1)
      ELSE
!              -----------------------------------------
!              place ME in the degree list of NDENSE(ME)
!              -----------------------------------------
        DEG = NDENSE(ME)
      ENDIF
      DEGREE(ME) = DEG
      MINDEG = min(DEG,MINDEG)
      JNEXT = HEAD(DEG)
      IF (JNEXT.NE. 0) LAST (JNEXT) = ME
      NEXT(ME) = JNEXT
      HEAD(DEG) = ME
!           ------------------------------
!           process next quasi dense row
!           ------------------------------
      ME    = INEXT
      IF (ME.NE.0) THEN
        IF (DEGREE(ME).GT.(TOTEL+1) ) GOTO 51
      ENDIF
      HEAD (TOTEL) = ME
!           ---------------------------------------
!           update dense row selection strategy
!           -------------------------------------
!   
      IF (IVersion .EQ.1 ) THEN
       THRESM = TOTEL
      ELSE
       THRESM=max(THRESM*2,MINDEN+(MAXDEN-MINDEN)/2)
!            THRESM = max(THRESM*2, MINDEN*2)
       THRESM = min(THRESM,NBD)
       IF (THRESM.GE.NBD) THRESM=TOTEL
      ENDIF
      NBD    = NBED
!
      GOTO 30
    ENDIF
!         -------------------------------------------------------------
!         -------------------------------------------------------------
    IF (DEGREE(ME).EQ.TOTEL+1) THEN
!         we have only  exactly "dense" rows that we
!         amalgamate at the root node
       IF (NBD.NE.NBED) THEN
          write(6,*) ' Internal ERROR quasi dense rows remains'
          CALL MUMPS_ABORT()
       ENDIF
!          1/ Go through all
!          non absorbed elements (root of the subgraph) 
!          and absorb in ME
!          2/ perform mass elimination of all dense rows
!          RMK: we could compute sum(NVPIV(d)) to check if = NBD
     NELME    = -(NEL+1)
     DO 59 X=1,N
      IF ((PE(X).GT.0_8) .AND. (ELEN(X).LT.0)) THEN
!            X is an unabsorbed element
       PE(X) = int(-ME,8)
!            W(X) = 0 could be suppressed ?? check it
      ELSEIF (DEGREE(X).EQ.TOTEL+1) THEN
!            X is a dense row, absorb it in ME (mass elimination)
       NEL   = NEL + NV(X)
       PE(X) = int(-ME,8)
       ELEN(X) = 0
       NV(X) = 0
      ENDIF
59      CONTINUE
!          ME is the root node
     ELEN(ME) = NELME
     NV(ME)   = NBD
     PE(ME)   = 0_8
     IF (NEL.NE.TOTEL) THEN
      write(6,*) 'Internal ERROR 2 detected in QAMD'
      write(6,*) ' NEL not equal to N: N, NEL =',N,NEL
      CALL MUMPS_ABORT()
     ENDIF
     GOTO 265
    ENDIF
  ENDIF
!       -------------------------------------------------------------
!       me represents the elimination of pivots nel+1 to nel+nv(me).
!       place me itself as the first in this set.  It will be moved
!       to the nel+nv(me) position when the permutation vectors are
!       computed.
!       -------------------------------------------------------------
  ELENME = ELEN (ME)
  ELEN (ME) = - (NEL + 1)
  NVPIV = NV (ME)
  NEL = NEL + NVPIV
  NDENSE(ME) = 0
!=======================================================================
!  CONSTRUCT NEW ELEMENT
!=======================================================================
!       -------------------------------------------------------------
!       At this point, me is the pivotal supervariable.  It will be
!       converted into the current element.  Scan list of the
!       pivotal supervariable, me, setting tree pointers and
!       constructing new list of supervariables for the new element,
!       me.  p is a pointer to the current position in the old list.
!       -------------------------------------------------------------
!       flag the variable "me" as being in Lme by negating nv (me)
  NV (ME) = -NVPIV
  DEGME = 0
  IF (ELENME .EQ. 0) THEN
!         ----------------------------------------------------------
!         construct the new element in place
!         ----------------------------------------------------------
    PME1 = PE (ME)
    PME2 = PME1 - 1
    DO 60 P = PME1, PME1 + int(LEN (ME) - 1,8)
      I = IW (P)
      NVI = NV (I)
      IF (NVI .GT. 0) THEN
!             ----------------------------------------------------
!             i is a principal variable not yet placed in Lme.
!             store i in new list
!             ----------------------------------------------------
        DEGME = DEGME + NVI
!             flag i as being in Lme by negating nv (i)
        NV (I) = -NVI
        PME2 = PME2 + 1_8
        IW (PME2) = I
!             ----------------------------------------------------
!             remove variable i from degree list.
!             ----------------------------------------------------
!             only done for non "dense" rows
        IF (DEGREE(I).LE.TOTEL) THEN
        ILAST = LAST (I)
        INEXT = NEXT (I)
        IF (INEXT .NE. 0) LAST (INEXT) = ILAST
        IF (ILAST .NE. 0) THEN
           NEXT (ILAST) = INEXT
        ELSE
!               i is at the head of the degree list
           HEAD (DEGREE (I)) = INEXT
        ENDIF
        ELSE
         NDENSE(ME) = NDENSE(ME) + NVI
        ENDIF
      ENDIF
60     CONTINUE
!         this element takes no new memory in iw:
    NEWMEM = 0
  ELSE
!         ----------------------------------------------------------
!         construct the new element in empty space, iw (pfree ...)
!         ----------------------------------------------------------
    P = PE (ME)
    PME1 = PFREE
    SLENME = LEN (ME) - ELENME
    KNT1_UPDATED = 0
    DO 120 KNT1 = 1, ELENME + 1
      KNT1_UPDATED = KNT1_UPDATED +1
      IF (KNT1 .GT. ELENME) THEN
!             search the supervariables in me.
        E = ME
        PJ = P
        LN = SLENME
      ELSE
!             search the elements in me.
        E = IW (P)
        P = P + 1
        PJ = PE (E)
        LN = LEN (E)
      ENDIF
!           -------------------------------------------------------
!           search for different supervariables and add them to the
!           new list, compressing when necessary. this loop is
!           executed once for each element in the list and once for
!           all the supervariables in the list.
!           -------------------------------------------------------
      KNT2_UPDATED = 0
      DO 110 KNT2 = 1, LN
        KNT2_UPDATED = KNT2_UPDATED+1
        I = IW (PJ)
        PJ = PJ + 1
        NVI = NV (I)
        IF (NVI .GT. 0) THEN
!               -------------------------------------------------
!               compress iw, if necessary
!               -------------------------------------------------
          IF (PFREE .GT. IWLEN) THEN
!                 prepare for compressing iw by adjusting
!                 pointers and lengths so that the lists being
!                 searched in the inner and outer loops contain
!                 only the remaining entries.
            PE (ME) = P
            LEN (ME) = LEN (ME) - KNT1_UPDATED
!                 Reset KNT1_UPDATED in case of recompress 
!                 at same iteration of the loop 120
            KNT1_UPDATED = 0
!                 Check if anything left in supervariable ME
            IF (LEN (ME) .EQ. 0) PE (ME) = 0_8
            PE (E) = PJ
            LEN (E) = LN - KNT2_UPDATED
!                 Reset KNT2_UPDATED in case of recompress 
!                 at same iteration of the loop 110
            KNT2_UPDATED = 0
!                 Check if anything left in element E
            IF (LEN (E) .EQ. 0) PE (E) = 0_8
            NCMPA = NCMPA + 1
!                 store first item in pe
!                 set first entry to -item
            DO 70 J = 1, N
              PN = PE (J)
              IF (PN .GT. 0) THEN
                PE (J) = int(IW (PN),8)
                IW (PN) = -J
              ENDIF
70             CONTINUE
!                 psrc/pdst point to source/destination
            PDST = 1
            PSRC = 1
            PEND = PME1 - 1
!                 while loop:
80             CONTINUE
            IF (PSRC .LE. PEND) THEN
!                   search for next negative entry
              J = -IW (PSRC)
              PSRC = PSRC + 1
              IF (J .GT. 0) THEN
                IW (PDST) = int(PE (J))
                PE (J) = PDST
                PDST = PDST + 1_8
!                     copy from source to destination
                LENJ = LEN (J)
                DO 90 KNT3 = 0, LENJ - 2
                  IW (PDST + KNT3) = IW (PSRC + KNT3)
90                 CONTINUE
                PDST = PDST + LENJ - 1
                PSRC = PSRC + LENJ - 1
              ENDIF
              GO TO 80
            ENDIF
!                 move the new partially-constructed element
            P1 = PDST
            DO 100 PSRC = PME1, PFREE - 1
              IW (PDST) = IW (PSRC)
              PDST = PDST + 1
100             CONTINUE
            PME1 = P1
            PFREE = PDST
            PJ = PE (E)
            P = PE (ME)
          ENDIF
!               -------------------------------------------------
!               i is a principal variable not yet placed in Lme
!               store i in new list
!               -------------------------------------------------
          DEGME = DEGME + NVI
!               flag i as being in Lme by negating nv (i)
          NV (I) = -NVI
          IW (PFREE) = I
          PFREE = PFREE + 1
!               -------------------------------------------------
!               remove variable i from degree link list
!               -------------------------------------------------
!             only done for non "dense" rows
          IF (DEGREE(I).LE.TOTEL) THEN
          ILAST = LAST (I)
          INEXT = NEXT (I)
          IF (INEXT .NE. 0) LAST (INEXT) = ILAST
          IF (ILAST .NE. 0) THEN
             NEXT (ILAST) = INEXT
          ELSE
!                 i is at the head of the degree list
             HEAD (DEGREE (I)) = INEXT
          ENDIF
          ELSE
           NDENSE(ME) = NDENSE(ME) + NVI
          ENDIF
        ENDIF
110       CONTINUE
      IF (E .NE. ME) THEN
!             set tree pointer and flag to indicate element e is
!             absorbed into new element me (the parent of e is me)
        PE (E) = int(-ME,8)
        W (E) = 0
      ENDIF
120     CONTINUE
    PME2 = PFREE - 1_8
!         this element takes newmem new memory in iw (possibly zero)
    NEWMEM = PFREE - PME1
    MEM = MEM + NEWMEM
    MAXMEM = max (MAXMEM, MEM)
  ENDIF
!       -------------------------------------------------------------
!       me has now been converted into an element in iw (pme1..pme2)
!       -------------------------------------------------------------
!       degme holds the external degree of new element
  DEGREE (ME) = DEGME
  PE (ME) = PME1
  LEN (ME) = int(PME2 - PME1 + 1_8)
!       -------------------------------------------------------------
!       make sure that wflg is not too large.  With the current
!       value of wflg, wflg+n must not cause integer overflow
!       -------------------------------------------------------------
  IF (WFLG .GT. MAXINT_N) THEN
    DO 130 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
130     CONTINUE
    WFLG = 2
  ENDIF
!=======================================================================
!  COMPUTE (w (e) - wflg) = |Le\Lme| FOR ALL ELEMENTS
!=======================================================================
!       -------------------------------------------------------------
!       Scan 1:  compute the external degrees of previous elements
!       with respect to the current element.  That is:
!            (w (e) - wflg) = |Le \ Lme|
!       for each element e that appears in any supervariable in Lme.
!       The notation Le refers to the pattern (list of
!       supervariables) of a previous element e, where e is not yet
!       absorbed, stored in iw (pe (e) + 1 ... pe (e) + iw (pe (e))).
!       The notation Lme refers to the pattern of the current element
!       (stored in iw (pme1..pme2)).   If (w (e) - wflg) becomes
!       zero, then the element e will be absorbed in scan 2.
!       -------------------------------------------------------------
  DO 150 PME = PME1, PME2
    I = IW (PME)
    IF (DEGREE(I).GT.TOTEL) GOTO 150
    ELN = ELEN (I)
    IF (ELN .GT. 0) THEN
!           note that nv (i) has been negated to denote i in Lme:
      NVI = -NV (I)
      WNVI = WFLG - NVI
      DO 140 P = PE (I), PE (I) + int(ELN - 1,8)
        E = IW (P)
        WE = W (E)
        IF (WE .GE. WFLG) THEN
!               unabsorbed element e has been seen in this loop
          WE = WE - NVI
        ELSE IF (WE .NE. 0) THEN
!               e is an unabsorbed element
!               this is the first we have seen e in all of Scan 1
          WE = DEGREE (E) + WNVI - NDENSE(E)
        ENDIF
        W (E) = WE
140       CONTINUE
    ENDIF
150   CONTINUE
!=======================================================================
!  DEGREE UPDATE AND ELEMENT ABSORPTION
!=======================================================================
!       -------------------------------------------------------------
!       Scan 2:  for each i in Lme, sum up the degree of Lme (which
!       is degme), plus the sum of the external degrees of each Le
!       for the elements e appearing within i, plus the
!       supervariables in i.  Place i in hash list.
!       -------------------------------------------------------------
  DO 180 PME = PME1, PME2
    I = IW (PME)
    IF (DEGREE(I).GT.TOTEL) GOTO 180
    P1 = PE (I)
    P2 = P1 + int(ELEN (I) - 1,8)
    PN = P1
    HASH = 0_8
    DEG = 0
!         ----------------------------------------------------------
!         scan the element list associated with supervariable i
!         ----------------------------------------------------------
    DO 160 P = P1, P2
      E = IW (P)
!           dext = | Le \ Lme |
      DEXT = W (E) - WFLG
      IF (DEXT .GT. 0) THEN
        DEG = DEG + DEXT
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + int(E,kind=8)
#if defined (NOAGG5)
!        ------------------------------
!        suppress aggressive absorption
!        ------------------------------
      ELSE IF (DEXT .EQ. 0) THEN
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + int(E,kind=8)
#else
!
!        ------------------------------
!        try aggressive absorption 
!         when possible
!
      ELSE IF ((DEXT .EQ. 0) .AND. &
  &                (NDENSE(ME).EQ.NBD)) THEN
!             aggressive absorption: e is not adjacent to me, but
!             |Le(G') \ Lme(G')| is 0 and all dense rows
!             are in me, so absorb it into me
          PE (E) = int(-ME,8)
          W (E)  = 0
      ELSE IF (DEXT.EQ.0) THEN
            IW(PN) = E
            PN     = PN+1
            HASH   = HASH + int(E,kind=8)
#endif
      ENDIF
160     CONTINUE
!         count the number of elements in i (including me):
    ELEN (I) = int(PN - P1 + 1)
!         ----------------------------------------------------------
!         scan the supervariables in the list associated with i
!         ----------------------------------------------------------
    P3 = PN
    DO 170 P = P2 + 1, P1 + int(LEN (I) - 1,8)
      J = IW (P)
      NVJ = NV (J)
      IF (NVJ .GT. 0) THEN
!             j is unabsorbed, and not in Lme.
!             add to degree and add to new list
!             add degree only of non-dense rows.
        IF (DEGREE(J).LE.TOTEL) DEG=DEG+NVJ
        IW (PN) = J
        PN = PN + 1
        HASH = HASH + int(J,kind=8)
      ENDIF
170     CONTINUE
!         ----------------------------------------------------------
!         update the degree and check for mass elimination
!         ----------------------------------------------------------
#if defined (NOAGG5)
    IF (DEG.EQ.0.AND.(NDENSE(ME).EQ.NBD).AND.(ELEN(I).GT.1)) THEN
!         When mass elimination will be performed then
!         absorb in ME all element adjacent to I
             P1 = PE (I)
!                  exclude ME --> -2
             P2 = P1 + int(ELEN (I),8) - 2_8
             DO P =P1,P2
               E      = IW(P)
               PE (E) = int(-ME,8)
               W (E)  = 0
             ENDDO
    ENDIF
!              .... Ready for mass elimination
#endif
    IF ((DEG .EQ. 0).AND.(NDENSE(ME).EQ.NBD)) THEN
!           -------------------------------------------------------
!           mass elimination
!           -------------------------------------------------------
!           There is nothing left of this node except for an
!           edge to the current pivot element.  elen (i) is 1,
!           and there are no variables adjacent to node i.
!           Absorb i into the current pivot element, me.
      PE (I) = int(-ME,8)
      NVI = -NV (I)
      DEGME = DEGME - NVI
      NVPIV = NVPIV + NVI
      NEL = NEL + NVI
      NV (I) = 0
      ELEN (I) = 0
    ELSE
!           -------------------------------------------------------
!           update the upper-bound degree of i
!           -------------------------------------------------------
!           the following degree does not yet include the size
!           of the current element, which is added later:
      DEGREE(I) = min (DEG+NBD-NDENSE(ME), &
  &                       DEGREE(I))
!           -------------------------------------------------------
!           add me to the list for i
!           -------------------------------------------------------
!           move first supervariable to end of list
      IW (PN) = IW (P3)
!           move first element to end of element part of list
      IW (P3) = IW (P1)
!           add new element to front of list.
      IW (P1) = ME
!           store the new length of the list in len (i)
      LEN (I) = int(PN - P1 + 1)
!           -------------------------------------------------------
!           place in hash bucket.  Save hash key of i in last (i).
!           -------------------------------------------------------
      HASH = mod (HASH, HMOD) + 1_8
      J = HEAD (HASH)
      IF (J .LE. 0) THEN
!             the degree list is empty, hash head is -j
        NEXT (I) = -J
        HEAD (HASH) = -I
      ELSE
!             degree list is not empty
!             use last (head (hash)) as hash head
        NEXT (I) = LAST (J)
        LAST (J) = I
      ENDIF
      LAST (I) = int(HASH,kind=kind(LAST))
    ENDIF
180   CONTINUE
  DEGREE (ME) = DEGME
!       -------------------------------------------------------------
!       Clear the counter array, w (...), by incrementing wflg.
!       -------------------------------------------------------------
  DMAX = max (DMAX, DEGME)
  WFLG = WFLG + DMAX
!       make sure that wflg+n does not cause integer overflow
  IF (WFLG .GT. MAXINT_N) THEN
    DO 190 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
190     CONTINUE
    WFLG = 2
  ENDIF
!       at this point, w (1..n) .lt. wflg holds
!=======================================================================
!  SUPERVARIABLE DETECTION
!=======================================================================
  DO 250 PME = PME1, PME2
    I = IW (PME)
    IF ( (NV(I).LT.0) .AND. (DEGREE(I).LE.TOTEL) ) THEN
!           only done for nondense rows
!           i is a principal variable in Lme
!           -------------------------------------------------------
!           examine all hash buckets with 2 or more variables.  We
!           do this by examing all unique hash keys for super-
!           variables in the pattern Lme of the current element, me
!           -------------------------------------------------------
      HASH = int(LAST (I),kind=8)
!           let i = head of hash bucket, and empty the hash bucket
      J = HEAD (HASH)
      IF (J .EQ. 0) GO TO 250
      IF (J .LT. 0) THEN
!             degree list is empty
        I = -J
        HEAD (HASH) = 0
      ELSE
!             degree list is not empty, restore last () of head
        I = LAST (J)
        LAST (J) = 0
      ENDIF
      IF (I .EQ. 0) GO TO 250
!           while loop:
200       CONTINUE
      IF (NEXT (I) .NE. 0) THEN
!             ----------------------------------------------------
!             this bucket has one or more variables following i.
!             scan all of them to see if i can absorb any entries
!             that follow i in hash bucket.  Scatter i into w.
!             ----------------------------------------------------
        LN = LEN (I)
        ELN = ELEN (I)
!             do not flag the first element in the list (me)
        DO 210 P = PE (I) + 1, PE (I) + int(LN - 1,8)
          W (IW (P)) = WFLG
210         CONTINUE
!             ----------------------------------------------------
!             scan every other entry j following i in bucket
!             ----------------------------------------------------
        JLAST = I
        J = NEXT (I)
!             while loop:
220         CONTINUE
        IF (J .NE. 0) THEN
!               -------------------------------------------------
!               check if j and i have identical nonzero pattern
!               -------------------------------------------------
!               jump if i and j do not have same size data structure
          IF (LEN (J) .NE. LN) GO TO 240
!               jump if i and j do not have same number adj elts
          IF (ELEN (J) .NE. ELN) GO TO 240
!               do not flag the first element in the list (me)
          DO 230 P = PE (J) + 1, PE (J) + int(LN - 1,8)
!                 jump if an entry (iw(p)) is in j but not in i
            IF (W (IW (P)) .NE. WFLG) GO TO 240
230           CONTINUE
!               -------------------------------------------------
!               found it!  j can be absorbed into i
!               -------------------------------------------------
          PE (J) = int(-I,8)
!               both nv (i) and nv (j) are negated since they
!               are in Lme, and the absolute values of each
!               are the number of variables in i and j:
          NV (I) = NV (I) + NV (J)
          NV (J) = 0
          ELEN (J) = 0
!               delete j from hash bucket
          J = NEXT (J)
          NEXT (JLAST) = J
          GO TO 220
!               -------------------------------------------------
240           CONTINUE
!               j cannot be absorbed into i
!               -------------------------------------------------
          JLAST = J
          J = NEXT (J)
        GO TO 220
        ENDIF
!             ----------------------------------------------------
!             no more variables can be absorbed into i
!             go to next i in bucket and clear flag array
!             ----------------------------------------------------
        WFLG = WFLG + 1
        I = NEXT (I)
        IF (I .NE. 0) GO TO 200
      ENDIF
    ENDIF
250   CONTINUE
!=======================================================================
!  RESTORE DEGREE LISTS AND REMOVE NONPRINCIPAL SUPERVAR. FROM ELEMENT
!=======================================================================
  P = PME1
  NLEFT = TOTEL - NEL
  DO 260 PME = PME1, PME2
    I = IW (PME)
    NVI = -NV (I)
    IF (NVI .GT. 0) THEN
!           i is a principal variable in Lme
!           restore nv (i) to signify that i is principal
      NV (I) = NVI
      IF (DEGREE(I).LE.TOTEL) THEN
!           -------------------------------------------------------
!           compute the external degree (add size of current elem)
!           -------------------------------------------------------
      DEG = min (DEGREE (I)+ DEGME - NVI, NLEFT - NVI)
      DEGREE (I) = DEG
      IDENSE = .FALSE.
!           
 IF ( (IVersion .NE. 1).AND. (THRESM.GE.0)) THEN
!           -------------------
!           Dense row detection
!           -------------------
!           DEGME is exact external degree of pivot ME |Le\Ve|, 
!           DEG is is approx external degree of I
!           Relaxed dense row selection based on:
!            1/ We want to avoid selecting dense  rows that are
!               almost completely represented by adj(ME)
!            1/ its density in reduced matrix and 
    IF (DEG+NVI .GE. THRESM) THEN
       IF (THRESM.EQ.TOTEL) THEN
!             We must be sure that I is exactly dense in reduced matrix
          IF ((ELEN(I).LE.2) .AND. ((DEG+NVI).EQ.NLEFT) ) THEN
!              DEG approximation is exact and I is dense 
             DEGREE(I) = TOTEL+1
             IDENSE = .TRUE.
          ENDIF
       ELSE
!             relaxed dense row detection
          IDENSE = .TRUE.
          IF ((ELEN(I).LE.2).AND.((DEG+NVI).EQ.NLEFT) ) THEN
             DEGREE(I) = TOTEL+1
          ELSE
             DEGREE(I) = TOTEL+1+DEGREE(I)
          ENDIF
       ENDIF
    ENDIF
    IF (IDENSE) THEN
!            update NDENSE of all elements in the list of element
!            adjacent to I (including ME).
       P1 = PE(I)
       P2 = P1 + int(ELEN(I) - 1,8)
       IF (P2.GE.P1) THEN
          DO 264 PJ=P1,P2
             E= IW(PJ)
             NDENSE (E) = NDENSE(E) + NVI
264           CONTINUE
       ENDIF
!            insert I in the list of dense rows
       NBD = NBD+NVI
       DEG = TOTEL
       IF (DEGREE(I).EQ.TOTEL+1) THEN
!              insert I at the end of the list
          NBED = NBED +NVI
          IF (LASTD.EQ.0) THEN
!                degree list is empty
             LASTD     = I
             HEAD(DEG) = I
             NEXT(I)   = 0
             LAST(I)   = 0
          ELSE
             NEXT(LASTD) = I
             LAST(I)     = LASTD
             LASTD       = I
             NEXT(I)     = 0
          ENDIF
       ELSE
!              insert I at the beginning of the list
          INEXT = HEAD(DEG)
          IF (INEXT .NE. 0) LAST (INEXT) = I
          NEXT (I) = INEXT
          HEAD (DEG) = I
          LAST(I)    = 0
          IF (LASTD.EQ.0) LASTD=I
       ENDIF
!            end of IDENSE=true
    ENDIF
!           end of THRESM>0
 ENDIF
!             
 IF (.NOT.IDENSE) THEN
!           -------------------------------------------------------
!           place the supervariable at the head of the degree list
!           -------------------------------------------------------
    INEXT = HEAD (DEG)
    IF (INEXT .NE. 0) LAST (INEXT) = I
    NEXT (I) = INEXT
    LAST (I) = 0
    HEAD (DEG) = I
 ENDIF
!           -------------------------------------------------------
!           save the new degree, and find the minimum degree
!           -------------------------------------------------------
 MINDEG = min (MINDEG, DEG)
      ENDIF
!           -------------------------------------------------------
!           place the supervariable in the element pattern
!           -------------------------------------------------------
      IW (P) = I
      P = P + 1
    ENDIF
260   CONTINUE
!=======================================================================
!  FINALIZE THE NEW ELEMENT
!=======================================================================
  NV (ME) = NVPIV + DEGME
!       nv (me) is now the degree of pivot (including diagonal part)
!       save the length of the list for the new element me
  LEN (ME) = int(P - PME1)
  IF (LEN (ME) .EQ. 0) THEN
!         there is nothing left of the current pivot element
    PE (ME) = 0_8
    W (ME) = 0
  ENDIF
  IF (NEWMEM .NE. 0) THEN
!         element was not constructed in place: deallocate part
!         of it (final size is less than or equal to newmem,
!         since newly nonprincipal variables have been removed).
    PFREE = P
    MEM = MEM - NEWMEM + int(LEN (ME),8)
  ENDIF
!=======================================================================
!       END WHILE (selecting pivots)
GO TO 30
ENDIF
!=======================================================================
265 CONTINUE
!=======================================================================
!  COMPUTE THE PERMUTATION VECTORS and update TREE
!=======================================================================
!     ----------------------------------------------------------------
!     The time taken by the following code is O(n).  At this
!     point, elen (e) = -k has been done for all elements e,
!     and elen (i) = 0 has been done for all nonprincipal
!     variables i.  At this point, there are no principal
!     supervariables left, and all elements are absorbed.
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!     compute the ordering of unordered nonprincipal variables
!     ----------------------------------------------------------------
DO 290 I = 1, N
  IF (ELEN (I) .EQ. 0) THEN
!         ----------------------------------------------------------
!         i is an un-ordered row.  Traverse the tree from i until
!         reaching an element, e.  The element, e, was the
!         principal supervariable of i and all nodes in the path
!         from i to when e was selected as pivot.
!         ----------------------------------------------------------
    J = int(-PE (I))
!         while (j is a variable) do:
270     CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        J = int(-PE (J))
        GO TO 270
      ENDIF
      E = J
!           ----------------------------------------------------------
!           get the current pivot ordering of e
!           ----------------------------------------------------------
      K = -ELEN (E)
!           ----------------------------------------------------------
!           traverse the path again from i to e, and compress the
!           path (all nodes point to e).  Path compression allows
!           this code to compute in O(n) time.  Order the unordered
!           nodes in the path, and place the element e at the end.
!           ----------------------------------------------------------
      J = I
!           while (j is a variable) do:
280       CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        JNEXT = int(-PE (J))
        PE (J) = int(-E,8)
        IF (ELEN (J) .EQ. 0) THEN
!               j is an unordered row
          ELEN (J) = K
          K = K + 1
        ENDIF
        J = JNEXT
      GO TO 280
      ENDIF
!         leave elen (e) negative, so we know it is an element
    ELEN (E) = -K
  ENDIF
290 CONTINUE
IF (COMPUTE_PERM) THEN
!     ----------------------------------------------------------------
!     reset the inverse permutation (elen (1..n)) to be positive,
!     and compute the permutation (last (1..n)).
!     ----------------------------------------------------------------
IF(COMPRESS) THEN
  LAST(1:N) = 0
  HEAD(1:TOTEL-N)=0
  DO I = 1, N
    K = abs (ELEN (I))
    IF ( K <= N ) THEN
      LAST (K) = I
    ELSE
      HEAD(K-N)=I
    ENDIF
  ENDDO
  I = 1
  DO K = 1, N
    IF(LAST (K) .NE. 0) THEN
      LAST(I) = LAST(K)
      ELEN(LAST(K)) = I
      I = I + 1
    ENDIF
  ENDDO
  DO K = N+1, TOTEL
    IF (HEAD(K-N) .NE. 0) THEN
      LAST(I)=HEAD(K-N)
      ELEN(HEAD(K-N)) = I
      I = I + 1
    ENDIF
  END DO
ELSE
   DO 300 I = 1, N
      K = abs (ELEN (I))
      LAST (K) = I
      ELEN (I) = K
300    CONTINUE
ENDIF
!=======================================================================
!      END OF COMPUTING PERMUTATIONS
!=======================================================================
 ENDIF
!=======================================================================
!  RETURN THE MEMORY USAGE IN IW
!=======================================================================
!     If maxmem is less than or equal to iwlen, then no compressions
!     occurred, and iw (maxmem+1 ... iwlen) was unused.  Otherwise
!     compressions did occur, and iwlen would have had to have been
!     greater than or equal to maxmem for no compressions to occur.
!     Return the value of maxmem in the pfree argument.
PFREE = MAXMEM
!===============================
!     Save PE in PARENT array
DO I=1,N
 PARENT(I) = int(PE(I))
ENDDO
!===============================
RETURN
END SUBROUTINE MUMPS_QAMD
!-----------------------------------------------------------------------
! MUMPS_CST_AMF: modified version of MUMPS_HAMF4 routine 
! implementing constraint minimum fill-in based 
! ordering.
! Written by Stephane Pralet iduring his post-doctorate at INPT-IRIT 
! (Oct. 2004- Oct. 2005)
!
!   Restrictive integer 64 bit variant :
!   it is assumed that IW array size can exceed 32-bit integer
!
SUBROUTINE MUMPS_CST_AMF (N, NBBUCK, &
  &     IWLEN, PE, PFREE, LEN, IW, NV, ELEN, &
  &     LAST, NCMPA, DEGREE, WF, NEXT, W, HEAD, &
  &     CONSTRAINT,THESON, PARENT)
IMPLICIT NONE
!
! Parameters
!    Input not modified
INTEGER, INTENT(IN)    :: N, NBBUCK
INTEGER(8), INTENT(IN) :: IWLEN
!     Input undefined on output 
INTEGER, INTENT(INOUT)  :: LEN(N), IW(IWLEN)
!     NV meaningful as input to encode compressed graphs
INTEGER, INTENT(INOUT)  :: NV(N)
! 
!     Output only 
INTEGER, INTENT(OUT)   :: NCMPA
INTEGER, INTENT(OUT)   :: ELEN(N), LAST(N), PARENT(N)
! 
!     Input/output
INTEGER(8), INTENT(INOUT) :: PFREE
INTEGER(8), INTENT(INOUT) :: PE(N)
! 
!     Internal Workspace only
!       Min fill approximation one extra array of size NBBUCK+2 
!       is also needed
INTEGER     :: NEXT(N), DEGREE(N), W(N)
INTEGER     :: HEAD(0:NBBUCK+1), WF(N)
!
!  Comments on the OUTPUT:
!  ----------------------
!  Let V= V0 U V1 the nodes of the initial graph (|V|=n). 
!  The assembly tree corresponds to the tree 
!    of the supernodes (or supervariables). Each node of the 
!    assembly tree is then composed of one principal variable 
!    and a list of secondary variables. The list of 
!    variable of a node (principal + secondary variables) then 
!    describes the structure of the diagonal bloc of the 
!    supernode. 
!  The elimination tree denotes the tree of all the variables(=node) and 
!    is therefore of order n.
!
!  The arrays NV(N) and PE(N) give a description of the 
!  assembly tree. 
!  
!   1/ Description of array nv(N) (on OUPUT)
!    nv(i)=0 i is a secondary variable 
!    N+1> nv(i) >0 i is a principal variable, nv(i) holds the 
!                  the number of elements in column i of L (true degree of i)
!
!   2/ Description of array PE(N) (on OUPUT)
!       pe(i) = -(father of variable/node i) in the elimination tree:
!       If nv (i) .gt. 0, then i represents a node in the assembly tree,
!       and the parent of i is -pe (i), or zero if i is a root.
!       If nv (i) = 0, then (i,-pe (i)) represents an edge in a
!       subtree, the root of which is a node in the assembly tree.
!   
!   3/ Example:
!      Let If be a root node father of Is in the assembly tree. 
!      If is the principal 
!      variable of the node If and let If1, If2, If3 be the secondary variables
!      of node If.
!      Is is the principal 
!      variable of the node Is and let Is1, Is2 be the secondary variables
!      of node Is.
!      
!      THEN: 
!        NV(If1)=NV(If2)=NV(If3) = 0  (secondary variables)
!        NV(Is1)=NV(Is2) = 0  (secondary variables)
!        NV(If) > 0  ( principal variable)
!        NV(Is) > 0  ( principal variable)
!        PE(If)  = 0 (root node)
!        PE(Is)  = -If (If is the father of Is in the assembly tree)
!        PE(If1)=PE(If2)=PE(If3)= -If  ( If is the principal variable)
!        PE(Is1)=PE(Is2)= -Is  ( Is is the principal variable)
!      
!
!
! HALOAMD_V1: (September 1997)
! **********
! Initial version designed to experiment the numerical (fill-in) impact 
! of taking into account the halo. This code should be able 
! to experiment no-halo, partial halo, complete halo.
! DATE: September 17th 1997
!
! HALOAMD is designed to process a gragh composed of two types
!            of nodes, V0 and V1, extracted from a larger gragh. 
!            V0^V1 = {}, 
!       
!            We used Min. degree heuristic to order only 
!            nodes in V0, but the adjacency to nodes
!            in V1 is taken into account during ordering.
!            Nodes in V1 are odered at last.
!            Adjacency between nodes of V1 need not be provided,
!            however |len(i)| must always corresponds to the number of 
!            edges effectively provided in the adjacency list of i.
!          On input :
!          ********
!            Nodes INODE in V1 are flagged with len(INODE) = -degree 
!            modif version HALO V3 (August 1998): 
!                                 if len(i) =0 and i \in V1 then 
!                                 len(i) must be set on input to -N-1
!          ERROR return (negative values in ncmpa)
!          ************
!            negative value in ncmpa indicates an error detected 
!               by HALOAMD.
!
!            The graph provided MUST follow the rule:
!             if (i,j) is an edge in the gragh then 
!             j must be in the adjacency list of i AND 
!             i must be in the adjacency list of j.
!    REMARKS
!    -------
!        
!        1/  Providing edges between nodes of V1 should not 
!            affect the final ordering, only the amount of edges 
!            of the halo should effectively affect the solution.
!            This code should work in the following cases:
!              1/ halo not provided
!              2/ halo partially provided
!              3/ complete halo
!              4/ complete halo+interconnection between nodes of V1.
!
!              1/ should run and provide identical results (w.r.t to current 
!               implementation of AMD in SCOTCH).
!             3/ and 4 should provide identical results.
!
!        2/ All modifications of the AMD initial code are indicated
!           with begin HALO .. end HALO
!
!            
!   Ordering of nodes in V0 is based on  approximate minimum
!       fill-in heuristic.
!   
!-----------------------------------------------------------------------
! begin CONSTRAINT
! CONSTRAINT(I) >= 0 : I can be selected
!                < 0 : I cannot be selected
!                > 0 : I release CONSTRAINT(I) 
! THESON(I) = 0 : I is a leaf in the supervariable representation
! THESON(I) > I : THESON(I) belongs to the same supervariable as I
!  Parameters:
INTEGER, INTENT(INOUT) :: CONSTRAINT(N)
INTEGER, INTENT(out) :: THESON(N)
INTEGER PREV,TOTO
! end CONSTRAINT
!-----------------------------------------------------------------------
! INPUT ARGUMENTS (unaltered):
!-----------------------------------------------------------------------
! n:    The matrix order.
!
!       Restriction:  n .ge. 1
! iwlen:        The length of iw (1..iwlen).  On input, the matrix is
!       stored in iw (1..pfree-1).  However, iw (1..iwlen) should be
!       slightly larger than what is required to hold the matrix, at
!       least iwlen .ge. pfree + n is recommended.  Otherwise,
!       excessive compressions will take place.
!       *** We do not recommend running this algorithm with ***
!       ***      iwlen .lt. pfree + n.                      ***
!       *** Better performance will be obtained if          ***
!       ***      iwlen .ge. pfree + n                       ***
!       *** or better yet                                   ***
!       ***      iwlen .gt. 1.2 * pfree                     ***
!       *** (where pfree is its value on input).            ***
!       The algorithm will not run at all if iwlen .lt. pfree-1.
!
!       Restriction: iwlen .ge. pfree-1
!-----------------------------------------------------------------------
! INPUT/OUPUT ARGUMENTS:
!-----------------------------------------------------------------------
! pe:   On input, pe (i) is the index in iw of the start of row i, or
!       zero if row i has no off-diagonal non-zeros.
!
!       During execution, it is used for both supervariables and
!       elements:
!
!       * Principal supervariable i:  index into iw of the
!               description of supervariable i.  A supervariable
!               represents one or more rows of the matrix
!               with identical nonzero pattern.
!       * Non-principal supervariable i:  if i has been absorbed
!               into another supervariable j, then pe (i) = -j.
!               That is, j has the same pattern as i.
!               Note that j might later be absorbed into another
!               supervariable j2, in which case pe (i) is still -j,
!               and pe (j) = -j2.
!       * Unabsorbed element e:  the index into iw of the description
!               of element e, if e has not yet been absorbed by a
!               subsequent element.  Element e is created when
!               the supervariable of the same name is selected as
!               the pivot.
!       * Absorbed element e:  if element e is absorbed into element
!               e2, then pe (e) = -e2.  This occurs when the pattern of
!               e (that is, Le) is found to be a subset of the pattern
!               of e2 (that is, Le2).  If element e is "null" (it has
!               no nonzeros outside its pivot block), then pe (e) = 0.
!
!       On output, pe holds the assembly tree/forest, which implicitly
!       represents a pivot order with identical fill-in as the actual
!       order (via a depth-first search of the tree).
!
!       On output:
!       If nv (i) .gt. 0, then i represents a node in the assembly tree,
!       and the parent of i is -pe (i), or zero if i is a root.
!       If nv (i) = 0, then (i,-pe (i)) represents an edge in a
!       subtree, the root of which is a node in the assembly tree.
!       On output:  (PE is copied on output into PARENT array)
!
! pfree:        On input, the matrix is stored in iw (1..pfree-1) and
!       the rest of the array iw is free.
!       During execution, additional data is placed in iw, and pfree
!       is modified so that components  of iw from pfree are free.
!       On output, pfree is set equal to the size of iw that
!       would have been needed for no compressions to occur.  If
!       ncmpa is zero, then pfree (on output) is less than or equal to
!       iwlen, and the space iw (pfree+1 ... iwlen) was not used.
!       Otherwise, pfree (on output) is greater than iwlen, and all the
!       memory in iw was used.
!
! nv: On input, encoding of compressed graph:
!        if NV(1) = -1 then graph is not compressed otherwise
!        NV(I) holds the weight of node I. 
!       During execution, abs (nv (i)) is equal to the number of rows
!       that are represented by the principal supervariable i.  If i is
!       a nonprincipal variable, then nv (i) = 0.  Initially,
!       nv (i) = 1 for all i.  nv (i) .lt. 0 signifies that i is a
!       principal variable in the pattern Lme of the current pivot
!       element me.  On output, nv (e) holds the true degree of element
!       e at the time it was created (including the diagonal part).
! begin HALO
!       On output, nv(I) can be used to find node in set V1.
!       Not true anymore : ( nv(I) = N+1 characterizes nodes in V1. 
!                 instead nodes in V1 are considered as a dense root node )
! end HALO
!-----------------------------------------------------------------------
! INPUT/MODIFIED (undefined on output):
!-----------------------------------------------------------------------
! len:  On input, len (i) 
!           positive or null (>=0) : i \in V0 and 
!                     len(i) holds the number of entries in row i of the
!                     matrix, excluding the diagonal.  
!           negative (<0) : i \in V1, and 
!                     -len(i) hold the number of entries in row i of the
!                     matrix, excluding the diagonal.
!                     len(i) = - | Adj(i) | if i \in V1                    
!                              or -N -1 if  | Adj(i) | = 0 and i \in V1 
!       The contents of len (1..n)
!       are undefined on output.
! iw:   On input, iw (1..pfree-1) holds the description of each row i
!       in the matrix.  The matrix must be symmetric, and both upper
!       and lower triangular parts must be present.  The diagonal must
!       not be present.  Row i is held as follows:
!
!               len (i):  the length of the row i data structure
!               iw (pe (i) ... pe (i) + len (i) - 1):
!                       the list of column indices for nonzeros
!                       in row i (simple supervariables), excluding
!                       the diagonal.  All supervariables start with
!                       one row/column each (supervariable i is just
!                       row i).
!               if len (i) is zero on input, then pe (i) is ignored
!               on input.
!
!               Note that the rows need not be in any particular order,
!               and there may be empty space between the rows.
!
!       During execution, the supervariable i experiences fill-in.
!       This is represented by placing in i a list of the elements
!       that cause fill-in in supervariable i:
!
!               len (i):  the length of supervariable i
!               iw (pe (i) ... pe (i) + elen (i) - 1):
!                       the list of elements that contain i.  This list
!                       is kept short by removing absorbed elements.
!               iw (pe (i) + elen (i) ... pe (i) + len (i) - 1):
!                       the list of supervariables in i.  This list
!                       is kept short by removing nonprincipal
!                       variables, and any entry j that is also
!                       contained in at least one of the elements
!                       (j in Le) in the list for i (e in row i).
!
!       When supervariable i is selected as pivot, we create an
!       element e of the same name (e=i):
!
!               len (e):  the length of element e
!               iw (pe (e) ... pe (e) + len (e) - 1):
!                       the list of supervariables in element e.
!
!       An element represents the fill-in that occurs when supervariable
!       i is selected as pivot (which represents the selection of row i
!       and all non-principal variables whose principal variable is i).
!       We use the term Le to denote the set of all supervariables
!       in element e.  Absorbed supervariables and elements are pruned
!       from these lists when computationally convenient.
!
!       CAUTION:  THE INPUT MATRIX IS OVERWRITTEN DURING COMPUTATION.
!       The contents of iw are undefined on output.
!-----------------------------------------------------------------------
! OUTPUT (need not be set on input):
!-----------------------------------------------------------------------
! elen: See the description of iw above.  At the start of execution,
!       elen (i) is set to zero.  During execution, elen (i) is the
!       number of elements in the list for supervariable i.  When e
!       becomes an element, elen (e) = -nel is set, where nel is the
!       current step of factorization.  elen (i) = 0 is done when i
!       becomes nonprincipal.
!
!       For variables, elen (i) .ge. 0 holds until just before the
!       permutation vectors are computed.  For elements,
!       elen (e) .lt. 0 holds.
!
!       On output elen (1..n) holds the inverse permutation (the same
!       as the 'INVP' argument in Sparspak).  That is, if k = elen (i),
!       then row i is the kth pivot row.  Row i of A appears as the
!       (elen(i))-th row in the permuted matrix, PAP^T.
! last: In a degree list, last (i) is the supervariable preceding i,
!       or zero if i is the head of the list.  In a hash bucket,
!       last (i) is the hash key for i.  last (head (hash)) is also
!       used as the head of a hash bucket if head (hash) contains a
!       degree list (see head, below).
!
!       On output, last (1..n) holds the permutation (the same as the
!       'PERM' argument in Sparspak).  That is, if i = last (k), then
!       row i is the kth pivot row.  Row last (k) of A is the k-th row
!       in the permuted matrix, PAP^T.
! ncmpa:        The number of times iw was compressed.  If this is
!       excessive, then the execution took longer than what could have
!       been.  To reduce ncmpa, try increasing iwlen to be 10% or 20%
!       larger than the value of pfree on input (or at least
!       iwlen .ge. pfree + n).  The fastest performance will be
!       obtained when ncmpa is returned as zero.  If iwlen is set to
!       the value returned by pfree on *output*, then no compressions
!       will occur.
! begin HALO
!        on output ncmpa <0 --> error detected during HALO_AMD:
!           error 1: ncmpa = -N , ordering was stopped.
! end HALO
!
!-----------------------------------------------------------------------
! LOCAL (not input or output - used only during execution):
!-----------------------------------------------------------------------
! degree:       If i is a supervariable, then degree (i) holds the
!       current approximation of the external degree of row i (an upper
!       bound).  The external degree is the number of nonzeros in row i,
!       minus abs (nv (i)) (the diagonal part).  The bound is equal to
!       the external degree if elen (i) is less than or equal to two.
!       We also use the term "external degree" for elements e to refer
!       to |Le \ Lme|.  If e is an element, then degree (e) holds |Le|,
!       which is the degree of the off-diagonal part of the element e
!       (not including the diagonal part).
! begin HALO
!       degree(I) = n+1 indicates that i belongs to V1
! end HALO
!
! head: head is used for degree lists.  head (deg) is the first
!       supervariable in a degree list (all supervariables i in a
!       degree list deg have the same approximate degree, namely,
!       deg = degree (i)).  If the list deg is empty then
!       head (deg) = 0.
!
!       During supervariable detection head (hash) also serves as a
!       pointer to a hash bucket.
!       If head (hash) .gt. 0, there is a degree list of degree hash.
!               The hash bucket head pointer is last (head (hash)).
!       If head (hash) = 0, then the degree list and hash bucket are
!               both empty.
!       If head (hash) .lt. 0, then the degree list is empty, and
!               -head (hash) is the head of the hash bucket.
!       After supervariable detection is complete, all hash buckets
!       are empty, and the (last (head (hash)) = 0) condition is
!       restored for the non-empty degree lists.
! next: next (i) is the supervariable following i in a link list, or
!       zero if i is the last in the list.  Used for two kinds of
!       lists:  degree lists and hash buckets (a supervariable can be
!       in only one kind of list at a time).
! w:    The flag array w determines the status of elements and
!       variables, and the external degree of elements.
!
!       for elements:
!          if w (e) = 0, then the element e is absorbed
!          if w (e) .ge. wflg, then w (e) - wflg is the size of
!               the set |Le \ Lme|, in terms of nonzeros (the
!               sum of abs (nv (i)) for each principal variable i that
!               is both in the pattern of element e and NOT in the
!               pattern of the current pivot element, me).
!          if wflg .gt. w (e) .gt. 0, then e is not absorbed and has
!               not yet been seen in the scan of the element lists in
!               the computation of |Le\Lme| in loop 150 below.
!
!       for variables:
!          during supervariable detection, if w (j) .ne. wflg then j is
!          not in the pattern of variable i
!
!       The w array is initialized by setting w (i) = 1 for all i,
!       and by setting wflg = 2.  It is reinitialized if wflg becomes
!       too large (to ensure that wflg+n does not cause integer
!       overflow).
!
! wf : integer array  used to store the already filled area of 
!      the variables adajcent to current pivot. 
!      wf is then used to update the score of variable i.
!
!-----------------------------------------------------------------------
! LOCAL INTEGERS:
!-----------------------------------------------------------------------
INTEGER :: DEG, DEGME, DEXT, DMAX, E, ELENME, ELN, I, &
  &        ILAST, INEXT, J, JLAST, JNEXT, K, KNT1, KNT2, KNT3, &
  &        LENJ, LN, ME, MINDEG, NEL, &
  &        NLEFT, NVI, NVJ, NVPIV, SLENME, WE, WFLG, WNVI, X, &
  &        NBFLAG, NREAL, LASTD, NELME, WF3, WF4, N2, PAS
 INTEGER KNT1_UPDATED, KNT2_UPDATED
 INTEGER(8) :: MAXMEM, MEM, NEWMEM
 INTEGER   :: MAXINT_N
 INTEGER(8)::  HASH, HMOD
 DOUBLE PRECISION ::   RMF, RMF1
 DOUBLE PRECISION ::   dummy
 INTEGER :: idummy
! deg:        the degree of a variable or element
! degme:      size, |Lme|, of the current element, me (= degree (me))
! dext:       external degree, |Le \ Lme|, of some element e
! dmax:       largest |Le| seen so far
! e:          an element
! elenme:     the length, elen (me), of element list of pivotal var.
! eln:        the length, elen (...), of an element list
! hash:       the computed value of the hash function
! hmod:       the hash function is computed modulo hmod = max (1,n-1)
! i:          a supervariable
! ilast:      the entry in a link list preceding i
! inext:      the entry in a link list following i
! j:          a supervariable
! jlast:      the entry in a link list preceding j
! jnext:      the entry in a link list, or path, following j
! k:          the pivot order of an element or variable
! knt1:       loop counter used during element construction
! knt2:       loop counter used during element construction
! knt3:       loop counter used during compression
! lenj:       len (j)
! ln:         length of a supervariable list
! maxint_n:   large integer to test risk of overflow on wflg
! maxmem:     amount of memory needed for no compressions
! me:         current supervariable being eliminated, and the
!                     current element created by eliminating that
!                     supervariable
! mem:        memory in use assuming no compressions have occurred
! mindeg:     current minimum degree
! nel:        number of pivots selected so far
! newmem:     amount of new memory needed for current pivot element
! nleft:      n - nel, the number of nonpivotal rows/columns remaining
! nvi:        the number of variables in a supervariable i (= nv (i))
! nvj:        the number of variables in a supervariable j (= nv (j))
! nvpiv:      number of pivots in current element
! slenme:     number of variables in variable list of pivotal variable
! we:         w (e)
! wflg:       used for flagging the w array.  See description of iw.
! wnvi:       wflg - nv (i)
! x:          either a supervariable or an element
! wf3:  off diagonal block area
! wf4:  diagonal block area
! mf : Minimum fill
! begin HALO
! nbflag:     number of flagged entries in the initial gragh.
! nreal :     number of entries on which ordering must be perfomed
!             (nreel = N- nbflag)
! nelme number of pivots selected when reaching the root
! lastd index of the last row in the list of dense rows
! end HALO
!-----------------------------------------------------------------------
! LOCAL POINTERS:
!-----------------------------------------------------------------------
INTEGER(8) P, P1, P2, P3, PDST, PEND, PJ, PME, PME1, PME2, &
  &           PN, PSRC
!             Any parameter (pe (...) or pfree) or local variable
!             starting with "p" (for Pointer) is an index into iw,
!             and all indices into iw use variables starting with
!             "p."  The only exception to this rule is the iwlen
!             input argument.
! p:          pointer into lots of things
! p1:         pe (i) for some variable i (start of element list)
! p2:         pe (i) + elen (i) -  1 for some var. i (end of el. list)
! p3:         index of first supervariable in clean list
! pdst:       destination pointer, for compression
! pend:       end of memory to compress
! pj:         pointer into an element or variable
! pme:        pointer into the current element (pme1...pme2)
! pme1:       the current element, me, is stored in iw (pme1...pme2)
! pme2:       the end of the current element
! pn:         pointer into a "clean" variable, also used to compress
! psrc:       source pointer, for compression
!-----------------------------------------------------------------------
!  FUNCTIONS CALLED:
!-----------------------------------------------------------------------
INTRINSIC max, min, mod, huge
INTEGER TOTEL
!=======================================================================
!  INITIALIZATIONS
!=======================================================================
!     HEAD (0:NBBUCK+1)
! begin HALO
!
! idummy holds the largest integer - 1
! dummy  = dble (idummy)
idummy = huge(idummy) - 1
dummy = dble(idummy)
!     variable with degree equal to N2 are in halo
!     bucket NBBUCK+1 used for HALO variables
N2 = -NBBUCK-1
! end HALO
! Distance betweeen elements of the N, ..., NBBUCK entries of HEAD
!
! update done on 20 Feb 2002 (PAS>= 1)
PAS = max((N/8), 1)
WFLG = 2
MAXINT_N=huge(WFLG)-N
NCMPA = 0
NEL = 0
HMOD = int(max (1, NBBUCK-1),kind=8)
DMAX = 0
MEM = PFREE - 1
MAXMEM = MEM
MINDEG = 0
!
NBFLAG = 0
LASTD  = 0
HEAD(0:NBBUCK+1) = 0
DO 10 I = 1, N
   THESON(I) = 0
   LAST (I) = 0
!        NV (I) = 1
   W (I) = 1
   ELEN (I) = 0
10 CONTINUE
TOTEL = 0
DO I=1,N
   IF (LEN(I).LT.0) THEN
      DEGREE (I) = N2
      NBFLAG     = NBFLAG +1
      IF (LEN(I).EQ.-N-1) THEN
!     variable in V1 with empty adj list 
         LEN (I)    = 0
!     Because of compress, we force skipping this
!     entry which is anyway empty
         PE (I)     = 0_8
      ELSE
         LEN (I)    = - LEN(I)
      ENDIF
!       end HALO V3
   ELSE
      TOTEL = TOTEL + NV(I)
      DEGREE(I) = 0
      DO P= PE(I) , PE(I)+int(LEN(I)-1,8)
         DEGREE(I) = DEGREE(I) + NV(IW(P))
      ENDDO
   ENDIF
ENDDO
!
!
!     number of entries to be ordered.
NREAL = N - NBFLAG
!     ----------------------------------------------------------------
!     initialize degree lists and eliminate rows with no off-diag. nz.
!     ----------------------------------------------------------------
DO 20 I = 1, N
  DEG = DEGREE (I)
  IF (DEG.EQ.N2) THEN
!            DEG = N2 (flagged variables are stored 
!                  in the degree list of NBBUCK + 1
!                  (safe: because max 
!                         max value of degree is NBBUCK)
!
       DEG = NBBUCK + 1
       IF (LASTD.EQ.0) THEN
!              degree list is empty
         LASTD     = I
         HEAD(DEG) = I
         NEXT(I)   = 0
         LAST(I)   = 0
       ELSE
         NEXT(LASTD) = I
         LAST(I)     = LASTD
         LASTD       = I
         NEXT(I)     = 0
       ENDIF
   GOTO 20
  ENDIF
!
!
  IF (DEG .GT. 0) THEN
     WF(I) = DEG
     IF (DEG.GT.N) THEN
      DEG = min(((DEG-N)/PAS) + N , NBBUCK)
     ENDIF
!           Note that if deg=0 then 
!           No fill-in will occur, 
!           but one variable is adjacent to I
!          ----------------------------------------------------------
!          place i in the degree list corresponding to its degree
!          ----------------------------------------------------------
     INEXT = HEAD (DEG)
     IF (INEXT .NE. 0) LAST (INEXT) = I
     NEXT (I) = INEXT
     HEAD (DEG) = I
  ELSE
!         ----------------------------------------------------------
!         we have a variable that can be eliminated at once because
!         there is no off-diagonal non-zero in its row.
!         ----------------------------------------------------------
     NEL = NEL + NV(I)
     ELEN (I) = -NEL
     PE (I) = 0_8
     W (I) = 0
  ENDIF
!=======================================================================
!
20 CONTINUE
!=======================================================================
!  WHILE (selecting pivots) DO
!=======================================================================
NLEFT = TOTEL-NEL
!=======================================================================
! =====================================================================
30 IF (NEL .LT. TOTEL) THEN
! =====================================================================
!  GET PIVOT OF MINIMUM DEGREE
!=======================================================================
!       -------------------------------------------------------------
!       find next supervariable for elimination
!       -------------------------------------------------------------
   DO 40 DEG = MINDEG, NBBUCK
      ME = HEAD (DEG)
      IF (ME .GT. 0) GO TO 50
40    CONTINUE
50    MINDEG = DEG
   IF (ME.LE.0) THEN
      NCMPA = -N
      CALL MUMPS_ABORT()
   ENDIF
   IF (DEG.GT.N) THEN
!        -------------------------------
!        Linear search to find variable 
!        with best score in the list
!        -------------------------------
!        While end of list list not reached
!         NEXT(J) = 0
      J = NEXT(ME)
      K = WF(ME)
!     if ME is not available
      IF(CONSTRAINT(ME) .LT. 0) THEN
         K = -1
      ENDIF
55       CONTINUE
      IF (J.GT.0) THEN
!     j is  available
         IF(CONSTRAINT(J) .GE. 0) THEN
            IF (WF(J).LT.K .OR. K .LT. 0) THEN
               ME = J
               K  = WF(ME)
            ENDIF
         ENDIF
         J= NEXT(J)
         GOTO 55
      ENDIF
      ILAST = LAST(ME)
      INEXT = NEXT(ME)
      IF (INEXT .NE. 0) LAST (INEXT) = ILAST
      IF (ILAST .NE. 0) THEN
         NEXT (ILAST) = INEXT
      ELSE
!          me is at the head of the degree list
         HEAD (DEG) = INEXT
      ENDIF
!     
   ELSE
!     select ME which verify the constraint           
!     if it is directly ok
      IF(CONSTRAINT(ME) .GE. 0) GOTO 59
56       CONTINUE
!     if ME has a successor exaine it
      IF(NEXT(ME) .NE. 0) THEN
         ME = NEXT(ME)
         IF(CONSTRAINT(ME) .GE. 0) THEN
            GOTO 59
         ELSE
            GOTO 56
         ENDIF
      ELSE
!     ME has no successor -> increase deg till finding a valid ME
!     57: increase deg till a non empty list is found
57          DEG = DEG+1
         ME = HEAD(DEG)
!     no empty found 
         IF(ME .GT. 0) THEN
!     good piv found
            IF(CONSTRAINT(ME) .GE. 0) THEN
               GOTO 59
            ELSE
!     else loop on next
               GOTO 56
            ENDIF
         ELSE
!     increase degree
            GOTO 57
         ENDIF
      ENDIF
59       PREV = LAST (ME)
      INEXT = NEXT (ME)
      IF(PREV .NE. 0) THEN
         NEXT(PREV) = INEXT
      ELSE
         HEAD (DEG) = INEXT
      ENDIF
!     remove ME from the x2 linked lists
      IF (INEXT .NE. 0) LAST (INEXT) = PREV
   ENDIF
!         -------------------------------------------------------------
!         remove chosen variable from link list
!         -------------------------------------------------------------
   TOTO = ME
5910    IF(TOTO .NE. 0) THEN
      J = CONSTRAINT(TOTO)
      IF(J .GT. 0) THEN
         CONSTRAINT(J) = 0
      ENDIF
      TOTO = THESON(TOTO)
      GOTO 5910
   ENDIF
!       -------------------------------------------------------------
!       me represents the elimination of pivots nel+1 to nel+nv(me).
!       place me itself as the first in this set.  It will be moved
!       to the nel+nv(me) position when the permutation vectors are
!       computed.
!       -------------------------------------------------------------
      ELENME = ELEN (ME)
      ELEN (ME) = - (NEL + 1)
      NVPIV = NV (ME)
      NEL = NEL + NVPIV
!=======================================================================
!  CONSTRUCT NEW ELEMENT
!=======================================================================
!       -------------------------------------------------------------
!       At this point, me is the pivotal supervariable.  It will be
!       converted into the current element.  Scan list of the
!       pivotal supervariable, me, setting tree pointers and
!       constructing new list of supervariables for the new element,
!       me.  p is a pointer to the current position in the old list.
!       -------------------------------------------------------------
!       flag the variable "me" as being in Lme by negating nv (me)
      NV (ME) = -NVPIV
      DEGME = 0
      IF (ELENME .EQ. 0) THEN
!         ----------------------------------------------------------
!         construct the new element in place
!         ----------------------------------------------------------
         PME1 = PE (ME)
         PME2 = PME1 - 1
         DO 60 P = PME1, PME1 + LEN (ME) - 1
            I = IW (P)
            NVI = NV (I)
            IF (NVI .GT. 0) THEN
!             ----------------------------------------------------
!             i is a principal variable not yet placed in Lme.
!             store i in new list
!             ----------------------------------------------------
               DEGME = DEGME + NVI
!             flag i as being in Lme by negating nv (i)
               NV (I) = -NVI
               PME2 = PME2 + 1
               IW (PME2) = I
               IF (DEGREE(I).NE.N2) THEN
!             ----------------------------------------------------
!             remove variable i from degree list. (only if i \in V0)
!             ----------------------------------------------------
                  ILAST = LAST (I)
                  INEXT = NEXT (I)
                  IF (INEXT .NE. 0) LAST (INEXT) = ILAST
                  IF (ILAST .NE. 0) THEN
                     NEXT (ILAST) = INEXT
                  ELSE
!               i is at the head of the degree list
                     IF (WF(I).GT.N) THEN
                        DEG = min(((WF(I)-N)/PAS) + N , NBBUCK)
                     ELSE
                        DEG = WF(I)
                     ENDIF
                     HEAD (DEG) = INEXT
                  ENDIF
               ENDIF
            ENDIF
60          CONTINUE
!         this element takes no new memory in iw:
         NEWMEM = 0
      ELSE
!         ----------------------------------------------------------
!         construct the new element in empty space, iw (pfree ...)
!         ----------------------------------------------------------
    P = PE (ME)
    PME1 = PFREE
    SLENME = LEN (ME) - ELENME
    KNT1_UPDATED = 0
    DO 120 KNT1 = 1, ELENME + 1
      KNT1_UPDATED = KNT1_UPDATED +1
      IF (KNT1 .GT. ELENME) THEN
!             search the supervariables in me.
        E = ME
        PJ = P
        LN = SLENME
      ELSE
!             search the elements in me.
        E = IW (P)
        P = P + 1
        PJ = PE (E)
        LN = LEN (E)
      ENDIF
!           -------------------------------------------------------
!           search for different supervariables and add them to the
!           new list, compressing when necessary. this loop is
!           executed once for each element in the list and once for
!           all the supervariables in the list.
!           -------------------------------------------------------
      KNT2_UPDATED = 0
      DO 110 KNT2 = 1, LN
        KNT2_UPDATED = KNT2_UPDATED+1
        I = IW (PJ)
        PJ = PJ + 1
        NVI = NV (I)
        IF (NVI .GT. 0) THEN
!               -------------------------------------------------
!               compress iw, if necessary
!               -------------------------------------------------
          IF (PFREE .GT. IWLEN) THEN
!                 prepare for compressing iw by adjusting
!                 pointers and lengths so that the lists being
!                 searched in the inner and outer loops contain
!                 only the remaining entries.
            PE (ME) = P
            LEN (ME) = LEN (ME) - KNT1_UPDATED
!                 Reset KNT1_UPDATED in case of recompress 
!                 at same iteration of the loop 120
            KNT1_UPDATED = 0
!                 Check if anything left in supervariable ME
            IF (LEN (ME) .EQ. 0) PE (ME) = 0_8
            PE (E) = PJ
            LEN (E) = LN - KNT2_UPDATED
!                 Reset KNT2_UPDATED in case of recompress 
!                 at same iteration of the loop 110
            KNT2_UPDATED = 0
!                 Check if anything left in element E
            IF (LEN (E) .EQ. 0) PE (E) = 0_8
            NCMPA = NCMPA + 1
!                 store first item in pe
!                 set first entry to -item
            DO 70 J = 1, N
              PN = PE (J)
              IF (PN .GT. 0) THEN
                PE (J) = int(IW (PN),8)
                IW (PN) = -J
              ENDIF
70             CONTINUE
!                 psrc/pdst point to source/destination
            PDST = 1
            PSRC = 1
            PEND = PME1 - 1
!                 while loop:
80             CONTINUE
            IF (PSRC .LE. PEND) THEN
!                   search for next negative entry
              J = -IW (PSRC)
              PSRC = PSRC + 1
              IF (J .GT. 0) THEN
                IW (PDST) = int(PE (J))
                PE (J) = PDST
                PDST = PDST + 1_8
!                     copy from source to destination
                LENJ = LEN (J)
                DO 90 KNT3 = 0, LENJ - 2
                  IW (PDST + KNT3) = IW (PSRC + KNT3)
90                 CONTINUE
                PDST = PDST + int(LENJ - 1,8)
                PSRC = PSRC + int(LENJ - 1,8)
              ENDIF
              GO TO 80
            ENDIF
!                 move the new partially-constructed element
            P1 = PDST
            DO 100 PSRC = PME1, PFREE - 1
              IW (PDST) = IW (PSRC)
              PDST = PDST + 1
100             CONTINUE
            PME1 = P1
            PFREE = PDST
            PJ = PE (E)
            P = PE (ME)
          ENDIF
!               -------------------------------------------------
!               i is a principal variable not yet placed in Lme
!               store i in new list
!               -------------------------------------------------
          DEGME = DEGME + NVI
!               flag i as being in Lme by negating nv (i)
          NV (I) = -NVI
          IW (PFREE) = I
          PFREE = PFREE + 1
        IF (DEGREE(I).NE.N2) THEN
!               -------------------------------------------------
!               remove variable i from degree link list 
!                            (only if i in V0)
!               -------------------------------------------------
          ILAST = LAST (I)
          INEXT = NEXT (I)
          IF (INEXT .NE. 0) LAST (INEXT) = ILAST
          IF (ILAST .NE. 0) THEN
            NEXT (ILAST) = INEXT
          ELSE
            IF (WF(I).GT.N) THEN
             DEG = min(((WF(I)-N)/PAS) + N , NBBUCK)
            ELSE
             DEG = WF(I)
            ENDIF
!                 i is at the head of the degree list
            HEAD (DEG) = INEXT
          ENDIF
        ENDIF
        ENDIF
110       CONTINUE
      IF (E .NE. ME) THEN
!             set tree pointer and flag to indicate element e is
!             absorbed into new element me (the parent of e is me)
        PE (E) = int(-ME,8)
        W (E) = 0
      ENDIF
120     CONTINUE
    PME2 = PFREE - 1
!         this element takes newmem new memory in iw (possibly zero)
    NEWMEM = PFREE - PME1
    MEM = MEM + NEWMEM
    MAXMEM = max (MAXMEM, MEM)
  ENDIF
!       -------------------------------------------------------------
!       me has now been converted into an element in iw (pme1..pme2)
!       -------------------------------------------------------------
!       degme holds the external degree of new element
  DEGREE (ME) = DEGME
  PE (ME) = PME1
  LEN (ME) = int(PME2 - PME1 + 1_8)
!       -------------------------------------------------------------
!       make sure that wflg is not too large.  With the current
!       value of wflg, wflg+n must not cause integer overflow
!       -------------------------------------------------------------
  IF (WFLG .GT. MAXINT_N) THEN
    DO 130 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
130     CONTINUE
    WFLG = 2
  ENDIF
!=======================================================================
!  COMPUTE (w (e) - wflg) = |Le\Lme| FOR ALL ELEMENTS
!=======================================================================
!       -------------------------------------------------------------
!       Scan 1:  compute the external degrees of previous elements
!       with respect to the current element.  That is:
!            (w (e) - wflg) = |Le \ Lme|
!       for each element e that appears in any supervariable in Lme.
!       The notation Le refers to the pattern (list of
!       supervariables) of a previous element e, where e is not yet
!       absorbed, stored in iw (pe (e) + 1 ... pe (e) + iw (pe (e))).
!       The notation Lme refers to the pattern of the current element
!       (stored in iw (pme1..pme2)).   If (w (e) - wflg) becomes
!       zero, then the element e will be absorbed in scan 2.
!       -------------------------------------------------------------
  DO 150 PME = PME1, PME2
    I = IW (PME)
    ELN = ELEN (I)
    IF (ELN .GT. 0) THEN
!           note that nv (i) has been negated to denote i in Lme:
      NVI = -NV (I)
      WNVI = WFLG - NVI
      DO 140 P = PE (I), PE (I) + int(ELN - 1,8)
        E = IW (P)
        WE = W (E)
        IF (WE .GE. WFLG) THEN
!               unabsorbed element e has been seen in this loop
          WE = WE - NVI
        ELSE IF (WE .NE. 0) THEN
!               e is an unabsorbed element
!               this is the first we have seen e in all of Scan 1
          WE = DEGREE (E) + WNVI
          WF(E) = 0
        ENDIF
        W (E) = WE
140       CONTINUE
    ENDIF
150   CONTINUE
!=======================================================================
!  DEGREE UPDATE AND ELEMENT ABSORPTION
!=======================================================================
!       -------------------------------------------------------------
!       Scan 2:  for each i in Lme, sum up the degree of Lme (which
!       is degme), plus the sum of the external degrees of each Le
!       for the elements e appearing within i, plus the
!       supervariables in i.  Place i in hash list.
!       -------------------------------------------------------------
  DO 180 PME = PME1, PME2
    I = IW (PME)
    P1 = PE (I)
    P2 = P1 + int(ELEN (I) - 1,8)
    PN = P1
    HASH = 0_8
    DEG  = 0
    WF3  = 0
    WF4  = 0
    NVI  = -NV(I)
!         ----------------------------------------------------------
!         scan the element list associated with supervariable i
!         ----------------------------------------------------------
    DO 160 P = P1, P2
      E = IW (P)
!           dext = | Le \ Lme |
      DEXT = W (E) - WFLG
      IF (DEXT .GT. 0) THEN
        IF ( WF(E) .EQ. 0 ) THEN
!              First time we meet e : compute wf(e) 
!              which holds the surface associated to element e 
!              it will later be deducted from fill-in 
!              area of all variables adjacent to e
         WF(E) = DEXT * ( (2 * DEGREE(E))  -  DEXT - 1)
        ENDIF
        WF4 = WF4 + WF(E)
        DEG = DEG + DEXT
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + int(E,kind=8)
      ELSE IF (DEXT .EQ. 0) THEN
#if defined (NOAGG4)
        IW (PN) = E
        PN = PN + 1
        HASH = HASH + int(E,kind=8)
#else
!             aggressive absorption: e is not adjacent to me, but
!             the |Le \ Lme| is 0, so absorb it into me
        PE (E) = int(-ME,8)
        W (E) = 0
#endif
      ENDIF
160     CONTINUE
!         count the number of elements in i (including me):
    ELEN (I) = int(PN - P1 + 1_8)
!         ----------------------------------------------------------
!         scan the supervariables in the list associated with i
!         ----------------------------------------------------------
    P3 = PN
    DO 170 P = P2 + 1, P1 + int(LEN (I) - 1,8)
      J = IW (P)
      NVJ = NV (J)
      IF (NVJ .GT. 0) THEN
!             j is unabsorbed, and not in Lme.
!             add to degree and add to new list
        DEG = DEG + NVJ
        WF3 = WF3 + NVJ
        IW (PN) = J
        PN = PN + 1
        HASH = HASH + int(J,kind=8)
      ENDIF
170     CONTINUE
!
    IF (DEGREE(I).EQ.N2) DEG = N2
!         ----------------------------------------------------------
!         update the degree and check for mass elimination
!         ----------------------------------------------------------
#if defined (NOAGG4)
    IF (ELEN(I).EQ.1 .AND. P3.EQ.PN) THEN
#else
    IF (DEG .EQ. 0) THEN
#endif
!           -------------------------------------------------------
!           mass elimination
!           -------------------------------------------------------
!           There is nothing left of this node except for an
!           edge to the current pivot element.  elen (i) is 1,
!           and there are no variables adjacent to node i.
!           Absorb i into the current pivot element, me.
       TOTO = I
5911        IF(TOTO .NE. 0) THEN
          J = CONSTRAINT(TOTO)
          IF(J .GT. 0) THEN
             CONSTRAINT(J) = 0
          ENDIF
          TOTO = THESON(TOTO)
          GOTO 5911
       ENDIF
      PE (I) = int(-ME,8)
      NVI = -NV (I)
      DEGME = DEGME - NVI
      NVPIV = NVPIV + NVI
      NEL = NEL + NVI
      NV (I) = 0
      ELEN (I) = 0
    ELSE
!           -------------------------------------------------------
!           update the upper-bound degree of i
!           -------------------------------------------------------
!           the following degree does not yet include the size
!           of the current element, which is added later:
! AMD            DEGREE (I) = min (DEGREE (I), DEG)
      IF (DEGREE(I).NE.N2) THEN
!                I does not belong to halo
!                dk = min (d(k-1)+degme, deg+degme)
           IF ( DEGREE (I).LT.DEG ) THEN
!                  Our appox degree is loose.
!                  we keep old value. Note that in 
!                  this case we cannot substract WF(I)
!                  for min-fill score.
             WF4 = 0
             WF3 = 0
           ELSE
             DEGREE(I)  = DEG
           ENDIF
      ENDIF
!
!           compute WF(I) taking into account size of block 3.0
      WF(I)      = WF4 + 2*NVI*WF3
!           -------------------------------------------------------
!           add me to the list for i
!           -------------------------------------------------------
!           move first supervariable to end of list
      IW (PN) = IW (P3)
!           move first element to end of element part of list
      IW (P3) = IW (P1)
!           add new element to front of list.
      IW (P1) = ME
!           store the new length of the list in len (i)
      LEN (I) = int(PN - P1 + 1_8)
      IF (DEG.NE.N2) THEN
!           -------------------------------------------------------
!           place in hash bucket.  Save hash key of i in last (i).
!           -------------------------------------------------------
      HASH = mod (HASH, HMOD) + 1_8
      J = HEAD (HASH)
      IF (J .LE. 0) THEN
!             the degree list is empty, hash head is -j
        NEXT (I) = -J
        HEAD (HASH) = -I
      ELSE
!             degree list is not empty
!             use last (head (hash)) as hash head
        NEXT (I) = LAST (J)
        LAST (J) = I
      ENDIF
      LAST (I) = int(HASH,kind=kind(LAST))
      ENDIF
    ENDIF
180   CONTINUE
  DEGREE (ME) = DEGME
!       -------------------------------------------------------------
!       Clear the counter array, w (...), by incrementing wflg.
!       -------------------------------------------------------------
  DMAX = max (DMAX, DEGME)
  WFLG = WFLG + DMAX
!       make sure that wflg+n does not cause integer overflow
  IF (WFLG .GT. MAXINT_N) THEN
    DO 190 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
190     CONTINUE
    WFLG = 2
  ENDIF
!       at this point, w (1..n) .lt. wflg holds
!=======================================================================
!  SUPERVARIABLE DETECTION
!=======================================================================
  DO 250 PME = PME1, PME2
    I = IW (PME)
    IF ( (NV (I) .LT. 0) .AND. (DEGREE(I).NE.N2) ) THEN
!           i is a principal variable in Lme
!           -------------------------------------------------------
!           examine all hash buckets with 2 or more variables.  We
!           do this by examing all unique hash keys for super-
!           variables in the pattern Lme of the current element, me
!           -------------------------------------------------------
      HASH = int(LAST (I),kind=8)
!           let i = head of hash bucket, and empty the hash bucket
      J = HEAD (HASH)
      IF (J .EQ. 0) GO TO 250
      IF (J .LT. 0) THEN
!             degree list is empty
        I = -J
        HEAD (HASH) = 0
      ELSE
!             degree list is not empty, restore last () of head
        I = LAST (J)
        LAST (J) = 0
      ENDIF
      IF (I .EQ. 0) GO TO 250
!           while loop:
200       CONTINUE
      IF (NEXT (I) .NE. 0) THEN
!             ----------------------------------------------------
!             this bucket has one or more variables following i.
!             scan all of them to see if i can absorb any entries
!             that follow i in hash bucket.  Scatter i into w.
!             ----------------------------------------------------
        LN = LEN (I)
        ELN = ELEN (I)
!             do not flag the first element in the list (me)
        DO 210 P = PE (I) + 1, PE (I) + int(LN - 1,8)
          W (IW (P)) = WFLG
210         CONTINUE
!             ----------------------------------------------------
!             scan every other entry j following i in bucket
!             ----------------------------------------------------
        JLAST = I
        J = NEXT (I)
!             while loop:
220         CONTINUE
        IF (J .NE. 0) THEN
           IF(CONSTRAINT(J) .LT. 0 &
  &                .AND. CONSTRAINT(I) .LT. 0) THEN
              GOTO 240
           ENDIF
           IF(CONSTRAINT(I) .GE. 0) THEN
              IF(CONSTRAINT(J) .LT. 0) THEN
                 TOTO = I
221                  IF(TOTO .NE. 0) THEN
                    IF(CONSTRAINT(TOTO) .EQ. J) THEN
                       GOTO 225
                    ENDIF
                    TOTO =THESON(TOTO)
                    GOTO 221
                 ENDIF
              ELSE
                 GOTO 225
              ENDIF
           ELSE
!     if I is locked see if it is freed thanks to J
              IF(CONSTRAINT(J) .GE. 0) THEN
                 TOTO = J
222                  IF(TOTO .NE. 0) THEN
                    IF(CONSTRAINT(TOTO) .EQ. I) THEN
                       GOTO 225
                    ENDIF
                    TOTO =THESON(TOTO)
                    GOTO 222
                 ENDIF
              ENDIF
           ENDIF
           GOTO 240
225            CONTINUE
!               -------------------------------------------------
!               check if j and i have identical nonzero pattern
!               -------------------------------------------------
!               jump if i and j do not have same size data structure
           IF (LEN (J) .NE. LN) GO TO 240
!               jump if i and j do not have same number adj elts
           IF (ELEN (J) .NE. ELN) GO TO 240
!               do not flag the first element in the list (me)
           DO 230 P = PE (J) + 1_8, PE (J) + int(LN - 1,8)
!                 jump if an entry (iw(p)) is in j but not in i
              IF (W (IW (P)) .NE. WFLG) GO TO 240
230            CONTINUE
!               -------------------------------------------------
!               found it!  j can be absorbed into i
!               -------------------------------------------------
!     update the supervariable composition
           TOTO = I
231            IF(THESON(TOTO) .NE. 0) THEN
              TOTO = THESON(TOTO)
              GOTO 231
           ENDIF
           THESON(TOTO) = J
           IF(CONSTRAINT(I) .LT. 0) THEN
              CONSTRAINT(I) = 0
           ENDIF
           PE (J) = int(-I,8)
           WF(I)  = max(WF(I),WF(J))
!               both nv (i) and nv (j) are negated since they
!               are in Lme, and the absolute values of each
!               are the number of variables in i and j:
           NV (I) = NV (I) + NV (J)
           NV (J) = 0
           ELEN (J) = 0
!               delete j from hash bucket
           J = NEXT (J)
           NEXT (JLAST) = J
           GO TO 220
!               -------------------------------------------------
240            CONTINUE
!               j cannot be absorbed into i
!               -------------------------------------------------
           JLAST = J
           J = NEXT (J)
           GO TO 220
        ENDIF
!             ----------------------------------------------------
!             no more variables can be absorbed into i
!             go to next i in bucket and clear flag array
!             ----------------------------------------------------
        WFLG = WFLG + 1
        I = NEXT (I)
        IF (I .NE. 0) GO TO 200
     ENDIF
    ENDIF
250  CONTINUE
!=======================================================================
!  RESTORE DEGREE LISTS AND REMOVE NONPRINCIPAL SUPERVAR. FROM ELEMENT
!=======================================================================
  P = PME1
  NLEFT = TOTEL - NEL
  DO 260 PME = PME1, PME2
     I = IW (PME)
     NVI = -NV (I)
     IF (NVI .GT. 0) THEN
!           i is a principal variable in Lme
!           restore nv (i) to signify that i is principal
        NV (I) = NVI
        IF (DEGREE(I).NE.N2) THEN
!           -------------------------------------------------------
!           compute the external degree (add size of current elem)
!           -------------------------------------------------------
           DEG = min (DEGREE (I) + DEGME - NVI, NLEFT - NVI)
           IF (DEGREE (I) + DEGME .GT. NLEFT ) THEN
!
            DEG = DEGREE(I)
            RMF1  = dble(DEG)*dble( (DEG-1) + 2*DEGME ) &
  &                 - dble(WF(I))
            DEGREE(I) = NLEFT - NVI
            DEG       = DEGREE(I)
            RMF = dble(DEG)*dble(DEG-1) &
  &                 -  dble(DEGME-NVI)*dble(DEGME-NVI-1)
            RMF = min(RMF, RMF1)
         ELSE
            DEG = DEGREE(I)
            DEGREE(I) = DEGREE (I) + DEGME - NVI
            RMF  = dble(DEG)*dble( (DEG-1) + 2*DEGME ) &
  &                 - dble(WF(I))
         ENDIF
         RMF =  RMF / dble(NVI+1)
!
         IF (RMF.LT.dummy) THEN
            WF(I) = int ( anint( RMF ))
         ELSEIF (RMF / dble(N) .LT. dummy) THEN
            WF(I) = int ( anint( RMF/dble(N) ))
         ELSE
            WF(I) = idummy
         ENDIF
         WF(I) = max(1,WF(I))
!           -------------------------------------------------------
!           place the supervariable at the head of the degree list
!           -------------------------------------------------------
         DEG = WF(I)
         IF (DEG.GT.N) THEN
            DEG = min(((DEG-N)/PAS) + N , NBBUCK)
         ENDIF
         INEXT = HEAD (DEG)
         IF (INEXT .NE. 0) LAST (INEXT) = I
         NEXT (I) = INEXT
         LAST (I) = 0
         HEAD (DEG) = I
!           -------------------------------------------------------
!           save the new degree, and find the minimum degree
!           -------------------------------------------------------
         MINDEG = min (MINDEG, DEG)
      ENDIF
!           -------------------------------------------------------
!           place the supervariable in the element pattern
!           -------------------------------------------------------
      IW (P) = I
      P = P + 1
   ENDIF
260 CONTINUE
!=======================================================================
!  FINALIZE THE NEW ELEMENT
!=======================================================================
NV (ME) = NVPIV + DEGME
!       fill_est = fill_est + nvpiv * (nvpiv + 2 * degme)
!       nv (me) is now the degree of pivot (including diagonal part)
!       save the length of the list for the new element me
LEN (ME) = int(P - PME1)
IF (LEN (ME) .EQ. 0) THEN
!         there is nothing left of the current pivot element
   PE (ME) = 0_8
   W (ME) = 0
ENDIF
IF (NEWMEM .NE. 0) THEN
!         element was not constructed in place: deallocate part
!         of it (final size is less than or equal to newmem,
!         since newly nonprincipal variables have been removed).
   PFREE = P
   MEM = MEM - NEWMEM + int(LEN (ME),8)
ENDIF
!=======================================================================
!       END WHILE (selecting pivots)
GO TO 30
ENDIF
!=======================================================================
! begin HALO V2
IF (NBFLAG.GT.0) THEN
!
!     All possible pivots (not flagged have been eliminated).
!     We amalgamate all flagged variables at the root and 
!     we finish the elimination tree.
!          1/ Go through all
!          non absorbed elements (root of the subgraph)
!          and absorb in ME
!          2/ perform mass elimination of all dense rows
     DO DEG = MINDEG, NBBUCK+1
       ME = HEAD (DEG)
       IF (ME .GT. 0) GO TO 51
     ENDDO
51      MINDEG = DEG
     NELME    = -(NEL+1)
     DO X=1,N
      IF ((PE(X).GT.0_8) .AND. (ELEN(X).LT.0)) THEN
!            X is an unabsorbed element
       PE(X) = int(-ME,8)
!            W(X) = 0 could be suppressed ?? check it
      ELSEIF (DEGREE(X).EQ.N2) THEN
!            X is a dense row, absorb it in ME (mass elimination)
       NEL   = NEL + NV(X)
       PE(X) = int(-ME,8)
       ELEN(X) = 0
!            Correct value of NV is (secondary variable)
       NV(X) = 0
      ENDIF
     ENDDO
!          ME is the root node
     ELEN(ME) = NELME
!          Correct value of NV is (principal variable)
     NV(ME)   = N-NREAL
     PE(ME)   = 0_8
!
ENDIF
! end HALO
!=======================================================================
!  COMPUTE THE PERMUTATION VECTORS
!=======================================================================
!     ----------------------------------------------------------------
!     The time taken by the following code is O(n).  At this
!     point, elen (e) = -k has been done for all elements e,
!     and elen (i) = 0 has been done for all nonprincipal
!     variables i.  At this point, there are no principal
!     supervariables left, and all elements are absorbed.
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!     compute the ordering of unordered nonprincipal variables
!     ----------------------------------------------------------------
DO 290 I = 1, N
   IF (ELEN (I) .EQ. 0) THEN
!         ----------------------------------------------------------
!         i is an un-ordered row.  Traverse the tree from i until
!         reaching an element, e.  The element, e, was the
!         principal supervariable of i and all nodes in the path
!         from i to when e was selected as pivot.
!         ----------------------------------------------------------
      J = int(-PE (I))
!         while (j is a variable) do:
270       CONTINUE
      IF (ELEN (J) .GE. 0) THEN
         J = int(-PE (J))
         GO TO 270
      ENDIF
      E = J
!           ----------------------------------------------------------
!           get the current pivot ordering of e
!           ----------------------------------------------------------
      K = -ELEN (E)
!           ----------------------------------------------------------
!           traverse the path again from i to e, and compress the
!           path (all nodes point to e).  Path compression allows
!           this code to compute in O(n) time.  Order the unordered
!           nodes in the path, and place the element e at the end.
!           ----------------------------------------------------------
      J = I
!           while (j is a variable) do:
280       CONTINUE
      IF (ELEN (J) .GE. 0) THEN
         JNEXT = int(-PE (J))
         PE (J) = int(-E,8)
         IF (ELEN (J) .EQ. 0) THEN
!               j is an unordered row
            ELEN (J) = K
            K = K + 1
         ENDIF
         J = JNEXT
         GO TO 280
      ENDIF
!         leave elen (e) negative, so we know it is an element
      ELEN (E) = -K
   ENDIF
290 CONTINUE
!     ----------------------------------------------------------------
!     reset the inverse permutation (elen (1..n)) to be positive,
!     and compute the permutation (last (1..n)).
!     ----------------------------------------------------------------
IF(.TRUE.) THEN
!       N is the size of the compressed graph.
!       If the graph was compressed on input then
!       indices in ELEN are in [1,TOTEL]
!       We build the inverse of ELEN in LAST (similar to
!       the pivot order but has zeros in it) and then compress
!       it. Since LAST is assumed to be of size N at the
!       interface level, we need another array to store
!       the inverse of ELEN for entries greater than N
!       We use DEGREE.
  LAST(1:N) = 0
  DEGREE(1:TOTEL-N)=0
  DO I = 1, N
    K = abs (ELEN (I))
    IF ( K <= N ) THEN
      LAST (K) = I
    ELSE
      DEGREE(K-N)=I
    ENDIF
  ENDDO
  I = 1
  DO K = 1, N
    IF(LAST (K) .NE. 0) THEN
      LAST(I) = LAST(K)
      ELEN(LAST(K)) = I
      I = I + 1
    ENDIF
  ENDDO
  DO K = N+1, TOTEL
    IF (DEGREE(K-N) .NE. 0) THEN
      LAST(I)=DEGREE(K-N)
      ELEN(DEGREE(K-N)) = I
      I = I + 1
    ENDIF
  END DO
ELSE
  DO 300 I = 1, N
     K = abs (ELEN (I))
     LAST (K) = I
     ELEN (I) = K
300   CONTINUE
ENDIF
!=======================================================================
!  RETURN THE MEMORY USAGE IN IW
!=======================================================================
!     If maxmem is less than or equal to iwlen, then no compressions
!     occurred, and iw (maxmem+1 ... iwlen) was unused.  Otherwise
!     compressions did occur, and iwlen would have had to have been
!     greater than or equal to maxmem for no compressions to occur.
!     Return the value of maxmem in the pfree argument.
PFREE = MAXMEM
!===============================
!     Save PE in PARENT array
DO I=1,N
 PARENT(I) = int(PE(I))
ENDDO
!===============================
RETURN
END SUBROUTINE MUMPS_CST_AMF
!-----------------------------------------------------------------------
! MUMPS_SYMQAMD: modified version of MUMPS_QAMD code to 
! designed to compute a symbolic factorization given 
! an input ordering (provided in PERM array) and possibly
! a schur area.
! ---------
SUBROUTINE MUMPS_SYMQAMD &
  &                ( THRESH, NDENSE, &
  &                 N, TOTEL, IWLEN, PE, PFREE, LEN, IW, NV, &
  &                 ELEN, LAST, NCMPA, DEGREE, HEAD, NEXT, W, &
  &                 PERM, LISTVAR_SCHUR, SIZE_SCHUR, &
  &                 AGG6, PARENT )
IMPLICIT NONE
!     Input not modified
INTEGER, INTENT(IN)    :: N, TOTEL, SIZE_SCHUR
LOGICAL, INTENT(IN)    :: AGG6
INTEGER, INTENT(IN)    :: THRESH
INTEGER(8), INTENT(IN) :: IWLEN
INTEGER, INTENT(IN)    :: LISTVAR_SCHUR(max(1,SIZE_SCHUR))
!     Input undefined on output 
INTEGER, INTENT(INOUT)  :: LEN(N), IW(IWLEN)
! 
!     Output only 
INTEGER, INTENT(OUT)   :: NCMPA
INTEGER, INTENT(OUT)   :: ELEN(N), LAST(TOTEL), PARENT(N)
! 
!     Input/output
INTEGER, INTENT(INOUT)    :: NV(N)
INTEGER(8), INTENT(INOUT) :: PFREE
INTEGER(8), INTENT(INOUT) :: PE(N)
INTEGER, INTENT(INOUT)    :: PERM(N)
! 
!     Internal Workspace only
INTEGER, INTENT(OUT) :: NDENSE(N), DEGREE(N), &
  &                        HEAD(TOTEL), NEXT(N), W(N)
!PA2JY: FIXME should HEAD/LAST (N) instead of TOTEL ?
!
!  =======================
!  INTERFACE DOCUMENTATION
!  SPECIFIC TO SYMQAMD.
!  =======================
!  (more details are sometimes
!   available in the
!   PREVIOUS DOCUMENTATION
!   section)
!
! N (in): the size of the matrix 
!         number of supervariables if blocked format
! TOTEL (in) : Number of variables to eliminate
!
! IWLEN (in): the length of the workspace IW
!
! PFREE (inout): says that IW(1:PFREE-1) contains the graph on input, see
!             below. (on output see meaning bellow)
! IW (inout):
!    On input, IW(1:PFREE-1) contains the orginal graph
!    On output it has been corrupted because IW(1:IWLEN) has been
!    used as workspace.
!
! LEN(inout):  On input, 
!       LEN (i) holds the number of entries in row i of the
!       matrix, excluding the diagonal.  The contents of LEN(1..N)
!       are undefined on output.
!
! PE(inout): On input PE(i) contains the pointers in IW to (the column
!       indices of) row i of the matrix.
!       On output it contains the tree:
!       - if I is a principal variable (NV(I) >0) then -pe(I) is the principal
!         variable of the father, or 0 if I is a root node.
!       - if I is a secondary variable (NV(I)=0) then -pe(I) is the principal
!         variable of the node it belongs to.
!
!       On output:  (PE is copied on output into PARENT array)
!
!  
! NV(inout): 
!          On input:  encoding of a blocked matrix 
!            if NV(1).NE.-1 then NV(I) holds the weight of node I. 
!          During execution, 
!            abs (nv (i)) is equal to the number of rows
!            that are represented by the principal supervariable i.  
!            If i is a nonprincipal variable, then nv (i) = 0.  
!            nv (i) .lt. 0 signifies that i is a
!            principal variable in the pattern Lme of the current pivot
!            element me. 
!          On output: 
!          - if i is a principal variable, NV(i) is the size of the front
!          in the multifrontal terminology.
!          - if i is a secondary variable, NV(i)=0
!
! PERM  (inout) : MUST BE SET TO HOLD THE POSITION OF VARIABLE I IN THE
!     PERMUTED ORDER.
!     PERM(I) = J means that I is the Jth pivot.
!     PERM IS NOT ALTERED IF SIZE_SCHUR = 0.
!     IF SIZE_SCHUR > 0 and variable I is part of the Schur,
!     then PERM(I) must be greater than N - SIZE_SCHUR.
!     In that case, PERM(I) is altered: it is set to N+1 internally !
!
! SIZE_SCHUR (in) :   > 0 means that the last SIZE_SCHUR variable 
!                in the order (such that PERM(I) >  N-SIZE_SCHUR) 
!                are part of the schur decompositon
!                and should remain ordered last and amalgamated
!                at the root of the elimination tree.
!
! LISTVAR_SCHUR(1:SIZE_SCHUR) (in): should be set on entry to the list of
!                variables (original indices) in the Schur complement
!
! THRESH (in): is used to set the local variable THRESM, corresponding
!      to the internal restarting feature.
!      <= 0 Recommended value. Automatic setting will be done.
!          Note that this does not correspond to the historical
!          documentation further below.
!       =  N Only exactly dense rows in the reduced matrix are selected.
!       >  1 and <= N THRESH correspond to the minimum density requirement.
!
!      At the moment if SIZE_SCHUR > 0 restarting functionality is disabled,
!      which means that performance is not optimal. It should work again with
!      a small modification but this has to be tested when it is re-enabled.
!
! ELEN (out) needs not be set on entry.
!      It contains the inverse
!      permutation on output. Not sure what it contains for the Schur
!      variables.
!      (it should be ok for the Schur too).
!
! LAST  used internally as working space; 
!       On output, last (1..n) holds the permutation,  i = last (k), then
!       row i is the kth pivot row.
!       Not used on output and 
!       Computation has been suppressed 
!       since in the context of blocked matrix format
!       one cannot so easily compute last out of elen 
!       (see end of MUMPS_QAMD in case of COMRPESS, 
!       because elen(i) \in [1:TOTEL] and not \in [1:N])
!
! AGG6 (in): controls if aggressive absorption should be authorized.
!
!  -------------------------------------------
!  ARGUMENTS USED INTERNALLY AS WORKARRAYS
!  Maybe some things are significant on output
!  but not in the normal cases of usage.
!  -------------------------------------------
!
!  NDENSE, LAST, NEXT, HEAD, DEGREE, W
!
!  ------
!  OUTPUT
!  ------
!
!  NCMPA (out): number of compressions.
!
!
!  ======================
!  PREVIOUS DOCUMENTATION
!  ======================
!
! NDENSE of an element is the number of dense rows in the element.
!-----------------------------------------------------------------------
! It is a modified version of MUMPS_QAMD
! designed to automatically detect and exploit dense or quasi dense 
! rows in the reduced matrix at any step of the minimum degree. 
! The input integer parameter THRESH defines the quasi density:
! THRESH : input parameter (not modified) 
!  THRESH is used to compute THRESM 
!   <=0 or N Only exactly dense rows in the reduced matrix are selected.
!   >1 and <=N THRESH correspond to the munimum density requirement.
!      Version 0: All dense and quasi dense rows are amalgamated at the 
!                 root node.
!      Version 1: Restart AMD with all quasi dense rows, and 
!                 increase density requirement.
!-----------------------------------------------------------------------
! Additionnal parameters/variables due to dense row manipulation:
!          
! Local variables:
! ---------------
INTEGER THRESM, NDME, PERMeqN
INTEGER NBD,NBED, NBDM, LASTD, NELME
LOGICAL IDENSE
! THRESM : Local Integer holding a 
!          potentially modified value of THRESH.
!          When quasi dense rows are reintegrated in the 
!          graph to be processed then THRESM is modified.
!   Note that if one sets THRESM to negative value then
!       <0 Classical AMD algorithm (no dense row detection)
! NDME  : number of dense row adjacent to me
! NELME number of pivots selected when reching the root
! LASTD index of the last row in the list of dense rows
! NBD is the total number of dense rows selected 
! NBED is the total number of exactly dense rows detected. 
! NBDM is the maximum number of dense rows selected 
! IDENSE is used to indicate that the supervariable I is a dense or
!        quasi-dense row.
!-----------------------------------------------------------------------
! INPUT ARGUMENTS (unaltered):
!-----------------------------------------------------------------------
! n:    The matrix order.
!
!       Restriction:  n .ge. 1
! iwlen:        The length of iw (1..iwlen).  On input, the matrix is
!       stored in iw (1..pfree-1).  However, iw (1..iwlen) should be
!       slightly larger than what is required to hold the matrix, at
!       least iwlen .ge. pfree + n is recommended.  Otherwise,
!       excessive compressions will take place.
!       *** We do not recommend running this algorithm with ***
!       ***      iwlen .lt. pfree + n.                      ***
!       *** Better performance will be obtained if          ***
!       ***      iwlen .ge. pfree + n                       ***
!       *** or better yet                                   ***
!       ***      iwlen .gt. 1.2 * pfree                     ***
!       *** (where pfree is its value on input).            ***
!       The algorithm will not run at all if iwlen .lt. pfree-1.
!
!       Restriction: iwlen .ge. pfree-1
!-----------------------------------------------------------------------
! INPUT/OUPUT ARGUMENTS:
!-----------------------------------------------------------------------
! pe:   On input, pe (i) is the index in iw of the start of row i, or
!       zero if row i has no off-diagonal non-zeros.
!
!       During execution, it is used for both supervariables and
!       elements:
!
!       * Principal supervariable i:  index into iw of the
!               description of supervariable i.  A supervariable
!               represents one or more rows of the matrix
!               with identical nonzero pattern.
!       * Non-principal supervariable i:  if i has been absorbed
!               into another supervariable j, then pe (i) = -j.
!               That is, j has the same pattern as i.
!               Note that j might later be absorbed into another
!               supervariable j2, in which case pe (i) is still -j,
!               and pe (j) = -j2.
!       * Unabsorbed element e:  the index into iw of the description
!               of element e, if e has not yet been absorbed by a
!               subsequent element.  Element e is created when
!               the supervariable of the same name is selected as
!               the pivot.
!       * Absorbed element e:  if element e is absorbed into element
!               e2, then pe (e) = -e2.  This occurs when the pattern of
!               e (that is, Le) is found to be a subset of the pattern
!               of e2 (that is, Le2).  If element e is "null" (it has
!               no nonzeros outside its pivot block), then pe (e) = 0.
!
!       On output, pe holds the assembly tree/forest, which implicitly
!       represents a pivot order with identical fill-in as the actual
!       order (via a depth-first search of the tree).
!
!       On output:
!       If nv (i) .gt. 0, then i represents a node in the assembly tree,
!       and the parent of i is -pe (i), or zero if i is a root.
!       If nv (i) = 0, then (i,-pe (i)) represents an edge in a
!       subtree, the root of which is a node in the assembly tree.
! pfree:        On input, the matrix is stored in iw (1..pfree-1) and
!       the rest of the array iw is free.
!       During execution, additional data is placed in iw, and pfree
!       is modified so that components  of iw from pfree are free.
!       On output, pfree is set equal to the size of iw that
!       would have been needed for no compressions to occur.  If
!       ncmpa is zero, then pfree (on output) is less than or equal to
!       iwlen, and the space iw (pfree+1 ... iwlen) was not used.
!       Otherwise, pfree (on output) is greater than iwlen, and all the
!       memory in iw was used.
!-----------------------------------------------------------------------
! INPUT/MODIFIED (undefined on output):
!-----------------------------------------------------------------------
! len:  On input, len (i) holds the number of entries in row i of the
!       matrix, excluding the diagonal.  The contents of len (1..n)
!       are undefined on output.
! iw:   On input, iw (1..pfree-1) holds the description of each row i
!       in the matrix.  The matrix must be symmetric, and both upper
!       and lower triangular parts must be present.  The diagonal must
!       not be present.  Row i is held as follows:
!
!               len (i):  the length of the row i data structure
!               iw (pe (i) ... pe (i) + len (i) - 1):
!                       the list of column indices for nonzeros
!                       in row i (simple supervariables), excluding
!                       the diagonal.  All supervariables start with
!                       one row/column each (supervariable i is just
!                       row i).
!               if len (i) is zero on input, then pe (i) is ignored
!               on input.
!
!               Note that the rows need not be in any particular order,
!               and there may be empty space between the rows.
!
!       During execution, the supervariable i experiences fill-in.
!       This is represented by placing in i a list of the elements
!       that cause fill-in in supervariable i:
!
!               len (i):  the length of supervariable i
!               iw (pe (i) ... pe (i) + elen (i) - 1):
!                       the list of elements that contain i.  This list
!                       is kept short by removing absorbed elements.
!               iw (pe (i) + elen (i) ... pe (i) + len (i) - 1):
!                       the list of supervariables in i.  This list
!                       is kept short by removing nonprincipal
!                       variables, and any entry j that is also
!                       contained in at least one of the elements
!                       (j in Le) in the list for i (e in row i).
!
!       When supervariable i is selected as pivot, we create an
!       element e of the same name (e=i):
!
!               len (e):  the length of element e
!               iw (pe (e) ... pe (e) + len (e) - 1):
!                       the list of supervariables in element e.
!
!       An element represents the fill-in that occurs when supervariable
!       i is selected as pivot (which represents the selection of row i
!       and all non-principal variables whose principal variable is i).
!       We use the term Le to denote the set of all supervariables
!       in element e.  Absorbed supervariables and elements are pruned
!       from these lists when computationally convenient.
!
!       CAUTION:  THE INPUT MATRIX IS OVERWRITTEN DURING COMPUTATION.
!       The contents of iw are undefined on output.
!-----------------------------------------------------------------------
! OUTPUT (need not be set on input):
!-----------------------------------------------------------------------
! nv:   During execution, abs (nv (i)) is equal to the number of rows
!       that are represented by the principal supervariable i.  If i is
!       a nonprincipal variable, then nv (i) = 0.  Initially,
!       nv (i) = 1 for all i.  nv (i) .lt. 0 signifies that i is a
!       principal variable in the pattern Lme of the current pivot
!       element me.  On output, nv (e) holds the true degree of element
!       e at the time it was created (including the diagonal part).
! elen: See the description of iw above.  At the start of execution,
!       elen (i) is set to zero.  During execution, elen (i) is the
!       number of elements in the list for supervariable i.  When e
!       becomes an element, elen (e) = -nel is set, where nel is the
!       current step of factorization.  elen (i) = 0 is done when i
!       becomes nonprincipal.
!
!       For variables, elen (i) .ge. 0 holds until just before the
!       permutation vectors are computed.  For elements,
!       elen (e) .lt. 0 holds.
!
!       On output elen (1..n) holds the inverse permutation (the same
!       as the 'INVP' argument in Sparspak).  That is, if k = elen (i),
!       then row i is the kth pivot row.  Row i of A appears as the
!       (elen(i))-th row in the permuted matrix, PAP^T.
! last: In a degree list, last (i) is the supervariable preceding i,
!       or zero if i is the head of the list.  In a hash bucket,
!       last (i) is the hash key for i.  last (head (hash)) is also
!       used as the head of a hash bucket if head (hash) contains a
!       degree list (see head, below).
!
!       On output, last (1..n) holds the permutation (the same as the
!       'PERM' argument in Sparspak).  That is, if i = last (k), then
!       row i is the kth pivot row.  Row last (k) of A is the k-th row
!       in the permuted matrix, PAP^T.
! ncmpa:        The number of times iw was compressed.  If this is
!       excessive, then the execution took longer than what could have
!       been.  To reduce ncmpa, try increasing iwlen to be 10% or 20%
!       larger than the value of pfree on input (or at least
!       iwlen .ge. pfree + n).  The fastest performance will be
!       obtained when ncmpa is returned as zero.  If iwlen is set to
!       the value returned by pfree on *output*, then no compressions
!       will occur.
!-----------------------------------------------------------------------
! LOCAL (not input or output - used only during execution):
!-----------------------------------------------------------------------
! degree:       If i is a supervariable, then degree (i) holds the
!       current approximation of the external degree of row i (an upper
!       bound).  The external degree is the number of nonzeros in row i,
!       minus abs (nv (i)) (the diagonal part).  The bound is equal to
!       the external degree if elen (i) is less than or equal to two.
!
!       We also use the term "external degree" for elements e to refer
!       to |Le \ Lme|.  If e is an element, then degree (e) holds |Le|,
!       which is the degree of the off-diagonal part of the element e
!       (not including the diagonal part).
! degree (I) =N+1 if I is an exactly dense row in reduced matrix.
!            =N+1+LAST_approximate_external_deg of I   
!                      if I is a quasi dense row in reduced matrix.
! All dense or quasi dense rows are stored in the list pointed 
!       by head(n). Quasi-dense rows (degree(I)=n) are stored first, 
!       and are followed by exactly dense rows in the reduced matrix.
!       LASTD holds the last row in this list of dense rows or is zero
!       if the list is empty.
! head: head is used for degree lists.  head (deg) is the first
!       supervariable in a degree list (all supervariables i in a
!       degree list deg have the same approximate degree, namely,
!       deg = degree (i)).  If the list deg is empty then
!       head (deg) = 0.
!
!       During supervariable detection head (hash) also serves as a
!       pointer to a hash bucket.
!       If head (hash) .gt. 0, there is a degree list of degree hash.
!               The hash bucket head pointer is last (head (hash)).
!       If head (hash) = 0, then the degree list and hash bucket are
!               both empty.
!       If head (hash) .lt. 0, then the degree list is empty, and
!               -head (hash) is the head of the hash bucket.
!       After supervariable detection is complete, all hash buckets
!       are empty, and the (last (head (hash)) = 0) condition is
!       restored for the non-empty degree lists.
! next: next (i) is the supervariable following i in a link list, or
!       zero if i is the last in the list.  Used for two kinds of
!       lists:  degree lists and hash buckets (a supervariable can be
!       in only one kind of list at a time).
! w:    The flag array w determines the status of elements and
!       variables, and the external degree of elements.
!
!       for elements:
!          if w (e) = 0, then the element e is absorbed
!          if w (e) .ge. wflg, then w (e) - wflg is the size of
!               the set |Le \ Lme|, in terms of nonzeros (the
!               sum of abs (nv (i)) for each principal variable i that
!               is both in the pattern of element e and NOT in the
!               pattern of the current pivot element, me).
!          if wflg .gt. w (e) .gt. 0, then e is not absorbed and has
!               not yet been seen in the scan of the element lists in
!               the computation of |Le\Lme| in loop 150 below.
!
!       for variables:
!          during supervariable detection, if w (j) .ne. wflg then j is
!          not in the pattern of variable i
!
!       The w array is initialized by setting w (i) = 1 for all i,
!       and by setting wflg = 2.  It is reinitialized if wflg becomes
!       too large (to ensure that wflg+n does not cause integer
!       overflow).
!-----------------------------------------------------------------------
! LOCAL INTEGERS:
!-----------------------------------------------------------------------
!     THRESM is used to
!        accelerate symolic factorization 
!         THRESM is dynamically updated to 
!                allow more quasi-dense row selection
!     ThresPrev holds last starting value 
!                   at the beginning of one iteration
!     ThresMin  holds minimum value of THRESH 
INTEGER :: FDEG, ThresMin, ThresPrev, IBEGSchur, NbSchur, &
  &        ThresMinINIT
INTEGER :: DEGMAX,THD, THDperm, THD_AGG
DOUBLE PRECISION :: RELDEN
LOGICAL :: AGG6_loc, DenseRows
LOGICAL :: SchurON
INTEGER :: DEG, DEGME, DEXT, DMAX, E, ELENME, ELN, I, &
  &        ILAST, INEXT, J, JLAST, JNEXT, K, KNT1, KNT2, KNT3, &
  &        LENJ, LN, ME, MINDEG, NEL, &
  &        NLEFT, NVI, NVJ, NVPIV, SLENME, WE, WFLG, WNVI, X
INTEGER KNT1_UPDATED, KNT2_UPDATED
INTEGER :: SIZE_SCHUR_LOC
INTEGER(8) MAXMEM, MEM, NEWMEM
INTEGER :: MAXINT_N
INTEGER(8) :: HASH, HMOD
LOGICAL :: COMPRESS
! deg:        the degree of a variable or element
! degme:      size, |Lme|, of the current element, me (= degree (me))
! dext:       external degree, |Le \ Lme|, of some element e
! dmax:       largest |Le| seen so far
! e:          an element
! elenme:     the length, elen (me), of element list of pivotal var.
! eln:        the length, elen (...), of an element list
! hash:       the computed value of the hash function
! hmod:       the hash function is computed modulo hmod = max (1,n-1)
! i:          a supervariable
! ilast:      the entry in a link list preceding i
! inext:      the entry in a link list following i
! j:          a supervariable
! jlast:      the entry in a link list preceding j
! jnext:      the entry in a link list, or path, following j
! k:          the pivot order of an element or variable
! knt1:       loop counter used during element construction
! knt2:       loop counter used during element construction
! knt3:       loop counter used during compression
! lenj:       len (j)
! ln:         length of a supervariable list
! maxint_n:   large integer to test risk of overflow on wflg
! maxmem:     amount of memory needed for no compressions
! me:         current supervariable being eliminated, and the
!                     current element created by eliminating that
!                     supervariable
! mem:        memory in use assuming no compressions have occurred
! mindeg:     current minimum degree
! nel:        number of pivots selected so far
! newmem:     amount of new memory needed for current pivot element
! nleft:      n - nel, the number of nonpivotal rows/columns remaining
! nvi:        the number of variables in a supervariable i (= nv (i))
! nvj:        the number of variables in a supervariable j (= nv (j))
! nvpiv:      number of pivots in current element
! slenme:     number of variables in variable list of pivotal variable
! we:         w (e)
! wflg:       used for flagging the w array.  See description of iw.
! wnvi:       wflg - nv (i)
! x:          either a supervariable or an element
!-----------------------------------------------------------------------
! LOCAL POINTERS:
!-----------------------------------------------------------------------
INTEGER(8) P, P1, P2, P3, PDST, PEND, PJ, PME, PME1, PME2, &
  &           PN, PSRC, PLN, PELN
!             Any parameter (pe (...) or pfree) or local variable
!             starting with "p" (for Pointer) is an index into iw,
!             and all indices into iw use variables starting with
!             "p."  The only exception to this rule is the iwlen
!             input argument.
! p:          pointer into lots of things
! p1:         pe (i) for some variable i (start of element list)
! p2:         pe (i) + elen (i) -  1 for some var. i (end of el. list)
! p3:         index of first supervariable in clean list
! pdst:       destination pointer, for compression
! pend:       end of memory to compress
! pj:         pointer into an element or variable
! pme:        pointer into the current element (pme1...pme2)
! pme1:       the current element, me, is stored in iw (pme1...pme2)
! pme2:       the end of the current element
! pn:         pointer into a "clean" variable, also used to compress
! psrc:       source pointer, for compression
!-----------------------------------------------------------------------
!  FUNCTIONS CALLED:
!-----------------------------------------------------------------------
INTRINSIC max, min, mod, maxval
!=======================================================================
!  INITIALIZATIONS
!=======================================================================
IF (N.EQ.1) THEN
     ELEN(1) = 1
     LAST(1) = 1
     PE(1) = 0_8
     IF (NV(1).LT.0) NV(1) = 1
     NCMPA = 0
     PARENT(1) = 0
     RETURN
ENDIF
AGG6_loc = AGG6
DenseRows = .FALSE.
!
!       We can now assume that N>1
!
!Symbolic  Intialize degrees with the order given by PERM
!
SIZE_SCHUR_LOC = SIZE_SCHUR
SIZE_SCHUR_LOC = min(N,SIZE_SCHUR_LOC)
SIZE_SCHUR_LOC = max(0,SIZE_SCHUR_LOC)
SchurON   = (SIZE_SCHUR_LOC > 0)
IBEGSchur = N-SIZE_SCHUR_LOC+1
THRESM    = THRESH  ! local value of THRESH
IF (THRESM.GT.N) THRESM = N
IF (THRESM.LT.0) THRESM = 0
!     Variables in the schur are considered as exactly dense
!     (Schur variables are ordered last, we check it here)
IF ( SchurON )  THEN
     DO I= 1, N
       IF ( PERM(I) .GE. IBEGSchur) THEN
           PERM(I) = N + 1
!               Because of compress, we force skipping this
!               entry which is anyway empty
          IF (LEN(I) .EQ.0) THEN
            PE(I) = 0_8
          ENDIF
       ENDIF
     ENDDO
ENDIF
!          
IF (SchurON) THEN
!
!         Only restriction is n>= THRESM > 0 
!
!         only exactly dense row will be selected
!         It should also work ok combined to 
!         quasi dense row selection. 
!           (To be Tested it seperately)
       THRESM    = N
       ThresMin  = N
       ThresPrev = N
ELSE
       THRESM    = max(int(31*N/32),THRESM)
       THRESM    = max(THRESM,1)
!
       DEGMAX= maxval(LEN)
       RELDEN=dble(PFREE-1)/dble(N)
       THD = int(RELDEN)*10 + (DEGMAX-int(RELDEN))/10 + 1
       IF (THD.LT.DEGMAX) THEN
        DenseRows = .TRUE.
        THDperm = N
        DO I = 1,N
         IF (LEN(I) .GT. THD) THEN
          THDperm =  min(THDperm,PERM(I))
         ENDIF
        ENDDO
        THRESM  = min(THRESM, THDperm)
       ENDIF
!   Compute ThresMin and initialise  ThresPrev
       ThresMin  = max( 3*THRESM / 4, 1)
       ThresPrev = THRESM
!
ENDIF  ! test on SchurON
!
ThresMinINIT = ThresMin/4
THD_AGG = max(128, min(TOTEL/2048, 1024))
IF (THRESM.GT.0) THEN
 IF ((THRESM.GT.N).OR.(THRESM.LT.2)) THEN
!      exactly dense rows only
    THRESM = N
 ENDIF
ENDIF
LASTD = 0
NBD   = 0
NBED  = 0
NBDM  = 0
WFLG = 2
MAXINT_N=huge(WFLG)-TOTEL
MINDEG = 1
NCMPA = 0
NEL = 0
HMOD = int(max (1, N-1),kind=8)
DMAX = 0
MEM = PFREE - 1
MAXMEM = MEM
DO I = 1, N
  NDENSE(I)= 0
  W (I) = 1
  ELEN (I) = 0
!        NV (I) = 1
!        DEGREE (I) = LEN (I)
ENDDO
DO I=1, N
  LAST (I) = 0
  HEAD (I) = 0
ENDDO
!     initialize degree
IF(NV(1) .LT. 0) THEN
   COMPRESS = .FALSE.
ELSE
   COMPRESS = .TRUE.
ENDIF
IF (COMPRESS) THEN
   DO I=1,N
      DEGREE(I) = 0
      DO P= PE(I) , PE(I)+int(LEN(I)-1,8)
         DEGREE(I) = DEGREE(I) + NV(IW(P))
      ENDDO
   ENDDO
ELSE
   DO I=1,N
      NV(I) = 1
      DEGREE (I) = LEN (I)
   ENDDO
ENDIF
!     ----------------------------------------------------------------
!     initialize degree lists and eliminate rows with no off-diag. nz.
!     ----------------------------------------------------------------
DO 20 I = 1, N
  DEG = DEGREE (I)
  IF (PERM(I).EQ.N) THEN
!          save that I is last in the order
     PERMeqN = I
     PERM(I) = N-1
  ENDIF
  FDEG = PERM(I)
  IF ( (DEG .GT. 0).OR.(PERM(I).EQ.N+1) ) THEN
!         ----------------------------------------------------------
!         place i in the degree list corresponding to its degree
!         or in the dense row list if i is dense or quasi dense.
!         ----------------------------------------------------------
!         test for row density
    IF ( (THRESM.GT.0) .AND. &
  &         (FDEG .GT.THRESM) ) THEN
!           I will be inserted in the degree list of N
      NBD = NBD+NV(I)
      IF (FDEG.NE.N+1) THEN
!
       DEGREE(I) = DEGREE(I)+TOTEL+2
!            insert I at the beginning of degree list of n
       DEG = N
       INEXT = HEAD (DEG)
       IF (INEXT .NE. 0) LAST (INEXT) = I
       NEXT (I) = INEXT
       HEAD (DEG) = I
       LAST(I)  = 0
       IF (LASTD.EQ.0) LASTD=I
      ELSE
!            Only Schur variables are concerned here
!            Property: LISTVAR_SCHUR (1) will 
!            be first in the list of schur variables
       NBED = NBED+NV(I)
       DEGREE(I) = TOTEL+1
!            insert I at the end of degree list of n
       DEG = N
       IF (LASTD.EQ.0) THEN
!              degree list is empty
         LASTD     = I
         HEAD(DEG) = I
         NEXT(I)   = 0
         LAST(I)   = 0
       ELSE
         NEXT(LASTD) = I
         LAST(I)     = LASTD
         LASTD       = I
         NEXT(I)     = 0
       ENDIF
      ENDIF
    ELSE
!           place i in the degree list corresponding to its degree
      INEXT = HEAD (FDEG)
      IF (INEXT .NE. 0) LAST (INEXT) = I
      NEXT (I) = INEXT
      HEAD (FDEG) = I
    ENDIF
  ELSE
!         ----------------------------------------------------------
!         we have a variable that can be eliminated at once because
!         there is no off-diagonal non-zero in its row.
!         ----------------------------------------------------------
    NEL = NEL + NV(I)
    ELEN (I) = -NEL
    PE (I) = 0_8
    W (I) = 0
  ENDIF
20 CONTINUE
!         We suppress dense row selection if none of them was found in A 
!         in the 1st pass
    IF ((NBD.EQ.0).AND.(THRESM.GT.0)) THRESM = N
!
!=======================================================================
!  WHILE (selecting pivots) DO
!=======================================================================
30 IF (NEL .LT. TOTEL) THEN
!=======================================================================
!  GET PIVOT OF MINIMUM DEGREE
!=======================================================================
!       -------------------------------------------------------------
!       find next supervariable for elimination
!       -------------------------------------------------------------
  DO 40 DEG = MINDEG, N
    ME = HEAD (DEG)
    IF (ME .GT. 0) GO TO 50
40   CONTINUE
50   MINDEG = DEG
!       -------------------------------------------------------------
!       We want to respect the ordering provided by the user
!       Therefefore if (DEG > THRESM .and. NBD.ge.0) then 
!       A quasi-dense variable might have a perm value 
!       smaller than ME. 
!       We thus in this case force restarting.
!       -------------------------------------------------------------
  IF ( (DEG.NE.N) .AND. &
  &    (DEG.GT.THRESM+1) .AND. (NBD.GT.0) ) THEN
     MINDEG = N
     GOTO 30
  ENDIF
  IF (DEGREE(ME).LE.TOTEL)  THEN
!       -------------------------------------------------------------
!       remove chosen variable from link list
!       -------------------------------------------------------------
    INEXT = NEXT (ME)
    IF (INEXT .NE. 0) LAST (INEXT) = 0
    HEAD (DEG) = INEXT
  ELSE
!
!         Because of restarting forced even if 
!         variable (not yet quasi dense) but of 
!         value of perm larger than thresm still
!         to be eliminated we have to reset MINDEB to 1
    MINDEG = 1
    NBDM = max(NBDM,NBD)
    IF (DEGREE(ME).GT.TOTEL+1) THEN
      IF (WFLG .GT. MAXINT_N) THEN
       DO  52 X = 1, N
        IF (W (X) .NE. 0) W (X) = 1
52        CONTINUE
       WFLG = 2
      ENDIF
      WFLG = WFLG + 1
51       CONTINUE
!           ---------------------------------------------------------
!           remove chosen variable from link list
!           ---------------------------------------------------------
      INEXT = NEXT (ME)
      IF (INEXT .NE. 0) THEN
         LAST (INEXT) = 0
      ELSE
         LASTD = 0
      ENDIF
!           ----------------------------------------------------------
!           build adjacency list of ME in quotient gragh
!           and calculate its external degree  in ndense(me)
!           ----------------------------------------------------------
      NDENSE(ME) = 0
      W(ME)      = WFLG
      P1 = PE(ME)
      P2 = P1 + int(LEN(ME) -1,8)
!           PLN-1 holds the pointer in IW to the last elet/var in adj list
!              of ME.  LEN(ME) will then be set to PLN-P1
!           PELN-1 hold the pointer in IW to the last elet in in adj list
!              of ME.  ELEN(ME) will then be set to PELN-P1
!           element adjacent to ME
      PLN       = P1
      PELN      = P1
      DO 55 P=P1,P2
        E= IW(P)
        IF (W(E).EQ.WFLG) GOTO 55
        W(E) = WFLG
        IF (PE(E).LT.0_8) THEN
!              E is a nonprincipal variable or absorbed element
          X = E
53           X = int(-PE(X))
          IF (W(X) .EQ.WFLG) GOTO 55
          W(X) = WFLG
          IF ( PE(X) .LT. 0_8 ) GOTO 53
          E = X
        ENDIF
!             -------------------------------------------
!             E is an unabsorbed element or a "dense" row
!                 (NOT already flagged)
!             -------------------------------------------
        IF (ELEN(E).LT.0) THEN
!              E is a new element in adj(ME)
         NDENSE(E) = NDENSE(E) - NV(ME)
         IW(PLN) = IW(PELN)
         IW(PELN) = E
         PLN  = PLN+1_8
         PELN = PELN + 1_8
!              update ndense of ME with all unflagged dense
!              rows in E
         PME1 = PE(E)
         DO 54 PME = PME1, PME1+int(LEN(E)-1,8)
          X = IW(PME)
          IF ((ELEN(X).GE.0).AND.(W(X).NE.WFLG)) THEN
!                X is a dense row 
           NDENSE(ME) = NDENSE(ME) + NV(X)
           W(X) = WFLG
          ENDIF
54          CONTINUE
        ELSE
!              E is a dense row 
         NDENSE(ME) = NDENSE(ME) + NV(E)
         IW(PLN)=E
         PLN = PLN+1_8
        ENDIF
55       CONTINUE
!           ----------------------------------------------
!           DEGREE(ME)-(TOTEL+2) holds last external degree computed
!           when Me was detected as dense
!           NDENSE(ME) is the exact external degree of ME
!           ----------------------------------------------
      WFLG     = WFLG + 1
      LEN(ME)  = int(PLN-P1)
      ELEN(ME) = int(PELN-P1)
      NDME = NDENSE(ME)+NV(ME)
      IF (NDENSE(ME).EQ.0) NDENSE(ME) =1
!           ---------------------------------------------------------
!           place ME in the degree list of NDENSE(ME), update DEGREE
!           ---------------------------------------------------------
      DEGREE(ME) = NDENSE(ME)
      DEG = PERM(ME)
      MINDEG = min(DEG,MINDEG)
      JNEXT = HEAD(DEG)
      IF (JNEXT.NE. 0) LAST (JNEXT) = ME
      NEXT(ME) = JNEXT
      HEAD(DEG) = ME
!           ------------------------------
!           process next quasi dense row
!           ------------------------------
      ME    = INEXT
      IF (ME.NE.0) THEN
        IF (DEGREE(ME).GT.(TOTEL+1) ) GOTO 51
      ENDIF
      HEAD (N) = ME
!           ---------------------------------------
!           update dense row selection strategy
!           -------------------------------------
      IF (THRESM.LT.N) THEN
       ThresMin  = max(THRESM+ThresMin,ThresPrev+ThresMin/2+1)
       ThresMin  = min(ThresMin, N)
       ThresPrev = ThresPrev+(N-ThresPrev)/2+ThresMinINIT
       THRESM    = max( &
  &         THRESM + int(sqrt(dble(ThresMin)))+ ThresMinINIT , &
  &         ThresPrev)
       THRESM    = min(THRESM,N)
       ThresMin  = min(THRESM, ThresMin)
       ThresPrev = THRESM
      ENDIF
      NBD    = NBED
!           get back to Min degree elimination loop
!
      GOTO 30
    ENDIF
!         -------------------------------------------------------------
!         -------------------------------------------------------------
    IF (DEGREE(ME).EQ.TOTEL+1) THEN
!         we have only  exactly "dense" rows that we
!         amalgamate at the root node
       IF (NBD.NE.NBED) THEN
    write(6,*) ' ERROR in MUMPS_SYMQAMD quasi dense rows remains'
    CALL MUMPS_ABORT()
     ENDIF
     NbSchur = 0   ! Only for checking
     NELME    = -(NEL+1)
     DO 59 X=1,N
      IF ((PE(X).GT.0) .AND. (ELEN(X).LT.0)) THEN
!            X is an unabsorbed element
!            -- Force sons to be linked to first node in Schur
       PE(X) = int(-LISTVAR_SCHUR(1),8)
!            W(X) = 0 could be suppressed ?? check it
      ELSEIF (DEGREE(X).EQ.TOTEL+1) THEN
!            X is a dense row, absorb it in ME (mass elimination)
       NEL   = NEL + NV(X)
       PE(X) = int(-ME,8)
       ELEN(X) = 0
       NV(X) = 0
       NbSchur = NbSchur+ 1
      ENDIF
59      CONTINUE
     IF (NbSchur.NE.SIZE_SCHUR_LOC) then
       write(6,*) ' Internal error 2 in MUMPS_SYMQAMD:', &
  &         ' Schur size expected:',SIZE_SCHUR_LOC, 'Real:', NbSchur
       CALL MUMPS_ABORT()
     ENDIF
!          ME is the root node
     ELEN(ME) = NELME
     NV(ME)   = NBD
     PE(ME)   = 0_8
     IF (NEL.NE.N) THEN
      write(6,*) 'Internal error 3 detected in MUMPS_SYMQAMD:'
      write(6,*) ' NEL not equal to N: N, NEL =',N,NEL
      CALL MUMPS_ABORT()
     ENDIF
     IF (ME.NE. LISTVAR_SCHUR(1)) THEN
!          -- Set all node in Schur list to point to LISTVAR_SCHUR(1)
       DO I=1, SIZE_SCHUR_LOC
         PE(LISTVAR_SCHUR(I)) = int(-LISTVAR_SCHUR(1),8)
       ENDDO
       PE(LISTVAR_SCHUR(1)) = 0_8
       NV( LISTVAR_SCHUR(1))= NV(ME)
       NV(ME)               = 0
       ELEN( LISTVAR_SCHUR(1)) = ELEN(ME)
       ELEN(ME)             = 0
     ENDIF
     GOTO 265
    ENDIF
  ENDIF
!       -------------------------------------------------------------
!       me represents the elimination of pivots nel+1 to nel+nv(me).
!       place me itself as the first in this set.  It will be moved
!       to the nel+nv(me) position when the permutation vectors are
!       computed.
!       -------------------------------------------------------------
  ELENME = ELEN (ME)
  ELEN (ME) = - (NEL + 1)
  NVPIV = NV (ME)
  NEL = NEL + NVPIV
  NDENSE(ME) = 0
!=======================================================================
!  CONSTRUCT NEW ELEMENT
!=======================================================================
!       -------------------------------------------------------------
!       At this point, me is the pivotal supervariable.  It will be
!       converted into the current element.  Scan list of the
!       pivotal supervariable, me, setting tree pointers and
!       constructing new list of supervariables for the new element,
!       me.  p is a pointer to the current position in the old list.
!       -------------------------------------------------------------
!       flag the variable "me" as being in Lme by negating nv (me)
  NV (ME) = -NVPIV
  DEGME = 0
  IF (ELENME .EQ. 0) THEN
!         ----------------------------------------------------------
!         construct the new element in place
!         ----------------------------------------------------------
    PME1 = PE (ME)
    PME2 = PME1 - 1
    DO 60 P = PME1, PME1 + int(LEN (ME) - 1,8)
      I = IW (P)
      NVI = NV (I)
      IF (NVI .GT. 0) THEN
!             ----------------------------------------------------
!             i is a principal variable not yet placed in Lme.
!             store i in new list
!             ----------------------------------------------------
        DEGME = DEGME + NVI
!             flag i as being in Lme by negating nv (i)
        NV (I) = -NVI
        PME2 = PME2 + 1
        IW (PME2) = I
!             ----------------------------------------------------
!             remove variable i from degree list.
!             ----------------------------------------------------
!             only done for non "dense" rows
        IF (DEGREE(I).LE.TOTEL) THEN
        ILAST = LAST (I)
        INEXT = NEXT (I)
        IF (INEXT .NE. 0) LAST (INEXT) = ILAST
        IF (ILAST .NE. 0) THEN
          NEXT (ILAST) = INEXT
        ELSE
!               i is at the head of the degree list
          HEAD (PERM(I)) = INEXT
        ENDIF
        ELSE
         NDENSE(ME) = NDENSE(ME) + NVI
        ENDIF
      ENDIF
60     CONTINUE
!         this element takes no new memory in iw:
    NEWMEM = 0
  ELSE
!         ----------------------------------------------------------
!         construct the new element in empty space, iw (pfree ...)
!         ----------------------------------------------------------
    P = PE (ME)
    PME1 = PFREE
    SLENME = LEN (ME) - ELENME
    KNT1_UPDATED = 0
    DO 120 KNT1 = 1, ELENME + 1
      KNT1_UPDATED = KNT1_UPDATED +1
      IF (KNT1 .GT. ELENME) THEN
!             search the supervariables in me.
        E = ME
        PJ = P
        LN = SLENME
      ELSE
!             search the elements in me.
        E = IW (P)
        P = P + 1
        PJ = PE (E)
        LN = LEN (E)
      ENDIF
!           -------------------------------------------------------
!           search for different supervariables and add them to the
!           new list, compressing when necessary. this loop is
!           executed once for each element in the list and once for
!           all the supervariables in the list.
!           -------------------------------------------------------
      KNT2_UPDATED = 0
      DO 110 KNT2 = 1, LN
        KNT2_UPDATED = KNT2_UPDATED+1
        I = IW (PJ)
        PJ = PJ + 1
        NVI = NV (I)
        IF (NVI .GT. 0) THEN
!               -------------------------------------------------
!               compress iw, if necessary
!               -------------------------------------------------
          IF (PFREE .GT. IWLEN) THEN
!                 prepare for compressing iw by adjusting
!                 pointers and lengths so that the lists being
!                 searched in the inner and outer loops contain
!                 only the remaining entries.
            PE (ME) = P
            LEN (ME) = LEN (ME) - KNT1_UPDATED
!                 Reset KNT1_UPDATED in case of recompress 
!                 at same iteration of the loop 120
            KNT1_UPDATED = 0
!                 Check if anything left in supervariable ME
            IF (LEN (ME) .EQ. 0) PE (ME) = 0
            PE (E) = PJ
            LEN (E) = LN - KNT2_UPDATED
!                 Reset KNT2_UPDATED in case of recompress 
!                 at same iteration of the loop 110
            KNT2_UPDATED = 0
!                 Check if anything left in element E
            IF (LEN (E) .EQ. 0) PE (E) = 0
            NCMPA = NCMPA + 1
!                 store first item in pe
!                 set first entry to -item
            DO 70 J = 1, N
              PN = PE (J)
              IF (PN .GT. 0) THEN
                PE (J) = int(IW (PN),8)
                IW (PN) = -J
              ENDIF
70             CONTINUE
!                 psrc/pdst point to source/destination
            PDST = 1
            PSRC = 1
            PEND = PME1 - 1
!                 while loop:
80             CONTINUE
            IF (PSRC .LE. PEND) THEN
!                   search for next negative entry
              J = -IW (PSRC)
              PSRC = PSRC + 1
              IF (J .GT. 0) THEN
                IW (PDST) = int(PE (J))
                PE (J) = PDST
                PDST = PDST + 1
!                     copy from source to destination
                LENJ = LEN (J)
                DO 90 KNT3 = 0, LENJ - 2
                  IW (PDST + KNT3) = IW (PSRC + KNT3)
90                 CONTINUE
                PDST = PDST + LENJ - 1
                PSRC = PSRC + LENJ - 1
              ENDIF
              GO TO 80
            ENDIF
!                 move the new partially-constructed element
            P1 = PDST
            DO 100 PSRC = PME1, PFREE - 1
              IW (PDST) = IW (PSRC)
              PDST = PDST + 1
100             CONTINUE
            PME1 = P1
            PFREE = PDST
            PJ = PE (E)
            P = PE (ME)
          ENDIF
!               -------------------------------------------------
!               i is a principal variable not yet placed in Lme
!               store i in new list
!               -------------------------------------------------
          DEGME = DEGME + NVI
!               flag i as being in Lme by negating nv (i)
          NV (I) = -NVI
          IW (PFREE) = I
          PFREE = PFREE + 1
!               -------------------------------------------------
!               remove variable i from degree link list
!               -------------------------------------------------
!             only done for non "dense" rows
          IF (DEGREE(I).LE.TOTEL) THEN
          ILAST = LAST (I)
          INEXT = NEXT (I)
          IF (INEXT .NE. 0) LAST (INEXT) = ILAST
          IF (ILAST .NE. 0) THEN
            NEXT (ILAST) = INEXT
          ELSE
!                 i is at the head of the degree list
            HEAD (PERM(I)) = INEXT
          ENDIF
          ELSE
           NDENSE(ME) = NDENSE(ME) + NVI
          ENDIF
        ENDIF
110       CONTINUE
      IF (E .NE. ME) THEN
!             set tree pointer and flag to indicate element e is
!             absorbed into new element me (the parent of e is me)
        PE (E) = int(-ME,8)
        W (E) = 0
      ENDIF
120     CONTINUE
    PME2 = PFREE - 1
!         this element takes newmem new memory in iw (possibly zero)
    NEWMEM = PFREE - PME1
    MEM = MEM + NEWMEM
    MAXMEM = max (MAXMEM, MEM)
  ENDIF
!       -------------------------------------------------------------
!       me has now been converted into an element in iw (pme1..pme2)
!       -------------------------------------------------------------
!       degme holds the external degree of new element
  DEGREE (ME) = DEGME
  PE (ME) = PME1
  LEN (ME) = int(PME2 - PME1 + 1_8)
!       -------------------------------------------------------------
!       make sure that wflg is not too large.  With the current
!       value of wflg, wflg+n must not cause integer overflow
!       -------------------------------------------------------------
  IF (WFLG .GT. MAXINT_N) THEN
    DO 130 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
130     CONTINUE
    WFLG = 2
  ENDIF
!=======================================================================
!  COMPUTE (w (e) - wflg) = |Le\Lme| FOR ALL ELEMENTS
!dense
!   COMPUTE (w(e) - wflg) = |Le(G')\Lme(G')| FOR ALL ELEMENTS
!   where G' is the subgraph of G excluding ''dense" rows)
!dense
!=======================================================================
!       -------------------------------------------------------------
!       Scan 1:  compute the external degrees of previous elements
!       with respect to the current element.  That is:
!            (w (e) - wflg) = |Le \ Lme|
!       for each element e that appears in any supervariable in Lme.
!       The notation Le refers to the pattern (list of
!       supervariables) of a previous element e, where e is not yet
!       absorbed, stored in iw (pe (e) + 1 ... pe (e) + iw (pe (e))).
!       The notation Lme refers to the pattern of the current element
!       (stored in iw (pme1..pme2)).   If (w (e) - wflg) becomes
!       zero, then the element e will be absorbed in scan 2.
!       aggressive absorption is possible only if NDENSE(ME) = NBD
!       which is true when only exactly dense rows have been selected.
!       -------------------------------------------------------------
  DO 150 PME = PME1, PME2
    I = IW (PME)
    IF (DEGREE(I).GT.TOTEL) GOTO 150
    ELN = ELEN (I)
    IF (ELN .GT. 0) THEN
!           note that nv (i) has been negated to denote i in Lme:
      NVI = -NV (I)
      WNVI = WFLG - NVI
      DO 140 P = PE (I), PE (I) + int(ELN - 1,8)
        E = IW (P)
        WE = W (E)
        IF (WE .GE. WFLG) THEN
!               unabsorbed element e has been seen in this loop
          WE = WE - NVI
        ELSE IF (WE .NE. 0) THEN
!               e is an unabsorbed element
!               this is the first we have seen e in all of Scan 1
          WE = DEGREE (E) + WNVI - NDENSE(E)
!n dense
        ENDIF
        W (E) = WE
140       CONTINUE
    ENDIF
150   CONTINUE
!=======================================================================
!  DEGREE UPDATE AND ELEMENT ABSORPTION
!=======================================================================
!       -------------------------------------------------------------
!       Scan 2:  for each i in Lme, sum up the degree of Lme (which
!       is degme), plus the sum of the external degrees of each Le
!       for the elements e appearing within i, plus the
!       supervariables in i.  Place i in hash list.
!       -------------------------------------------------------------
  AGG6_loc = (AGG6 .OR. (DEGREE(ME) .LT. THD_AGG))
  DO 180 PME = PME1, PME2
    I = IW (PME)
    IF (DEGREE(I).GT.TOTEL) GOTO 180
    P1 = PE (I)
    P2 = P1 + int(ELEN (I) - 1,8)
    PN = P1
    HASH = 0_8
    DEG = 0
!         ----------------------------------------------------------
!         scan the element list associated with supervariable i
!         ----------------------------------------------------------
    DO 160 P = P1, P2
      E = IW (P)
!           dext = | Le \ Lme |
      DEXT = W (E) - WFLG
      IF (DEXT .GT. 0) THEN
        DEG = DEG + DEXT
        IW (PN) = E
        PN = PN + 1_8
        HASH = HASH + int(E,kind=8)
!        ------------------------------
!        suppress aggressive absorption
!        ------------------------------
      ELSE IF (.NOT. AGG6_loc .AND. DEXT .EQ. 0) THEN
        IW (PN) = E
        PN = PN + 1_8
        HASH = HASH + int(E,kind=8)
!
!        ------------------------------
!        try aggressive absorption
!         when possible
      ELSE IF (AGG6_loc .AND. (DEXT .EQ. 0) .AND. &
  &            ((NDENSE(ME).EQ.NBD).OR.(NDENSE(E).EQ.0))) THEN
!             aggressive absorption: e is not adjacent to me, but
!             |Le(G') \ Lme(G')| is 0 and all dense rows
!             are in me, so absorb it into me
          PE (E) = int(-ME,8)
          W (E)  = 0
       ELSE IF (AGG6_loc .AND. DEXT.EQ.0) THEN
            IW(PN) = E
            PN     = PN+1
            HASH   = HASH + int(E,kind=8)
      ENDIF
160     CONTINUE
!         count the number of elements in i (including me):
    ELEN (I) = int(PN - P1 + 1)
!         ----------------------------------------------------------
!         scan the supervariables in the list associated with i
!         ----------------------------------------------------------
    P3 = PN
    DO 170 P = P2 + 1, P1 + int(LEN (I) - 1,8)
      J = IW (P)
      NVJ = NV (J)
      IF (NVJ .GT. 0) THEN
!             j is unabsorbed, and not in Lme.
!             add to degree and add to new list
!             add degree only of non-dense rows.
        IF (DEGREE(J).LE.TOTEL) DEG=DEG+NVJ
        IW (PN) = J
        PN = PN + 1
        HASH = HASH + int(J,kind=8)
      ENDIF
170     CONTINUE
!         ----------------------------------------------------------
!         update the degree and check for mass elimination
!         ----------------------------------------------------------
    IF (((ELEN(I).EQ.1).AND.(P3.EQ.PN)) &
  &     .OR. &
  &         (AGG6_loc.AND.(DEG .EQ. 0).AND.(NDENSE(ME).EQ.NBD)) &
  &       ) &
  &    THEN
!           -------------------------------------------------------
!           mass elimination
!           -------------------------------------------------------
!           There is nothing left of this node except for an
!           edge to the current pivot element.  elen (i) is 1,
!           and there are no variables adjacent to node i.
!           Absorb i into the current pivot element, me.
      PE (I) = int(-ME,8)
      NVI = -NV (I)
      DEGME = DEGME - NVI
      NVPIV = NVPIV + NVI
      NEL = NEL + NVI
      NV (I) = 0
      ELEN (I) = 0
    ELSE
!           -------------------------------------------------------
!           update the upper-bound degree of i
!           -------------------------------------------------------
!           the following degree does not yet include the size
!           of the current element, which is added later:
      DEGREE(I) = min (DEG+NBD-NDENSE(ME), &
  &                       DEGREE(I))
!           -------------------------------------------------------
!           add me to the list for i
!           -------------------------------------------------------
!           move first supervariable to end of list
      IW (PN) = IW (P3)
!           move first element to end of element part of list
      IW (P3) = IW (P1)
!           add new element to front of list.
      IW (P1) = ME
!           store the new length of the list in len (i)
      LEN (I) = int(PN - P1 + 1)
!           -------------------------------------------------------
!           place in hash bucket.  Save hash key of i in last (i).
!           -------------------------------------------------------
      HASH = mod (HASH, HMOD) + 1_8
      J = HEAD (HASH)
      IF (J .LE. 0) THEN
!             the degree list is empty, hash head is -j
        NEXT (I) = -J
        HEAD (HASH) = -I
      ELSE
!             degree list is not empty
!             use last (head (hash)) as hash head
        NEXT (I) = LAST (J)
        LAST (J) = I
      ENDIF
      LAST (I) = int(HASH,kind=kind(LAST))
    ENDIF
180   CONTINUE
  DEGREE (ME) = DEGME
!       -------------------------------------------------------------
!       Clear the counter array, w (...), by incrementing wflg.
!       -------------------------------------------------------------
  DMAX = max (DMAX, DEGME)
  WFLG = WFLG + DMAX
!       make sure that wflg+n does not cause integer overflow
  IF (WFLG .GT. MAXINT_N) THEN
    DO 190 X = 1, N
      IF (W (X) .NE. 0) W (X) = 1
190     CONTINUE
    WFLG = 2
  ENDIF
!       at this point, w (1..n) .lt. wflg holds
!=======================================================================
!  SUPERVARIABLE DETECTION
!=======================================================================
  DO 250 PME = PME1, PME2
    I = IW (PME)
    IF ( (NV(I).LT.0) .AND. (DEGREE(I).LE.TOTEL) ) THEN
!           only done for nondense rows
!           i is a principal variable in Lme
!           -------------------------------------------------------
!           examine all hash buckets with 2 or more variables.  We
!           do this by examing all unique hash keys for super-
!           variables in the pattern Lme of the current element, me
!           -------------------------------------------------------
      HASH = int(LAST (I),kind=8)
!           let i = head of hash bucket, and empty the hash bucket
      J = HEAD (HASH)
      IF (J .EQ. 0) GO TO 250
      IF (J .LT. 0) THEN
!             degree list is empty
        I = -J
        HEAD (HASH) = 0
      ELSE
!             degree list is not empty, restore last () of head
        I = LAST (J)
        LAST (J) = 0
      ENDIF
      IF (I .EQ. 0) GO TO 250
!           while loop:
200       CONTINUE
      IF (NEXT (I) .NE. 0) THEN
       X = I
!             ----------------------------------------------------
!             this bucket has one or more variables following i.
!             scan all of them to see if i can absorb any entries
!             that follow i in hash bucket.  Scatter i into w.
!             ----------------------------------------------------
        LN = LEN (I)
        ELN = ELEN (I)
!             do not flag the first element in the list (me)
        DO 210 P = PE (I) + 1, PE (I) + int(LN - 1,8)
          W (IW (P)) = WFLG
210         CONTINUE
!             ----------------------------------------------------
!             scan every other entry j following i in bucket
!             ----------------------------------------------------
        JLAST = I
        J = NEXT (I)
!             while loop:
220         CONTINUE
        IF (J .NE. 0) THEN
!               -------------------------------------------------
!               check if j and i have identical nonzero pattern
!               -------------------------------------------------
!               jump if i and j do not have same size data structure
          IF (LEN (J) .NE. LN) GO TO 240
!               jump if i and j do not have same number adj elts
          IF (ELEN (J) .NE. ELN) GO TO 240
!               do not flag the first element in the list (me)
          DO 230 P = PE (J) + 1, PE (J) + int(LN - 1,8)
!                 jump if an entry (iw(p)) is in j but not in i
            IF (W (IW (P)) .NE. WFLG) GO TO 240
230           CONTINUE
!               -------------------------------------------------
!               found it!  j can be absorbed into i
!               -------------------------------------------------
          IF (PERM(J).GT.PERM(X)) THEN
          ! J is absorbed by X
            PE (J) = int(-X,8)
            NV (X) = NV (X) + NV (J)
            NV (J) = 0
            ELEN (J) = 0
          ELSE
          ! X is absorbed by J
            PE (X) = int(-J,8)
            NV (J) = NV (X) + NV (J)
            NV (X) = 0
            ELEN (X) = 0
            X = J
          ENDIF
!               both nv (i) and nv (j) are negated since they
!               are in Lme, and the absolute values of each
!               are the number of variables in i and j:
!               delete j from hash bucket
          J = NEXT (J)
          NEXT (JLAST) = J
          GO TO 220
!               -------------------------------------------------
240           CONTINUE
!               j cannot be absorbed into i
!               -------------------------------------------------
          JLAST = J
          J = NEXT (J)
        GO TO 220
        ENDIF
!             ----------------------------------------------------
!             no more variables can be absorbed into i
!             go to next i in bucket and clear flag array
!             ----------------------------------------------------
        WFLG = WFLG + 1
        I = NEXT (I)
        IF (I .NE. 0) GO TO 200
      ENDIF
    ENDIF
250   CONTINUE
!=======================================================================
!  RESTORE DEGREE LISTS AND REMOVE NONPRINCIPAL SUPERVAR. FROM ELEMENT
!=======================================================================
!       ------------------------------
!       Update thresm for having more
!       quasi dense rows to select
!       ------------------------------
  IF ( .NOT.DenseRows.AND.(THRESM .GT. 0).AND.(THRESM.LT.N) ) &
  &       THEN
    THRESM = max(ThresMin, THRESM-NVPIV)
  ENDIF
  P = PME1
  NLEFT = TOTEL - NEL
  DO 260 PME = PME1, PME2
    I = IW (PME)
    NVI = -NV (I)
    IF (NVI .GT. 0) THEN
!           i is a principal variable in Lme
!           restore nv (i) to signify that i is principal
      NV (I) = NVI
      IF (DEGREE(I).LE.TOTEL) THEN
!           -------------------------------------------------------
!           compute the external degree (add size of current elem)
!           -------------------------------------------------------
      DEG = min (DEGREE (I)+ DEGME - NVI, NLEFT - NVI)
      DEGREE (I) = DEG
      IDENSE = .FALSE.
!           
!           -------------------
!           Dense row detection
!           -------------------
      IF (THRESM.GT.0) THEN
       IF (PERM(I) .GT. THRESM) THEN
!             relaxed dense row detection
         IDENSE = .TRUE.
!
         DEGREE(I) = DEGREE(I)+TOTEL+2
       ENDIF
       IF (IDENSE) THEN
!            update NDENSE of all elements in the list of element
!            adjacent to I (including ME).
         P1 = PE(I)
         P2 = P1 + int(ELEN(I) - 1,8)
         IF (P2.GE.P1) THEN
         DO 264 PJ=P1,P2
           E= IW(PJ)
           NDENSE (E) = NDENSE(E) + NVI
264          CONTINUE
         ENDIF
!            insert I in the list of dense rows
         NBD = NBD+NVI
         FDEG = N
         DEG = N
!              insert I at the beginning of the list
         INEXT = HEAD(DEG)
         IF (INEXT .NE. 0) LAST (INEXT) = I
         NEXT (I) = INEXT
         HEAD (DEG) = I
         LAST(I)    = 0
         IF (LASTD.EQ.0) LASTD=I
!            end of IDENSE=true
       ENDIF
!           end of THRESM>0
      ENDIF
!             
      IF (.NOT.IDENSE) THEN
      FDEG = PERM(I)
!           -------------------------------------------------------
!           place the supervariable at the head of the degree list
!           -------------------------------------------------------
      INEXT = HEAD (FDEG)
      IF (INEXT .NE. 0) LAST (INEXT) = I
      NEXT (I) = INEXT
      LAST (I) = 0
      HEAD (FDEG) = I
      ENDIF
!           -------------------------------------------------------
!           save the new degree, and find the minimum degree
!           -------------------------------------------------------
      MINDEG = min (MINDEG, FDEG)
      ENDIF
!           -------------------------------------------------------
!           place the supervariable in the element pattern
!           -------------------------------------------------------
      IW (P) = I
      P = P + 1
    ENDIF
260   CONTINUE
!=======================================================================
!  FINALIZE THE NEW ELEMENT
!=======================================================================
  NV (ME) = NVPIV + DEGME
!       nv (me) is now the degree of pivot (including diagonal part)
!       save the length of the list for the new element me
  LEN (ME) = int(P - PME1)
  IF (LEN (ME) .EQ. 0) THEN
!         there is nothing left of the current pivot element
    PE (ME) = 0_8
    W (ME) = 0
  ENDIF
  IF (NEWMEM .NE. 0) THEN
!         element was not constructed in place: deallocate part
!         of it (final size is less than or equal to newmem,
!         since newly nonprincipal variables have been removed).
    PFREE = P
    MEM = MEM - NEWMEM + int(LEN (ME),8)
  ENDIF
!=======================================================================
!       END WHILE (selecting pivots)
GO TO 30
ENDIF
!=======================================================================
265 CONTINUE
!=======================================================================
!  COMPUTE THE PERMUTATION VECTORS
!=======================================================================
!     ----------------------------------------------------------------
!     The time taken by the following code is O(n).  At this
!     point, elen (e) = -k has been done for all elements e,
!     and elen (i) = 0 has been done for all nonprincipal
!     variables i.  At this point, there are no principal
!     supervariables left, and all elements are absorbed.
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!     compute the ordering of unordered nonprincipal variables
!     ----------------------------------------------------------------
DO 290 I = 1, N
  IF (ELEN (I) .EQ. 0) THEN
!         ----------------------------------------------------------
!         i is an un-ordered row.  Traverse the tree from i until
!         reaching an element, e.  The element, e, was the
!         principal supervariable of i and all nodes in the path
!         from i to when e was selected as pivot.
!         ----------------------------------------------------------
    J = int(-PE (I))
!         while (j is a variable) do:
270     CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        J = int(-PE (J))
        GO TO 270
      ENDIF
      E = J
!           ----------------------------------------------------------
!           get the current pivot ordering of e
!           ----------------------------------------------------------
      K = -ELEN (E)
!           ----------------------------------------------------------
!           traverse the path again from i to e, and compress the
!           path (all nodes point to e).  Path compression allows
!           this code to compute in O(n) time.  Order the unordered
!           nodes in the path, and place the element e at the end.
!           ----------------------------------------------------------
      J = I
!           while (j is a variable) do:
280       CONTINUE
      IF (ELEN (J) .GE. 0) THEN
        JNEXT = int(-PE (J))
        PE (J) = int(-E,8)
        IF (ELEN (J) .EQ. 0) THEN
!               j is an unordered row
          ELEN (J) = K
          K = K + 1
        ENDIF
        J = JNEXT
      GO TO 280
      ENDIF
!         leave elen (e) negative, so we know it is an element
    ELEN (E) = -K
  ENDIF
290 CONTINUE
!     ----------------------------------------------------------------
!     reset the inverse permutation (elen (1..n)) to be positive,
!     and compute the permutation (last (1..n)).
!     ----------------------------------------------------------------
DO 300 I = 1, N
  K = abs (ELEN (I))
!        LAST (K) = I
!        LAST (K) = I
  ELEN (I) = K
300 CONTINUE
IF (.NOT.SchurON) THEN
!       -----------------------------
!       restore PERM(I)=N  for PERMeqN
!       -----------------------------
  PERM(PERMeqN) = N
ENDIF
!=======================================================================
!  RETURN THE MEMORY USAGE IN IW
!=======================================================================
!     If maxmem is less than or equal to iwlen, then no compressions
!     occurred, and iw (maxmem+1 ... iwlen) was unused.  Otherwise
!     compressions did occur, and iwlen would have had to have been
!     greater than or equal to maxmem for no compressions to occur.
!     Return the value of maxmem in the pfree argument.
PFREE = MAXMEM
!===============================
!     Save PE in PARENT array
DO I=1,N
 PARENT(I) = int(PE(I))
ENDDO
!===============================
RETURN
END SUBROUTINE MUMPS_SYMQAMD
