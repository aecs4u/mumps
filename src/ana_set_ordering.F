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
SUBROUTINE MUMPS_SET_ORDERING(N, KEEP, SYM, NPROCS, IORD, &
  &                       NBQD, AvgDens, &
  &                       PROK, MP)
IMPLICIT NONE
INTEGER, intent(in)    :: N, KEEP(500), NPROCS, SYM
INTEGER, intent(in)    :: NBQD, AvgDens, MP
LOGICAL, intent(in)    :: PROK
INTEGER, intent(inout) :: IORD
INTEGER MAXQD
PARAMETER (MAXQD=2)
INTEGER SMALLSYM, SMALLUNS
PARAMETER (SMALLUNS=5000, SMALLSYM=10000)
#if ! defined(metis) && ! defined(parmetis) && ! defined(metis4) && ! defined(parmetis3)
IF ( IORD .EQ. 5 ) THEN
  IF (PROK) WRITE(MP,*) &
  &  'WARNING: METIS not available. Ordering set to default.'
  IORD = 7
END IF
#endif
#if ! defined(pord)
IF ( IORD .EQ. 4 ) THEN
  IF (PROK) WRITE(MP,*) &
  &  'WARNING: PORD not available. Ordering set to default.'
  IORD = 7
END IF
#endif
#if ! defined(scotch) && !  defined(ptscotch)
IF ( IORD .EQ. 3 ) THEN
  IF (PROK) WRITE(MP,*) &
  &  'WARNING: SCOTCH not available. Ordering set to default.'
  IORD = 7
END IF
#endif
IF (IORD.EQ.4.AND.N.EQ.1) THEN
  IF (PROK) WRITE(MP,*) &
  &  'WARNING: PORD not available for matrices of order 1'
 IORD=0
ENDIF
IF (IORD.EQ.7) THEN
  IF (SYM.NE.0) THEN
    IF ( N.LE.SMALLSYM ) THEN
       IF (NBQD.GE.MAXQD) THEN
         IORD = 6
       ELSE
        IF (KEEP(53).GT.0) THEN
          IORD = 0
        ELSE
         IORD = 2
        ENDIF
       ENDIF
    ELSE
#if  defined(metis) || defined(parmetis) || defined(metis4) || defined(parmetis3)
       IORD = 5
#else
#  if defined(scotch) || defined(ptscotch)
       IORD = 3
#  else
#    if defined(pord)
         IORD = 4
#    else
         IORD = 6
#    endif
#  endif
#endif
    ENDIF
  ELSE
    IF ( N.LE.SMALLUNS ) THEN
      IF (NBQD.GE.MAXQD) THEN
        IORD = 6
      ELSE
        IF (KEEP(53).GT.0) THEN
          IORD = 0
        ELSE
        IORD = 2
        ENDIF
      ENDIF
    ELSE
#if  defined(metis) || defined(parmetis) || defined(metis4) || defined(parmetis3)
      IORD = 5
#else
#  if defined(scotch) || defined(ptscotch)
      IORD = 3
#  else
#    if defined(pord)
        IORD = 4
#    else
        IORD = 6
#    endif
#  endif
#endif
    ENDIF
  ENDIF
ENDIF
IF (IORD.EQ.4.AND.N.EQ.1) IORD=0
RETURN
END SUBROUTINE MUMPS_SET_ORDERING
