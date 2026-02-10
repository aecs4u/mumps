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
SUBROUTINE MUMPS_SET_VERSION( VERSION_STR )
IMPLICIT NONE
CHARACTER(LEN=*) :: VERSION_STR
CHARACTER(LEN=*) :: V;
PARAMETER (V = "5.8.2" )
IF ( len(V) .GT. 30 ) THEN
   WRITE(*,*) "Version string too long ( >30 characters )"
   CALL MUMPS_ABORT()
END IF
VERSION_STR = V
RETURN
END SUBROUTINE MUMPS_SET_VERSION
