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
!     Define constants for possible modes:
!
!       memory_save  = compute the size of the save
!                      file and of the structure
!       save         = save the instance
!       restore      = restore the instance
!       restore_ooc  = restore the ooc part of the
!                      instance
!       fake_restore = extract from the saved file
!                      the size of the save file and of
!                      the structure
!
      INTEGER, PARAMETER :: memory_save_mode=1
      INTEGER, PARAMETER :: save_mode=2
      INTEGER, PARAMETER :: restore_mode=3
      INTEGER, PARAMETER :: restore_ooc_mode=4
      INTEGER, PARAMETER :: fake_restore_mode=5
