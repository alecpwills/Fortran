! File Name: random0_mod.f08
! Author: Alec Wills
! Date: 04/04/18
! Purpose: PROGRAM PURPOSE HERE

! Begin program:
MODULE chapman_random

! Require explicit variable declaration
IMPLICIT NONE


! SHARED VARIABLE DICTIONARY
! ======================
INTEGER :: n = 9876    ! Shared seed


! PROCEDURES
! ======================
! ======================
CONTAINS
! SUBROUTINES
! ======================

SUBROUTINE seed ( iseed )
!
! Purpose:
! To set the seed for random number generator random0.
!
! Record of revisions:
! Date        Programmer            Description of change
! ====        ==========            =====================
! 11/23/15    S. J. Chapman         Original code

IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: iseed ! Value to initialize sequence

! Set seed
n = ABS ( iseed )

END SUBROUTINE seed



SUBROUTINE random0 ( ran )
!
! Purpose:
! Subroutine to generate a pseudorandom number with a uniform
! distribution in the range 0. <= ran < 1.0.
! Record of revisions:
! Date     Programmer               Description of change
! ====     ==========               =====================
! 11/23/15 S. J. Chapman            Original code

IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
REAL(8), INTENT(OUT) :: ran        ! Random number

! Calculate next number
n = MOD (8121 * n + 28411, 134456 )

! Generate random value from this number
ran = REAL(n) / 134456.

END SUBROUTINE random0

! END MODULE
! ======================

END MODULE chapman_random


