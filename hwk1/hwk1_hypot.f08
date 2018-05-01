! File: hwk1_hypot.f08
! Author: Alec Wills
! Date: 01/31/18
! Purpose: To accept two inputs of the legs of a right triangle
!          and output the length of the hypotenuse.

! Begin program:
PROGRAM hypoten

! Require explicit variable declaration
IMPLICIT NONE


! VARIABLE DICTIONARY
! ======================
REAL :: leg1            ! The first leg length
REAL :: leg2            ! The second leg length
REAL :: hypotenuse      ! The hypotenuse length.

! EXECUTION OF PROGRAM
! ======================

! Prompt for user input.
WRITE(*,*) "Please input two (real) leg lengths for the triangle: "
READ(*,*) leg1, leg2

! Use Pythagorean Theorem to find the length of the hypotenuse.
hypotenuse = SQRT( leg1**2 + leg2**2 )

! Print out the result and echo back the input values.
WRITE(*,*) "For leg lengths of ", leg1, " and ", leg2, "units,&
           & the triangle has a hypotenuse of length ", hypotenuse, "units."

! END PROGRAM
! ======================

STOP 0
END PROGRAM hypoten
