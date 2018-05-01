!     Purpose: To multiply two numbers together.
!     Author:  Alec Wills
!     Date: 1-26-18

program mult2
! Turn off the implicit typing mechanism
IMPLICIT NONE

! Declare variables to be multiplied and the result
REAL :: a
REAL :: b
REAL :: c

! Initialize the requested factors
WRITE(*,*) 'What are the two (real) numbers you wish to multiply?\n'
READ(*,*) a, b

! Initialize the desired result
c = a*b

! Print out the result and echo back the input factors.
WRITE(*,*) 'The product of', a, 'and', b, 'is: ', c

! End program
END PROGRAM
