! Purpose: To calculate the logarithm of a number in an arbitrary base
! Author:  Alec Wills
! Date     1/28/2018

program logarbbase

! Require explicit variable definitions
IMPLICIT NONE

! Define parameter e
REAL, PARAMETER :: e = 2.718281828459045235360287471352662497&
 &5724709369995

! Define required variables
REAL :: base
REAL :: logarg
REAL :: result

! Initialize the variables

WRITE(*,*) 'Please input the (real) base and the argument of the &
  &logarithm in that base to evaluate: '
READ(*,*) base, logarg

result = LOG(logarg)/LOG(base)

WRITE(*,*) 'The value of log base', base, 'of', logarg, 'is: ', result

END PROGRAM
