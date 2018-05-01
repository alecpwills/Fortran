! Program: As defined in problem 3.8 of Chapman, to test IF statements
! Author:  Alec Wills
! Date:    1/29/18

program logfrac

! Require explicit variable declarations
IMPLICIT NONE

! Declare the required variables

REAL :: y
REAL :: x

! Input the independent variable
WRITE(*,*) 'Please input x for y(x)=ln(1/(1-x)): '
READ(*,*) x

! Calculate the y-value as long as x < 1.0

sizecheck: IF (x < 1.0) THEN
      y = LOG(1/(1-x))
      WRITE(*,*) 'For x = ', x, 'y(x) = ', y, '.'
ELSE IF (ABS(x - 1.0) < 0.00001) THEN
      WRITE(*,*) 'Variable input within precision bounds - division by zero &
      &not allowed.'
ELSE sizecheck
      WRITE(*,*) 'Invalid variable: LOG(x) not defined for negative & 
      &arguments.'
END IF sizecheck



STOP 0
END PROGRAM
