! File: cross_prod.f08
! Author: Alec Wills
! Date: 03/02/18
! Purpose: To calculate the cross product of a pair of Euclidean vectors.

! Begin program:
PROGRAM cross

! Use module
USE cross_corr

! Require explicit variable declaration
IMPLICIT NONE


! VARIABLE DICTIONARY
! ======================
REAL, DIMENSION(3) :: array1, array2    ! Input arrays
REAL, DIMENSION(3) :: outarr            ! Resulting cross product

! EXECUTION OF PROGRAM
! ======================

! Read in the user vectors
WRITE(*,*) "Please input the components of the first vector:"
READ(*,*) array1
WRITE(*,*) "Please input the components of the second vector:"
READ(*,*) array2

! Echo back the input arrays.
WRITE(*,100) array1, array2
100 FORMAT ("You have input the vectors ", F10.5, " i + ",F10.5, &
             &" j + ", F10.5, " k and ", F10.5, "i + ", F10.5, &
             &" j +", F10.5, " k.")

! Call the cross product subroutine from the cross_corr module.
CALL cross_product(array1, array2, outarr)

! Write out the result.
WRITE(*, 200) outarr
200 FORMAT ("The cross product of your two vectors is ", F10.5, &
            &" i + ", F10.5, " j + ", F10.5, " k.")


! END PROGRAM
! ======================

STOP 0
END PROGRAM cross
