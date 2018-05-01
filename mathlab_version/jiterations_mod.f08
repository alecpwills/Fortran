! File: jiterations_mod.f08
! Author: Alec Wills
! Date: April 23 2018
! Editor: VSCode
! Purpose: MODULE PURPOSE HERE

! Begin module:
MODULE jiterations
use MPI
! Require explicit variable declaration and save for use
IMPLICIT NONE
SAVE

! SHARED VARIABLE DICTIONARY
! ======================


! PROCEDURES
! ======================
CONTAINS


! SUBROUTINES
! ======================

! Performs an iteration on a given array, sweeping the values into the next array
SUBROUTINE j_sweep1d(nx, ny, dx, dy, tol, s, e, a, f, b)

INTEGER, INTENT(IN) :: nx, ny, s, e   ! Bins and the number extent a process has in the y values
REAL(KIND=8), INTENT(IN) :: dx, dy
REAL(KIND=8), INTENT(IN) :: a(0:nx+1, s-1:e+1), f(0:nx+1, s-1:e+1)
REAL(KIND=8), INTENT(INOUT) :: b(0:nx+1, s-1:e+1)
REAL(KIND=8), INTENT(IN) :: tol                             ! Relaxation tolerance


! Local calculation variables
INTEGER :: i, j, ierr                             ! Loop indices
REAL(KIND=8) :: prefactor, dyterm, dxterm, fterm  ! Hold temporary values in calculating elements



prefactor = (1.0/(2.0*(dx**2+dy**2)))

DO i=1, nx ! Exclude first and final row
    DO j=s, e ! Exclude ghost cells
        dxterm = (dy**2)*(a(i-1, j)+a(i+1, j))
        dyterm = (dx**2)*(a(i, j-1)+a(i, j+1))
        fterm = (dx**2 + dy**2)*f(i, j)
        b(i,j) = prefactor*(dyterm + dxterm - fterm)
    END DO
END DO
CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
END SUBROUTINE j_sweep1d

! FUNCTIONS
! ======================

! Loops over the given local a, b arrays and calculates the cumulative
! squared distance, for error estimation in a specific process
FUNCTION diff(nx, s, e, a, b) RESULT(sum)

INTEGER :: nx, s, e
REAL(KIND=8) :: a(0:nx+1, s-1:e+1), b(0:nx+1, s-1:e+1)
INTEGER :: i, j
REAL(KIND=8) :: sum

sum = 0.0d0                     ! Initialize sum to zero
DO i=1, nx
    DO j=s, e
        sum = sum + (a(i,j) - b(i,j))**2
    END DO
END DO

END FUNCTION diff


! END MODULE
! ======================


END MODULE jiterations
