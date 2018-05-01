! File: initcond_mod.f08
! Author: Alec Wills
! Date: April 22 2018
! Editor: VSCode
! Purpose: To be used as the initializing module for the Poisson problem.

! Begin module:
MODULE initcond

! Require explicit variable declaration and save for use
IMPLICIT NONE
SAVE

! SHARED VARIABLE DICTIONARY
! ======================
REAL(KIND=8), PARAMETER :: LX = 40.0d0, LY = 15.0d0
REAL(KIND=8), PARAMETER :: DX = 0.1d0, DY = 0.1d0
REAL(KIND=8), PARAMETER :: TOL = 1.0d-5

INTEGER, PARAMETER :: NX = FLOOR(LX/DX), NY = FLOOR(LY/DY)

! PROCEDURES
! ======================
CONTAINS

! SUBROUTINES
! ======================
SUBROUTINE posgrid2d(nx, s, e, returngrid)

INTEGER, INTENT(IN) :: nx, s, e
REAL(KIND=8), INTENT(OUT) :: returngrid(0:nx+1, s-2:e+1) ! s-2 to hold x-values, s-1 will be ghost column
INTEGER :: i, j  ! Loop indices

! Fill in grid with position values
DO i=0, nx+1
    returngrid(i, s-2) = DX*i   ! Populate the x-values
    DO j=s-1, e+1
        returngrid(i, j) = DY*j ! Populate the y-values in the given x-value row
    END DO
END DO


END SUBROUTINE posgrid2d



! Initialize the process-contained potential grid
SUBROUTINE phi_init(nx, s, e, a, b, f)

INTEGER, INTENT(IN) :: nx, s, e  ! The number of x coordinates is constant across the individual arrays, s and e depend on process
! Initialize the arrays, all with the same x-dimension, but with y-dimensions varying around s and e (for ghost zones)
REAL(KIND=8), INTENT(OUT) :: a(0:nx+1, s-1:e+1), b(0:nx+1, s-1:e+1), f(0:nx+1, s-1:e+1)
REAL(KIND=8) :: posgrid(0:nx+1, s-2:e+1)
INTEGER :: i, j         ! Loop indices

CALL posgrid2d(nx, s, e, posgrid)


! Handle boundary conditions
! ==========================
! Begin with initialization to arbitrary constant potential: USER MAY EDIT TO SPECIFY PARTICULAR CONDITIONS
a = 50.0d0
b = 50.0d0

! Source array
f = 0.0d0
f(NX/2, NY/2) = -10.0d0



! UPPER BOUNDARY CONDITIONS (x, y=ymax): USER MAY EDIT TO SPECIFY PARTICULAR CONDITIONS
! Only relevant if the process actually has e = NY, so set ghost zone e+1 to be boundary condition
IF (e == NY) THEN
    DO i=0, nx+1
        a(i, e+1) = 5.0d0*posgrid(i, s-2) - 100.0d0
        b(i, e+1) = 5.0d0*posgrid(i, s-2) - 100.0d0
    END DO
END IF

! LOWER BOUNDARY CONDITIONS (x, y=ymin): USER MAY EDIT TO SPECIFY PARTICULAR CONDITIONS
! Only relevant if the process has s = 1, so set the ghost zone s to be boundary condition
IF (s == 1) THEN
    DO i=0, nx+1
        a(i, s-1) = 0.0d0
        b(i, s-1) = 0.0d0
    END DO
END IF


DO j=s-1, e+1   ! Only from s to e because s-1 and e+1 are ghost zones from other processes
    ! LEFT BOUNDARY CONDITIONS (x=xmin, y): USER MAY EDIT TO SPECIFY PARTICULAR CONDITIONS
    a(0, j) = 0.0d0
    b(0, j) = 0.0d0
    
    ! RIGHT BOUNDARY CONDITIONS (x=xmax, y): USER MAY EDIT TO SPECIFY PARTICULAR CONDITIONS

    a(nx+1, j) = 0.0d0
    b(nx+1, j) = 0.0d0
END DO


END SUBROUTINE phi_init


! FUNCTIONS
! ======================


! END MODULE
! ======================


END MODULE initcond
