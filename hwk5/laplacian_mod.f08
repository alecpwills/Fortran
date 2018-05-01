! File Name: laplacian_mod.f08
! Author: Alec Wills
! Date: 03/25/18
! Purpose: To read in an array of coordinate points and use Jacobi iteration
!          to relax a potential across the given rectangular domain, where
!          a linear potential is specified at the boundaries.

! Begin module:
MODULE laplacian
! Require explicit variable declaration
IMPLICIT NONE
SAVE



! PROCEDURES
! ======================
! ======================

! SUBROUTINES
! ======================
CONTAINS

SUBROUTINE potential_relax(nx, ny, dx, dy, pos_grid, phi_grid)
IMPLICIT NONE

! INPUT PARAMETER DICTIONARY
! ==========================
INTEGER, INTENT(IN) :: nx, ny      ! Bin numbers
REAL, INTENT(IN) :: dx, dy         ! Increment spacing
REAL, INTENT(IN), DIMENSION(0:nx, 0:ny+1) :: pos_grid ! Coordinate grid


! OUTPUT PARAMETER DICTIONARY
! ===========================
REAL, INTENT(OUT), DIMENSION(0:nx, 0:ny) :: phi_grid ! Potential output grid

! LOCAL VARIABLES
! ===============
INTEGER :: i, j, k                                    ! Loop indices
REAL :: prefactor, dyterm, dxterm                  ! Hold temporary values in calculating elements
REAL :: previous_element                ! Matrix element to test against
INTEGER :: count                        ! Counter for number breaking tolerance
REAL :: tol                             ! Relaxation tolerance
CHARACTER :: response                   ! User response to questions
! The various parameters that can be set for the potential
REAL :: Vo, ml, bl, mr, br, md, bd, mu, bu


! EXECUTION
! =========

! By default, the potential parameters are going to be zero.
Vo = 0
ml = 0
bl = 0
mr = 0
br = 0
md = 0
bd = 0
mu = 0
bu = 0

! Ask the user if they would like to specify an initial potential
WRITE(*,*) "Would you like to specify an initial potential across &
           &the region? (y/n)"
READ(*,*) response

! If yes, then read in the potential
IF (response == 'y') THEN
   WRITE(*,*) "Please input the real potential in Volts."
   READ(*,*) Vo
ELSE IF (response == 'n') THEN
   WRITE(*,*) "No initial potential specified. The default is 0 V."
ELSE
   WRITE(*,*) "Invalid input. Aborting program."
   STOP 1
END IF

! Ask the user to specify linear potential parameters:
WRITE(*,*) "By default, no potential is applied to the boundaries. &
           &Would you like to specify any? (y/n)"
READ(*,*) response

IF (response == 'y') THEN
100   WRITE(*,*) "Please select a boundary to specify (real) LINEAR &
              &potential parameters for: slope, intercept."
   WRITE(*,*) "l = left, r = right, u = upper, b = bottom (lower)"
   READ(*,*) response
   IF (response == 'l') THEN
      WRITE(*,*) "Input slope and intercept for the left boundary:"
      READ(*,*) ml, bl
   ELSE IF (response == 'r') THEN
      WRITE(*,*) "Input slope and intercept for the right boundary:"
      READ(*,*) mr, br
   ELSE IF (response == 'u') THEN
      WRITE(*,*) "Input slope and intercept for the upper boundary:"
      READ(*,*) mu, bu
   ELSE IF (response == 'b') THEN
      WRITE(*,*) "Input slope and intercept for the lower boundary:"
      READ(*,*) md, bd
   ELSE
      WRITE(*,*) "Invalid input. Aborting program."
      STOP 2
   END IF
   WRITE(*,*) "Would you like to specify another boundary potential? (y/n)"
   READ(*,*) response
   IF (response == 'y') THEN
      GOTO 100
   ELSE IF (response == 'n') THEN
      WRITE(*, *) "Remaining boundaries grounded to 0 V."
   ELSE
      WRITE(*,*) "Invalid input. Aborting program."
      STOP 3
   END IF
END IF

! Ask for tolerance in relaxation:
WRITE(*,*) "Please input desired tolerance for relaxation:"
READ(*,*) tol



! Initialize phi grid to start at potential aVo everywhere
phi_grid = Vo

! Initialize the boundary conditions: to start, (x, y=ymax) = mu*x+bu
DO i=0, nx
   phi_grid(i, ny) = mu*pos_grid(i, 0)+bu
END DO

! (x, y=0) = md*x+bd
DO i=0, nx
   phi_grid(i, 0) = md*pos_grid(i, 0)+bd
END DO

! (x=0, y) = ml*x+bl
DO j=0, ny
   ! acces pos_grid(0, j+1) to account for pos_grid having 1 more col
   phi_grid(0, j) = ml*pos_grid(0, j+1)+bl 
END DO

! (x=xmax, y) = amr*x+abr
DO j=0, ny
   phi_grid(nx, j) = mr*pos_grid(0, j+1)+br
END DO

! Calculate each new Phi_ij

count = 1           ! Initialize count to be above zero to start loop
k = 0               ! Iteration counter
DO WHILE (count /= 0)
   WRITE(*,*) "Beginning iteration", k, "."
   count = 0    ! Set count to zero at beginning of iteration
   DO i=1, nx-1 ! Exclude first and final row
      DO j=1, ny-1 ! Exclude first and final column
         previous_element = phi_grid(i, j) ! Get element before change
         prefactor = (1.0/(2.0*(dx**2+dy**2)))
         dxterm = (dy**2)*(phi_grid(i-1, j)+phi_grid(i+1, j))
         dyterm = (dx**2)*(phi_grid(i, j-1)+phi_grid(i, j+1))
         phi_grid(i,j) = prefactor*(dyterm+dxterm)
         ! Test to see if new-previous within tolerance
         IF (ABS(phi_grid(i, j) - previous_element) >= tol) THEN
            count = count + 1
         END IF
         
      END DO
   END DO
   k = k + 1
   WRITE(*,*) count, "elements broke the tolerance condition."
END DO

WRITE(*,*) "Your potential mesh relaxed in ", k, "iterations."
   
END SUBROUTINE potential_relax


! END MODULE
! ======================

END MODULE laplacian


