! File: relax.f08
! Author: Alec Wills
! Date: 03/24/18
! Purpose: To determine the electric potential on a 2D rectangular domain
!          subject to parameters input by the user, by way of Jacobi iteration
!          to relax to the specified tolerance.

! Begin program:
PROGRAM relax

! Use Laplacian module
USE laplacian

! Require explicit variable declaration
IMPLICIT NONE


! VARIABLE DICTIONARY
! ======================
REAL, ALLOCATABLE, DIMENSION(:, :) :: pos_grid  ! Coordinate array
REAL, ALLOCATABLE, DIMENSION(:, :) :: phi_grid  ! Potential array
REAL :: Lx, Ly                                  ! Lengths for x and y
REAL :: dx, dy                                  ! Increment sizes for x and y
REAL :: incr                                    ! Increment variable
REAL :: tol                                     ! Error tolerance
REAL :: xin, yin                                ! input points to find
! Possible inputs for boundary potentials
REAL :: mr, br, ml, bl, mu, bu, md, bd, Vo

INTEGER :: pos_status, phi_status               ! Error ints for allocation
INTEGER :: nx, ny                               ! The bins for x and y
INTEGER :: i,j                                  ! Loop indices
INTEGER :: oLUN, ioerr                          ! Open file parameters
INTEGER :: iin, jin                             ! Indices given input point

CHARACTER :: response                           ! Response string



! EXECUTION OF PROGRAM
! ======================

! Prompt user for the rectangle parameters
WRITE(*,*) "Please input the (real) extent of the x- and y-dimensions."
READ(*,*) Lx, Ly
WRITE(*,*) "Please input the (real) increments dx and dy."
READ(*,*) dx, dy

! Calculate the number of bins to use in coordinate array and echo back
nx = FLOOR(Lx/dx)
ny = FLOOR(Ly/dy)

WRITE(*,*) "With a length of ", Lx, " x units and ", Ly, " y units and&
           & respective increments ", dx, " and ", dy, " your grid has&
           & nx = ", nx, " x bins and ny = ", ny, " y bins."

! Warn the user if their bins will be cutoff due to uneven increment
IF (( Lx/dx-nx > 0.001).OR.( Ly/dy-ny > 0.001)) THEN
   WRITE(*,*) "WARNING: Increment choice does not evenly divide length&
              & choice in one or more dimensions. Your bins will not &
              &cover the full range requested."
   END IF



! Allocate the arrays:
! Use 0:nx to include zero, 0:ny+1 to include zero but +1 for x-column
ALLOCATE(pos_grid(0:nx, 0:ny+1), STAT=pos_status)
ALLOCATE(phi_grid(0:nx, 0:ny), STAT=phi_status)

! If the arrays are allocated, initialize:
IF ( ALLOCATED(pos_grid) ) THEN
   ! Generate the x-coordinate values in the first column
   DO i=0, nx
      pos_grid(i, 0) = dx*i
      
      ! For each x-value, generate the y-values in each column
      DO j=1, ny+1
      pos_grid(i, j) = dy*(j-1)    ! j-1 to include 0
      END DO
   END DO
END IF


! We now have an array of position coordinates.
IF ( ALLOCATED(phi_grid) ) THEN
   CALL potential_relax(nx=nx, ny=ny, dx=dx, dy=dy, pos_grid=pos_grid, phi_grid=phi_grid)
END IF

! Write out the values to a .dat file
OPEN(newunit=oLUN, FILE='output.dat', STATUS='REPLACE', ACTION='WRITE', IOSTAT=ioerr)

WRITE(*,*) "Writing coordinate-pair and potential file."
DO i=0, nx
   DO j=0, ny
      WRITE(oLUN, *) pos_grid(i,0), pos_grid(i, j+1), phi_grid(i, j)
   END DO
END DO

! Prompt user for point to consider
WRITE(*,*) "Would you like to know the potential at a given coordinate pair? (y/n)"
READ(*,*) response

IF (response == 'y') THEN
200   WRITE(*,*) "Input the x and y values."
   READ(*,*) xin, yin
   iin = FLOOR(xin/dx)
   jin = FLOOR(yin/dy)
   WRITE(*,*) "The potential V(x,y) at your point is approximately", &
               & phi_grid(iin, jin), "V."
   WRITE(*,*) "Would you like to know another point's potential? (y/n)"
   READ(*,*) response
   IF (response == 'y') THEN
      GOTO 200
   ELSE IF (response == 'n') THEN
      WRITE(*,*) "Program complete. Deallocating array memory."
   ELSE
      WRITE(*,*) "Invalid input. Aborting program."
      STOP 3
   END IF
ELSE IF (response == 'n') THEN
   WRITE(*,*) "Program complete. Deallocating array memory."
ELSE
   WRITE(*,*) "Invalid input. Aborting program."
END IF


! Deallocate the arrays
DEALLOCATE(pos_grid, STAT=pos_status)
DEALLOCATE(phi_grid, STAT=pos_status)

! END PROGRAM
! ======================

STOP 0
END PROGRAM relax
