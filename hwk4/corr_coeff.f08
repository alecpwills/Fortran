! File: corr_coeff.f08
! Author: Alec Wills
! Date: 03/02/18
! Purpose: To determine the linear fit to a set of coordinate pairs
!          in a specified input value, with columns of the x-y pairs.

! Begin program:
PROGRAM corr

! Import the module from which we will use the correlation subroutine.
USE cross_corr


! Require explicit variable declaration
IMPLICIT NONE


! VARIABLE DICTIONARY
! ======================
REAL, ALLOCATABLE, DIMENSION(:, :) :: coords    ! Input coordinate array
CHARACTER(20) :: filename                       ! Filename to open
INTEGER :: npairs                               ! Number of coordinates
REAL :: m, b, r                                 ! Stats variables
INTEGER :: iLUN, ioerr                          ! Open parameters
INTEGER :: allocerr                             ! Allocate parameters
INTEGER :: i                                    ! Read loop index

! EXECUTION OF PROGRAM
! ======================

! Prompt for input filename:
WRITE(*,*) "Please input file name containing (x,y) coordinate pairs:"
READ(*,*) filename

! Open the file to read the coordinate data points
OPEN(newunit=iLUN, FILE=filename, STATUS='OLD', ACTION='READ', IOSTAT=ioerr)

! If unsuccessful open, stop the program.
IF (ioerr /= 0) THEN
   WRITE(*,*) "Error opening input file."
   STOP 1
END IF

! Find the number of coordinate pairs

npairs = 0                   ! Initialize the sum to zero.
DO
   READ(iLUN, *, IOSTAT=ioerr) 
   IF (ioerr /= 0) EXIT      ! Since DO won't stop yet, if end of file exit 
   npairs = npairs + 1
END DO

! Allocate the memory
WRITE(*,*) "Allocating coordinate array..."
ALLOCATE(coords(npairs, 2), STAT = allocerr)

! If the allocation was successful, proceed.
IF (allocerr == 0) THEN
   WRITE(*,*) "Allocation successful. Rewinding input file to write data."
   REWIND(UNIT=iLUN)    ! Rewind the file to find the beginning coordinate.
   DO i=1, npairs       ! Loop over the file to input the coordinates into arr
      READ(iLUN, *, IOSTAT=ioerr) coords(i, 1), coords(i, 2)
      IF (ioerr /= 0) EXIT      ! Exit the loop if the file is finished
   END DO
ELSE
   WRITE(*,*) "Error allocating memory"
   STOP 2
END IF

WRITE(*,*) "Array assembled. Beginning linear fit."

! Call fitting subroutine from the cross_corr module
CALL corr_coeff(coords, npairs, m, b, r)

! Deallocate the memory of the array.
DEALLOCATE(coords, STAT=allocerr)

! END PROGRAM
! ======================

STOP 0
END PROGRAM corr
