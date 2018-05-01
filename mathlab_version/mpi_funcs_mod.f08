! File: mpi_funcs_mod.f08
! Author: Alec Wills
! Date: April 22 2018
! Editor: VSCode
! Purpose: MODULE PURPOSE HERE

! Begin module:
MODULE mpi_funcs
USE mpi
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

!  This subroutine is for producing a decomposition of a 1-d array
!  when given a number of processors.  It may be used in "direct" product
!  decomposition.  The values returned assume a "global" domain in [1:n]
SUBROUTINE MPE_DECOMP1D(n, numprocs, myid, s, e)

INTEGER, INTENT(IN) :: n, numprocs, myid
INTEGER, INTENT(OUT) :: s, e

INTEGER :: nlocal, deficit

nlocal = n/numprocs
s = myid*nlocal+1
deficit = MOD(n, numprocs)
s = s + MIN(myid, deficit)

IF (myid < deficit) then   
    nlocal = nlocal + 1
END IF

e = s + nlocal - 1

IF ( (e > n).OR.(myid == (numprocs-1)) ) e = n

END SUBROUTINE MPE_DECOMP1D


! For use of exchanging the values in the different processes to neighbor ghost zones
SUBROUTINE exchange1d(nx, s, e, a, comm1d, nbrbottom, nbrtop)

INTEGER, INTENT(IN) :: nx, s, e    ! Process y ranges and x range
INTEGER, INTENT(IN) :: comm1d, nbrbottom, nbrtop  ! Communicator grouping the decomposition, and the neighbor numbers
REAL(KIND=8), INTENT(IN) :: a(0:nx+1, s-1:e+1)    ! The grid to send/receive values

INTEGER :: ierr   ! Local variable for routines
! Sends the nx+1 values in the e-row of a given process to the process above it, storing in the ghost zone
! Labels as having been received from below
! Note: a(0, y) is fixed by boundary conditions, so we needn't deal with sending that information again

CALL MPI_SENDRECV( a(1,e), nx, MPI_DOUBLE_PRECISION, nbrtop, 0, a(1,s-1), nx, MPI_DOUBLE_PRECISION, &
                    nbrbottom, 0, comm1d, MPI_STATUS_IGNORE, ierr)
! Does the same thing, but to the process below as though it was from the process above

CALL MPI_SENDRECV( a(1,s), nx, MPI_DOUBLE_PRECISION, nbrbottom, 1, a(1,e+1), nx, MPI_DOUBLE_PRECISION, &
                     nbrtop, 1, comm1d, MPI_STATUS_IGNORE, ierr)

END SUBROUTINE exchange1d

! FUNCTIONS
! ======================


! END MODULE
! ======================


END MODULE mpi_funcs