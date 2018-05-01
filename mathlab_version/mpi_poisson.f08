! File: mpi_poisson.f08
! Author: Alec Wills
! Date: April 22 2018
! Editor: VSCode
! Purpose: To solve the Poisson equation with boundary conditions using parallel processing (MPI).
!          Here we use a one-dimensional decomposition of the processes.

! Begin program:
PROGRAM mpi_poisson

USE MPI
USE initcond
USE mpi_funcs
USE jiterations


! Require explicit variable declaration
IMPLICIT NONE


! VARIABLE DICTIONARY
! ======================

! MPI RELATED VARIABLES
INTEGER :: ierr, myid, numprocs
INTEGER :: comm1d, nbrbottom, nbrtop
INTEGER :: s, e
REAL(KIND=8), ALLOCATABLE :: a(:, :), b(:, :), f(:, :)
REAL(KIND=8) :: t1, t2, tdiff
INTEGER :: status(MPI_STATUS_SIZE)
INTEGER :: mpivect, fh

! MPE RELATED VARIABLES
! Not used on Mathlab submission
!INTEGER :: pcd, mcd, pygd, mygd, ppgzone, mpgzone
!INTEGER :: pisone, misone, ppgztwo, mpgztwo, pistwo, mistwo
!INTEGER :: pac, mac


! Other variables
INTEGER :: allocerr, globaliter
REAL(KIND=8) :: diffnorm, dwork
INTEGER :: oLUN, ioerr, i, j
REAL(KIND=8), ALLOCATABLE :: result(:, :)
INTEGER :: temps, tempe


! EXECUTION OF PROGRAM
! ======================
CALL MPI_INIT(ierr)
CALL MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
CALL MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

! Print out number of processes created, if manager process
IF (myid == 0) THEN
    WRITE(*,*) "The number of processes made for this program:", numprocs
END IF

! Have manager ask for the length and width dimensions
IF (myid == 0) THEN
    ! Echo out the parameters set in the initcond_mod
    WRITE(*,*) "Initial conditions found:"
    WRITE(*,*) "Lx = ", LX
    WRITE(*,*) "Ly = ", LY
    WRITE(*,*) "dx = ", DX
    WRITE(*,*) "dy = ", DX
    WRITE(*,*) "NX = ", NX
    WRITE(*,*) "ny = ", NY
    WRITE(*,*) "tol = ", TOL

    WRITE(*,*) "With a length of ", LX, " x units and ", LY, " y units and&
           & respective increments ", DX, " and ", DY, " your grid has&
           & nx = ", NX, " x bins and ny = ", NY, " y bins."

    ! Warn the user if their bins will be cutoff due to uneven increment
    IF (( LX/DX-NX > 0.001).OR.( LY/DY-NY > 0.001)) THEN
        WRITE(*,*) "WARNING: Increment choice does not evenly divide length&
                  & choice in one or more dimensions. Your bins will not &
                  &cover the full range requested."
    END IF
END IF

! Decompose current domain into different segments
! Separates in a Cartesian manner, isperiodic = .false., reorder = .true. to allow MPI to efficiently segment
!ierr = MPE_LOG_EVENT(pcd, 1, "+CD")
CALL MPI_CART_CREATE(MPI_COMM_WORLD, 1, (/ numprocs /), (/ .false. /), .true., comm1d, ierr)

! Each process gets its position in the newly created communicator, as well as its neighbors
CALL MPI_COMM_RANK(comm1d, myid, ierr)
CALL MPI_Cart_shift(comm1d, 0, 1, nbrbottom, nbrtop, ierr)

WRITE(*,*) "myid: ", myid, ", nbrbottom: ", nbrbottom, ", nbrtop: ", nbrtop

! Evenly distribute the grid points amongst the processes
call MPE_DECOMP1D(NY, numprocs, myid, s, e )

WRITE(*,*) "Process ", myid, "gets y bins ", s, " through ", e

! Allocate the arrays a, b, and f now given the s and e
ALLOCATE(a(0:NX+1, s-1:e+1))
ALLOCATE(b(0:NX+1, s-1:e+1))
ALLOCATE(f(0:NX+1, s-1:e+1))

! Check for allocation
IF (.NOT.(ALLOCATED(a).OR.ALLOCATED(b).OR.ALLOCATED(f))) THEN
    WRITE(*,*) "Error in array allocation. Terminating program."
    STOP 1
ELSE
    WRITE(*,*) "Allocation of local arrays in process ", myid, " successful."
END IF

CALL MPI_BARRIER( MPI_COMM_WORLD, ierr)
! Initialize the boundary conditions in each process
IF (myid == 0) WRITE(*,*) "Setting initial conditions in local arrays."
CALL phi_init(NX, s, e, a, b, f)
IF (myid == 0) WRITE(*,*) "Initial conditions set. Beginning iteration process."

call MPI_BARRIER( MPI_COMM_WORLD, ierr )   ! Create a barrier -- all processes pause until all caught up to here
t1 = MPI_WTIME()  ! Get time for the beginning of the loop

! Begin the iteration process
globaliter = 0 ! Count number of iterations globally
DO WHILE (.TRUE.)
    globaliter = globaliter + 1
    CALL exchange1d(NX, s, e, a, comm1d, nbrbottom, nbrtop)  ! Send ghost row information to neighbors, from a
    
    CALL j_sweep1d(NX, NY, DX, DY, TOL, s, e, a, f, b) ! Iterate over a, saving in b
    
    CALL exchange1d(NX, s, e, b, comm1d, nbrbottom, nbrtop) ! Send ghost row information to neighbors, from b
    
    CALL j_sweep1d(NX, NY, DX, DY, TOL, s, e, b, f, a) ! Iterate over b, saving into a

    dwork = diff(NX, s, e, a, b) ! Calculate the total difference amongst elements of a and b

    ! Reduce the values of dwork into diffnorm, summing over them
	CALL MPI_Allreduce(dwork, diffnorm, 1, MPI_DOUBLE_PRECISION, MPI_SUM, comm1d, ierr)
    
    ! Have manager write out the process
    IF ((myid == 0).AND.(MOD(globaliter, 10) == 0))  THEN 
        WRITE(*,*) "Iteration: ", globaliter, "." 
        WRITE(*,*) "The current accumulated error is: ", diffnorm
    END IF

    ! If the overall sum of the differences is less than the specified tolerance, assume relaxed solution
    IF (diffnorm < TOL) EXIT
END DO
CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
! Find time after completion to write out.
t2 = MPI_WTIME()
tdiff = t2-t1

IF (myid == 0) THEN 
    WRITE(*,*) "Mesh relaxed in ", globaliter, " iterations."
    WRITE(*,*) "Relaxation took ", tdiff, "seconds."
END IF

CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

! OUTPUT RESULTS TO PLOTTABLE FILE
! ================================
CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
IF (myid == 0) WRITE(*,*) "Allocating space for result array."
IF (myid == 0) ALLOCATE(result(0:NX+1, 0:NY+1))
IF (myid == 0) WRITE(*,*) "Concatenating results across processes..."

IF (myid == 0) THEN
    WRITE(*,*) "Copying manager local result array into final results."
    result(0:NX+1, s-1:e+1) = a(0:NX+1, s-1:e+1)
END IF
CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
IF (myid == 0) WRITE(*,*) "Sending worker process arrays."

IF (myid /= 0) THEN
    CALL MPI_SEND(a(0:NX+1, s-1:e+1), (NX+2)*(e-s+3), MPI_DOUBLE_PRECISION, 0, 10, MPI_COMM_WORLD,ierr)
 END IF
 IF (myid == 0) THEN
    DO i=1,numprocs-1
        CALL MPE_DECOMP1D(NY, numprocs, i, temps, tempe)
        CALL MPI_RECV(result(0:NX+1, temps-1:tempe+1), &
            (NX+2)*(tempe-temps+3), MPI_DOUBLE_PRECISION, &
             i, 10, MPI_COMM_WORLD, status, ierr)
    END DO
 END IF

IF (myid == 0) WRITE(*,*) "Opening file to write."
IF (myid == 0) THEN
    OPEN(newunit=oLUN, FILE='output.dat', STATUS='REPLACE', ACTION='WRITE', IOSTAT=ioerr)
    INQUIRE(FILE = 'output.dat', number=oLUN)
    IF (oLUN == -1) WRITE(*,*) "Error opening file."
END IF

IF (myid == 0) WRITE(*,*) ioerr
CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
IF (myid == 0) WRITE(*,*) "Writing coordinate-pair and potential file."
IF (myid == 0) THEN
    DO i=0, NX+1
        DO j=0, NY+1
            WRITE(oLUN, *) DX*i, DY*j, result(i, j)
       END DO
    END DO
END IF
CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
IF (myid == 0) WRITE(*,*) "Finished writing results to output file."


! END PROGRAM
! ======================
CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
!!ierr = MPE_FINISH_LOG("log.log")
CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

IF (myid == 0) WRITE(*,*) "Deallocating f."
DEALLOCATE(f)
IF (myid == 0) WRITE(*,*) "Deallocating a."
DEALLOCATE(a)
IF (myid == 0) WRITE(*,*) "Deallocating b."
DEALLOCATE(b)
IF (myid == 0)  WRITE(*,*) "Deallocating result."
IF (myid == 0 ) DEALLOCATE(result)

CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
IF (myid == 0) WRITE(*,*) "Closing write file."
IF (myid == 0) THEN
    CLOSE(unit=oLUN)
END IF

CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
IF (myid == 0) WRITE(*,*) "Finalizing MPI_Session."


CALL MPI_FINALIZE(ierr)

STOP 0
END PROGRAM mpi_poisson