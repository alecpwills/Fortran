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
INCLUDE 'mpe_logf.h'

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
INTEGER :: pcd, mcd, pygd, mygd, ppgzone, mpgzone
INTEGER :: pisone, misone, ppgztwo, mpgztwo, pistwo, mistwo
INTEGER :: pac, mac


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
    WRITE(*,*) "nx = ", NX
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

!ierr = MPE_Start_log()
IF (myid == 0) WRITE(*,*) "Setting logging IDs."
! GET UNIQUE STATE EVENT IDS
ierr = MPE_Log_get_state_eventIDs(pcd, mcd)
ierr = MPE_Log_get_state_eventIDs(pygd, mygd)
ierr = MPE_Log_get_state_eventIDs(ppgzone, mpgzone)
ierr = MPE_Log_get_state_eventIDs(pisone, misone)
ierr = MPE_Log_get_state_eventIDs(ppgztwo, mpgztwo)
ierr = MPE_Log_get_state_eventIDs(pistwo, mistwo)
ierr = MPE_Log_get_state_eventIDs(pac, mac)

IF (myid == 0) THEN
    WRITE(*,*) "Setting logging states."
    ierr = MPE_DESCRIBE_STATE(pcd, mcd, "Cartesian Decomposition", "red:vlines3")
    ierr = MPE_DESCRIBE_STATE(pygd, mygd, "Y-Grid Distribution","orange:gray3")
    ierr = MPE_DESCRIBE_STATE(ppgzone, mpgzone, "Push Ghost Zones 1", "white:light_gray")
    ierr = MPE_DESCRIBE_STATE(pisone, misone, "Iteration Step 1", "blue:gray")
    ierr = MPE_DESCRIBE_STATE(ppgztwo, mpgztwo, "Push Ghost Zones 2", "green:light_gray")
    ierr = MPE_DESCRIBE_STATE(pistwo, mistwo, "Iteration Step 2", "purple:gray")
    ierr = MPE_DESCRIBE_STATE(pac, mac, "Array Concatenation", "pink:gray")
END IF


! Decompose current domain into different segments
! Separates in a Cartesian manner, isperiodic = .false., reorder = .true. to allow MPI to efficiently segment
ierr = MPE_LOG_EVENT(pcd, 1, "+CD")
CALL MPI_CART_CREATE(MPI_COMM_WORLD, 1, numprocs, .false., .true., comm1d, ierr)

! Each process gets its position in the newly created communicator, as well as its neighbors
CALL MPI_COMM_RANK(comm1d, myid, ierr)
CALL MPI_Cart_shift(comm1d, 0, 1, nbrbottom, nbrtop, ierr)
ierr = MPE_LOG_EVENT(mcd, 1, "-CD")
WRITE(*,*) "myid: ", myid, ", nbrbottom: ", nbrbottom, ", nbrtop: ", nbrtop

! Evenly distribute the grid points amongst the processes
ierr = MPE_LOG_EVENT(pygd, 1, "+YGD")
call MPE_DECOMP1D(NY, numprocs, myid, s, e )
ierr = MPE_LOG_EVENT(mygd, 1, "-YGD")

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

!CALL MPI_BARRIER( MPI_COMM_WORLD, ierr)
! Initialize the boundary conditions in each process
IF (myid == 0) WRITE(*,*) "Setting initial conditions in local arrays."
CALL phi_init(NX, s, e, a, b, f)
IF (myid == 0) WRITE(*,*) "Initial conditions set. Beginning iteration process."

!CALL MPI_BARRIER( MPI_COMM_WORLD, ierr )   ! Create a barrier -- all processes pause until all caught up to here
t1 = MPI_WTIME()  ! Get time for the beginning of the loop

! Begin the iteration process
globaliter = 0 ! Count number of iterations globally
DO WHILE (.TRUE.)
    globaliter = globaliter + 1
    ierr = MPE_LOG_EVENT(ppgzone, globaliter, "+PGZ1")
    CALL exchange1d(NX, s, e, a, comm1d, nbrbottom, nbrtop)  ! Send ghost row information to neighbors, from a
    ierr = MPE_LOG_EVENT(mpgzone, globaliter, "-PGZ1")

    ierr = MPE_LOG_EVENT(pisone, globaliter, "+IS1")
    CALL j_sweep1d(NX, NY, DX, DY, TOL, s, e, a, f, b) ! Iterate over a, saving in b
    ierr = MPE_LOG_EVENT(misone, globaliter, "-IS1")

    ierr = MPE_LOG_EVENT(ppgztwo, globaliter, "+PGZ2")
    CALL exchange1d(NX, s, e, b, comm1d, nbrbottom, nbrtop) ! Send ghost row information to neighbors, from b
    ierr = MPE_LOG_EVENT(mpgztwo, globaliter, "-PGZ2")

    ierr = MPE_LOG_EVENT(pistwo, globaliter, "+IS2")
    CALL j_sweep1d(NX, NY, DX, DY, TOL, s, e, b, f, a) ! Iterate over b, saving into a
    ierr = MPE_LOG_EVENT(mistwo, globaliter, "-IS2")

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

! Find time after completion to write out.
t2 = MPI_WTIME()
tdiff = t2-t1

IF (myid == 0) THEN 
    WRITE(*,*) "Mesh relaxed in ", globaliter, " iterations."
    WRITE(*,*) "Relaxation took ", tdiff, "seconds."
END IF



! OUTPUT RESULTS TO PLOTTABLE FILE
! ================================
IF (myid == 0) WRITE(*,*) "Allocating space for result array."
IF (myid == 0) ALLOCATE(result(0:NX+1, 0:NY+1))
IF (myid == 0) WRITE(*,*) "Concatenating results across processes..."

IF (myid == 0) THEN
    WRITE(*,*) "Copying manager local result array into final results."
    result(0:NX+1, s-1:e+1) = a(0:NX+1, s-1:e+1)
END IF

IF (myid == 0) WRITE(*,*) "Sending worker process arrays."

ierr = MPE_LOG_EVENT(pac, 1, "+AC")
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
ierr = MPE_LOG_EVENT(mac, 1, "-AC")

IF (myid == 0) WRITE(*,*) "Opening file to write."
IF (myid == 0) THEN
    OPEN(newunit=oLUN, FILE='output.dat', STATUS='REPLACE', ACTION='WRITE', IOSTAT=ioerr)
    INQUIRE(FILE = 'output.dat', number=oLUN)
    IF (oLUN == -1) WRITE(*,*) "Error opening file."
END IF

IF (myid == 0) WRITE(*,*) "Writing coordinate-pair and potential file."
IF (myid == 0) THEN
    DO i=0, NX+1
        DO j=0, NY+1
            WRITE(oLUN, *) DX*i, DY*j, result(i, j)
       END DO
    END DO
END IF
IF (myid == 0) WRITE(*,*) "Finished writing results to output file."


! END PROGRAM
! ======================
IF (myid == 0) WRITE(*,*) "Deallocating f."
DEALLOCATE(f)
IF (myid == 0) WRITE(*,*) "Deallocating a."
DEALLOCATE(a)
IF (myid == 0) WRITE(*,*) "Deallocating b."
DEALLOCATE(b)
IF (myid == 0)  WRITE(*,*) "Deallocating result."
IF (myid == 0 ) DEALLOCATE(result)


IF (myid == 0) WRITE(*,*) "Closing write file."
IF (myid == 0) THEN
    CLOSE(unit=oLUN)
END IF

IF (myid == 0) WRITE(*,*) "Finalizing MPI_Session."

CALL MPI_FINALIZE(ierr)

STOP 0
END PROGRAM mpi_poisson