! File: poly_int.f08
! Author: Alec Wills
! Date: 02/16/18
! Purpose: PROGRAM PURPOSE HERE

! Begin program:
PROGRAM poly_int_png

! Require explicit variable declaration
IMPLICIT NONE


! VARIABLE DICTIONARY
! ======================
REAL :: a, b                 ! The user-defined lower and upper limits
REAL :: xi, dx               ! The variable and increment values
INTEGER :: N                 ! The number of bins to use
REAL :: integrals, integralm ! The returned value of the integrations
REAL :: fi                   ! The function value for xi
REAL :: midpoint,simpsons    ! The real attribute of the defined function
INTEGER :: iostat            ! For open status
CHARACTER(80) :: iomsg       ! For error message if needed
INTEGER :: iostat2
CHARACTER(80) :: iomsg2      ! Do the same for the write file

! EXECUTION OF PROGRAM
! ======================

! Open the file from which to read the bins to use
OPEN(UNIT=10, FILE='./bins.dat', STATUS='OLD', ACTION='READ', IOSTAT=iostat, IOMSG=iomsg)

! Open the file to write the values to
OPEN(UNIT=20, FILE='./binsmidsim.dat', STATUS='REPLACE', ACTION='WRITE', IOSTAT=iostat2, IOMSG=iomsg2)

! Define the required integration limits
a = 0.0
b = 10.0


! Begin if statement to guarantee that the open was successful
openif: IF (iostat == 0) THEN

   readloop: DO
                READ(10, *, IOSTAT=iostat) N     ! Get the bin value
                IF (iostat /= 0) EXIT
                       integralm = midpoint(a, b, N)
                       integrals = simpsons(a, b, N)
                       WRITE(20, 300)N, integralm, integrals
                       300 FORMAT (I5, T10, F10.5, T22, F10.5)
                       WRITE (*, 100) a, b, N, integralm
                       100 FORMAT ("The midpoint rule integral of the function&
                         & from ", F10.4," to ", F10.4, " with ", I10," bins is&
                         & approximately ", F10.6, " units squared.")
                       WRITE (*, 200) a, b, N, integrals
                       200 FORMAT ("The Simpson's rule integral of the function&
                         & from ", F10.4," to ", F10.4, " with ", I10," bins is&
                         & approximately ", F10.6, " units squared.")
              END DO readloop
ELSE IF (iostat /= 0) THEN
        WRITE(*,*) "Error reading file."
        STOP 2
END IF openif

! Close file
CLOSE (UNIT=10)
CLOSE (UNIT=20)

! END PROGRAM
! ======================

STOP 0
END PROGRAM poly_int_png

! FUNCTION DECLARATIONS
! =====================

FUNCTION midpoint(a, b, N) RESULT(integral)
REAL :: a, b               ! Initialize the limits of integration
REAL :: xi, dx             ! Initialize the function points and increments
INTEGER :: N               ! Initialize the steps to discretize into
REAL :: fi, integral       ! Initialize the function values and integral

        ! Find step increment dx to use given number of bins chosen:
        dx = (b-a)/(1.0*N)

        ! Echo back the parameters input and the increment value
        WRITE(*,150) a, b, N, dx
        150 FORMAT ("For limits ", F10.5, " to ", F10.5, " with", I5 " bins&
                    & the increment dx =", F10.5, " units.")

        ! Initialize the integral to zero
        integral = 0.0

        ! Initialize xi to the midpoint between a and a+dx, begin loop
        xi = a + 0.5*dx
        DO i = 1, N, 1
                fi = xi**2 + 3*xi
                integral = integral + fi*dx
                xi = xi + dx
        END DO
END FUNCTION midpoint
        

FUNCTION simpsons(a, b, N) RESULT(integral)
REAL :: a, b               ! Initialize the limits of integration
REAL :: xi, dx             ! Initialize the function points and increments
REAL :: xi_half, xi_1      ! Initialize incremented xi values
INTEGER :: N               ! Initialize the steps to discretize into
REAL :: integral           ! Initialize the integral
REAL :: fi, fi_half, fi_1  ! Initialize the incremented function values
REAL :: ai_half            ! Initialize the area of a given bin

        ! Find increment given the bounds and bin size
        dx = (b-a)/(1.0*N)

        ! Echo back the parameters input and the increment value
        WRITE(*,150) a, b, N, dx
        150 FORMAT ("For limits ", F10.5, " to ", F10.5, " with", I5 " bins&
                    & the increment dx =", F10.5, " units.")

        ! Initialize integral to zero
        integral = 0.0

        ! Initialize beginning xi and begin loop
        xi = a
        DO i = 1, N, 1
                xi_half = xi + 0.5*dx   ! Increment half step
                xi_1 = xi + dx          ! Increment one step
                fi = xi**2 + 3*xi
                fi_half = xi_half**2 + 3*xi_half
                fi_1 = xi_1**2 + 3*xi_1
                ai_half = (1.0/6.0)*(fi+4*fi_half+fi_1)*dx
                integral = integral + ai_half
                xi = xi + dx            ! To start at xi_1 next round
        END DO
END FUNCTION simpsons
