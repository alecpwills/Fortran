! File: cos_int.f08
! Author: Alec Wills
! Date: 02/16/18
! Purpose: To calculate the numerical integral of the given cosine function
!          using either the midpoint or Simpson's method for a variable
!          integration range from a to b with a specified number of bins N.
!
! NOTE: Comparison with NIntegrate[] in Mathematica for 0 to 10 yields a
!       match in this program using 512 bins in the Simpson's case. From
!       0 to 100, 8096 bins yield a relatively close answer (up to the 
!       difference in the thousandths place (again with Simpson's).
!       32,768 bin matches up to 5 decimal places. 

! Begin program:
PROGRAM cos_int

! Require explicit variable declaration
IMPLICIT NONE


! VARIABLE DICTIONARY
! ======================
REAL :: a, b               ! The user-defined lower and upper limits
REAL :: xi, dx             ! The variable and increment values
INTEGER :: N               ! The number of bins to use
INTEGER :: case            ! The case selector for type of integration
REAL :: integral           ! The returned value of the integration
REAL :: fi                 ! The function value for xi
REAL :: midpoint,simpsons  ! The real attribute of the defined function


! EXECUTION OF PROGRAM
! ======================

! Have user input the integration limits:
WRITE(*,*) "Please input the (real, lower then higher)&
           & integration limits a and b:"
READ(*,*) a,b

! Have user input the desired number of bins to use:
WRITE(*,*) "Please input the integer number of bins to discretize with:"
READ(*,*) N

! Have user select which integration method to use:
WRITE(*,*) "Select an integration method:"
WRITE(*,*) "1 - Midpoint Rule"
WRITE(*,*) "2 - Simpson's Rule"
WRITE(*,*) "3 - Both, for comparison"
READ(*,*) case

! Do the integrations depending on the desired method
intmethod: SELECT CASE (case)
        
        CASE (1) 
                integral = midpoint(a,b,N)
                WRITE (*, 100) a, b, N, integral
                100 FORMAT ("The midpoint rule integral of the function&
                     & from ", F10.4," to ", F10.4, " with ", I10," bins is&
                     & approximately ", F10.6, " units squared.")
                                                      
        CASE (2) 
                integral = simpsons(a,b,N)
                WRITE (*, 200) a, b, N, integral
                200 FORMAT ("The Simpson's rule integral of the function&
                     & from ", F10.4," to ", F10.4, " with ", I10," bins is&
                     & approximately ", F10.6, " units squared.")

        CASE (3) 
                integral = midpoint(a,b,N)
                WRITE(*, 100) a, b, N, integral
                integral = simpsons(a,b,N)
                WRITE(*, 200) a, b, N, integral

        CASE DEFAULT
                WRITE(*,*) "Integration method selection invalid."
                STOP 1

        END SELECT intmethod


! END PROGRAM
! ======================

STOP 0
END PROGRAM cos_int

! FUNCTION DECLARATIONS
! =====================

FUNCTION midpoint(a, b, N) RESULT(integral)
REAL, INTENT(IN) :: a, b          ! Initialize the limits of integration
REAL :: xi, dx                    ! Initialize function points/increments
INTEGER, INTENT(IN) :: N          ! Initialize the steps to discretize into
REAL :: fi, integral              ! Initialize the function values and integral

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
                fi = COS(xi**2 + 3*xi + 5)
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
                fi = COS(xi**2 + 3*xi + 5)
                fi_half = COS(xi_half**2 + 3*xi_half + 5)
                fi_1 = COS(xi_1**2 + 3*xi_1 + 5)
                ai_half = (1.0/6.0)*(fi+4*fi_half+fi_1)*dx
                integral = integral + ai_half
                xi = xi + dx            ! To start at xi_1 next round
        END DO
END FUNCTION simpsons
