! File Name: cross_prod.mod
! Author: Alec Wills
! Date: 03/02/18
! Purpose: Module to be used for calculating cross products and doing fits.

! Begin module:
MODULE cross_corr


! Require explicit variable declaration
IMPLICIT NONE
SAVE


! SHARED VARIABLE DICTIONARY
! ======================


! PROCEDURES
! ======================
! ======================

! FUNCTIONS
! ======================


! SUBROUTINES
! ======================
CONTAINS
  SUBROUTINE cross_product (arr1, arr2, arrout)
    IMPLICIT NONE       ! Require explicit typing
    
    REAL, DIMENSION(3), INTENT(IN) :: arr1, arr2  ! First 3D array, noneditable
    REAL, DIMENSION(3), INTENT(OUT) :: arrout     ! Output 3D array
    
    arrout(1) = arr1(2)*arr2(3) - arr2(1)*arr1(3) ! x-component
    arrout(2) = arr1(3)*arr2(1) - arr2(3)*arr1(1) ! y-component
    arrout(3) = arr1(1)*arr2(2) - arr2(1)*arr1(2) ! z-component
    RETURN
  END SUBROUTINE cross_product


  SUBROUTINE corr_coeff (arr, npairs, m, b, r)
    IMPLICIT NONE
    ! Define the allocatable array that will be input -- (x,y) pairs
    REAL, ALLOCATABLE, DIMENSION(:,:), INTENT(IN) :: arr

    ! Define the number of coordinate pairs input and loop index
    INTEGER, INTENT(IN) :: npairs
    INTEGER :: i
    
    ! Define the output parameters from the fit
    REAL, INTENT(OUT) :: m, b, r
    
    ! Define the various statistical values in the equation
    REAL :: sumxy, sumx, sumxsq, sumy, sumysq   ! Declare the sums.
    REAL :: ybar, xbar, numer, denom            ! Numer/Denom for r coeff.
    
    ! INITIALIZE SUMS TO ZERO
    sumx = 0
    sumy = 0
    sumxy = 0
    sumxsq = 0
    sumysq = 0
    
    ! Calculate the sums
    DO i=1, npairs, 1
       sumx = sumx + arr(i, 1)
       sumxy = sumxy + arr(i, 1)*arr(i, 2)
       sumy = sumy + arr(i, 2)
       sumxsq = sumxsq + (arr(i, 1))**2
       sumysq = sumysq + (arr(i, 2))**2
    END DO
    
    ! Using the sums and number of pairs, find the averages
    xbar = sumx/REAL(npairs)
    ybar = sumy/REAL(npairs)
   
    ! Use the formulas to find fit values
    m = (sumxy - (sumx)*ybar)/(sumxsq - (sumx)*xbar)
    b = ybar - m*xbar
    numer = REAL(npairs)*sumxy - sumx*sumy
    denom = SQRT((REAL(npairs)*sumxsq-(sumx)**2)*(REAL(npairs)*sumysq-sumy**2))
    r = numer/denom
    
    ! Write out results of fitting
    WRITE(*, 1000) npairs, sumx, sumy, xbar,ybar, sumxy, sumxsq, sumysq, m, b, r
    1000 FORMAT("STATISTICS SUMMARY:",/,"=====================================",/,&
                &"Number of Coordinate Pairs = ", T30, I5, /,&
                &"Sum of X Values = ", T30, F20.5, /,&
                &"Sum of Y Values = ", T30, F20.5, /,&
                &"Average X Value = ", T30, F20.5, /,&
                &"Average Y Value = ", T30, F20.5, /,&
                &"Sum of X*Y Values = ", T30, F20.5, /,&
                &"Sum of X**2 Values = ", T30, F20.5, /,&
                &"Sum of Y**2 Values = ", T30, F20.5, 2/,&
                &"Your linear model is y = ", T30, F10.5"x + ",T50, F10.5,/&
                &"With an r = ", T30, F10.5)
    
    RETURN
END SUBROUTINE corr_coeff

! END MODULE
! ======================

END MODULE cross_corr


