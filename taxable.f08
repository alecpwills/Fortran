! Purpose: To test case structures by doing 3-10 in Chapman.
! Author:  Alec Wills
! Date:    1/29/18

program taxable
 
! Require explicit variable declaration.
IMPLICIT NONE

! Declare parameters (the Medicare levy)
REAL, PARAMETER :: levy = 0.015

! Declare variables
INTEGER :: income
REAL :: medtax
REAL :: inctax
REAL :: tottax

! Ask for income received
WRITE(*,*) 'Please input the income received:'
READ(*,*) income

! Begin case selection

SELECT CASE (income)

      CASE (0:6000)
         inctax = 0.0
         WRITE(*,*) 'No income tax required in this bracket, for an income of &
         &', income, 'dollars.'
         medtax = levy * income
         WRITE(*,*) 'The Medicare levy amount is:', medtax, 'dollars.'
         tottax = inctax + medtax
         WRITE(*,*) 'The total tax payable is:', tottax, 'dollars.'

      CASE (6001:34000)
         inctax = 0.15*(income-6000.0)
         medtax = levy * income
         tottax = inctax + medtax
         WRITE(*,*) 'For an income of ', income, 'dollars, you owe',&
         &inctax, 'dollars of income tax.'
         WRITE(*,*) 'The Medicare levy amount is:', medtax, 'dollars.'
         WRITE(*,*) 'The total tax payable is:', tottax, 'dollars.'
         
      CASE (34001:80000)
         inctax = 4200.0 + 0.3*(income-34000)
         medtax = levy * income
         tottax = inctax + medtax
         WRITE(*,*) 'For an income of ', income, 'dollars, you owe',&
         &inctax, 'dollars of income tax.'
         WRITE(*,*) 'The Medicare levy amount is:', medtax, 'dollars.'
         WRITE(*,*) 'The total tax payable is:', tottax, 'dollars.'

      CASE (80001:180000)
         inctax = 18000.0 + 0.4*(income-80000)
         medtax = levy * income
         tottax = inctax + medtax
         WRITE(*,*) 'For an income of ', income, 'dollars, you owe',&
         &inctax, 'dollars of income tax.'
         WRITE(*,*) 'The Medicare levy amount is:', medtax, 'dollars.'
         WRITE(*,*) 'The total tax payable is:', tottax, 'dollars.'

      CASE (180001:)
         inctax = 58000.0 + 0.45*(income-180000.0)
         medtax = levy * income
         tottax = inctax + medtax
         WRITE(*,*) 'For an income of ', income, 'dollars, you owe',&
         &inctax, 'dollars of income tax.'
         WRITE(*,*) 'The Medicare levy amount is:', medtax, 'dollars.'
         WRITE(*,*) 'The total tax payable is:', tottax, 'dollars.'
      CASE DEFAULT
         WRITE(*,*) 'Invalid income amount - please input a positive number or&
         &0.'
      END SELECT

STOP 0
END PROGRAM
