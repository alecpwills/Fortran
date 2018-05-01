! File: chap6-10n11.f08
! Author: Alec Wills
! Date: 02/28/18
! Purpose: To convert an input vector read in as either rectangular or
!          polar to the vector in the other coordinate system.


MODULE CONVERSIONS              ! Evidently required to have array-returning f
CONTAINS

PURE FUNCTION rect_to_pol(inarr) RESULT(outarr)
REAL, DIMENSION(2), INTENT(IN) :: inarr ! Input array, in (x,y) form
REAL, DIMENSION(2) :: outarr            ! Output array, in (r, theta)

! Generate the radius
outarr(1) = SQRT(inarr(1)**2 + inarr(2)**2)

! Generate the angle
outarr(2) = ATAN2(inarr(2),inarr(1))

END FUNCTION rect_to_pol

PURE FUNCTION pol_to_rect(inarr) RESULT(outarr)
REAL, DIMENSION(2), INTENT(IN) :: inarr ! Input array, in (r,t) form
REAL, DIMENSION(2) :: outarr            ! Output array, in (x,y)

! Generate Euclidean components
outarr(1) = inarr(1)*COS(inarr(2))      ! x component
outarr(2) = inarr(1)*SIN(inarr(2))      ! y component

END FUNCTION pol_to_rect

END MODULE



! Begin program:
PROGRAM rect_polar_conv

USE CONVERSIONS                         ! To get the functions returning arrays


! Require explicit variable declaration
IMPLICIT NONE

! VARIABLE DICTIONARY
! ======================
REAL, DIMENSION(2) :: input_vector      ! Declare input vector as array
REAL, DIMENSION(2) :: output_vector     ! Declare output vector as same
INTEGER :: iiostat, oiostat             ! Error for input
INTEGER :: iLUN, oLUN                   ! Declare I/O LUNs
INTEGER :: conv_case                    ! Declare case selector

! EXECUTION OF PROGRAM
! ======================

! Open input file for vector reading
OPEN(newunit=iLUN,  file="./input.dat", status='OLD', IOSTAT=iiostat)
OPEN(newunit=oLUN, file="./output.dat", status='REPLACE', IOSTAT=oiostat)

! Check for proper opening
IF ((iiostat /= 0) .OR. (oiostat /= 0)) THEN
        WRITE(*,*) "Error opening input or output file."
        STOP 1
END IF

! Prompt for conversion selection
WRITE(*,*) "Please select a conversion type:"
WRITE(*,*) "1 --- Polar vectors to Euclidean vectors"
WRITE(*,*) "2 --- Euclidean vectors to Polar vectors"
READ(*,*) conv_case

! Begin case selection
SELECT CASE (conv_case)
    CASE (1)    ! Polar to Euclidean
        ereadloop: DO WHILE (iiostat == 0)
            READ(iLUN, *, IOSTAT=iiostat) input_vector
            IF (iiostat /= 0) EXIT      ! Because it won't stop after end file
            output_vector = pol_to_rect(input_vector) 
            WRITE(oLUN, 100) input_vector, output_vector
            100 FORMAT (F10.5, T11, F10.5, T22, F10.5, T33, F10.5)
            WRITE(*, 200) input_vector, output_vector
            200 FORMAT ('The input vector', F10.5, 'r + ', F10.5, 't &
                        &converts to', F10.5, 'x + ', F10.5, 'y.')
        END DO ereadloop
    CASE (2)    ! Euclidean to polar
        preadloop: DO WHILE (iiostat == 0)
            READ(iLUN, *, IOSTAT=iiostat) input_vector
            IF (iiostat /= 0) EXIT
            output_vector = rect_to_pol(input_vector) 
            WRITE(oLUN, 100) input_vector, output_vector
            WRITE(*, 300) input_vector, output_vector
            300 FORMAT ('The input vector', F10.5, 'x + ', F10.5, 'y &
                        &converts to', F10.5, 'r + ', F10.5, 't.')
        END DO preadloop
END SELECT

! Close files
CLOSE(unit=iLUN)
CLOSE(unit=oLUN)

! END PROGRAM
! ======================

STOP 0
END PROGRAM rect_polar_conv

! PROCEDURES
! ======================
! ======================

! FUNCTIONS
! ======================

! SUBROUTINES
! ======================
