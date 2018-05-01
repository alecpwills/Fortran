! File: hwk1_decay.f08
! Author: Alec Wills
! Date: 01/31/18
! Purpose: PROGRAM PURPOSE HERE

! Begin program:
PROGRAM decay

! Require explicit variable declaration
IMPLICIT NONE


! VARIABLE DICTIONARY
! ======================
REAL :: mass_init       ! Initial mass
REAL :: half_life       ! Decay half-life
REAL :: t_elapsed       ! Elapsed time
REAL :: decay_con       ! Decay constant, to relate half-life to the e exponent
REAL :: mass_left       ! Remaining mass


! EXECUTION OF PROGRAM
! ======================

! Prompt user for the input parameters
WRITE(*,*) "What is the (real) inital mass of your sample in grams?"
READ(*,*) mass_init

WRITE(*,*) "What is the (real) half-life of the radioactive substance&
           & in seconds?"
READ(*,*) half_life

WRITE(*,*) "How much (real) time (in seconds) has elapsed?"
READ(*,*) t_elapsed

! Echo back parameter inputs
WRITE(*,*) "You have designated that your substance initially had a &
           &mass of", mass_init, "grams with a half-life of",&
           half_life, "seconds. You indicated", t_elapsed, "seconds&
           & have passed."

! Calculate the decay constant so we can use EXP() intrinsic function
! Instead of using (1/2)**(real power)

decay_con = LOG(2.0)/half_life

! Report the decay constant to the user
WRITE(*,*) "For the designated half-life, the decay constant for exponential&
           & decay is", decay_con, "inverse seconds."

! Calculate the mass of the remaining sample, according to the
! exponential decay 
mass_left = mass_init*EXP( -decay_con*t_elapsed )

! Print the resulting mass.
WRITE(*,*) "With the above parameters, your sample has", mass_left,&
           &"grams remaining."

! END PROGRAM
! ======================

STOP 0
END PROGRAM decay
