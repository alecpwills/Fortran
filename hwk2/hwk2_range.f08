! File: hwk2_range.f08
! Author: Alec Wills
! Date: 02/07/18
! Purpose: To calculate the horizontal range of a projectile (assuming no air
!          resistance), given an initial velocity and an angle of fire.
!          TESTED ON:
!          (init_vel, init_ang) ------> range
!          (9.0, 30.0)    ------------> 7.15796185
!          (9.0, 45.0)    ------------> 8.26530552
!          (9.0, 60.0)    ------------> 7.15797186


! Begin program:
PROGRAM proj_range

! Require explicit variable declaration
IMPLICIT NONE


! VARIABLE DICTIONARY
! ======================
REAL, PARAMETER :: g = 9.8      ! Define gravitational constant as required
REAL, PARAMETER :: PI = 3.14159 ! Define Pi to be used in angle conversion
REAL :: init_vel                ! Initialize velocity parameter
REAL :: init_ang                ! Initialize angle parameter
REAL :: init_ang_r              ! Initialize the angle converted to radians
REAL :: range                   ! Initialize the range

! EXECUTION OF PROGRAM
! ======================

! Ask for the given inputs: initial velocity and angle of fire
WRITE(*,*) "Please input the (real) initial velocity in m/s and the (real) angle of fire in degrees:"
READ(*,*) init_vel, init_ang

! Do not allow angles less than zero
IF (init_ang < 0) THEN
      WRITE(*,*) "Error: The input angle is negative.&
                 & The projectile will fire at the ground.&
                 & Failsafe mechanism engaged."
      range = 0.0

! Do not allow angles greater than 90 degrees
ELSE IF (init_ang > 90) THEN
      WRITE(*,*) "Error: The input angle is greater than 90 degrees.&
                 & The projectile might fire at the user.&
                 & Failsafe mechanism engaged."
      range = 0.0

! Proper angle input
ELSE
      ! Convert the given degree angle to radians
      init_ang_r = PI*init_ang/180.0     ! There are PI/180 radians per deg

      ! Report the conversion to the user
      WRITE(*,*) "An initial angle of", init_ang, "degrees is equivalent to",&
                 & init_ang_r, "radians."

      ! Calculate the range given the equation and the converted angle.

      range = (init_vel**2)*SIN(2*init_ang_r)/g

      ! Echo back the input parameters and answer
      WRITE(*,*) "For an initial velocity of", init_vel,&
                 &"m/s  and an angle of fire", init_ang, "&
                 &degrees, your projectile has a horizontal range of:&
                 & ", range, "meters."
END IF

! END PROGRAM
! ======================

STOP 0
END PROGRAM proj_range
