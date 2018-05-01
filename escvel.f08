PROGRAM esc_vel
  ! Program to compute the escape velocity from an object given a
  ! REAL MASS, and a REAL RADIUS.

  ! Force explicit variable declarations.
  IMPLICIT NONE

  ! Define gravitational constant
  REAL, PARAMETER :: G = 6.673E-11 !N(m**-2)(kg**-2)

  ! Define variables to be input by user.
  REAL :: M !mass (kg)
  REAL :: R !radius (m)

  ! Define return variables to print from program
  REAL :: vesc

  ! Ask user to input variables to calculate with
  WRITE(*,*) 'Please input the mass and radius of the object to escape from:'
  READ(*,*) M, R

  ! Calculate escape velocity using Newtonian equation
  ! THERE IS AN ERROR IN THE RECENT FORTRAN EDITION, THE SQRT INCLUDES R
  vesc = SQRT((2*G*M)/R)

  ! Report answer while echoing back input values:
  WRITE(*,*) 'With a mass of', M, 'kg and a radius of', R, 'm, your object has an escape velocity of', vesc, 'm/s.'

  END PROGRAM esc_vel
  
