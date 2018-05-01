PROGRAM temp_conv
! Take a temperature in Fahrenheit and convert it to a temperature in Kelvin.
IMPLICIT NONE
REAL :: tempF
REAL :: tempK
WRITE(*,*) 'Please enter the temperature in Fahrenheit you would like to convert: '
READ(*,*) tempF
tempK = ((5./9.)*tempF - 32.0) + 273.15
WRITE(*,*) 'You have converted ', tempF, 'degrees F to ',  tempK, ' K.'
END PROGRAM temp_conv
