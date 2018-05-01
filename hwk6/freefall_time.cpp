/*
* File: freefall_time.cpp
* Author: Alec Wills 
* Date: 04/04/18
* Description: To calculate the freefall time of an object starting at a given
	       distance d.
*/


#include <iostream> // Input and output
#include <cmath> // For the square root

// Constant used in the program, not altered so can be outside function
const double ACCEL_GRAV = 9.8;

int main() {
  double dist, time;
  
  // Prompt user for the initial distance
  std::cout << "What is the initial distance (in meters) from which the object falls? ";
  std::cin >> dist;
  
  // Calculate freefall time
  time = std::sqrt( (2.0)*(dist/ACCEL_GRAV) );

  // Print the freefall time
  std::cout << "For a given initial distance " << dist << " m, \n";
  std::cout << "the freefall time is " << time << " seconds." << std::endl;
  
return 0;
}
