CC=gfortran
CPP=g++
C=-c

all: xtime xpi

xtime: freefall_time.o
	$(CPP) -o xtime freefall_time.o

freefall_time.o: freefall_time.cpp
	$(CPP) $(C) freefall_time.cpp

xpi: random0_mod.o pi_mc.o
	$(CC) -o xpi pi_mc.o random0_mod.o

random0_mod.o: random0_mod.f08
	$(CC) $(C) random0_mod.f08

pi_mc.o: pi_mc.f08
	$(CC) $(C) pi_mc.f08

clean:
	rm -rf *.o *.mod x*
