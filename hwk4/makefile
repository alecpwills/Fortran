# Specify compiler for program:
CC=gfortran

# Specify object building flag
CFLAGS=-c

# Specify targets

all: xcorr xcross

# Build the cross product program, dependent on the source code and
# the module object file
xcross: cross_prod.f08 cross_corr_mod.o
	$(CC) -o xcross cross_prod.f08 cross_corr_mod.o

# Build the correlation program, dependent on the source code and 
# the module object file
xcorr: corr_coeff.f08 cross_corr_mod.o
	$(CC) -o xcorr corr_coeff.f08 cross_corr_mod.o

# Build the module object file
cross_corr_mod.o: cross_corr_mod.f08
	$(CC) $(CFLAGS) cross_corr_mod.f08

