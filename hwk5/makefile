# Specify compiler for program:
CC=gfortran

# Specify object building flag
CFLAGS=-c

# Specify targets

all: xrelax

# Build the relaxation program, dependent on the source code and
# the module object file
xrelax: laplacian_mod.o relax.o
	$(CC) -o xrelax relax.o laplacian_mod.o

# Build the relax object file
relax.o: relax.f08
	$(CC) $(CFLAGS) relax.f08

# Build the module object file
laplacian_mod.o: laplacian_mod.f08
	$(CC) $(CFLAGS) laplacian_mod.f08

# Clean the object files and module files
clean:
	rm *o *mod xrelax

