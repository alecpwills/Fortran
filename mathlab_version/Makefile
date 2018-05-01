MPI=mpif90
GF=gfortran
# Used on local machine just for the logging functions
#MPE=/home/alec/usr/mpe/bin/mpefc

all: initcond_mod.o mpi_funcs_mod.o jiterations_mod.o xmpi_poisson

jiterations_mod.o: jiterations_mod.f08
	$(MPI) -c jiterations_mod.f08

mpi_funcs_mod.o: mpi_funcs_mod.f08
	$(MPI) -c mpi_funcs_mod.f08

initcond_mod.o: initcond_mod.f08
	$(GF) -c initcond_mod.f08

mpi_poisson.o: mpi_poisson.f08
	$(MPI) -c mpi_poisson.f08

xmpi_poisson: mpi_poisson.o initcond_mod.o mpi_funcs_mod.o jiterations_mod.o
	$(MPI) -o xmpi_poisson mpi_poisson.o initcond_mod.o mpi_funcs_mod.o jiterations_mod.o

clean:
	rm -rf *.o *.mod 

clobber:
	rm -rf *.o *.mod xmpi_poisson *.dat *.clog2 *.slog2
