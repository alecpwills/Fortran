SHELL = /bin/sh
CC    = mpicc
FC    = mpifort
LIBS  = 

all: all-redirect

PROGRAMS = nbodypipe

nbodypipe_SOURCES = nbodypipe.c
nbodypipe_OBJECTS = nbodypipe.o

SOURCES = ${nbodypipe_SOURCES}

all-redirect: ${PROGRAMS}

clean:
	rm -f *.o ${PROGRAMS} *~ PI*

.SUFFIXES:
.SUFFIXES: .o .c .f .f90
.c:
	${CC} ${CFLAGS} -o $* $< ${LIBS}
.c.o:
	${CC} ${CFLAGS} -c $<
.f:
	${FC} ${FCFLAGS} -o $* $< ${LIBS}
.f.o:
	${FC} ${FCFLAGS} -c $<

sources:
	@echo ${SOURCES}
