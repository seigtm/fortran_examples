FC=gfortran-9
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto -g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan
FOPT=
#FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/list_io.f90 -J obj/ -o obj/list_io.o
	$(FC) $(FFLAGS) $(FOPT) -c src/list_process.f90 -J obj/ -o obj/list_process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/environment.o ./obj/list_io.o ./obj/list_process.o ./obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt
