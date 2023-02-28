FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/trapezoidal_rule.f90 -J obj/ -o obj/trapezoidal_rule.o
	$(FC) $(FFLAGS) -c src/trapezoidal_io.f90 -J obj/ -o obj/trapezoidal_io.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/environment.o obj/trapezoidal_rule.o obj/trapezoidal_io.o obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt
