FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/source_io.f90 -J obj/ -o obj/source_io.o
	$(FC) $(FFLAGS) $(FOPT) -c src/source_process.f90 -J obj/ -o obj/source_process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/environment.o ./obj/source_io.o ./obj/source_process.o ./obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/source.f90.diff
