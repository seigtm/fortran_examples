FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/matrix_IO.f90 -J obj/ -o obj/matrix_IO.o
	$(FC) $(FFLAGS) $(FOPT) -c src/matrix_process.f90 -J obj/ -o obj/matrix_process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app obj/environment.o obj/matrix_IO.o obj/matrix_process.o obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt
