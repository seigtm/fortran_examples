FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto

all:
	$(FC) $(FFLAGS) -o bin/app ./src/main.f90

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
