#! /bin/bash

FC_FLAGS="-O0 -g -check all -fpe0 -warn -traceback -debug"
FC_FLAGDS="-O0 -g -Wextra -Warray-temporaries -Wconversion  -Wall -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan"

#FC_FLAGDS="-O3 -g"

$MPI_FC $FC_FLAGDS -c mpiio.F90

$MPI_FC $FC_FLAGDS -c write_indirection.f90
$MPI_FC -o write_indirection mpiio.o write_indirection.o 

$MPI_FC $FC_FLAGDS -c write.f90
$MPI_FC -o write mpiio.o write.o 

$MPI_FC $FC_FLAGDS -c read.f90
$MPI_FC -o read mpiio.o read.o 

$FC -o read_sequentiel read_sequentiel.f90
