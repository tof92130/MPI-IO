#! /bin/bash

$MPI_FC -O0 -g -check all -fpe0 -warn -traceback -debug extended -c mpiio.F90

$MPI_FC -O0 -g -check all -fpe0 -warn -traceback -debug extended -c write_indirection.f90
$MPI_FC -o write_indirection mpiio.o write_indirection.o 

$MPI_FC -O0 -g -check all -fpe0 -warn -traceback -debug extended -c write.f90
$MPI_FC -o write mpiio.o write.o 

$MPI_FC -O0 -g -check all -fpe0 -warn -traceback -debug extended -c read.f90
$MPI_FC -o read mpiio.o read.o 

$FC -o read_sequentiel read_sequentiel.f90
