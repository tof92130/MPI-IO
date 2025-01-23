#! /bin/bash

$MPI_FC -c mpiio.f90

$MPI_FC -c write_indirection.f90
$MPI_FC -o write_indirection mpiio.o write_indirection.o 

$MPI_FC -c write.f90
$MPI_FC -o write mpiio.o write.o 

$MPI_FC -c read.f90
$MPI_FC -o read mpiio.o read.o 

$FC -o read_sequentiel read_sequentiel.f90
