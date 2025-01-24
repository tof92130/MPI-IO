# MPI-IO

compilation:

mpif90 -c mpiio.f90
mpif90 -c write_indirection.f90
mpif90 -o write_indirection mpiio.o write_indirection.o 

mpif90 -c mpiio.f90
mpif90 -c write.f90
mpif90 -o write mpiio.o write.o 

mpif90 -c mpiio.f90
mpif90 -c read.f90
mpif90 -o read mpiio.o read.o 

mpif90 -o read_sequentiel read_sequentiel.f90

execution:

mpirun -n  6 ./write
mpirun -n  6 ./write_indirection
mpirun -n  4 ./read
./read_sequentiel


xxd donnees.dat
