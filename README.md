# MPI-IO

compilation:

mpif90 -O3 write.f90 -o write
mpif90 -O3 read.f90  -o read

mpiifort -O3 write.90 -o write
mpiifort -O3 read.90  -o read

execution
mpirun -n  6 ./write
mpirun -n  4 ./read

 mpif90 -c mpiio.f90
 mpif90 -c write_indirection.f90
 mpif90 -o write_indirection mpiio.o write_indirection.o 

 mpif90 -c mpiio.f90
 mpif90 -c write.f90
 mpif90 -o write mpiio.o write.o 

