# MPI-IO

compilation:

mpif90 -O3 write.f90 -o write
mpif90 -O3 read.f90  -o read

mpiifort -O3 write.90 -o write
mpiifort -O3 read.90  -o read

execution
mpirun -n  6 ./write
mpirun -n  4 ./read
