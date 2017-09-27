# MPI-IO

compilation:

mpif90 write.90 -o write
mpif90 read.90  -o read

mpiifort write.90 -o write
mpiifort read.90  -o read

execution
mpirun -n 10 ./write
mpirun -n  4 ./read
