program write_at
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use iso_c_binding, only: c_loc,C_NEW_LINE
  use iso_fortran_env
  use mpi
  use mpiio
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  implicit none
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer(int32)             :: dim,dimGlob
  integer(int32)             :: i,rank,unit,iErr
  integer(int32)             :: char_size,int_size,file_size
  integer(int32)             :: iRank,sizeMPI
  integer(MPI_OFFSET_KIND)   :: offset
  integer(int32)  , pointer  :: valeurs(:)
  integer(int32)             :: statut(MPI_STATUS_SIZE)
  integer(int32)             :: comm
  character(len=:), pointer  :: header=>null()
  character(1)               :: lf
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_init(iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  comm=MPI_COMM_WORLD
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_comm_rank(comm,rank   ,iErr)
  call mpi_comm_size(comm,sizeMPI,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  allocate(character(len=64) :: header)
  lf=C_NEW_LINE
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_type_size(mpi_character, char_size, ierr)
  call mpi_type_size(mpi_integer  ,  int_size, ierr)
  if( rank==0 )then
    print '(/"char_size (octets)=",i1)',char_size
    print '( "int_size  (octets)=",i1)',int_size
  endif
  call mpi_barrier(comm,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ! Définition des données
  dim=1+rank
  
  allocate(valeurs(1:dim))  
  valeurs(1:dim)=[(100*rank+iRank,iRank=1,dim)]  
  !valeurs(1:dim)=[(1+ rank + sizeMPI*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
  
  do iRank=0,sizeMPI-1
    if( iRank==0.and.rank==0 )print '(/"Chaque process connait sa dimension et ses valeurs")'
    if( iRank==rank )then
      print '("rank ",i3,2x,"dim=",i3,2x,"valeurs ",*(i4,1x))',rank,dim,valeurs(1:dim) !> format norme fortran 2008
    endif
    call mpi_barrier(comm,iErr)
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_allreduce(dim,dimGlob,1,mpi_integer,mpi_sum,comm,ierr)    
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  iErr=mpiio_open_write(comm=comm,name="donnees.dat",unit=unit)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  offset=0_MPI_OFFSET_KIND
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  write(header,'("dim=",i0)')dimGlob ; header(64:64)=lf
  !write(header,'("dim=",i0,a)')dimGlob,lf
  
  if( rank==0 )then
    print '(/"rank ",i3," ecrit dimGlob=",i10," avec un offset en octets= ",i10)',rank,dimGlob,offset
    print '(a)',trim(header(1:63))
    call mpi_file_write_at( &
    &    unit              ,&
    &    offset            ,&  !> on retrouve ici l'offset
    &        trim(header)  ,&  !> le tableau à écrire     
    &    len(trim(header)) ,&  !> le nombre d'éléments    
    &    mpi_character     ,&  !> le type d'éléments      
    &    statut            ,&
    &    iErr               )
  endif
  offset=offset+len(header)*char_size ! Décalage
  call mpi_barrier(comm,ierr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ecriture des blocs de données
  iErr=mpiio_write_int(comm=comm, unit=unit, offset=offset, data=valeurs)
  !iErr=mpiio_write_cptr(comm=comm, unit=unit, offset=offset, data=c_loc(valeurs),nBytes=dim*int_size)  
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  iErr=mpiio_close(unit)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  deallocate(header)
  deallocate(valeurs)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_finalize(iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
end program write_at
