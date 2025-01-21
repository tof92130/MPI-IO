
program mpi_io_write_with_indices
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use iso_c_binding, only: c_loc,C_NEW_LINE,c_ptr,c_null_char
  use iso_fortran_env
  use mpi
  use mpiio
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  implicit none
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer(int32)                     :: dim,dimGlob
  integer(int32)           , pointer :: dimRank(:)
  integer(int32)                     :: i,rank,size,unit,iErr
  integer(int32)                     :: char_size,int_size,file_size
  integer(int32)                     :: iRank
  integer(MPI_OFFSET_KIND)           :: offset
  integer(int32)                     :: filetype
  integer(int32)                     :: statut(MPI_STATUS_SIZE)
  integer(int32)                     :: comm
  character(len=:)         , pointer :: header=>null()
  character(1)                       :: lf
  integer(int32)           , pointer :: valeurs(:)
  integer(int32)           , pointer :: indices(:)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  offset = 0
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_init(iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  comm=MPI_COMM_WORLD
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_comm_rank(comm,rank,iErr)
  call mpi_comm_size(comm,size,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  allocate(character(len=64) :: header)
  lf=C_NEW_LINE
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_type_size(mpi_character,char_size,iErr)
  call mpi_type_size(mpi_integer  , int_size,iErr)
  if( rank==0 )then
    print '(/"char_size (octets)=",i1)',char_size
    print '( "int_size  (octets)=",i1)',int_size
  endif
  call mpi_barrier(comm,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Définition des données
  dim=1+rank
  
  allocate(indices(1:dim)) ; indices(1:dim)=[(1+ rank + size*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
  allocate(valeurs(1:dim)) ; valeurs(1:dim)=[(1+ rank + size*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
  
  !> Calcul de dimGlob
  call mpi_allreduce(dim,dimGlob,1,mpi_integer,mpi_sum,comm,ierr)  
  
  do iRank=0,size-1
    if( iRank==0.and.rank==0 )print '(/"Chaque process connait sa dimension et ses valeurs")'
    if( iRank==rank )then
      print '("rank ",i3,2x,"dim=",i3,2x,"indices ",*(i4,1x))',rank,dim,indices(1:dim) !> format norme fortran 2008
    endif
    call mpi_barrier(comm,iErr)
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ouvrir le fichier en mode collectif
  iErr=mpiio_open_write(comm=comm,name="donnees.dat",unit=unit)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  write(header,'("dim=",i0)')dimGlob ; header(64:64)=lf
  !write(header,'("dim=",i0,a)')dimGlob,lf
  
  if( rank==0 )then
    print '(/"rank ",i3," ecrit dimGlob=",i10," avec un offset en octets= ",i10)',rank,dimGlob,offset
    !print '(a)',trim(header(1:63))
    
    call mpi_file_write_at(     &
    &    unit                  ,&
    &    offset                ,&  !> on retrouve ici l'offset
   !&    c_loc(header)         ,&  !> le tableau à écrire     
   !&    len(header)*char_size ,&  !> le nombre d'éléments    
   !&    mpi_byte              ,&  !> le type d'éléments      
    &        header            ,&  !> le tableau à écrire     
    &    len(header)           ,&  !> le nombre d'éléments    
    &    mpi_character         ,&  !> le type d'éléments      
    &    statut                ,&
    &    iErr                   )
    
  endif
  
  offset = len(header)*char_size
  call mpi_barrier(comm,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  iErr=mpiio_write_cptr_with_indx(comm=comm, unit=unit, offset=offset, indx=indices, data=c_loc(valeurs), nBytes=int_size)
  !iErr=mpiio_write_with_indx(comm=comm, unit=unit, offset=offset, indx=indices, data=c_loc(valeurs), nBytes=int_size)
  !iErr=mpiio_write_with_indx(comm=comm, unit=unit, offset=offset, indx=indices, data=valeurs)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  ! Fermer le fichier
  iErr=mpiio_close(unit)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  ! Libérer les types dérivés et la mémoire
  deallocate(valeurs, indices)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call MPI_FINALIZE(iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  
end program mpi_io_write_with_indices