
program mpi_io_write_with_indices
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use iso_c_binding, only: c_loc,C_NEW_LINE,c_ptr,c_null_char
  use iso_fortran_env
  use mpi
  use space_mpiio
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  implicit none
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer(int32)                     :: dim,dimGlob
  integer(int32)                     :: i,rank,size,unit,iErr
  integer(int32)                     :: aint_size,char_size,int_size,file_size
  integer(int32)                     :: iRank
  integer(MPI_OFFSET_KIND)           :: offset
  integer(int32)                     :: comm
  character(len=:)         , pointer :: header=>null()
  character(1)                       :: lf
  integer(int64)           , pointer :: indices(:)
  character(128)                     :: buffer
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
  call mpi_type_size(mpi_integer  ,int_size ,iErr)
  call mpi_type_size(mpi_aint     ,aint_size,iErr)
  
  if( rank==0 )then
    print '(/"char_size (octets)=",i0)',char_size
    print '( "int32             =",i0)',int32       
    print '( "int_size  (octets)=",i0)',int_size
    print '( "int64             =",i0)',int64       
    print '( "aint_size (octets)=",i0)',aint_size
    print '( "MPI_OFFSET_KIND   =",i0)',MPI_OFFSET_KIND
  endif
  call mpi_barrier(comm,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Définition des données
  dim=1+rank
  allocate(indices(1:dim)) ; indices(1:dim)=[(1+ rank + size*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
  
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
  offset=0_MPI_OFFSET_KIND
  
  write(buffer,'("rank ",i3.3,1x,"octets ecrit: ",i0)')rank,offset
  iErr=mpiio_message(comm=comm, buffer=buffer)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> header
  write(header,'("dim=",i0)')dimGlob ; header(64:64)=lf
  iErr=mpiio_global_write(comm=comm, unit=unit, offset=offset, data=header)
  write(buffer,'("rank ",i3.3,1x,"octets ecrit: ",i0)')rank,offset
  iErr=mpiio_message(comm=comm, buffer=buffer)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ecriture des données int32 indexées
  block
    integer(int32), pointer :: valeurs(:)
    allocate(valeurs(1:dim)) ; valeurs(1:dim)=[(1+ rank + size*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
        
    iErr=mpiio_write_with_indx(comm=comm, unit=unit, offset=offset, data_indx=indices, data=valeurs)
    !iErr=mpiio_write_with_indx_cptr(comm=comm, unit=unit, offset=offset, data_indx=indices, data_cptr=c_loc(valeurs), data_size=sizeof(valeurs))
    
    deallocate(valeurs)

    write(buffer,'("rank ",i3.3,1x,"Ecriture des données int32 indexées",t100,"octets écrits: ",i0)')rank,offset
    iErr=mpiio_message(comm=comm, buffer=buffer)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ecriture des données real64 indexées
  block
    real(real64), pointer :: valeurs(:)
    allocate(valeurs(1:dim)) ; valeurs(1:dim)=[(1+ rank + size*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
    
    iErr=mpiio_write_with_indx(comm=comm, unit=unit, offset=offset, data_indx=indices, data=valeurs)
    
    deallocate(valeurs)
    
    write(buffer,'("rank ",i3.3,1x,"Ecriture des données real64 indexées",t100,"octets écrits: ",i0)')rank,offset
    iErr=mpiio_message(comm=comm, buffer=buffer)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ecriture des données character indexées
  block
    character(80) , pointer :: valeurs(:)
    allocate(valeurs(1:dim))
    do iRank=0,dim-1
      write(valeurs(iRank+1),'("rank",i3.3," valeur=""",i3.3,"""")')rank,1+ rank + size*iRank-iRank*(iRank+1)/2
    enddo    
    valeurs(:)(80:80)=lf
    
    iErr=mpiio_write_with_indx(comm=comm, unit=unit, offset=offset, data_indx=indices, data=valeurs)
    
    deallocate(valeurs)
    
    write(buffer,'("rank ",i3.3,1x,"Ecriture des données character indexées",t100,"octets écrits: ",i0)')rank,offset
    iErr=mpiio_message(comm=comm, buffer=buffer)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  !> Fermeture du fichier
  iErr=mpiio_close(unit=unit)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  !> Nettoyage mémoire
  deallocate(indices)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call MPI_FINALIZE(iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  
end program mpi_io_write_with_indices