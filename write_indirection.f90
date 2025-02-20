
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
  integer(int32)                     :: rankMPI,sizeMPI,unit,iErr
  integer(int32)                     :: aint_size,char_size,int_size
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
  call mpi_comm_rank(comm,rankMPI,iErr)
  call mpi_comm_size(comm,sizeMPI,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  allocate(character(len=64) :: header)
  lf=C_NEW_LINE
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_type_size(mpi_character,char_size,iErr)
  call mpi_type_size(mpi_integer  ,int_size ,iErr)
  call mpi_type_size(mpi_aint     ,aint_size,iErr)
  
  if( rankMPI==0 )then
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
  dim=1+rankMPI
  allocate(indices(1:dim)) ; indices(1:dim)=[(1+ rankMPI + sizeMPI*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
  
  !> Calcul de dimGlob
  call mpi_allreduce(dim,dimGlob,1,mpi_integer,mpi_sum,comm,ierr)  
  
  do iRank=0,sizeMPI-1
    if( iRank==0.and.rankMPI==0 )print '(/"Chaque process connait sa dimension et ses valeurs")'
    if( iRank==rankMPI )then
      print '("rankMPI ",i3,2x,"dim=",i3,2x,"indices ",*(i4,1x))',rankMPI,dim,indices(1:dim) !> format norme fortran 2008
    endif
    call mpi_barrier(comm,iErr)
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ouvrir le fichier en mode collectif
  iErr=mpiio_open_write(comm=comm,name="donnees.dat",unit=unit)
  offset=0_MPI_OFFSET_KIND
  
  write(buffer,'("rankMPI ",i3.3,1x,"octets ecrit: ",i0)')rankMPI,offset
  iErr=mpiio_message(comm=comm, buffer=buffer)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> header
  write(header,'("dim=",i0)')dimGlob ; header(64:64)=lf
  iErr=mpiio_global_write(comm=comm, unit=unit, offset=offset, data=header)
  write(buffer,'("rankMPI ",i3.3,1x,"octets ecrit: ",i0)')rankMPI,offset
  iErr=mpiio_message(comm=comm, buffer=buffer)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ecriture des données int32 indexées
  block
    integer(int32), pointer :: valeurs(:)
    allocate(valeurs(1:dim)) ; valeurs(1:dim)=[(1+ rankMPI + sizeMPI*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
    
    iErr=mpiio_write_with_indx(comm=comm, unit=unit, offset=offset, data_indx=indices, stride=1, data=valeurs)
    !iErr=mpiio_write_with_indx_cptr(comm=comm, unit=unit, offset=offset, data_indx=indices, data_cptr=c_loc(valeurs), data_size=sizeof(valeurs))
    
    deallocate(valeurs)

    write(buffer,'("rankMPI ",i3.3,1x,"Ecriture des données int32 indexées",t100,"octets écrits: ",i0)')rankMPI,offset
    iErr=mpiio_message(comm=comm, buffer=buffer)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ecriture des données real64 indexées
  block
    use iso_c_binding, only: c_loc,c_f_pointer
    real(real64)  , pointer :: valeurs    (:)
    
    integer(int32)          :: i
    real(real64)  , pointer :: valeursBloc(:)
    real(real64)            :: t0

    allocate(valeurs(1:dim)) ; valeurs(1:dim)=[(1+ rankMPI + sizeMPI*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
    
    iErr=mpiio_write_with_indx(comm=comm, unit=unit, offset=offset, data_indx=indices, stride=1, data=valeurs)
    write(buffer,'("rankMPI ",i3.3,1x,"Ecriture des données real64 indexées",t100,"octets écrits: ",i0)')rankMPI,offset
    iErr=mpiio_message(comm=comm, buffer=buffer)
    
    !>>> TEST
    t0=mpiio_part2block_real64(comm=comm, data_indx=indices, stride=1, data=valeurs, dataBloc=valeursBloc)
   
    write(buffer,'("rankMPI ",i3.3,1x,"TEST Rangement par bloc des données real64 indexées",t100,"t0: ",e12.5)')rankMPI,t0
    iErr=mpiio_message(comm=comm, buffer=buffer)
    
    do iRank=0,sizeMPI-1
      if( iRank==rankMPI )then
        print '("rankMPI",i3)',rankMPI
        do i=1,size(valeursBloc)
          print '(3x,"valeursBloc(:,",i0")=",*(f4.0,1x))',i,valeursBloc(i)
        enddo
      endif
      call mpi_barrier(comm,iErr)
    enddo  
    !<<< TEST
    
    deallocate(valeurs) ; valeurs=>null()
    deallocate(valeursBloc)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ecriture des données real64 indexées
  block
    complex(real64), pointer :: valeurs(:)
    allocate(valeurs(1:dim))
    valeurs(1:dim)%re=[(1+ rankMPI + sizeMPI*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
    valeurs(1:dim)%im=[(1+ rankMPI + sizeMPI*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
    
    iErr=mpiio_write_with_indx(comm=comm, unit=unit, offset=offset, data_indx=indices, stride=1, data=valeurs)
    
    deallocate(valeurs)
    
    write(buffer,'("rankMPI ",i3.3,1x,"Ecriture des données complex128 indexées",t100,"octets écrits: ",i0)')rankMPI,offset
    iErr=mpiio_message(comm=comm, buffer=buffer)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ecriture des données character indexées
  block
    character(80) , pointer :: valeurs(:)
    allocate(valeurs(1:dim))
    do iRank=0,dim-1
      write(valeurs(iRank+1),'("rankMPI",i3.3," valeur=""",i5.5,"""")')rankMPI,1+ rankMPI + sizeMPI*iRank-iRank*(iRank+1)/2
    enddo    
    valeurs(:)(80:80)=lf
    
    iErr=mpiio_write_with_indx(comm=comm, unit=unit, offset=offset, data_indx=indices, stride=80, data=valeurs)
    
    deallocate(valeurs)
    
    write(buffer,'("rankMPI ",i3.3,1x,"Ecriture des données character indexées",t100,"octets écrits: ",i0)')rankMPI,offset
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