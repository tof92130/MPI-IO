program write_at
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use iso_c_binding, only: c_loc,C_NEW_LINE
  use iso_fortran_env
  use mpi
  use space_mpiio
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
  !block
  !  integer(int32) :: i
  !  character(48)  :: buffer(10)
  !  do i=1,10
  !    write(buffer(i),'("rank:",i3.3," ligne: ",i2.2)')rank,i
  !  enddo
  !  buffer(:)(48:48)=C_NEW_LINE
  !  iErr=mpiio_global_write(comm=comm, unit=unit, offset=offset, string=buffer)    
  !end block
  !
  !iErr=mpiio_close(unit)
  !call mpi_finalize(iErr)
  !stop
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  allocate(character(len=64) :: header)
  lf=C_NEW_LINE
  write(header,'("dim=",i0)')dimGlob ; header(64:64)=lf  
  
  iErr=mpiio_global_write(comm=comm, unit=unit, offset=offset, string=header)    
  
  deallocate(header)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ecriture des blocs de données int32
  block
    integer(int32)  , pointer  :: valeurs(:)
    allocate(valeurs(1:dim))
    valeurs(1:dim)=[(100*rank+iRank,iRank=1,dim)]  
    
    do iRank=0,sizeMPI-1
      if( iRank==0.and.rank==0 )print '(/"Chaque process connait sa dimension et ses valeurs")'
      if( iRank==rank )then
        print '("rank ",i3,2x,"dim=",i3,2x,"valeurs ",*(i4,1x))',rank,dim,valeurs(1:dim) !> format norme fortran 2008
      endif
      call mpi_barrier(comm,iErr)
    enddo
    
    iErr=mpiio_write_block(comm=comm, unit=unit, offset=offset, data=valeurs)
    
    deallocate(valeurs)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ecriture des blocs de données real64
  block
    real(real64), pointer  :: valeurs(:)
    allocate(valeurs(1:dim))
    valeurs(1:dim)=[(100*rank+iRank,iRank=1,dim)]  
    
    do iRank=0,sizeMPI-1
      if( iRank==0.and.rank==0 )print '(/"Chaque process connait sa dimension et ses valeurs")'
      if( iRank==rank )then
        print '("rank ",i3,2x,"dim=",i3,2x,"valeurs ",*(f4.0,1x))',rank,dim,valeurs(1:dim) !> format norme fortran 2008
      endif
      call mpi_barrier(comm,iErr)
    enddo
    
    iErr=mpiio_write_block(comm=comm, unit=unit, offset=offset, data=valeurs)

    deallocate(valeurs)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Ecriture des blocs de données string
  block
    integer(int32)           :: i
    character(80) , pointer  :: valeurs(:)
    allocate(valeurs(1:dim))    
    do i=1,dim
      write(valeurs(i),'("rank:",i3.3," valeurs=""",i3.3,"""")')rank,100*rank+i  
    enddo
    valeurs(:)(80:80)=lf
    
    do iRank=0,sizeMPI-1
      if( iRank==0.and.rank==0 )print '(/"Chaque process connait sa dimension et ses valeurs")'
      if( iRank==rank )then
        do i=1,dim
          print '(a)',valeurs(i)(1:79)
        enddo
      endif
      call mpi_barrier(comm,iErr)
    enddo
    
    iErr=mpiio_write_block(comm=comm, unit=unit, offset=offset, data=valeurs)
  
    deallocate(valeurs)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Fermeture du fichier
  iErr=mpiio_close(unit)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Nettoyage mémoire
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_finalize(iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
end program write_at
