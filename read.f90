program read_at
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use iso_fortran_env
  use mpi
  use space_mpiio
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  implicit none
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !integer(int32)             :: statut(mpi_status_size)
  !integer(int32)             :: requete
  integer(int32)             :: rank,comm
  integer(int32)             :: iRank,sizeMPI
  integer(int32)             :: iErr, unit
  integer(MPI_OFFSET_KIND)   :: offset
  integer(MPI_OFFSET_KIND)   :: file_size
  integer(int32)             :: char_size,int_size
  !logical                    :: termine
  integer(int32)             :: dim,dimGlob 
  integer(int32)             :: n0,n1
  character(len=:), pointer  :: header=>null()
  integer(int64)   , pointer :: indices(:)
  character(128)             :: buffer
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
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_type_size(mpi_character,char_size,ierr)
  call mpi_type_size(MPI_INTEGER,int_size,iErr)
  
  if( rank==0 )then
    print '(/"char_size (octets)=",i1)',char_size
    print '( "int_size  (octets)=",i1)',int_size
  endif
  call mpi_barrier(comm,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> ouverture du fichier "donnees.dat" en lecture
  iErr=mpiio_open_read(comm=comm,unit=unit,name="donnees.dat")
  offset=0
  call mpi_file_get_size(unit, file_size, iErr)
  
  write(buffer,'("rank ",i3.3,1x,"Ouverture du fichier ",t100,"octets lus: ",i0,"/",i0)')rank,offset,file_size
  iErr=mpiio_message(comm=comm, buffer=buffer)  
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Lecture globale
  
  iErr=mpiio_global_read_string(comm=comm, unit=unit, offset=offset, data=header)
  print *,rank,"header:",header
  read(header(5:63), '(i10)') dimGlob  ! debut en position 5  
  
  write(buffer,'("rank ",i3.3,1x,"Lecture globale Terminée",t100,"octets lus: ",i0,"/",i0)')rank,offset,file_size
  iErr=mpiio_message(comm=comm, buffer=buffer)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  if( 0==1 )then !> lecture entrelacee
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Lecture avec index int32  
    block 
      integer(int32), pointer :: dimRank(:)
      integer(int32), pointer :: valeurs(:)
      integer(int32)          :: i,idx
      
      n0= rank   *dimGlob/sizeMPI+1
      n1=(rank+1)*dimGlob/sizeMPI
      dim=n1-n0+1
      
      allocate(dimRank(0:sizeMPI-1))
      call mpi_allgather(                 &
      &    dim       , 1, mpi_integer    ,&
      &    dimRank(0), 1, mpi_integer    ,&
      &    comm                          ,&
      &    iErr                           )
      
      allocate(indices(1:dim))
      idx=0
      do iRank=0,sizeMPI-1
        do i=1,dimRank(iRank)
          idx=idx+1
          if( iRank==rank )indices(i)=idx
        enddo
      enddo
      deallocate(dimRank)
      
      write(buffer,'("rank ",i3.3,1x,"indices: ",*(i3,1x))')rank,indices(1:dim)
      iErr=mpiio_message(comm=comm, buffer=buffer)  
      
      iErr=mpiio_read_with_index(comm=comm, unit=unit, offset=offset, data_indx=indices, data=valeurs)
      
      write(buffer,'("rank ",i3.3,1x,"valeurs: ",*(i3,1x))')rank,valeurs(:)
      iErr=mpiio_message(comm=comm, buffer=buffer)  
      
      deallocate(indices)
      deallocate(valeurs)
     
      write(buffer,'("rank ",i3.3,1x,"Lecture avec index int32 Terminée",t100,"octets lus: ",i0,"/",i0)')rank,offset,file_size
      iErr=mpiio_message(comm=comm, buffer=buffer)
    end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  else 
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Lecture Block int32  
    block
      integer(int32), pointer :: valeurs(:)
      
      iErr=mpiio_read_block(comm=comm, unit=unit, dimGlob=dimGlob, offset=offset, data=valeurs)
      
      write(buffer,'("rank ",i3.3,1x,"valeurs: ",*(i3,1x))')rank,valeurs(:)
      iErr=mpiio_message(comm=comm, buffer=buffer)
      
      deallocate(valeurs)  
      
      write(buffer,'("rank ",i3.3,1x,"Lecture Block int32 Terminée",t100,"octets lus: ",i0,"/",i0)')rank,offset,file_size
      iErr=mpiio_message(comm=comm, buffer=buffer)
    end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  endif
  
  
  if( 0==0 )then ! lecture entrelacee
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Lecture avec index real64  
    block 
      integer(int32), pointer :: dimRank(:)
      real(real64)  , pointer :: valeurs(:)
      integer(int32)          :: i,idx
      
      n0= rank   *dimGlob/sizeMPI+1
      n1=(rank+1)*dimGlob/sizeMPI
      dim=n1-n0+1
      
      allocate(dimRank(0:sizeMPI-1))
      call mpi_allgather(                 &
      &    dim       , 1, mpi_integer    ,&
      &    dimRank(0), 1, mpi_integer    ,&
      &    comm                          ,&
      &    iErr                           )
      
      allocate(indices(1:dim))
      idx=0
      do iRank=0,sizeMPI-1
        do i=1,dimRank(iRank)
          idx=idx+1
          if( iRank==rank )indices(i)=idx
        enddo
      enddo
      deallocate(dimRank)
      
      !write(buffer,'("rank ",i3.3,1x,"index: ",*(i3,1x))')rank,indices(1:dim)
      !iErr=mpiio_message(comm=comm, buffer=buffer)  
      
      iErr=mpiio_read_with_index(comm=comm, unit=unit, offset=offset, data_indx=indices, data=valeurs)
      
      write(buffer,'("rank ",i3.3,1x,"valeurs: ",*(f4.0,1x))')rank,valeurs(:)
      iErr=mpiio_message(comm=comm, buffer=buffer)  
      
      deallocate(indices)
      deallocate(valeurs)
      write(buffer,'("rank ",i3.3,1x,"Lecture avec index real64 Terminée",t100,"octets lus: ",i0,"/",i0)')rank,offset,file_size
      iErr=mpiio_message(comm=comm, buffer=buffer)
    end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  else
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Lecture Block real64
    block
      real(real64), pointer :: valeurs(:)
      
      iErr=mpiio_read_block(comm=comm, unit=unit, dimGlob=dimGlob, offset=offset, data=valeurs)
      
      !print *,rank,valeurs(:)
      write(buffer,'("rank ",i3.3,1x,"valeurs: ",*(f5.0,1x))')rank,valeurs(:)
      iErr=mpiio_message(comm=comm, buffer=buffer)  
      
      deallocate(valeurs)
      !print *,offset,file_size
      
      write(buffer,'("rank ",i3.3,1x,"Lecture Block real64 Terminée",t100,"octets lus: ",i0,"/",i0)')rank,offset,file_size
      iErr=mpiio_message(comm=comm, buffer=buffer)
    end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  endif


  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !n0= rank   *dimGlob/sizeMPI+1
  !n1=(rank+1)*dimGlob/sizeMPI
  !dim=n1-n0+1
  !
  !do iRank=0,sizeMPI-1
  !  if( iRank==0.and.rank==0    )print '()'
  !  if( iRank==rank )then
  !    print '("rank ",i3,2x,"n0=",i3,2x,"n1=",i3,3x,"dim=",i3)',rank,n0,n1,dim
  !  endif
  !  call mpi_barrier(comm,iErr)
  !enddo
  !
  !allocate(dimRank(0:sizeMPI-1))
  !call mpi_allgather(                 &
  !&    dim       , 1, mpi_integer    ,&
  !&    dimRank(0), 1, mpi_integer    ,&
  !&    comm                          ,&
  !&    iErr                           )
  !
  !if( rank==0 )then
  !  print '()'
  !  do iRank=0,sizeMPI-1
  !    print '("dimRank(",i2,")=",i3,4x,"sum(dimRank(0:",i2,"))=",i3)',iRank,dimRank(iRank),iRank-1,sum(dimRank(0:iRank-1))
  !  enddo
  !endif
  !call mpi_barrier(comm,iErr)
  !
  !do iRank=0,rank-1
  !  offset=offset+(dimRank(iRank)*int_size)
  !enddo  
  !deallocate(dimRank)
  !
  !do iRank=0,sizeMPI-1
  !  if( iRank==0.and.rank==0    )print '()'
  !  if( iRank==rank )then
  !    print '("rank ",i3," offset= ",i10)',rank,offset
  !  endif
  !  call mpi_barrier(comm,iErr)
  !enddo
  !!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !
  !!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !allocate(valeurs(1:dim))
  !
  !  
  !call MPI_FILE_IREAD_AT( & !> lecture non blocante
  !&    unit              ,&
  !&    offset            ,&
  !&    valeurs(1)        ,&
  !&    dim               ,&
  !&    MPI_INTEGER       ,&
  !&    requete           ,&
  !&    iErr               )
  !
  !do iRank=0,sizeMPI-1
  !  call mpi_test(requete,termine,statut,iErr)
  !  if( iRank==0.and.rank==0    )print '()'
  !  if( iRank==rank )then
  !    if( termine )then
  !      print '("rank ",i3," mpi_test lecture terminee")',rank
  !    else
  !      print '("rank ",i3," mpi_test lecture en cours")',rank
  !    endif
  !  endif
  !  call mpi_barrier(comm,iErr)
  !enddo
  !  
  !!call mpi_test(requete,termine,statut,iErr)
  !call mpi_wait(requete,statut,iErr)
  !
  !do iRank=0,sizeMPI-1
  !  if( iRank==0.and.rank==0    )print '()'
  !  if( iRank==rank )then
  !    print '("rank ",i3," mpi_wait lecture terminee")',rank
  !  endif
  !  call mpi_barrier(comm,iErr)
  !enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  iErr=mpiio_close(unit)
  write(buffer,'("rank ",i3.3,1x,"Fermeture du fichier",t100,"octets lus: ",i0,"/",i0)')rank,offset,file_size
  iErr=mpiio_message(comm=comm, buffer=buffer)  
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_finalize(iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
end program read_at


