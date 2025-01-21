module mpiio
  use iso_fortran_env
  use iso_c_binding, only: c_ptr
  use mpi
  implicit none
  
  interface          mpiio_write_with_indx
    module procedure mpiio_write_cptr_with_indx
    module procedure mpiio_write_int_with_indx
    module procedure mpiio_write_double_with_indx
  end interface mpiio_write_with_indx
  
contains

  function     mpiio_open_read(comm,unit,name) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32), intent(in)  :: comm
    integer(int32), intent(out) :: unit
    character(*)  , intent(in)  :: name
    !>
    integer(int32)              :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_file_open(  &
    &    comm           ,&
    &    trim(name)     ,&
    &    MPI_MODE_RDONLY,&
    &    mpi_info_null  ,&
    &    unit           ,&
    &    iErr            )
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_open_read 
  
  function     mpiio_open_write(comm,unit,name) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32), intent(in)  :: comm
    integer(int32), intent(out) :: unit
    character(*)  , intent(in)  :: name
    !>
    integer(int32)              :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_FILE_OPEN(                    &
    &    comm                             ,&
    &    trim(name)                       ,&
    &    MPI_MODE_CREATE + MPI_MODE_WRONLY,&
    &    MPI_INFO_NULL                    ,&
    &    unit                             ,&
    &    iErr                              )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_open_write 
  
  function     mpiio_close(unit) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32), intent(inout) :: unit
    integer(int32)                :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_FILE_CLOSE(unit, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_close
  
  function     mpiio_write_cptr_with_indx(comm, unit, offset, indx, data, nBytes) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int32)          , intent(in)    :: indx(:)
    type(c_ptr)             , intent(in)    :: data(:)
    integer(int32)          , intent(in)    :: nBytes
    !>
    integer(int32)                          :: dim, dimGlob
    integer(int32)                          :: filetype
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    dim=size(indx)
    
    call mpi_allreduce(dim,dimGlob,1,mpi_integer,mpi_sum,comm,ierr)  
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_TYPE_CREATE_INDEXED_Block( &
    &    dim                           ,&
    &    1                             ,&
    &    indx-1                        ,& ! int32
    &    MPI_INTEGER                   ,&
    &    filetype                      ,&
    &    iErr                           )
        
    call MPI_TYPE_COMMIT(filetype, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définition de la vue globale (chaque processus écrit dans sa position définie par indx)
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& ! deplacement initial
    &    MPI_INTEGER                   ,&
    &    filetype                      ,&
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    
    call MPI_FILE_WRITE_ALL(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim*nBytes                    ,&
    &    MPI_BYTE                      ,&
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    
    offset=offset+dimGlob*nBytes
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
    ! Libérer les types dérivés et la mémoire
    call MPI_TYPE_FREE(filetype, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    
  end function mpiio_write_cptr_with_indx
  
  function     mpiio_write_int_with_indx(comm, unit, offset, indx, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int32)          , intent(in)    :: indx(:)
    integer(int32)          , intent(in)    :: data   (:)
    !>
    integer(int32)                          :: dim, dimGlob
    integer(int32)                          :: filetype
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    dim=size(indx)
    
    call mpi_allreduce(dim,dimGlob,1,mpi_integer,mpi_sum,comm,ierr)  
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_TYPE_CREATE_INDEXED_Block( &
    &    dim                           ,&
    &    1                             ,&
    &    indx-1                        ,& ! int32
    &    MPI_INTEGER                   ,&
    &    filetype                      ,&
    &    iErr                           )
        
    call MPI_TYPE_COMMIT(filetype, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définir la vue globale (chaque processus écrit dans sa position définie par indx)
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& ! deplacement initial
    &    MPI_INTEGER                   ,&
    &    filetype                      ,&
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    
    !> Écriture collective
    call MPI_FILE_WRITE_ALL(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,&
    &    MPI_INTEGER                   ,&
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    
    offset=offset+dimGlob* 4  ! <=
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
    ! Libérer les types dérivés et la mémoire
    call MPI_TYPE_FREE(filetype, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    
  end function mpiio_write_int_with_indx
  
  function     mpiio_write_double_with_indx(comm, unit, offset, indx, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int32)          , intent(in)    :: indx(:)
    real(real64)            , intent(in)    :: data(:)
    !>
    integer(int32)                          :: dim, dimGlob
    integer(int32)                          :: filetype
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    dim=size(indx)
    call mpi_allreduce(dim,dimGlob,1,mpi_integer,mpi_sum,comm,ierr)  
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_TYPE_CREATE_INDEXED_Block( &
    &    dim                           ,&
    &    1                             ,&
    &    indx-1                        ,& ! int32
    &    MPI_INTEGER                   ,&
    &    filetype                      ,&
    &    iErr                           )
    
    call MPI_TYPE_COMMIT(filetype, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définir la vue globale (chaque processus écrit dans sa position définie par indx)
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& ! deplacement initial
    &    MPI_INTEGER                   ,&
    &    filetype                      ,&
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    
    !> Écriture collective
    call MPI_FILE_WRITE_ALL(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,&
    &    MPI_REAL8                     ,&
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    
    offset=offset+dimGlob* 8  ! <=
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
    ! Libérer les types dérivés et la mémoire
    call MPI_TYPE_FREE(filetype, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    
  end function mpiio_write_double_with_indx
  
  function     mpiio_write_int(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int32)          , intent(in)    :: data(:)
    !>
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: sizeMPI,rankMPI,iRank
    integer(int32)                          :: dim,dimGlob
    integer(int32)          , pointer       :: dimRank(:)
    integer(int32)                          :: filetype
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    
    dim=size(data)
    
    allocate(dimRank(0:sizeMPI-1))

    call mpi_allgather(                 &
    &    dim       , 1, mpi_integer    ,&
    &    dimRank(0), 1, mpi_integer    ,&
    &    comm                          ,&
    &    iErr                           )
    
    do iRank=0,rankMPI-1
      offset=offset+(dimRank(iRank)*4) ! <= 4 = int_size
    enddo

    
    call MPI_FILE_write_at_ALL( &  !> Colleltives
    &    unit                  ,&
    &    offset                ,&  !> on retrouve ici l'offset
    &    data                  ,&  !> le tableau à écrire     
    &    dim                   ,&  !> le nombre d'éléments    
    &    MPI_INTEGER           ,&  !> le type d'éléments      
    &    statut                ,&
    &    iErr                   )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    do iRank=rankMPI,sizeMPI-1
      offset=offset+(dimRank(iRank)*4) ! <= 4 = int_size
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
     
    return
  end function mpiio_write_int
  
  function     mpiio_write_cptr(comm, unit, offset, data, nBytes) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    type(c_ptr)             , intent(in)    :: data
    integer(int32)          , intent(in)    :: nBytes
    !>
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: sizeMPI,rankMPI,iRank
    integer(int32)          , pointer       :: nBytesRank(:)
    integer(int32)                          :: filetype
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    
    allocate(nBytesRank(0:sizeMPI-1))
    
    call mpi_allgather(                &
    &    nBytes       , 1, mpi_integer,&
    &    nBytesRank(0), 1, mpi_integer,&
    &    comm                         ,&
    &    iErr                          )
    
    do iRank=0,rankMPI-1
      offset=offset+nBytesRank(iRank) ! <= 4 = int_size
    enddo
        
    call MPI_FILE_write_at_ALL( &  !> Colleltives
    &    unit                  ,&
    &    offset                ,&  !> on retrouve ici l'offset
    &    data                  ,&  !> le tableau à écrire     
    &    nBytes                ,&  !> le nombre d'éléments    
    &    MPI_BYTE              ,&  !> le type d'éléments      
    &    statut                ,&
    &    iErr                   )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    do iRank=rankMPI,sizeMPI-1
      offset=offset+nBytesRank(iRank)
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(nBytesRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    return
  end function mpiio_write_cptr
  
end module mpiio
