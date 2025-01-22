!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!> Cell.f90
!> Christophe Peyret
!> Created on Tue Jan 21  2025
!> Copyright (c) 1995-2025 ONERA/DAAA. All rights reserved.
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


!>  function     mpiio_open_read             (comm,unit,name)                         result(iErr)
!>  function     mpiio_open_write            (comm,unit,name)                         result(iErr)
!>  function     mpiio_open_write            (comm,unit,name)                         result(iErr)
!>  function     mpiio_write_cptr_with_indx  (comm, unit, offset, indx, data, nBytes) result(iErr)
!>  function     mpiio_write_int_with_indx   (comm, unit, offset, indx, data)         result(iErr)
!>  function     mpiio_write_double_with_indx(comm, unit, offset, indx, data)         result(iErr)
!>  function     mpiio_write_int             (comm, unit, offset, data)               result(iErr)
!>  function     mpiio_write_cptr            (comm, unit, offset, data, nBytes)       result(iErr)

module mpiio
  use iso_fortran_env
  use iso_c_binding, only: c_ptr
  use mpi
  implicit none
  
  interface          mpiio_write_with_indx
    module procedure mpiio_write_cptr_with_indx
    module procedure mpiio_write_int_with_indx
  end interface
  
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
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    if( .not.iErr==mpi_success )then
      print *, 'Erreur durant ouverture du fichier'
      call mpi_abort(mpi_comm_world, 2, iErr)
      call mpi_finalize(iErr)
    endif
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
  

  function     mpiio_global_write_string(comm, unit, offset, string) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    character(*)            , intent(in)    :: string
    integer(int32)                          :: iErr
    !>
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: rankMPI,iRank
    integer(int32)                          :: iErr1
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Ecriture des blocs de données
    call mpi_comm_rank(comm,rankMPI,iErr1)
    
    if( rankMPI==0 )then
      print '("string:      ",a )',    string
      print '("len(string): ",i0)',len(string)
      call mpi_file_write_at( &
      &    unit              ,&
      &    offset            ,&  !> on retrouve ici l'offset
      &    string            ,&  !> le tableau à écrire     
      &    len(string)       ,&  !> le nombre d'éléments    
      &    mpi_character     ,&  !> le type d'éléments      
      &    statut            ,&
      &    iErr               )
    endif
    
    call MPI_BCAST(iErr,1,MPI_INTEGER,0,comm,ierr1)
    
    offset=offset+len(string)  !*char_size=1 ! Décalage
    call mpi_barrier(comm,ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_global_write_string

  function     mpiio_global_write_cptr(comm, unit, offset, data_cptr, data_size) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    type(c_ptr)             , intent(in)    :: data_cptr
    integer(int64)                          :: data_size
    integer(int32)                          :: iErr
    !>
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: rankMPI,iRank
    integer(int32)                          :: iErr1
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Ecriture des blocs de données
    call mpi_comm_rank(comm,rankMPI,iErr1)
    
    if( rankMPI==0 )then
      print'("data_size=",i0)',data_size
      call mpi_file_write_at( &
      &    unit              ,&
      &    offset            ,&  !> on retrouve ici l'offset
      &    data_cptr          ,&  !> le tableau à écrire     
      &    int(data_size,kind=int32) ,&  !> le nombre d'éléments    
      &    mpi_byte          ,&  !> le type d'éléments      
      &    statut            ,&
      &    iErr               )
    endif
    
    call MPI_BCAST(iErr, 1, MPI_INTEGER, 0, comm, ierr1)
    
    offset=offset+data_size  !*char_size=1 ! Décalage
    call mpi_barrier(comm,ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_global_write_cptr
  
  function     mpiio_write_cptr_with_indx(comm, unit, offset, indx, data_cptr, data_size) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int32)          , intent(in)    :: indx(:)
    type(c_ptr)             , intent(in)    :: data_cptr(:)
    integer(int64)          , intent(in)    :: data_size
    !>
    integer(int32)                          :: data_size_glob
    integer(int32)                          :: dim
    integer(int32)                          :: filetype
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    dim=size(indx)
    
    call mpi_allreduce(data_size,data_size_glob,1,mpi_integer8,mpi_sum,comm,ierr)  
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
    &    data_cptr                     ,&
    &    int(data_size,kind=int32)     ,&
    &    MPI_BYTE                      ,&
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    
    offset=offset+data_size_glob
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
    integer(int32)          , intent(in)    :: data(:)
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
  
  function     mpiio_write_cptr(comm, unit, offset, data_cptr, data_size) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    type(c_ptr)             , intent(in)    :: data_cptr(:)
    integer(int64)          , intent(in)    :: data_size
    !>
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: sizeMPI,rankMPI,iRank
    integer(int64)          , pointer       :: data_size_Rank(:)
    integer(int32)                          :: filetype
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    
    allocate(data_size_Rank(0:sizeMPI-1))
    
    call mpi_allgather(                   &
    &    data_size        ,1,mpi_integer8,&
    &    data_size_Rank(0),1,mpi_integer8,&
    &    comm                            ,&
    &    iErr                             )
    
    do iRank=0,rankMPI-1
      offset=offset+data_size_Rank(iRank)
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    call MPI_FILE_write_at_ALL(     &  !> Colleltives
    &    unit                      ,&
    &    offset                    ,&  !> on retrouve ici l'offset
    &    data_cptr                 ,&  !> le tableau à écrire     
    &    int(data_size,kind=int32) ,&  !> le nombre d'éléments    
    &    MPI_BYTE                  ,&  !> le type d'éléments      
    &    statut                    ,&
    &    iErr                       )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    do iRank=rankMPI,sizeMPI-1
      offset=offset+data_size_Rank(iRank)
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(data_size_Rank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    return
  end function mpiio_write_cptr
  
end module mpiio
