!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!> mppiio.f90
!> Christophe Peyret
!> Created on Tue Jan 21  2025
!> Copyright (c) 1995-2025 ONERA/DAAA. All rights reserved.
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

!>  function mpiio_open_read              (comm,unit,name)                                      result(iErr)
!>  function mpiio_open_write             (comm,unit,name)                                      result(iErr)
!>  function mpiio_close                       (unit)                                           result(iErr)

!>  function mpiio_global_write_string    (comm, unit, offset, string)                          result(iErr)
!>  function mpiio_global_write_string_tab(comm, unit, offset, string)                          result(iErr)
!>  function mpiio_global_write_int32     (comm, unit, offset, string)                          result(iErr)
!>  function mpiio_global_write_int64     (comm, unit, offset, string)                          result(iErr)
!>  function mpiio_global_write_real64    (comm, unit, offset, string)                          result(iErr)
!x  function mpiio_global_write_cptr      (comm, unit, offset, data_cptr, data_size)            result(iErr)

!>  function mpiio_write_with_indx_string (comm, unit, offset, data_indx, data)                 result(iErr)
!>  function mpiio_write_with_indx_int32  (comm, unit, offset, data_indx, data)                 result(iErr)
!>  function mpiio_write_with_indx_int64  (comm, unit, offset, data_indx, data)                 result(iErr)
!>  function mpiio_write_with_indx_real64 (comm, unit, offset, data_indx, data)                 result(iErr)
!x  function mpiio_write_with_indx_cptr   (comm, unit, offset, data_indx, data_cptr, data_size) result(iErr)

!>  function mpiio_write_block_string     (comm, unit, offset,            data)                  result(iErr)
!>  function mpiio_write_block_int32      (comm, unit, offset,            data)                  result(iErr)
!>  function mpiio_write_block_int64      (comm, unit, offset,            data)                  result(iErr)
!>  function mpiio_write_block_real64     (comm, unit, offset,            data)                  result(iErr)
!x  function mpiio_write_cptr             (comm, unit, offset, data_cptr, data_size)             result(iErr)

module space_mpiio
  use iso_fortran_env
  !use iso_c_binding, only: c_ptr
  use mpi
  implicit none
  
  public :: mpiio_open_read,mpiio_open_write,mpiio_close
  
  interface          mpiio_global_write          ! PDM_io_global_write
    module procedure mpiio_global_write_string
    module procedure mpiio_global_write_string_tab      
  end interface
  public :: mpiio_global_write
  
  interface          mpiio_write_with_indx        ! PDM_io_par_interlaced_write
    module procedure mpiio_write_with_indx_string
    module procedure mpiio_write_with_indx_int32
    module procedure mpiio_write_with_indx_int64
    module procedure mpiio_write_with_indx_real64
  end interface
  public :: mpiio_write_with_indx
  
  interface          mpiio_write_block            ! PDM_io_par_block_write
    module procedure mpiio_write_block_string
    module procedure mpiio_write_block_int32
    module procedure mpiio_write_block_int64
    module procedure mpiio_write_block_real64
  end interface
  public :: mpiio_write_block
  
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
      print *, 'Erreur mpiio_open_read'
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
    call mpi_file_open(                    &
    &    comm                             ,&
    &    trim(name)                       ,&
    &    MPI_MODE_CREATE + MPI_MODE_WRONLY,&
    &    MPI_INFO_NULL                    ,&
    &    unit                             ,&
    &    iErr                              )    
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    if( .not.iErr==mpi_success )then
      print *, 'Erreur mpiio_open_write'
      call mpi_abort(mpi_comm_world, 2, iErr)
      call mpi_finalize(iErr)
    endif
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_file_set_size(unit, 0_8, ierr)    ! Réinitialiser la taille du fichier à 0
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
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    if( .not.iErr==mpi_success )then
      print *, 'Erreur mpiio_close'
      call mpi_abort(mpi_comm_world, 2, iErr)
      call mpi_finalize(iErr)
    endif
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_close

  !>>> mpiio_global_write

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
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Ecriture des blocs de données
    call mpi_comm_rank(comm,rankMPI,iErr)
    
    if( rankMPI==0 )then
      !print '("string:      ",a )',    string
      !print '("len(string): ",i0)',len(string)
      call mpi_file_write_at( &
      &    unit              ,&
      &    offset            ,&  !> on retrouve ici l'offset
      &    string            ,&  !> le tableau à écrire     
      &    len(string)       ,&  !> le nombre d'éléments    
      &    mpi_character     ,&  !> le type d'éléments      
      &    statut            ,&
      &    iErr               )
    endif
    
    offset=offset+sizeof(string)  !*char_size=1 ! Décalage
    call mpi_barrier(comm,ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_global_write_string
  
  function     mpiio_global_write_string_tab(comm, unit, offset, string) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    character(*)            , intent(in)    :: string(:)
    integer(int32)                          :: iErr
    !>
    integer(int32)                          :: sizeofstring                
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: rankMPI
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Ecriture des blocs de données
    call mpi_comm_rank(comm,rankMPI,iErr)
    
    sizeofstring=sizeof(string)
    if( rankMPI==0 )then
      !print '("string:         ",*(a,1x) )',    string(:)
      !print '("size(string):   ",i0)',size(string(:))
      !print '("len(string): ",i0)',len (string)
      
      print '("mpiio_global_write_string_tab sizeofstring: ",i0)',sizeofstring
      call mpi_file_write_at(       &
      &    unit                    ,&
      &    offset                  ,&  !> on retrouve ici l'offset
      &    string(:)               ,&  !> le tableau à écrire     
      &    sizeofstring            ,&  !> le nombre d'éléments    
      &    mpi_character           ,&  !> le type d'éléments      
      &    statut                  ,&
      &    ierr                     )
      
    endif
    offset=offset+sizeofstring  !*char_size=1 ! Décalage
    
    call mpi_barrier(comm,ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_global_write_string_tab
  
  !function     mpiio_global_write_cptr(comm, unit, offset, data_cptr, data_size) result(iErr)
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  integer(int32)          , intent(in)    :: comm
  !  integer(int32)          , intent(inout) :: unit
  !  integer(MPI_OFFSET_KIND), intent(inout) :: offset
  !  type(c_ptr)             , intent(in)    :: data_cptr
  !  integer(int64)                          :: data_size
  !  integer(int32)                          :: iErr
  !  !>
  !  integer(int32)                          :: statut(MPI_STATUS_SIZE)
  !  integer(int32)                          :: rankMPI,iRank
  !  integer(int32)                          :: iErr1
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  !> Ecriture des blocs de données
  !  call mpi_comm_rank(comm,rankMPI,iErr1)
  !  
  !  if( rankMPI==0 )then
  !    print'("data_size=",i0)',data_size
  !    call mpi_file_write_at( &
  !    &    unit              ,&
  !    &    offset            ,&  !> on retrouve ici l'offset
  !    &    data_cptr         ,&  !> le tableau à écrire     
  !    !    int(data_size,kind=int32) ,&  !> le nombre d'éléments    
  !    &    data_size         ,&  !> le nombre d'éléments    
  !    &    mpi_byte          ,&  !> le type d'éléments      
  !    &    statut            ,&
  !    &    iErr               )
  !  endif
  !  
  !  call MPI_BCAST(iErr, 1, MPI_INTEGER, 0, comm, ierr1)
  !  
  !  offset=offset+data_size  !*char_size=1 ! Décalage
  !  call mpi_barrier(comm,ierr)
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  return
  !end function mpiio_global_write_cptr

  !<<< mpiio_global_write
  
  !>>> mpiio_write_with_indx
  function     mpiio_write_with_indx_string(comm, unit, offset, data_indx, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    character(*)            , intent(in)    :: data     (:)
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int64)                          :: dimGlob
    integer(int32)                          :: mpi_new_type
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_character                    !> <=
    dim=size(data_indx)
    if( .not.dim==0 )nBytes=sizeof(data(1)) ! print '("nBytes=",i0)',nBytes
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_type_create_hindexed_block( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    nBytes                         ,& !> blocklength
    &    nBytes*(data_indx-1)           ,& !> array_of_displacements en octets
    &    mpi_type                       ,& !> Old type
    &    mpi_new_type                   ,& !> New type créé
    &    iErr                            )
    
    call mpi_type_commit(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définir la vue globale (chaque processus écrit dans sa position définie par data_indx)
    call mpi_file_set_view(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_new_type                  ,& !> New type
    &    "native"                      ,&
    &    mpi_info_null                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data(:)                       ,&
    &    dim*nBytes                    ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
    !> Libérer le type dérivé et la mémoire
    call mpi_type_free(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
    call mpi_allreduce(int(dim,kind=int64),dimGlob,1,mpi_integer8,mpi_sum,comm,ierr)  
    offset=offset+(dimGlob*nBytes)-1
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_with_indx_string
  
  function     mpiio_write_with_indx_int32(comm, unit, offset, data_indx, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    integer(int32)          , intent(in)    :: data(:)
    !>
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int64)                          :: dimGlob
    integer(int32)                          :: mpi_type
    integer(int32)                          :: mpi_new_type
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_integer                      !> <==
    dim=size(data_indx)
    if( .not.dim==0 )nBytes=sizeof(data(1))
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<    
    call MPI_TYPE_CREATE_HINDEXED_BLOCK( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    1                              ,& !> blocklength
    &    nBytes*(data_indx-1)           ,& !> array_of_displacements en octets
    &    mpi_type                       ,& !> Old type
    &    mpi_new_type                   ,& !> New type
    &    iErr                            )
    
    call MPI_TYPE_COMMIT(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définir la vue globale (chaque processus écrit dans sa position définie par data_indx)
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_new_type                  ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call MPI_FILE_WRITE_ALL(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
    !> Libérer le type dérivé et la mémoire
    call MPI_TYPE_FREE(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
    call mpi_allreduce(int(dim,kind=int64),dimGlob,1,mpi_integer8,mpi_sum,comm,ierr)  
    offset=offset+dimGlob*nBytes
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_with_indx_int32

  function     mpiio_write_with_indx_int64(comm, unit, offset, data_indx, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    integer(int64)          , intent(in)    :: data(:)
    !>
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int64)                          :: dimGlob
    integer(int32)                          :: mpi_type
    integer(int32)                          :: mpi_new_type
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_integer8                      !> <==
    dim=size(data_indx)
    if( .not.dim==0 )nBytes=sizeof(data(1))
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<    
    call MPI_TYPE_CREATE_HINDEXED_BLOCK( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    1                              ,& !> blocklength
    &    nBytes*(data_indx-1)           ,& !> array_of_displacements en octets
    &    mpi_type                       ,& !> Old type
    &    mpi_new_type                   ,& !> New type
    &    iErr                            )
    
    call MPI_TYPE_COMMIT(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définir la vue globale (chaque processus écrit dans sa position définie par data_indx)
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_new_type                  ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call MPI_FILE_WRITE_ALL(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
    !> Libérer le type dérivé et la mémoire
    call MPI_TYPE_FREE(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
    call mpi_allreduce(int(dim,kind=int64),dimGlob,1,mpi_integer8,mpi_sum,comm,ierr)  
    offset=offset+dimGlob*nBytes
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_with_indx_int64
  
  function     mpiio_write_with_indx_real64(comm, unit, offset, data_indx, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    real(real64)            , intent(in)    :: data(:)
    !>
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int64)                          :: dimGlob
    integer(int32)                          :: mpi_type
    integer(int32)                          :: mpi_new_type
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_real8
    dim=size(data_indx)
    if( .not.dim==0 )nBytes=sizeof(data(1))
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<    
    call MPI_TYPE_CREATE_HINDEXED_BLOCK( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    1                              ,& !> blocklength
    &    nBytes*(data_indx-1)           ,& !> array_of_displacements en octets
    &    mpi_type                       ,& !> Old type
    &    mpi_new_type                   ,& !> New type
    &    iErr                            )
    
    call MPI_TYPE_COMMIT(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définir la vue globale (chaque processus écrit dans sa position définie par data_indx)
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_new_type                  ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call MPI_FILE_WRITE_ALL(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
    !> Libérer le type dérivé et la mémoire
    call MPI_TYPE_FREE(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
    call mpi_allreduce(int(dim,kind=int64),dimGlob,1,mpi_integer8,mpi_sum,comm,ierr)  
    offset=offset+dimGlob*nBytes
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_with_indx_real64

  
  !function     mpiio_write_with_indx_cptr(comm, unit, offset, data_indx, data_cptr, data_size) result(iErr)
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  integer(int32)          , intent(in)    :: comm
  !  integer(int32)          , intent(inout) :: unit
  !  integer(MPI_OFFSET_KIND), intent(inout) :: offset
  ! !integer(int32)          , intent(in)    :: data_indx(:)
  !  integer(int64)          , intent(in)    :: data_indx(:)
  !  type(c_ptr)             , intent(in)    :: data_cptr!(:)
  !  integer(int64)          , intent(in)    :: data_size
  !  !>
  !  integer(int32)                          :: nBytes
  !  integer(int32)                          :: data_size_glob
  !  integer(int32)                          :: dim
  !  integer(int32)                          :: mpi_new_type
  !  integer(int32)                          :: iErr
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  dim=size(data_indx)
  !  nBytes=data_size/dim ; print '("mpiio_write_with_indx_cptr nBytes=",i0)',nBytes
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  !call MPI_TYPE_CREATE_INDEXED_Block( &
  !  !&    dim                           ,&
  !  !&    1                             ,&
  !  !&    data_indx-1                   ,& ! int32
  !  !&    MPI_INTEGER                   ,&
  !  !&    mpi_new_type                      ,&
  !  !&    iErr                           )
  !  
  !  call MPI_TYPE_CREATE_HINDEXED_BLOCK( &
  !  &    dim                            ,& !> count
  !  &    1                              ,& !> blocklength
  !  &    nBytes*(data_indx-1)           ,& !> array_of_displacements
  !  &    MPI_BYTE                       ,& !> old type
  !  &    mpi_new_type                      ,& !> new type
  !  &    iErr                            )
  !  
  !  call MPI_TYPE_COMMIT(mpi_new_type, iErr)
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  !> Définition de la vue globale (chaque processus écrit dans sa position définie par data_indx)
  !  call MPI_FILE_SET_VIEW(             &
  !  &    unit                          ,&
  !  &    offset                        ,& !> deplacement initial
  !  &    MPI_BYTE                      ,& !> old type
  !  &    mpi_new_type                     ,& !> new type
  !  &    "native"                      ,&
  !  &    MPI_INFO_NULL                 ,&
  !  &    iErr                           )
  !  
  !  call MPI_FILE_WRITE_ALL(            &
  !  &    unit                          ,&
  !  &    data_cptr                     ,&
  !  &    data_size                     ,&
  !  &    MPI_BYTE                      ,&
  !  &    MPI_STATUS_IGNORE             ,&
  !  &    iErr                           )
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  ! Libérer les types dérivés et la mémoire
  !  call MPI_TYPE_FREE(mpi_new_type, iErr)
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  call mpi_allreduce(data_size,data_size_glob,1,mpi_integer8,mpi_sum,comm,ierr)  
  !  offset=offset+data_size_glob
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  return
  !end function mpiio_write_with_indx_cptr

  !<<< mpiio_write_with_indx

  !>>> mpiio_write_block
  function     mpiio_write_block_string(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    character(*)            , intent(in)    :: data(:) !>  <==
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int32)          , allocatable   :: dimRank(:)
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: sizeMPI,rankMPI,iRank
    integer(int32)                          :: iErr    
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    mpi_type=mpi_character                     !>  <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    dim=size(data)
    if( .not.dim==0 )nBytes=sizeof(data(1))
    allocate(dimRank(0:sizeMPI-1))
    
    call mpi_allgather(                 &
    &    dim*nBytes, 1, mpi_integer    ,&
    &    dimRank(0), 1, mpi_integer    ,&
    &    comm                          ,&
    &    iErr                           )
    
    do iRank=0,rankMPI-1
      offset=offset+dimRank(iRank)
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_FILE_write_at_ALL( &  !> Colleltives
    &    unit                  ,&
    &    offset                ,&  !> on retrouve ici l'offset
    &    data                  ,&  !> le tableau à écrire     
    &    (dim*nBytes)          ,&  !> le nombre d'éléments    
    &    mpi_type              ,&  !> le type d'éléments      
    &    statut                ,&
    &    iErr                   )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    do iRank=rankMPI,sizeMPI-1
      offset=offset+dimRank(iRank) ! <= 4 = int_size
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_block_string

  function     mpiio_write_block_int32(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int32)          , intent(in)    :: data(:) !>  <==
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int32)          , allocatable   :: dimRank(:)
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: sizeMPI,rankMPI,iRank
    integer(int32)                          :: iErr    
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    mpi_type=mpi_integer                      !>  <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    dim=size(data)
    if( .not.dim==0 )nBytes=sizeof(data(1))
    allocate(dimRank(0:sizeMPI-1))
    
    call mpi_allgather(                 &
    &    dim*nBytes, 1, mpi_integer    ,&
    &    dimRank(0), 1, mpi_integer    ,&
    &    comm                          ,&
    &    iErr                           )
    
    do iRank=0,rankMPI-1
      offset=offset+dimRank(iRank)
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_FILE_write_at_ALL( &  !> Colleltives
    &    unit                  ,&
    &    offset                ,&  !> on retrouve ici l'offset
    &    data                  ,&  !> le tableau à écrire     
    &    dim                   ,&  !> le nombre d'éléments    
    &    mpi_type              ,&  !> le type d'éléments      
    &    statut                ,&
    &    iErr                   )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    do iRank=rankMPI,sizeMPI-1
      offset=offset+dimRank(iRank) ! <= 4 = int_size
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_block_int32
  
  function     mpiio_write_block_int64(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data(:) !>  <==
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int32)          , allocatable   :: dimRank(:)
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: sizeMPI,rankMPI,iRank
    integer(int32)                          :: iErr    
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    mpi_type=mpi_integer8                      !>  <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    dim=size(data)
    if( .not.dim==0 )nBytes=sizeof(data(1))
    allocate(dimRank(0:sizeMPI-1))
    
    call mpi_allgather(                 &
    &    dim*nBytes, 1, mpi_integer    ,&
    &    dimRank(0), 1, mpi_integer    ,&
    &    comm                          ,&
    &    iErr                           )
    
    do iRank=0,rankMPI-1
      offset=offset+dimRank(iRank)
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_FILE_write_at_ALL( &  !> Colleltives
    &    unit                  ,&
    &    offset                ,&  !> on retrouve ici l'offset
    &    data                  ,&  !> le tableau à écrire     
    &    dim                   ,&  !> le nombre d'éléments    
    &    mpi_type              ,&  !> le type d'éléments      
    &    statut                ,&
    &    iErr                   )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    do iRank=rankMPI,sizeMPI-1
      offset=offset+dimRank(iRank) ! <= 4 = int_size
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_block_int64
  
  function     mpiio_write_block_real64(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(inout) :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    real(real64)            , intent(in)    :: data(:) !>  <==
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int32)          , allocatable   :: dimRank(:)
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: sizeMPI,rankMPI,iRank
    integer(int32)                          :: iErr    
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    mpi_type=mpi_real8                         !>  <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    dim=size(data)
    if( .not.dim==0 )nBytes=sizeof(data(1))
    allocate(dimRank(0:sizeMPI-1))
    
    call mpi_allgather(                 &
    &    dim*nBytes, 1, mpi_integer    ,&
    &    dimRank(0), 1, mpi_integer    ,&
    &    comm                          ,&
    &    iErr                           )
    
    do iRank=0,rankMPI-1
      offset=offset+dimRank(iRank)
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_FILE_write_at_ALL( &  !> Colleltives
    &    unit                  ,&
    &    offset                ,&  !> on retrouve ici l'offset
    &    data                  ,&  !> le tableau à écrire     
    &    dim                   ,&  !> le nombre d'éléments    
    &    mpi_type              ,&  !> le type d'éléments      
    &    statut                ,&
    &    iErr                   )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    do iRank=rankMPI,sizeMPI-1
      offset=offset+dimRank(iRank) ! <= 4 = int_size
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_block_real64

  !function     mpiio_write_cptr(comm, unit, offset, data_cptr, data_size) result(iErr)
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  integer(int32)          , intent(in)    :: comm
  !  integer(int32)          , intent(inout) :: unit
  !  integer(MPI_OFFSET_KIND), intent(inout) :: offset
  !  type(c_ptr)             , intent(in)    :: data_cptr(:)
  !  integer(int64)          , intent(in)    :: data_size
  !  !>
  !  integer(int32)                          :: statut(MPI_STATUS_SIZE)
  !  integer(int32)                          :: sizeMPI,rankMPI,iRank
  !  integer(int64)          , pointer       :: data_size_Rank(:)
  !  integer(int32)                          :: mpi_new_type
  !  integer(int32)                          :: iErr
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  !> Ecriture des blocs de données
  !  call mpi_comm_size(comm,sizeMPI,iErr)
  !  call mpi_comm_rank(comm,rankMPI,iErr)
  !  
  !  allocate(data_size_Rank(0:sizeMPI-1))
  !  
  !  call mpi_allgather(                   &
  !  &    data_size        ,1,mpi_integer8,&
  !  &    data_size_Rank(0),1,mpi_integer8,&
  !  &    comm                            ,&
  !  &    iErr                             )
  !  
  !  do iRank=0,rankMPI-1
  !    offset=offset+data_size_Rank(iRank)
  !  enddo
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
  !  call MPI_FILE_write_at_ALL(     &  !> Colleltives
  !  &    unit                      ,&
  !  &    offset                    ,&  !> on retrouve ici l'offset
  !  &    data_cptr                 ,&  !> le tableau à écrire     
  !  &    int(data_size,kind=int32) ,&  !> le nombre d'éléments    
  !  &    MPI_BYTE                  ,&  !> le type d'éléments      
  !  &    statut                    ,&
  !  &    iErr                       )
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  do iRank=rankMPI,sizeMPI-1
  !    offset=offset+data_size_Rank(iRank)
  !  enddo
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  deallocate(data_size_Rank)
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !<<< mpiio_write_block
  !
  !  return
  !end function mpiio_write_cptr
  
end module space_mpiio
