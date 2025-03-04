!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!> mppiio.f90
!> Christophe Peyret
!> Created on Tue Jan 21  2025
!> Copyright (c) 1995-2025 ONERA/DAAA. All rights reserved.
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

!>  function mpiio_message                   (comm, buffer)                                       result(iErr)

!>  function mpiio_open_read                 (comm,unit,name)                                     result(iErr)
!>  function mpiio_open_write                (comm,unit,name)                                     result(iErr)
!>  function mpiio_close                          (unit)                                          result(iErr)

!>  function mpiio_global_read_string        (comm, unit, offset, data)                           result(iErr)
!>  function mpiio_global_read_int32         (comm, unit, offset, data)                           result(iErr)
!>  function mpiio_global_read_int64         (comm, unit, offset, data)                           result(iErr)

!>  function mpiio_read_with_indx_int32      (comm, unit, offset, data_indx, data)                result(iErr)
!>  function mpiio_read_with_indx_real64     (comm, unit, offset, data_indx, data)                result(iErr)
!>  function mpiio_read_with_indx_complex128 (comm, unit, offset, data_indx, data)                result(iErr)

!>  function mpiio_read_block_int32          (comm, unit, offset, data)                           result(iErr)
!>  function mpiio_read_block_real64         (comm, unit, offset, data)                           result(iErr)
!>  function mpiio_read_block_complex128     (comm, unit, dimGlob, offset, data)                  result(iErr)

!>  function mpiio_global_write_string       (comm, unit, offset, data)                            result(iErr)
!>  function mpiio_global_write_string_tab   (comm, unit, offset, data)                            result(iErr)
!>  function mpiio_global_write_int32        (comm, unit, offset, data)                            result(iErr)
!>  function mpiio_global_write_int64        (comm, unit, offset, data)                            result(iErr)
!x  function mpiio_global_write_cptr         (comm, unit, offset, data_cptr, data_size)            result(iErr)

!>  function mpiio_write_with_indx_string    (comm, unit, offset, data_indx, stride, data)         result(iErr)
!>  function mpiio_write_with_indx_int32     (comm, unit, offset, data_indx, stride, data)         result(iErr)
!>  function mpiio_write_with_indx_int64     (comm, unit, offset, data_indx, stride, data)         result(iErr)
!>  function mpiio_write_with_indx_real32    (comm, unit, offset, data_indx, stride, data)         result(iErr)
!>  function mpiio_write_with_indx_real64    (comm, unit, offset, data_indx, stride, data)         result(iErr)
!>  function mpiio_write_with_indx_complex128(comm, unit, offset, data_indx, stride, data)         result(iErr)

!x  function mpiio_write_with_indx_cptr   (comm, unit, offset, data_indx, data_cptr, data_size) result(iErr)

!>  function mpiio_write_block_string        (comm, unit, offset,            data)                  result(iErr)
!>  function mpiio_write_block_int32         (comm, unit, offset,            data)                  result(iErr)
!>  function mpiio_write_block_int64         (comm, unit, offset,            data)                  result(iErr)
!>  function mpiio_write_block_real64        (comm, unit, offset,            data)                  result(iErr)
!>  function mpiio_write_block_complex128    (comm, unit, offset,            data)                  result(iErr)
!x  function mpiio_write_cptr                (comm, unit, offset, data_cptr, data_size)             result(iErr)

!>  function mpiio_part2block_real64         (comm,data_indx,stride,data, dataBloc)                      result(t0)

module space_mpiio
  use iso_fortran_env
  !use iso_c_binding, only: c_ptr
  use mpi
  implicit none
  
  ! ecriture en native call MPI_FILE_SET_VIEW(fh, 0_8, MPI_CHARACTER, filetype, 'native', MPI_INFO_NULL, ierr)
  
  public :: mpiio_message
  public :: mpiio_open_read,mpiio_open_write
  public :: mpiio_close
  
  interface          mpiio_global_read
    module procedure mpiio_global_read_string
    module procedure mpiio_global_read_int32
    module procedure mpiio_global_read_int64
  end interface
  public :: mpiio_global_read
  
  interface          mpiio_read_with_index
    module procedure mpiio_read_with_indx_int32
    module procedure mpiio_read_with_indx_real64  
    module procedure mpiio_read_with_indx_complex128
  end interface
  public :: mpiio_read_with_index
  
  interface          mpiio_read_block
    module procedure mpiio_read_block_int32
   !module procedure mpiio_read_block_int64
    module procedure mpiio_read_block_real64
    module procedure mpiio_read_block_complex128
  end interface
  public :: mpiio_read_block
  
  interface          mpiio_global_write
    module procedure mpiio_global_write_string
    module procedure mpiio_global_write_string_tab
    module procedure mpiio_global_write_int32
  end interface
  public :: mpiio_global_write
    
  interface          mpiio_write_with_indx
    module procedure mpiio_write_with_indx_string
    module procedure mpiio_write_with_indx_int32
    module procedure mpiio_write_with_indx_int64
    module procedure mpiio_write_with_indx_real32
    module procedure mpiio_write_with_indx_real64
    module procedure mpiio_write_with_indx_complex128
  end interface
  public :: mpiio_write_with_indx
    
  interface          mpiio_write_block
    module procedure mpiio_write_block_string
    module procedure mpiio_write_block_int32
    module procedure mpiio_write_block_int64
    module procedure mpiio_write_block_real32
    module procedure mpiio_write_block_real64
    module procedure mpiio_write_block_complex128
  end interface
  public :: mpiio_write_block
  
contains
  
  function     mpiio_message(comm, buffer) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32) , intent(in)    :: comm
    character(*)   , intent(in)    :: buffer
    !>
    integer(int32)                 :: rankMPI
    integer(int32)                 :: sizeMPI
    integer(int32)                 :: output
    integer(int32)                 :: length
    integer(int32)                 :: iRank,iErr
    character(len=:), allocatable  :: cTab0(:)
    !external                       :: mpi_gather
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    call mpi_comm_rank(comm,rankMPI,iErr)
    call mpi_comm_size(comm,sizeMPI,iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    output=output_unit    
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    length=len(buffer)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    allocate( character(len=length) :: cTab0(1:sizeMPI) )
    
    call mpi_gather(                      &
    &    buffer   , length, mpi_character,&
    &    cTab0(1) , length, mpi_character,&
    &    0                               ,&
    &    comm                            ,&
    &    iErr                             )
    
    if( rankMPI==0 )then
      if( sizeMPI>1 )write(output,'()')
      do iRank=1,sizeMPI
        if( .not.len_trim(cTab0(iRank))==0 )then   !> on retire le cas d'une chaine vide (utilitse par plotInit => micro)
          write(output,'(a)')trim(cTab0(iRank))
        endif
      enddo
    endif
    
    deallocate(cTab0)
    
    call mpi_barrier(comm,iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<    
    return
  end function mpiio_message
  
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
      print '("file: """,a,""" Erreur mpiio_open_read")',trim(name)
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
      print '("file: """,a,""" Erreur mpiio_open_write")',trim(name)
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
  
  
  !>>>         mpiio_global_read
  function     mpiio_global_read_string(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    character(*)            , intent(in)    :: data
    integer(int32)                          :: iErr
    !>
    integer(int32)                          :: dim
    integer(int32)                          :: mpi_type
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    mpi_type=mpi_character                     !>  <==
    dim=len(data)
    
    !block
    !  character(128) :: buffer
    !  write(buffer,'("mpiio_global_read_string len(data)=",i3)')len(data)
    !  iErr=mpiio_message(comm=comm, buffer=buffer)  
    !end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_file_set_view(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    mpi_info_null                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> lecture collective
    call mpi_file_read_all(             &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    
    !block
    !  character(128) :: buffer
    !  write(buffer,'("mpiio_global_read_string data: ",a)')data
    !  iErr=mpiio_message(comm=comm, buffer=buffer)  
    !end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> offset  
    offset=offset+dim  ! Décalage
    call mpi_barrier(comm,ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_global_read_string
  
  function     mpiio_global_read_int32(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int32)          , intent(in)    :: data
    integer(int32)                          :: iErr
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: rankMPI
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    mpi_type=mpi_integer                       !>  <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_file_set_view(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    mpi_info_null                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> lecture collective
    call mpi_file_read_all(             &
    &    unit                          ,&
    &    data                          ,&
    &    1                             ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> offset
    offset=offset+sizeof(data)
    call mpi_barrier(comm,ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_global_read_int32

  function     mpiio_global_read_int64(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data
    integer(int32)                          :: iErr
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: rankMPI
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    mpi_type=mpi_integer8                      !>  <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_file_set_view(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    mpi_info_null                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> lecture collective
    call mpi_file_read_all(             &
    &    unit                          ,&
    &    data                          ,&
    &    1                             ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<     
    !> offset
    offset=offset+sizeof(data)
    call mpi_barrier(comm,ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_global_read_int64
  !<<<         mpiio_global_read
  
  !>>>         mpiio_read_with_index
  function     mpiio_read_with_indx_int32(comm, unit, offset, data_indx, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    integer(int32)          , pointer       :: data     (:)
    !>
    integer(int64)          , pointer       :: disp_indx(:)
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
    allocate(data(1:dim))
    if( .not.dim==0 )nBytes=sizeof(data(1))

    allocate(disp_indx(1:dim))
    disp_indx=nBytes*(data_indx(1:dim)-1)
    !block
    !  character(128) :: buffer
    !  write(buffer,'("mpiio_read_with_indx_int32 nBytes=",i3,3x,"dim=",i3)')nBytes,dim
    !  iErr=mpiio_message(comm=comm, buffer=buffer)  
    !end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_TYPE_CREATE_HINDEXED_BLOCK( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    1                              ,& !> blocklength
    &    disp_indx                      ,& !> array_of_displacements en octets
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
    call MPI_FILE_read_ALL(             &
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

    deallocate(disp_indx)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_read_with_indx_int32
  
  function     mpiio_read_with_indx_real64(comm, unit, offset, data_indx, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    real(real64)            , pointer       :: data     (:)
    !>
    integer(MPI_OFFSET_KIND), pointer       :: disp_indx(:)
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int64)                          :: dimGlob
    integer(int32)                          :: mpi_type
    integer(int32)                          :: mpi_new_type
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_real8                         !> <==
    dim=size(data_indx)
    allocate(data(1:dim))
    if( .not.dim==0 )nBytes=sizeof(data(1))
    
    allocate(disp_indx(1:dim))
    disp_indx(1:dim)=nBytes*(data_indx(1:dim)-1)

    !block
    !  character(128) :: buffer
    !  write(buffer,'("mpiio_read_with_indx_int32 nBytes=",i3,3x,"dim=",i3)')nBytes,dim
    !  iErr=mpiio_message(comm=comm, buffer=buffer)  
    !end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_TYPE_CREATE_HINDEXED_BLOCK( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    1                              ,& !> blocklength
    &    disp_indx                      ,& !> array_of_displacements en octets
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
    call MPI_FILE_read_ALL(             &
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
    
    deallocate(disp_indx)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_read_with_indx_real64
  
  function     mpiio_read_with_indx_complex128(comm, unit, offset, data_indx, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    complex(real64)         , pointer       :: data     (:)
    !>
    integer(MPI_OFFSET_KIND), pointer       :: disp_indx(:)
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int64)                          :: dimGlob
    integer(int32)                          :: mpi_type
    integer(int32)                          :: mpi_new_type
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_complex16                     !> <==
    dim=size(data_indx)
    allocate(data(1:dim))
    if( .not.dim==0 )nBytes=sizeof(data(1))
    
    allocate(disp_indx(1:dim))
    disp_indx(1:dim)=nBytes*(data_indx(1:dim)-1)
    
    !block
    !  character(128) :: buffer
    !  write(buffer,'("mpiio_read_with_indx_complex128 nBytes=",i3,3x,"dim=",i3)')nBytes,dim
    !  iErr=mpiio_message(comm=comm, buffer=buffer)  
    !end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_TYPE_CREATE_HINDEXED_BLOCK( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    1                              ,& !> blocklength
    &    disp_indx                      ,& !> array_of_displacements en octets
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
    call MPI_FILE_read_ALL(             &
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
    
    deallocate(disp_indx)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_read_with_indx_complex128
  !<<<         mpiio_read_with_index
  
  !>>>         mpiio_read_block
  function     mpiio_read_block_int32(comm, unit, dimGlob, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(int32)          , intent(in)    :: dimGlob
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int32)          , pointer       :: data(:) !>  <==
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: n0,n1,dim
    integer(int32)          , allocatable   :: dimRank(:)
    integer(int32)                          :: sizeMPI,rankMPI
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    mpi_type=mpi_integer                       !>  <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
    n0= rankMPI   *dimGlob/sizeMPI+1
    n1=(rankMPI+1)*dimGlob/sizeMPI
    dim=n1-n0+1
    
    !block
    !  character(128) :: buffer
    !  write(buffer,'("rank ",i3.3,1x,"n0=",i3,2x,"n1=",i3,3x,"dim=",i3)')rankMPI,n0,n1,dim
    !  iErr=mpiio_message(comm=comm, buffer=buffer)  
    !end block
    
    allocate(data(1:dim))
    nBytes=sizeof(data(1))
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
    allocate(dimRank(0:sizeMPI-1))
    
    call mpi_allgather(           &
    &    dim*nBytes,1,mpi_integer,&
    &    dimRank(0),1,mpi_integer,&
    &    comm                    ,&
    &    iErr                     )
    
    offset=offset+sum(dimRank(0:rankMPI-1))
    
    !block
    !  character(128) :: buffer
    !  write(buffer,'("rank ",i3.3,1x,"mpiio_read_block_int32 offset=",i0)')rankMPI,offset
    !  iErr=mpiio_message(comm=comm, buffer=buffer)  
    !end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définir la vue globale (chaque processus écrit dans sa position définie par data_indx)
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    !> Écriture collective
    call MPI_FILE_read_ALL(             &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    offset=offset+sum(dimRank(rankMPI:sizeMPI-1))
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_read_block_int32
  
  function     mpiio_read_block_real64(comm, unit, dimGlob, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(int32)          , intent(in)    :: dimGlob
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    real(real64)            , pointer       :: data(:) !>  <==
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: n0,n1,dim
    integer(int32)          , allocatable   :: dimRank(:)
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: sizeMPI,rankMPI
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
    n0= rankMPI   *dimGlob/sizeMPI+1
    n1=(rankMPI+1)*dimGlob/sizeMPI    
    dim=n1-n0+1
    
    !block
    !  character(128) :: buffer
    !  write(buffer,'("rank ",i3.3,1x,"mpiio_read_block_real64: n0=",i3,2x,"n1=",i3,3x,"dim=",i3)')rankMPI,n0,n1,dim
    !  iErr=mpiio_message(comm=comm, buffer=buffer)  
    !end block
        
    allocate(data(1:dim))
    nBytes=sizeof(data(1))
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
    allocate(dimRank(0:sizeMPI-1))
    
    call mpi_allgather(           &
    &    dim*nBytes,1,mpi_integer,&
    &    dimRank(0),1,mpi_integer,&
    &    comm                    ,&
    &    iErr                     )
    
    offset=offset+sum(dimRank(0:rankMPI-1))
    
    !block
    !  character(128) :: buffer
    !  write(buffer,'("rank ",i3.3,1x,"mpiio_read_block_real64: offset=",i0)')rankMPI,offset
    !  iErr=mpiio_message(comm=comm, buffer=buffer)  
    !end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définir la vue globale (chaque processus écrit dans sa position définie par data_indx)
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    !> Écriture collective
    call MPI_FILE_read_ALL(             &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    offset=offset+sum(dimRank(rankMPI:sizeMPI-1))
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_read_block_real64
  
  function     mpiio_read_block_complex128(comm, unit, dimGlob, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(int32)          , intent(in)    :: dimGlob
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    complex(real64)         , pointer       :: data(:) !>  <==
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: n0,n1,dim
    integer(int32)          , allocatable   :: dimRank(:)
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: sizeMPI,rankMPI
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    mpi_type=mpi_complex16                      !>  <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
    n0= rankMPI   *dimGlob/sizeMPI+1
    n1=(rankMPI+1)*dimGlob/sizeMPI    
    dim=n1-n0+1
    
    !block
    !  character(128) :: buffer
    !  write(buffer,'("rank ",i3.3,1x,"mpiio_read_block_complex128: n0=",i3,2x,"n1=",i3,3x,"dim=",i3)')rankMPI,n0,n1,dim
    !  iErr=mpiio_message(comm=comm, buffer=buffer)  
    !end block
        
    allocate(data(1:dim))
    nBytes=sizeof(data(1))
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
    allocate(dimRank(0:sizeMPI-1))
    
    call mpi_allgather(           &
    &    dim*nBytes,1,mpi_integer,&
    &    dimRank(0),1,mpi_integer,&
    &    comm                    ,&
    &    iErr                     )
    
    offset=offset+sum(dimRank(0:rankMPI-1))
    
    !block
    !  character(128) :: buffer
    !  write(buffer,'("rank ",i3.3,1x,"mpiio_read_block_complex128: offset=",i0)')rankMPI,offset
    !  iErr=mpiio_message(comm=comm, buffer=buffer)  
    !end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définir la vue globale (chaque processus écrit dans sa position définie par data_indx)
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    !> Écriture collective
    call MPI_FILE_read_ALL(             &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    offset=offset+sum(dimRank(rankMPI:sizeMPI-1))
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_read_block_complex128
  !<<<         mpiio_read_block
  
  !>>>         mpiio_global_write
  function     mpiio_global_write_string(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    character(*)            , intent(in)    :: data
    integer(int32)                          :: iErr
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: dim
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: rankMPI
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_character                      !>  <==
    
    call mpi_comm_rank(comm,rankMPI,iErr)    
    if( rankMPI==0 )then
      dim=len(data)
    else
      dim=0
    endif
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_file_set_view(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    mpi_info_null                 ,&
    &    iErr                           )
    
    if( .not.iErr==mpi_success )then
      stop "mpiio_global_write_string Format binaire non supporté"
    endif
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Offset    
    offset=offset+len(data)  !*char_size=1 ! Décalage
    call mpi_barrier(comm,ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_global_write_string
  
  function     mpiio_global_write_string_tab(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    character(*)            , intent(in)    :: data(:)
    integer(int32)                          :: iErr
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: data_size0,data_size                
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: rankMPI
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Ecriture des blocs de données
    mpi_type=mpi_character                      !>  <==
    
    data_size0=len(data)*size(data)
    call mpi_comm_rank(comm,rankMPI,iErr)
    if( rankMPI==0 )then
      data_size=data_size0
    else
      data_size=0
    endif
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_file_set_view(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    mpi_info_null                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data(:)                       ,&
    &    data_size                     ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Offset
    offset=offset+data_size0 
    call mpi_barrier(comm,ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_global_write_string_tab
  
  function     mpiio_global_write_int32(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int32)          , intent(in)    :: data
    integer(int32)                          :: iErr
    !>
    integer(int32)                          :: dim
    integer(int32)                          :: mpi_type
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: rankMPI
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_integer                       !>  <==
    
    call mpi_comm_rank(comm,rankMPI,iErr)
    if( rankMPI==0 )then
      dim=1
    else
      dim=0
    endif
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_file_set_view(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    mpi_info_null                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Offset    
    offset=offset+sizeof(data)
    call mpi_barrier(comm,ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_global_write_int32

  function     mpiio_global_write_int64(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data
    integer(int32)                          :: iErr
    !>
    integer(int32)                          :: dim
    integer(int32)                          :: mpi_type
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: rankMPI
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_integer                       !>  <==
    
    call mpi_comm_rank(comm,rankMPI,iErr)
    if( rankMPI==0 )then
      dim=1
    else
      dim=0
    endif
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_file_set_view(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    mpi_info_null                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim                           ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Offset    
    offset=offset+sizeof(data)
    call mpi_barrier(comm,ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<    return
  end function mpiio_global_write_int64
  
  !function     mpiio_global_write_cptr(comm, unit, offset, data_cptr, data_size) result(iErr)
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  integer(int32)          , intent(in)    :: comm
  !  integer(int32)          , intent(in)    :: unit
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
  
  !>>>         mpiio_write_with_indx
  !function     mpiio_write_with_indx_string(comm, unit, offset, data_indx, data) result(iErr)
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  integer(int32)          , intent(in)    :: comm
  !  integer(int32)          , intent(in)    :: unit
  !  integer(MPI_OFFSET_KIND), intent(inout) :: offset
  !  integer(int64)          , intent(in)    :: data_indx(:)
  !  character(*)            , intent(in)    :: data     (:)
  !  !>
  !  integer(int32)                          :: nBytes=0
  !  integer(int32)                          :: dim
  !  integer(int64)                          :: dimGlob
  !  integer(int32)                          :: mpi_type
  !  integer(int32)                          :: mpi_new_type
  !  integer(int32)                          :: iErr
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  mpi_type=mpi_character                     !> <=
  !  nBytes=len(data(1))                        !> <==
  !  dim=size(data_indx)
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  call mpi_type_create_hindexed_block( & !> version octets ppour avoir des indx en int64
  !  &    dim                            ,&
  !  &    nBytes                         ,& !> blocklength
  !  &    nBytes*(data_indx-1)           ,& !> array_of_displacements en octets
  !  &    mpi_type                       ,& !> Old type
  !  &    mpi_new_type                   ,& !> New type créé
  !  &    iErr                            )
  !  
  !  call mpi_type_commit(mpi_new_type, iErr)
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  !> Définir la vue globale (chaque processus écrit dans sa position définie par data_indx)
  !  call mpi_file_set_view(             &
  !  &    unit                          ,&
  !  &    offset                        ,& !> deplacement initial
  !  &    mpi_type                      ,& !> Old type
  !  &    mpi_new_type                  ,& !> New type
  !  &    "native"                      ,&
  !  &    mpi_info_null                 ,&
  !  &    iErr                           )
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
  !  !> Écriture collective
  !  call mpi_file_write_all(            &
  !  &    unit                          ,&
  !  &    data(:)                       ,&
  !  &    dim*nBytes                    ,& !> dimension
  !  &    mpi_type                      ,& !> Old type
  !  &    MPI_STATUS_IGNORE             ,&
  !  &    iErr                           )
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
  !  !> Libérer le type dérivé et la mémoire
  !  call mpi_type_free(mpi_new_type, iErr)
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
  !  call mpi_allreduce(int(dim,kind=int64),dimGlob,1,mpi_integer8,mpi_sum,comm,ierr)  
  !  offset=offset+(dimGlob*nBytes)-1
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  return
  !end function mpiio_write_with_indx_string
  

  function     mpiio_write_with_indx_string(comm, unit, offset, data_indx, stride, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    integer(int32)          , intent(in)    :: stride
    character(*)            , intent(in)    :: data     (:)
    !>
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int64)                          :: dimGlob
    integer(int32)                          :: mpi_type
    integer(int32)                          :: mpi_new_type
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_character                     !> <=
   !nBytes=len(data(1))                        !> <==
    nBytes=stride                              !> <==
    dim=size(data_indx)
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


  function     mpiio_write_with_indx_int32(comm, unit, offset, data_indx, stride, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    integer(int32)          , intent(in)    :: stride
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
    mpi_type=mpi_integer                       !> <==
    nBytes=int32*stride                        !> <==
    dim=size(data_indx)
    
    !block
    !  character(80) :: buffer
    !  write(buffer,'("mpiio_write_with_indx_int32 dim=",i0)' ),dim
    !  iErr=mpiio_message(comm=comm, buffer=buffer)
    !end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<    
    call mpi_type_create_hindexed_block( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    stride                         ,& !> blocklength
    &    nBytes*(data_indx-1)           ,& !> array_of_displacements en octets
    &    mpi_type                       ,& !> Old type
    &    mpi_new_type                   ,& !> New type
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
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim*stride                    ,& !> dimension
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
    offset=offset+dimGlob*nBytes
    
    !block
    !  character(80) :: buffer
    !  write(buffer,'("mpiio_write_with_indx_int32 dim    =",i0)' ),dim
    !  iErr=mpiio_message(comm=comm, buffer=buffer)
    !  write(buffer,'("mpiio_write_with_indx_int32 dimGlob=",i0)' ),dimGlob
    !  iErr=mpiio_message(comm=comm, buffer=buffer)
    !  write(buffer,'("mpiio_write_with_indx_int32 offset=",i0)' ),offset
    !  iErr=mpiio_message(comm=comm, buffer=buffer)
    !end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_with_indx_int32
  
  function     mpiio_write_with_indx_int64(comm, unit, offset, data_indx, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
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
    nBytes=int64                               !> <==
    dim=size(data_indx)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_type_create_hindexed_block( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    1                              ,& !> blocklength
    &    nbytes*(data_indx-1)           ,& !> array_of_displacements en octets
    &    mpi_type                       ,& !> old type
    &    mpi_new_type                   ,& !> new type
    &    ierr                            )
    
    call mpi_type_commit(mpi_new_type, ierr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définir la vue globale (chaque processus écrit dans sa position définie par data_indx)
    call mpi_file_set_view(              &
    &    unit                           ,&
    &    offset                         ,& !> deplacement initial
    &    mpi_type                       ,& !> old type
    &    mpi_new_type                   ,& !> new type
    &    "native"                       ,&
    &    mpi_info_null                  ,&
    &    ierr                            )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call mpi_file_write_all(             &
    &    unit                           ,&
    &    data                           ,&
    &    dim                            ,& !> dimension
    &    mpi_type                       ,& !> Old type
    &    mpi_status_ignore              ,&
    &    iErr                            )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
    !> Libérer le type dérivé et la mémoire
    call mpi_type_free(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
    call mpi_allreduce(int(dim,kind=int64),dimGlob,1,mpi_integer8,mpi_sum,comm,ierr)  
    offset=offset+dimGlob*nBytes
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_with_indx_int64
  
  function     mpiio_write_with_indx_real32(comm, unit, offset, data_indx, stride, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    integer(int32)          , intent(in)    :: stride
    real(real32)            , intent(in)    :: data(:)
    !>
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int64)                          :: dimGlob
    integer(int32)                          :: mpi_type
    integer(int32)                          :: mpi_new_type
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_real                         !> <==
    nBytes=real32*stride                      !> <==
    dim=size(data_indx)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<    
    call mpi_type_create_hindexed_block( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    stride                         ,& !> blocklength
    &    nBytes*(data_indx-1)           ,& !> array_of_displacements en octets
    &    mpi_type                       ,& !> Old type
    &    mpi_new_type                   ,& !> New type
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
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim*stride                    ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    mpi_status_ignore             ,&
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
  end function mpiio_write_with_indx_real32
  
  function     mpiio_write_with_indx_real64(comm, unit, offset, data_indx, stride, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    integer(int32)          , intent(in)    :: stride
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
    mpi_type=mpi_real8                         !> <==
    nBytes=real64*stride                       !> <==
    dim=size(data_indx)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<    
    call mpi_type_create_hindexed_block( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    stride                         ,& !> blocklength
    &    nBytes*(data_indx-1)           ,& !> array_of_displacements en octets
    &    mpi_type                       ,& !> Old type
    &    mpi_new_type                   ,& !> New type
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
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim*stride                    ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    mpi_status_ignore             ,&
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
  
  function     mpiio_write_with_indx_complex128(comm, unit, offset, data_indx, stride, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    integer(int32)          , intent(in)    :: stride
    complex(real64)         , intent(in)    :: data(:)
    !>
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: dim
    integer(int64)                          :: dimGlob
    integer(int32)                          :: mpi_type
    integer(int32)                          :: mpi_new_type
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_complex16                     !> <==
    nBytes=(2*real64)*stride                  !> <==
    dim=size(data_indx)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<    
    call mpi_type_create_hindexed_block( & !> version octets ppour avoir des indx en int64
    &    dim                            ,&
    &    stride                         ,& !> blocklength
    &    nBytes*(data_indx-1)           ,& !> array_of_displacements en octets
    &    mpi_type                       ,& !> Old type
    &    mpi_new_type                   ,& !> New type
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
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Écriture collective
    call MPI_FILE_WRITE_ALL(            &
    &    unit                          ,&
    &    data                          ,&
    &    dim*stride                    ,& !> dimension
    &    mpi_type                      ,& !> Old type
    &    mpi_status_ignore             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Libérer le type dérivé et la mémoire
    call mpi_type_free(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_allreduce(int(dim,kind=int64),dimGlob,1,mpi_integer8,mpi_sum,comm,ierr)  
    offset=offset+dimGlob*nBytes
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_with_indx_complex128
  
  function     mpiio_write_with_indx_cptr(comm, unit, offset, data_indx, data_cptr, data_size) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    use iso_c_binding, only: c_ptr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    integer(int64)          , intent(in)    :: data_indx(:)
    type(c_ptr)             , intent(in)    :: data_cptr
    integer(int64)          , intent(in)    :: data_size
    !>
    integer(int32)                          :: nBytes=0
    integer(int32)                          :: data_size_glob
    integer(int32)                          :: dim
    integer(int32)                          :: mpi_type
    integer(int32)                          :: mpi_new_type
    integer(int32)                          :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mpi_type=mpi_byte                          !> <=
    dim=size(data_indx)
    nBytes=data_size/dim ; print '("mpiio_write_with_indx_cptr nBytes=",i0)',nBytes
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_type_create_hindexed_block( &
    &    dim                            ,& !> count
    &    nBytes                         ,& !> blocklength
    &    nBytes*(data_indx-1)           ,& !> array_of_displacements
    &    mpi_type                       ,& !> old type
    &    mpi_new_type                   ,& !> new type
    &    iErr                            )
    
    call mpi_type_commit(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Définition de la vue globale (chaque processus écrit dans sa position définie par data_indx)
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_new_type                  ,& !> new type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    
    call MPI_FILE_WRITE_ALL(            &
    &    unit                          ,&
    &    data_cptr                     ,&
    &    dim*nBytes                    ,& !> dimension
    &    mpi_type                      ,&
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! Libérer les types dérivés et la mémoire
    call MPI_TYPE_FREE(mpi_new_type, iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_allreduce(data_size,data_size_glob,1,mpi_integer8,mpi_sum,comm,ierr)  
    offset=offset+data_size_glob
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_with_indx_cptr
  !<<<         mpiio_write_with_indx
  
  !>>>         mpiio_write_block
  function     mpiio_write_block_string(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
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
    !if( .not.dim==0 )nBytes=sizeof(data(1))
    if( .not.dim==0 )nBytes=len(data(1))
    dim=dim*nBytes
    allocate(dimRank(0:sizeMPI-1))
    
    call mpi_allgather(                 &
    &    dim       , 1, mpi_integer    ,&
    &    dimRank(0), 1, mpi_integer    ,&
    &    comm                          ,&
    &    iErr                           )
    
    do iRank=0,rankMPI-1
      offset=offset+dimRank(iRank)
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data(:)                       ,&
    &    dim                           ,&  !> le nombre d'éléments    
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
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
    integer(int32)          , intent(in)    :: unit
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
    mpi_type=mpi_integer                      !> <==
    nBytes=int32                              !> <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    dim=size(data)
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
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data(:)                       ,&
    &    dim                           ,&  !> le nombre d'éléments    
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> offset
    do iRank=rankMPI,sizeMPI-1
      offset=offset+dimRank(iRank)
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
    integer(int32)          , intent(in)    :: unit
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
    mpi_type=mpi_integer8                      !> <==
    nBytes=int64                               !> <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    dim=size(data)
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
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data(:)                       ,&
    &    dim                           ,&  !> le nombre d'éléments    
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> offset
    do iRank=rankMPI,sizeMPI-1
      offset=offset+dimRank(iRank)
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_block_int64
  
  function     mpiio_write_block_real32(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    real(real32)            , intent(in)    :: data(:) !>  <==
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
    mpi_type=mpi_real                          !> <==
    nBytes=real32                              !> <==
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
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data(:)                       ,&
    &    dim                           ,&  !> le nombre d'éléments    
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> offset
    do iRank=rankMPI,sizeMPI-1
      offset=offset+dimRank(iRank) ! <= 4 = int_size
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_block_real32
  
  function     mpiio_write_block_real64(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
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
    mpi_type=mpi_real8                         !> <==
    nBytes=real64                              !> <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    dim=size(data)
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
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data(:)                       ,&
    &    dim                           ,& !> le nombre d'éléments    
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> offset
    do iRank=rankMPI,sizeMPI-1
      offset=offset+dimRank(iRank) ! <= 4 = int_size
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
  end function mpiio_write_block_real64
  
  function     mpiio_write_block_complex128(comm, unit, offset, data) result(iErr)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32)          , intent(in)    :: comm
    integer(int32)          , intent(in)    :: unit
    integer(MPI_OFFSET_KIND), intent(inout) :: offset
    complex(real64)         , intent(in)    :: data(:) !>  <==
    !>
    integer(int32)                          :: mpi_type
    integer(int32)                          :: nBytes
    integer(int32)                          :: dim
    integer(int32)          , allocatable   :: dimRank(:)
    integer(int32)                          :: statut(MPI_STATUS_SIZE)
    integer(int32)                          :: sizeMPI,rankMPI,iRank
    integer(int32)                          :: iErr    
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    mpi_type=mpi_complex16                     !> <==
    nBytes=(2*real64)                         !> <==
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    !> Ecriture des blocs de données
    call mpi_comm_size(comm,sizeMPI,iErr)
    call mpi_comm_rank(comm,rankMPI,iErr)    
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    dim=size(data)
    allocate(dimRank(0:sizeMPI-1))
    
    block
      character(128) :: buffer
      write(buffer,'("rank ",i3.3,1x,"mpiio_read_block_complex128: nBytes=",i3,2x,3x,"dim=",i3)')rankMPI,nBytes,dim
      iErr=mpiio_message(comm=comm, buffer=buffer)  
    end block
    
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
    call MPI_FILE_SET_VIEW(             &
    &    unit                          ,&
    &    offset                        ,& !> deplacement initial
    &    mpi_type                      ,& !> Old type
    &    mpi_type                      ,& !> New type
    &    "native"                      ,&
    &    MPI_INFO_NULL                 ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Écriture collective
    call mpi_file_write_all(            &
    &    unit                          ,&
    &    data(:)                       ,&
    &    dim                           ,&  !> le nombre d'éléments    
    &    mpi_type                      ,& !> Old type
    &    MPI_STATUS_IGNORE             ,&
    &    iErr                           )
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> offset
    do iRank=rankMPI,sizeMPI-1
      offset=offset+dimRank(iRank) ! <= 4 = int_size
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    deallocate(dimRank)

    block
      character(128) :: buffer
      write(buffer,'("rank ",i3.3,1x,"mpiio_read_block_complex128: offset=",i0)')rankMPI,offset
      iErr=mpiio_message(comm=comm, buffer=buffer)  
    end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    return
  end function mpiio_write_block_complex128

  !function     mpiio_write_cptr(comm, unit, offset, data_cptr, data_size) result(iErr)
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  integer(int32)          , intent(in)    :: comm
  !  integer(int32)          , intent(in)    :: unit
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
  
  subroutine to_nativen_int32(comm,val,be_val)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32), intent(in)    :: comm
    integer(int32), intent(in)    :: val
    integer(int32), intent(inout) :: be_val
    integer(int8)                 :: bytes(4)
    integer(int32)                :: pos
    integer(int32)                :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    call mpi_pack(val,1,mpi_integer,bytes,4,pos,comm,iErr)
    be_val=transfer(bytes,be_val)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  end subroutine to_nativen_int32
  
  subroutine     to_nativen_int32_tab(comm,val,be_val)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32), intent(in)    :: comm
    integer(int32), intent(in)    :: val   (:)
    integer(int32), intent(inout) :: be_val(:)
    !>
    integer(int32)                :: n,sizeofVal
    integer(int8) , allocatable   :: bytes(:)
    integer(int32)                :: pos
    integer(int32)                :: iErr
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    n=size(val)
    sizeofVal=sizeof(val(1))
    allocate(bytes(n*sizeofVal))
    pos=1
    
    call mpi_pack(val,n,mpi_integer,bytes,n*sizeofVal,pos,comm,iErr)
    be_val=transfer(bytes,be_val)
    deallocate(bytes)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  end subroutine to_nativen_int32_tab
  
  function        mpiio_part2block_real64(comm, data_indx,stride,data, dataBloc) result(t0)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    use iso_c_binding
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32), intent(in)  :: comm
    integer(int64), intent(in)  :: data_indx(:)   !> Tableau de position globale des données locales
    real(real64)  , intent(in)  :: data     (:)   !> Matrice des données entrelacees
    real(real64)  , pointer     :: dataBloc (:)   !> Matrice des données regroupées par blocs
    real(real64)                :: t0
    
    integer(int32)              :: i,iErr
    integer(int32)              :: stride,dim
    integer(int64)              :: dimGlb
    integer(int32)              :: iRank,jRank,rankMPI,sizeMPI
    
    integer(int32)              :: taillePaquet
    integer(int32), allocatable :: send_counts   (  :), recv_counts   (  :)
    integer(int32), allocatable :: send_displs   (  :), recv_displs   (  :)  ! <= avec des deplacement en int32
    integer(int64), allocatable :: send_data_indx(  :), recv_data_indx(  :)
    real(real64)  , allocatable :: send_data     (:,:), recv_data     (:,:)
    character(120)              :: buffer
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    t0=mpi_wtime()
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Initialisation de stride et de m
    dim=size(data)/stride
    
    !> Initialisation MPI
    call MPI_Comm_rank(comm, rankMPI, iErr)
    call MPI_Comm_size(comm, sizeMPI, iErr)
    
    !> Allocation des structures de communication
    allocate(send_counts(0:sizeMPI-1), recv_counts(0:sizeMPI-1))
    allocate(send_displs(0:sizeMPI-1), recv_displs(0:sizeMPI-1))
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Étape 1 : Déterminer combien d'éléments chaque processus doit envoyer et recevoir
    send_counts=0
    recv_counts=0
    
    call mpi_allReduce(int(dim,kind=int64),dimGlb,1,mpi_integer8,mpi_sum,comm,iErr)
    !write(buffer,'("mpiio_part2block dimGlb:",i3)')dimGlb
    !iErr=mpiio_message(comm=comm, buffer=buffer)  
    
    taillePaquet=int(dimGlb/sizeMPI,kind=int32)
    !write(buffer,'("mpiio_part2block taillePaquet:",i3)')taillePaquet
    !iErr=mpiio_message(comm=comm, buffer=buffer)  
    
    do i=1,dim
      jRank=min((data_indx(i)-1)/taillePaquet,sizeMPI-1) !> Détermination du proc cible
      send_counts(jRank)=send_counts(jRank)+1
    enddo
    
    !write(buffer,'("mpiio_part2block send_counts:",*(i3,1x) )')send_counts(:)
    !iErr=mpiio_message(comm=comm, buffer=buffer)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Étape 2 : Échange des tailles avec MPI_Alltoall
    call MPI_Alltoall(send_counts, 1, mpi_integer, recv_counts, 1, mpi_integer, comm, iErr)
    
    !write(buffer,'("mpiio_part2block recv_counts:",*(i3,1x) )')recv_counts(:)
    !iErr=mpiio_message(comm=comm, buffer=buffer)  
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Étape 3 : Calcul des décalages pour MPI_Alltoallv
    send_displs(0)=0_int32
    recv_displs(0)=0_int32
    do iRank=1,sizeMPI-1
      send_displs(iRank)=send_displs(iRank-1)+send_counts(iRank-1)
      recv_displs(iRank)=recv_displs(iRank-1)+recv_counts(iRank-1)
    enddo
    
    !write(buffer,'("mpiio_part2block send_displs:",*(i3,1x) )')send_displs(:) ; iErr=mpiio_message(comm=comm, buffer=buffer)  
    !write(buffer,'("mpiio_part2block recv_displs:",*(i3,1x) )')recv_displs(:) ; iErr=mpiio_message(comm=comm, buffer=buffer)  
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Étape 4 : Allocation des buffers de communication
    allocate(send_data     (1:stride,1:sum(send_counts))) ! Stockage colonne-major (entrelacement des données)
    allocate(send_data_indx(         1:sum(send_counts)))
    
    allocate(recv_data     (1:stride,1:sum(recv_counts))) ! Stockage colonne-major (entrelacement des données)        
    allocate(recv_data_indx(         1:sum(recv_counts)))    
    
    !> Organisation des données à envoyer
    if( rankMPI==0 )print'()'
    do i=1,dim
      jRank= min((data_indx(i)-1)/taillePaquet,sizeMPI-1)
      send_data     (1:stride,send_displs(jRank)+1) = data     (stride*(i-1)+1:stride*i)  !> Transfert de colonnes complètes
      send_data_indx(         send_displs(jRank)+1) = data_indx(i)
      
      send_displs(jRank)=send_displs(jRank)+1
    enddo
    
    !> Réinitialisation des send_displs
    send_displs(0)=0
    do iRank=1,sizeMPI-1
      send_displs(iRank)=send_displs(iRank-1)+send_counts(iRank-1)
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Étape 5 : Communication des données avec MPI_Alltoallv
    call MPI_Alltoallv(                                                &
    &    send_data, stride*send_counts, stride*send_displs, mpi_real8, &
    &    recv_data, stride*recv_counts, stride*recv_displs, mpi_real8, &
    &    comm, iErr                                                    )
    
    call MPI_Alltoallv(                                          &
    &    send_data_indx, send_counts, send_displs, mpi_integer8, &
    &    recv_data_indx, recv_counts, recv_displs, mpi_integer8, &
    &    comm, ierr                                              )
    
    !do iRank=0,sizeMPI-1
    !  if( iRank==rankMPI )then
    !    print '(/"rank",i3)',rankMPI
    !    do i=1,dim
    !      print '(3x,"recv_data_indx(:,",i4")=",i4)',i,recv_data_indx(i)
    !    enddo
    !  endif
    !  call mpi_barrier(comm,iErr)
    !enddo  
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! Étape 6 : Tri local des données suivant data_indx
    allocate(dataBloc(1:stride*sum(recv_counts))) 
    
    block
      integer(int64) :: j,minData_indx
      mindata_indx=minval(recv_data_indx)
      do i=1,size(recv_data,2)
        j=recv_data_indx(i)-mindata_indx+1
        dataBloc(stride*(j-1)+1:stride*j) = recv_data(1:stride,i)
      enddo
    end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Nettoyage
    deallocate(send_data, send_counts, send_displs)
    deallocate(recv_data, recv_counts, recv_displs)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    t0=mpi_wtime()-t0
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    return
  end function    mpiio_part2block_real64
  
  
  !function        mpiio_part2block_real64_test(comm, data_indx, stride, data, dataBloc) result(t0)
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  use mpi
  !  use iso_c_binding
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  !  integer(int32), intent(in)     :: comm
  !  integer(int64), intent(in)     :: data_indx(:)
  !  real(real64),   intent(in)     :: data(:)
  !  real(real64),   pointer        :: dataBloc(:)
  !  real(real64)                   :: t0
  !  
  !  integer(int32)                 :: i,iErr
  !  integer(int32)                 :: stride
  !  integer(int64)                 :: dim, dimGlb
  !  integer(int32)                 :: iRank, jRank, rankMPI, sizeMPI
  !  integer(int64)                 :: taillePaquet  ! En 64 bits pour grands problèmes
  !  
  !  integer(int32), allocatable    :: send_counts(:), recv_counts(:)
  !  integer(int32), allocatable    :: send_displs(:), recv_displs(:)  ! Restent à 0, gérés par types
  !  integer(int32), allocatable    :: send_displs(:), recv_displs(:)  ! Restent à 0, gérés par types
  !  
  !  integer(int64), allocatable    :: send_data_indx(  :), recv_data_indx(  :)
  !  real(real64)  , allocatable    :: send_data     (:,:), recv_data     (:,:)
  !  integer(int32), allocatable    :: send_types(:), recv_types(:)
  !  character(120)                 :: buffer
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  !  t0 = mpi_wtime()
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>        
  !  !> Initialisation MPI
  !  call MPI_Comm_rank(comm, rankMPI, iErr)
  !  call MPI_Comm_size(comm, sizeMPI, iErr)
  !  
  !  block
  !    integer :: len
  !    call MPI_Get_library_version(buffer, len, iErr)
  !    iErr=mpiio_message(comm=comm, buffer=buffer)
  !  end block
  !  
  !  !> Initialisation de stride et de m
  !  dim = size(data) / int(stride,kind=int64)
  !  
  !  write(buffer,'("mpiio_part2block stride:",i3)')stride
  !  iErr=mpiio_message(comm=comm, buffer=buffer)  
  !
  !  write(buffer,'("mpiio_part2block dim:",i3)')dim
  !  iErr=mpiio_message(comm=comm, buffer=buffer)  
  !  
  !  !> Allocation des structures de communication
  !  allocate(send_counts(0:sizeMPI-1), recv_counts(0:sizeMPI-1))
  !  allocate(send_displs(0:sizeMPI-1), recv_displs(0:sizeMPI-1))
  !  allocate(send_types (0:sizeMPI-1), recv_types (0:sizeMPI-1))
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  !> Étape 1 : Déterminer combien d'éléments chaque processus doit envoyer et recevoir  
  !  send_counts(:) = 0
  !  recv_counts(:) = 0
  !  
  !  call MPI_AllReduce(dim, dimGlb, 1, MPI_INTEGER8, MPI_SUM, comm, iErr)
  !  write(buffer,'("mpiio_part2block dimGlb:",i3)')dimGlb
  !  iErr=mpiio_message(comm=comm, buffer=buffer)  
  !  
  !  taillePaquet=(dimGlb/sizeMPI)
  !  write(buffer,'("mpiio_part2block taillePaquet:",i3)')taillePaquet
  !  iErr=mpiio_message(comm=comm, buffer=buffer)  
  !  
  !  do i=1,dim
  !    jRank = min((data_indx(i)-1)/taillePaquet, int(sizeMPI-1,kind=int64)) !> Détermination du proc cible
  !    send_counts(jRank) = send_counts(jRank) + 1
  !  enddo
  !  write(buffer,'("mpiio_part2block send_counts:",*(i3,1x) )')send_counts(:)
  !  iErr=mpiio_message(comm=comm, buffer=buffer)  
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  !> Étape 2 : Échange des tailles avec MPI_Alltoall
  !  
  !  call MPI_Alltoall(send_counts, 1, MPI_INTEGER, recv_counts, 1, MPI_INTEGER, comm, iErr)
  !  
  !  write(buffer,'("mpiio_part2block recv_counts:",*(i3,1x) )')recv_counts(:)
  !  iErr=mpiio_message(comm=comm, buffer=buffer)  
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  !> Étape 3 : Calcul des décalages pour MPI_Alltoallvw
  !  send_displs(:) = 0 !> Déplacements à 0, offsets gérés par types dérivés
  !  recv_displs(:) = 0 !> Déplacements à 0, offsets gérés par types dérivés
  !  
  !  do iRank = 1, sizeMPI-1
  !    send_displs(iRank) = send_displs(iRank-1) + send_counts(iRank-1)
  !    recv_displs(iRank) = recv_displs(iRank-1) + recv_counts(iRank-1)
  !  enddo    
  !  
  !  write(buffer,'("mpiio_part2block send_displs:",*(i3,1x) )')send_displs(:) ; iErr=mpiio_message(comm=comm, buffer=buffer)  
  !  write(buffer,'("mpiio_part2block recv_displs:",*(i3,1x) )')recv_displs(:) ; iErr=mpiio_message(comm=comm, buffer=buffer)  
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  !> Étape 4 : Allocation des buffers de communication
  !  allocate(send_data     (1:stride,1:sum(int(send_counts,kind=int64)))) ! Stockage colonne-major (entrelacement des données)
  !  allocate(send_data_indx(         1:sum(int(send_counts,kind=int64))))
  !  
  !  allocate(recv_data     (1:stride, 1:sum(int(recv_counts,kind=int64)))) ! Stockage colonne-major (entrelacement des données)
  !  allocate(recv_data_indx(          1:sum(int(recv_counts,kind=int64))))
  !  
  !  !> Organisation des données à envoyer
  !  if (rankMPI == 0) print '()'
  !  do i=1,dim
  !    !jRank = min((data_indx(i)-1) / taillePaquet, int(sizeMPI-1,kind=int64))
  !    jRank = min((data_indx(i)-1) / taillePaquet, sizeMPI-1)
  !    send_data     (1:stride,send_displs(jRank)+1) = data     (stride*(i-1)+1:stride*i)!> Transfert de colonnes complètes
  !    send_data_indx(         send_displs(jRank)+1) = data_indx(i)
  !    
  !    send_displs(jRank)=send_displs(jRank)+1
  !  enddo
  !  
  !  !> Réinitialisation des send_displs
  !  send_displs(0)=0
  !  do iRank =1,sizeMPI-1
  !    send_displs(iRank)=send_displs(iRank-1)+stride*send_counts(iRank-1)
  !  enddo
  !  
  !  recv_displs(0)=0
  !  do iRank=1,sizeMPI-1
  !    recv_displs(iRank)=recv_displs(iRank-1)+stride*recv_counts(iRank-1)
  !  enddo    
  !  
  !  !> Création des types dérivés pour send_data (real64)
  !  do iRank=0,sizeMPI-1
  !    call MPI_Type_create_hindexed(1, [stride * send_counts(iRank)],[send_displs(iRank) * 8_MPI_ADDRESS_KIND],mpi_real8, send_types(iRank), iErr)
  !    call MPI_Type_commit(send_types(iRank), iErr)
  !  enddo
  !  
  !  do iRank=0,sizeMPI-1
  !    call MPI_Type_create_hindexed(1, [stride * recv_counts(iRank)],[recv_displs(iRank) * 8_MPI_ADDRESS_KIND],mpi_real8, recv_types(iRank), iErr)
  !    call MPI_Type_commit(recv_types(iRank), iErr)
  !  enddo
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !          
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  !> Étape 5 : Communication des données avec MPI_Alltoallw
  !  
  !  write(buffer,'("mpiio_part2block_test coucou0")') ; iErr=mpiio_message(comm=comm, buffer=buffer)  
  !  
  !  call MPI_Alltoallw(                                   &
  !  &    send_data, send_counts, send_displs, send_types, &
  !  &    recv_data, recv_counts, recv_displs, recv_types, &
  !  &    comm, iErr                                       )
  !  
  !  !> Libération des types pour real64
  !  do iRank=0,sizeMPI-1
  !    if (send_counts(iRank)>0 .or. recv_counts(iRank)>0 )then  ! Vérifie si le type a été utilisé      
  !      call MPI_Type_free(send_types(iRank), iErr)
  !      call MPI_Type_free(recv_types(iRank), iErr)
  !    endif
  !  enddo
  !  
  !  write(buffer,'("mpiio_part2block_test coucou1")') ; iErr=mpiio_message(comm=comm, buffer=buffer)  
  !  
  !  !> Création des types dérivés pour send_data_indx (int64)
  !  do iRank=0,sizeMPI-1
  !    call MPI_Type_create_hindexed(1,[send_counts(iRank)],[send_displs(iRank) * 8_MPI_ADDRESS_KIND],MPI_INTEGER8,send_types(iRank),iErr)
  !    call MPI_Type_commit(send_types(iRank), iErr)
  !  enddo
  !  
  !  do iRank=0,sizeMPI-1
  !    call MPI_Type_create_hindexed(1,[recv_counts(iRank)],[recv_displs(iRank) * 8_MPI_ADDRESS_KIND],MPI_INTEGER8,recv_types(iRank),iErr)
  !    call MPI_Type_commit(recv_types(iRank), iErr)
  !  enddo
  !  
  !  write(buffer,'("mpiio_part2block_test coucou2")') ; iErr=mpiio_message(comm=comm, buffer=buffer)
  !  
  !  !> Réinitialisation des send_displs
  !  !send_displs(0)=0
  !  !do iRank =1,sizeMPI-1
  !  !  send_displs(iRank)=send_displs(iRank-1)+send_counts(iRank-1)
  !  !enddo
  !  !
  !  !recv_displs(0)=0
  !  !do iRank=1,sizeMPI-1
  !  !  recv_displs(iRank)=recv_displs(iRank-1)+recv_counts(iRank-1)
  !  !enddo    
  !
  !  !> Communication des indices avec MPI_Alltoallw (int64)
  !  call MPI_Alltoallw(                                        &
  !  &    send_data_indx, send_counts, send_displs, send_types, &
  !  &    recv_data_indx, recv_counts, recv_displs, recv_types, &
  !  &    comm, iErr                                            )
  !  
  !  !> Libération des types pour real64
  !  do iRank=0,sizeMPI-1
  !    if (send_counts(iRank)>0 .or. recv_counts(iRank)>0 )then  ! Vérifie si le type a été utilisé      
  !      call MPI_Type_free(send_types(iRank), iErr)
  !      call MPI_Type_free(recv_types(iRank), iErr)
  !    endif
  !  enddo
  !  
  !  write(buffer,'("mpiio_part2block_test coucou3")') ; iErr=mpiio_message(comm=comm, buffer=buffer)  
  !  
  !  !do iRank=0,sizeMPI-1
  !  !  if( iRank==rankMPI )then
  !  !    print '(/"rank",i3)',rankMPI
  !  !    do i=1,dim
  !  !      print '(3x,"recv_data_indx(:,",i4")=",i4)',i,recv_data_indx(i)
  !  !    enddo
  !  !  endif
  !  !  call mpi_barrier(comm,iErr)
  !  !enddo  
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  ! Étape 6 : Tri local des données suivant data_indx
  !  !allocate(dataBloc(1:stride * sum(int(recv_counts, int64))))
  !  allocate(dataBloc(1:stride*sum(recv_counts))) 
  !  
  !  block
  !    integer(int64) :: j, minData_indx  ! j et minData_indx en 64 bits
  !    minData_indx = minval(recv_data_indx)
  !    do i = 1, size(recv_data, 2)
  !      j = recv_data_indx(i) - minData_indx + 1
  !      dataBloc(stride*(j-1)+1:stride*j) = recv_data(1:stride, i)
  !    enddo
  !  end block
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !  !> Nettoyage  
  !  deallocate(send_data, send_counts, send_displs)
  !  deallocate(recv_data, recv_counts, recv_displs)
  !  deallocate(send_data_indx, recv_data_indx)
  !  deallocate(send_types, recv_types)
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  !  t0=mpi_wtime()-t0
  !  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !  return
  !end function mpiio_part2block_real64_test


  function mpiio_part2block_real64_test(comm, data_indx, stride, data, dataBloc) result(t0)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    use mpi
    use iso_c_binding
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    integer(int32), intent(in)  :: comm
    integer(int64), intent(in)  :: data_indx(:)   !> Tableau de position globale des données locales
    real(real64)  , intent(in)  :: data(:)        !> Matrice des données entrelacées
    real(real64)  , pointer     :: dataBloc(:)    !> Matrice des données regroupées par blocs
    real(real64)                :: t0
    
    integer(int32)              :: i, iErr
    integer(int32)              :: stride, dim, dimGlb
    integer(int32)              :: iRank, jRank, rankMPI, sizeMPI
    integer(int32)              :: taillePaquet
    integer(int32), allocatable :: send_counts(:), recv_counts(:)
    integer(int64), allocatable :: send_displs(:), recv_displs(:)   ! <= avec des deplacement en int64
    !integer(MPI_ADDRESS_KIND), allocatable :: send_displs(:), recv_displs(:)
    
    integer(int64), allocatable :: send_data_indx(  :), recv_data_indx(  :)
    real(real64),   allocatable :: send_data     (:,:), recv_data     (:,:)

    integer,        allocatable :: send_requests(:), recv_requests(:)  ! Pour MPI_Isend/Irecv
    character(120)              :: buffer
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    t0 = mpi_wtime()
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Initialisation de stride et de dim
    dim = size(data) / stride
    
    !> Initialisation MPI
    call MPI_Comm_rank(comm, rankMPI, iErr)
    call MPI_Comm_size(comm, sizeMPI, iErr)
    
    !> Allocation des structures de communication
    allocate(send_counts  (0:sizeMPI-1), recv_counts  (0:sizeMPI-1))
    allocate(send_displs  (0:sizeMPI-1), recv_displs  (0:sizeMPI-1))
    allocate(send_requests(0:sizeMPI-1), recv_requests(0:sizeMPI-1))  ! Requêtes pour MPI_Isend/Irecv
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Étape 1 : Déterminer combien d'éléments chaque processus doit envoyer et recevoir
    send_counts = 0_int32
    recv_counts = 0_int32
    
    call mpi_allreduce(dim, dimglb, 1, mpi_integer, mpi_sum, comm, ierr)
    !write(buffer,'("mpiio_part2block dimGlb:",i3)')dimGlb
    !iErr=mpiio_message(comm=comm, buffer=buffer)

    taillePaquet = (dimGlb / sizeMPI)
    !write(buffer,'("mpiio_part2block taillePaquet:",i3)') taillePaquet
    !iErr = mpiio_message(comm=comm, buffer=buffer)  
    
    do i = 1, dim
      jRank = min((data_indx(i)-1) / taillePaquet, sizeMPI-1)  ! Détermination du proc cible
      send_counts(jRank) = send_counts(jRank) + 1
    enddo
    
    !write(buffer,'("mpiio_part2block send_counts:",*(i3,1x))') send_counts(:)
    !iErr = mpiio_message(comm=comm, buffer=buffer)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Étape 2 : Échange des tailles avec MPI_Alltoall
    call MPI_Alltoall(send_counts, 1, MPI_INTEGER, recv_counts, 1, MPI_INTEGER, comm, iErr)
    
    !write(buffer,'("mpiio_part2block recv_counts:",*(i3,1x))') recv_counts(:)
    !iErr = mpiio_message(comm=comm, buffer=buffer)  
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Étape 3 : Calcul des décalages
    send_displs(0) = 0_MPI_ADDRESS_KIND
    recv_displs(0) = 0_MPI_ADDRESS_KIND
    do iRank = 1, sizeMPI-1
      send_displs(iRank)=send_displs(iRank-1)+int(send_counts(iRank-1),kind=int64)
      recv_displs(iRank)=recv_displs(iRank-1)+int(recv_counts(iRank-1),kind=int64)
    enddo
    
    !write(buffer,'("mpiio_part2block send_displs:",*(i3,1x))')send_displs(:) ; iErr=mpiio_message(comm=comm, buffer=buffer)  
    !write(buffer,'("mpiio_part2block recv_displs:",*(i3,1x))')recv_displs(:) ; iErr=mpiio_message(comm=comm, buffer=buffer)  
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Étape 4 : Allocation des buffers de communication
    allocate(send_data     (1:stride,1:sum(send_counts))) ! Stockage colonne-major (entrelacement des données)
    allocate(send_data_indx(         1:sum(send_counts)))
    allocate(recv_data     (1:stride,1:sum(recv_counts))) ! Stockage colonne-major (entrelacement des données)
    allocate(recv_data_indx(         1:sum(recv_counts)))    
    
    !> Organisation des données à envoyer
    if( rankMPI==0 )print '()'
    do i = 1, dim
      jRank = min((data_indx(i)-1) / taillePaquet, sizeMPI-1)
      send_data     (1:stride, send_displs(jRank)+1) = data     (stride*(i-1)+1:stride*i)  !> Transfert de colonnes complètes
      send_data_indx(          send_displs(jRank)+1) = data_indx(i)
      
      send_displs(jRank)=send_displs(jRank)+1
    enddo
    
    ! Réinitialisation des send_displs
    send_displs(0) = 0
    do iRank = 1, sizeMPI-1
      send_displs(iRank) = send_displs(iRank-1) + int(send_counts(iRank-1),kind=int64)
    enddo
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Étape 5 : Communication des données avec MPI_Isend/MPI_Irecv
    
    !> Premier envoi : données real64
    do iRank = 0, sizeMPI-1
      if (send_counts(iRank) > 0) then
        call mpi_isend(send_data(1, send_displs(irank)+1), stride * send_counts(irank), mpi_real8, irank, 1, comm, send_requests(irank), ierr)
      else
        send_requests(iRank) = MPI_REQUEST_NULL
      endif
      if (recv_counts(iRank) > 0) then
        call mpi_irecv(recv_data(1, recv_displs(irank)+1), stride * recv_counts(irank), mpi_real8, irank, 1, comm, recv_requests(irank), ierr)
      else
        recv_requests(irank) = mpi_request_null
      endif
    enddo
    
    !> Attente de la fin des communications pour real64
    call mpi_waitall(sizempi, send_requests, mpi_statuses_ignore, ierr)
    call mpi_waitall(sizempi, recv_requests, mpi_statuses_ignore, ierr)

    !> Second envoi : indices int64
    do iRank = 0, sizeMPI-1
      if (send_counts(iRank) > 0) then
        call mpi_isend(send_data_indx(send_displs(irank)+1), send_counts(irank), mpi_integer8, irank, 2, comm, send_requests(irank), ierr)
      else
        send_requests(irank) = mpi_request_null
      endif
      if (recv_counts(iRank) > 0) then
        call MPI_Irecv(recv_data_indx(recv_displs(iRank)+1), recv_counts(iRank), MPI_INTEGER8, iRank, 2, comm, recv_requests(iRank), iErr)
      else
        recv_requests(irank) = mpi_request_null
      endif
    enddo
    
    !> Attente de la fin des communications pour int64
    call mpi_waitall(sizempi, send_requests, mpi_statuses_ignore, ierr)
    call mpi_waitall(sizempi, recv_requests, mpi_statuses_ignore, ierr)
    
    !do iRank=0,sizeMPI-1
    !  if( iRank==rankMPI )then
    !    print '(/"rank",i3)',rankMPI
    !    do i=1,dim
    !      print '(3x,"recv_data_indx(:,",i4")=",i4)',i,recv_data_indx(i)
    !    enddo
    !  endif
    !  call mpi_barrier(comm,iErr)
    !enddo  
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Étape 6 : Tri local des données suivant data_indx
    allocate(dataBloc(1:stride*sum(recv_counts))) 
    block
      integer(int64) :: j,minData_indx
      minData_indx=minval(recv_data_indx)
      do i=1,size(recv_data,2)
        j=recv_data_indx(i)-minData_indx+1
        dataBloc(stride*(j-1)+1:stride*j) = recv_data(1:stride, i)
      enddo
    end block
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !> Nettoyage
    deallocate(send_data, send_counts, send_displs)
    deallocate(recv_data, recv_counts, recv_displs)
    deallocate(send_data_indx, recv_data_indx)
    deallocate(send_requests, recv_requests)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    t0 = mpi_wtime() - t0
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    return
end function mpiio_part2block_real64_test

end module space_mpiio