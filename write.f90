program write_at
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  use mpi
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  implicit none
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer                  :: dim,dimGlob
  integer, pointer         :: dimRank(:)
  integer                  :: i,rank,unit,iErr,int_size,file_size
  integer                  :: iRank,sizeMPI
  integer(MPI_OFFSET_KIND) :: offset
  integer, pointer         :: valeurs(:)
  integer                  :: statut(MPI_STATUS_SIZE)
  integer                  :: comm
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
  dim=1+rank
  allocate(valeurs(1:dim))
  valeurs(1:dim)=[(100*rank+iRank,iRank=1,dim)]
  
  do iRank=0,sizeMPI-1
    if( iRank==0.and.rank==0 )print '(/"Chaque process connait sa dimension et ses valeurs")'
    if( iRank==rank )then
      print '("rank ",i3,2x,"dim=",i3,2x,"valeurs ",*(i4,1x))',rank,dim,valeurs(1:dim) !> format norme fortran 2008
    endif
    call mpi_barrier(comm,iErr)
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  allocate(dimRank(0:sizeMPI-1))
  call mpi_allgather(                 &
  &    dim       , 1, mpi_integer    ,&
  &    dimRank(0), 1, mpi_integer    ,&
  &    comm                          ,&
  &    iErr                           )
  
  do iRank=0,sizeMPI-1
    if( iRank==0.and.rank==0 )print '(/"Chaque process connait les dimensions de tous les process (mpi_allgather)")'
    if( iRank==rank )then
      print '("rank ",i3,2x,"dim=",*(i4,1x))',rank,dimRank(:) !> format norme fortran 2008
     !print '("dimRank(",i2,")=",i3,4x,"sum(dimRank(0:",i2,"))=",i3)',iRank,dimRank(iRank),iRank-1,sum(dimRank(0:iRank-1))
    endif
    call mpi_barrier(comm,iErr)
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  do iRank=0,sizeMPI-1
    if( iRank==0.and.rank==0    )print '()'
    if( iRank==rank )then
      print '("rank ",i3," ouvre le fichier donnees.dat (mpi_file_open)")',rank
    endif
    call mpi_barrier(comm,iErr)
  enddo
  
  call mpi_file_open(                       &
  &    comm                                ,&
  &    "donnees.dat"                       ,& !> le nom du fichier
  &    ior(MPI_MODE_WRONLY,MPI_MODE_CREATE),& !> on indique que c'est pour ecrite
  &    MPI_INFO_NULL                       ,&
  &    unit                                ,&
  &    iErr                                 )
  
  offset=0
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_type_size(MPI_INTEGER,int_size,iErr)
  if( rank==0 )then
    print '(/"int_size (octets)=",i1)',int_size
  endif
  call mpi_barrier(comm,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  dimGlob=sum(dimRank(0:sizeMPI-1))
  
  if( rank==0 )then
    print '(/"rank ",i3," ecrit dimGlob=",i10," avec un offset en octets= ",i10)',rank,dimGlob,offset
    call mpi_file_write_at( &
    &    unit              ,&
    &    offset            ,&  !> on retrouve ici l'offset
    &    dimGlob           ,&  !> le tableau à écrire
    &    1                 ,&  !> le nombre d'éléments
    &    MPI_INTEGER       ,&  !> le type d'éléments
    &    statut            ,&
    &    iErr               )
  endif
  
  offset=offset+int_size
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  do iRank=0,rank-1
    offset=offset+(dimRank(iRank)*int_size)
  enddo
  
  do iRank=0,sizeMPI-1
    if( iRank==0.and.rank==0    )print '()'
    if( iRank==rank )then
      print '("rank ",i3," ecrit ",i10," valeurs avec un offset en octets= ",i10)',rank,dim,offset
    endif
    call mpi_barrier(comm,iErr)
  enddo
  
  !call mpi_file_write_at( &
  call MPI_FILE_write_at_ALL( &  !> Colleltives
  &    unit                  ,&
  &    offset                ,&  !> on retrouve ici l'offset
  &    valeurs(1)            ,&  !> le tableau à écrire
  &    dim                   ,&  !> le nombre d'éléments
  &    MPI_INTEGER           ,&  !> le type d'éléments
  &    statut                ,&
  &    iErr                   )
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  do iRank=0,sizeMPI-1
    if( iRank==0.and.rank==0 )print '()'
    if( iRank==rank )then
      print '("rank ",i3," ferme le fichier donnees.dat (mpi_file_close)")',rank
    endif
    call mpi_barrier(comm,iErr)
  enddo
  
  call mpi_file_close(    &
  &    unit              ,&
  &    iErr               )
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  deallocate(dimRank)
  deallocate(valeurs)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_finalize(iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
end program write_at
