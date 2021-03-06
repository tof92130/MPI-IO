program read_at
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use mpi
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  implicit none
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer                  :: statut(mpi_status_size)
  integer                  :: requete
  integer                  :: rank,comm
  integer                  :: iRank,sizeMPI
  integer                  :: iErr, unit
  integer(MPI_OFFSET_KIND) :: offset
  integer(MPI_OFFSET_KIND) :: file_size
  integer                  :: int_size
  logical                  :: termine
  integer                  :: dim,dimGlob 
  integer                  :: n0,n1
  integer, pointer         :: valeurs(:)
  integer, pointer         :: dimRank(:)
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
  !> ouverture du fichier "donnees.dat" en lecture
  call mpi_file_open(  &
  &    comm           ,&
  &    "donnees.dat"  ,&
  &    MPI_MODE_RDONLY,&
  &    mpi_info_null  ,&
  &    unit           ,&
  &    iErr            )
  
  !> Verification ouverture du fichier
  if ( .not.iErr== mpi_success )then
     print *, 'attention erreur lors ouverture du fichier'
     call mpi_abort(mpi_comm_world, 2, iErr)
     call mpi_finalize(iErr)
  end if
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_type_size(MPI_INTEGER,int_size,iErr)
  if( rank==0 )then
    print '()'
    print '("Taille entier en octets=",i10)',int_size
  endif
  call mpi_barrier(comm,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_file_get_size(unit, file_size, iErr)
  if( rank==0 )then
    print '()'
    print '("taille du fichier en octets=",i10," => nombre d''entiers=",i10)',file_size,file_size/int_size
  endif
  call mpi_barrier(comm,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Lecture de dimGlob
  
  offset=0
  
  call MPI_FILE_READ_ALL( &
  &    unit              ,&
  &    dimGlob           ,& !> variable
  &    1                 ,& !> dimension
  &    MPI_INTEGER       ,& !> type
  &    statut            ,&
  &    iErr               )
  
  offset=offset+int_size !> lecture d'un entier uniquement
  
  do iRank=0,sizeMPI-1
    if( iRank==0.and.rank==0    )print '()'
    if( iRank==rank )then
      print '("rank ",i3," dimGlob=",i3)',rank,dimGlob
    endif
    call mpi_barrier(comm,iErr)
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Dimensionnement
  n0= rank   *dimGlob/sizeMPI+1
  n1=(rank+1)*dimGlob/sizeMPI
  dim=n1-n0+1
  
  do iRank=0,sizeMPI-1
    if( iRank==0.and.rank==0    )print '()'
    if( iRank==rank )then
      print '("rank ",i3,2x,"n0=",i3,2x,"n1=",i3,3x,"dim=",i3)',rank,n0,n1,dim
    endif
    call mpi_barrier(comm,iErr)
  enddo
  
  
  allocate(dimRank(0:sizeMPI-1))
  call mpi_allgather(                 &
  &    dim       , 1, mpi_integer    ,&
  &    dimRank(0), 1, mpi_integer    ,&
  &    comm                          ,&
  &    iErr                           )
  
  if( rank==0 )then
    print '()'
    do iRank=0,sizeMPI-1
      print '("dimRank(",i2,")=",i3,4x,"sum(dimRank(0:",i2,"))=",i3)',iRank,dimRank(iRank),iRank-1,sum(dimRank(0:iRank-1))
    enddo
  endif
  call mpi_barrier(comm,iErr)
  
  
  do iRank=0,rank-1
    offset=offset+(dimRank(iRank)*int_size)
  enddo
  
  do iRank=0,sizeMPI-1
    if( iRank==0.and.rank==0    )print '()'
    if( iRank==rank )then
      print '("rank ",i3," Debut => offset Debut: ",i10)',rank,offset
    endif
    call mpi_barrier(comm,iErr)
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  allocate(valeurs(1:dim))
  
  if( 0==1 )then
    
    call MPI_FILE_READ_AT_ALL( &  !> plus performant en collectif
    &    unit              ,&
    &    offset            ,&
    &    valeurs(1)        ,&
    &    dim               ,&
    &    MPI_INTEGER       ,&
    &    statut            ,&
    &    iErr               )
    
  else
    
    call MPI_FILE_IREAD_AT( & !> lecture non blocante
    &    unit              ,&
    &    offset            ,&
    &    valeurs(1)        ,&
    &    dim               ,&
    &    MPI_INTEGER       ,&
    &    requete           ,&
    &    iErr               )
        
    do iRank=0,sizeMPI-1
      call mpi_test(requete,termine,statut,iErr)
      if( iRank==0.and.rank==0    )print '()'
      if( iRank==rank )then
        if( termine )then
          print '("rank ",i3," mpi_test lecture terminee")',rank
        else
          print '("rank ",i3," mpi_test lecture en cours")',rank
        endif
      endif
      call mpi_barrier(comm,iErr)
    enddo
    
   !call mpi_test(requete,termine,statut,iErr)
    call mpi_wait(requete,statut,iErr)
    
    do iRank=0,sizeMPI-1
      if( iRank==0.and.rank==0    )print '()'
      if( iRank==rank )then
        print '("rank ",i3," mpi_wait lecture terminee")',rank
      endif
      call mpi_barrier(comm,iErr)
    enddo
    
    
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_file_close(    &
  &    unit              ,&
  &    iErr               )
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  do iRank=0,sizeMPI-1
    if( iRank==0.and.rank==0    )print '()'
    if( iRank==rank )then
      print '("rank ",i3,2x,"dim=",i3,2x,"valeurs ",*(i4,1x))',rank,dim,valeurs(1:dim) !> format norme fortran 2008
    endif
    call mpi_barrier(comm,iErr)
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  deallocate(dimRank)
  deallocate(valeurs)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call mpi_finalize(iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
end program read_at


