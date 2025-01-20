program mpi_io_write_with_indices
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use iso_c_binding, only: c_loc,C_NEW_LINE
  use iso_fortran_env
  use mpi
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  implicit none
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer(int32)                     :: dim,dimGlob
  integer(int32)           , pointer :: dimRank(:)
  integer(int32)                     :: i,rank,size,unit,iErr
  integer(int32)                     :: char_size,int_size,file_size
  integer(int32)                     :: iRank
  integer(MPI_OFFSET_KIND)           :: offset
  integer(int32)                     :: filetype
  integer(int32)                     :: statut(MPI_STATUS_SIZE)
  integer(int32)                     :: comm
  character(len=:)         , pointer :: header=>null()
  character(1)                       :: lf
  
  integer                  , pointer :: valeurs(:)
  integer                  , pointer :: indices(:)
  integer                  , pointer :: blocklengths (:)
  integer(MPI_ADDRESS_KIND), pointer :: displacements(:)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  offset = 0
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
  call mpi_type_size(mpi_integer  , int_size,iErr)
  if( rank==0 )then
    print '(/"char_size (octets)=",i1)',char_size
    print '( "int_size  (octets)=",i1)',int_size
  endif
  call mpi_barrier(comm,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Définition des données
  dim=1+rank
  
  allocate(indices(1:dim)) ; indices(1:dim)=[(1+ rank + size*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
  allocate(valeurs(1:dim)) ; valeurs(1:dim)=[(1+ rank + size*iRank-iRank*(iRank+1)/2 ,iRank=0,dim-1)]
  
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
  call MPI_FILE_OPEN(                    &
  &    comm                             ,&
  &    "donnees.dat"                    ,&
  &    MPI_MODE_CREATE + MPI_MODE_WRONLY,&
  &    MPI_INFO_NULL                    ,&
  &    unit                             ,&
  &    iErr                              )
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  allocate(dimRank(0:size-1))
  call mpi_allgather(                 &
  &    dim       , 1, mpi_integer    ,&
  &    dimRank(0), 1, mpi_integer    ,&
  &    comm                          ,&
  &    iErr                           )
  dimGlob=sum(dimRank(0:size-1))  
  deallocate(dimRank)
  
  write(header,'("dim=",i0)')dimGlob ; header(64:64)=lf
  !write(header,'("dim=",i0,a)')dimGlob,lf
  
  if( rank==0 )then
    print '(/"rank ",i3," ecrit dimGlob=",i10," avec un offset en octets= ",i10)',rank,dimGlob,offset
    print '(a)',trim(header(1:63))
    call mpi_file_write_at( &
    &    unit              ,&
    &    offset            ,&  !> on retrouve ici l'offset
    &        trim(header)  ,&  !> le tableau à écrire     
    &    len(trim(header)) ,&  !> le nombre d'éléments    
    &    mpi_character     ,&  !> le type d'éléments      
    &    statut            ,&
    &    iErr               )
  endif
  
  offset = len(header)*char_size
  call mpi_barrier(comm,iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Préparer les types dérivés pour les indices
  allocate(blocklengths(1:dim))
  blocklengths(1:dim)=1
  !do i=1,dim
  !  displacements(i)= (indices(i)-1) * int_size  ! Décalage en octets
  !end do
  allocate(displacements(1:dim))
  displacements(1:dim)=[((indices(i)-1)*int_size ,i=1,dim)]
  
  call MPI_TYPE_CREATE_HINDEXED( &
  &    dim                      ,&
  &    blocklengths             ,&
  &    displacements            ,&
  &    MPI_INTEGER              ,&
  &    filetype                 ,&
  &    iErr                      )
  
  call MPI_TYPE_COMMIT(          &          
  &    filetype                 ,&
  &    iErr                      )
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ! Définir la vue globale (chaque processus écrit dans sa position définie par indices)
  call MPI_FILE_SET_VIEW( &
  &    unit              ,&
  &    offset            ,&
  &    MPI_INTEGER       ,&
  &    MPI_INTEGER       ,&
  &    "native"          ,&
  &    MPI_INFO_NULL     ,&
  &    iErr               )
  
  ! Écriture collective
  call MPI_FILE_WRITE_ALL( &
  &    unit               ,&
  &    valeurs            ,&
  &    dim                ,&
  &    MPI_INTEGER        ,&
  &    MPI_STATUS_IGNORE  ,&
  &    iErr                )
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  ! Libérer les types dérivés et la mémoire
  call MPI_TYPE_FREE(filetype, iErr)
  deallocate(valeurs, indices, blocklengths, displacements)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  ! Fermer le fichier
  call MPI_FILE_CLOSE(unit, iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call MPI_FINALIZE(iErr)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  
end program mpi_io_write_with_indices
