!> Initialisation : Chaque processus initialise son sendbuf avec rank * block_size * nproc + i. Avec block_size = 1000000 et nproc = 4 :
!  Rang 0 : [       1,        2, ...,  4000000]
!  Rang 1 : [ 4000001,  4000002, ...,  8000000]
!  Rang 2 : [ 8000001,  8000002, ..., 12000000]
!  Rang 3 : [12000001, 12000002, ..., 16000000]

!> Envoi : Chaque processus divise son sendbuf en 4 blocs de 1 million d’entiers chacun :
!  Rang 0 envoie [      1: 1000000] à 0 
!                [1000001: 2000000] à 1
!                [2000001: 3000000] à 2
!                [3000001: 4000000] à 3
!  Rang 1 envoie [4000001: 5000000] à 0
!                [5000001: 6000000] à 1 
!  etc.

!> Réception : Chaque processus reçoit un bloc de 1 million d’entiers de chaque autre processus :
!  Rang 0 reçoit [       1: 1000000] de 0,
!                [ 4000001: 5000000] de 1,
!                [ 8000001: 9000000] de 2,
!                [12000001:13000000] de 3.
!  Rang 1 reçoit [ 1000001: 2000000] de 0,
!                [ 5000001: 6000000] de 1,
!  etc.

!> La sortie montre les 10 premiers éléments du premier bloc reçu par chaque processus :
!
!  Rang 0 : [      1:     10] (début de son propre bloc).
!  Rang 1 : [1000001:1000010] (début du bloc de Rang 0).
!  Rang 2 : [2000001:2000010] (début du bloc de Rang 0).
!  Rang 3 : [3000001:3000010] (début du bloc de Rang 0).

program mpi_alltoallw_64bit
  use mpi
  implicit none
  
  integer(int32), parameter      :: nproc = 4
  integer(int32)                 :: rank, size, ierr, i
  integer(int32)                 :: comm = MPI_COMM_WORLD
  integer(int32)                 :: sendbuf(100), recvbuf(100)            !> Buffers simplifiés
  integer(int32)                 :: sendcounts(nproc), recvcounts(nproc)
  integer(int32)                 :: sdispls(nproc),rdispls(nproc)
  integer(int32)                 :: sendtypes(nproc), recvtypes(nproc)
  integer(kind=MPI_ADDRESS_KIND) :: displacements(1)                      !> 64 bits pour les offsets
  integer(int32)                 :: blocklengths(1)
  integer(int32)                 :: oldtypes(1)
  integer(int32)                 :: version, subversion
  
  call MPI_Init(ierr)
  call MPI_Comm_rank(comm, rank, ierr)
  call MPI_Comm_size(comm, size, ierr)
  
  call MPI_Get_version(version, subversion, ierr)
  if (rank == 0) print *, "MPI Version : ", version, ".", subversion
  
  if ( .not.size==nproc) then
    if (rank == 0) print *, "Ce programme nécessite exactement 4 processus."
    call MPI_Finalize(ierr)
    stop
  endif
  
  ! Initialisation des buffers (exemple)
  do i=1,100
    sendbuf(i)=100*rank+i
  enddo
  recvbuf(:)=0
  
  !> Configuration des counts et déplacements
  do i=1,nproc
    sendcounts(i) = 1     ! 1 bloc par processus
    recvcounts(i) = 1
    sdispls   (i) = 0     ! Déplacements dans sendbuf gérés par les types
    rdispls   (i) = 0     ! Déplacements dans recvbuf gérés par les types
    
    ! Création d'un type dérivé pour l'envoi
    blocklengths(1) = 10  ! Taille du bloc (exemple : 10 entiers)
    displacements(1) = (i-1) * 1000000000_MPI_ADDRESS_KIND  ! Grand déplacement en 64 bits
    oldtypes(1) = MPI_INTEGER
    call MPI_Type_create_struct(1, blocklengths, displacements, oldtypes, sendtypes(i), ierr)
    call MPI_Type_commit(sendtypes(i), ierr)
    
    ! Type dérivé pour la réception
    displacements(1) = (i-1) * 1000000000_MPI_ADDRESS_KIND  ! Même logique
    call MPI_Type_create_struct(1, blocklengths, displacements, oldtypes, recvtypes(i), ierr)
    call MPI_Type_commit(recvtypes(i), ierr)
  enddo
  
  ! Appel à MPI_Alltoallw
  call MPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes, &
  &                  recvbuf, recvcounts, rdispls, recvtypes, &
  &                  comm, ierr)
  
  ! Affichage pour vérification
  print *, "Rang ", rank, " a reçu : ", recvbuf(1:40)
  
  ! Libération des types
  do i = 1, nproc
    call MPI_Type_free(sendtypes(i), ierr)
    call MPI_Type_free(recvtypes(i), ierr)
  enddo
  
  call MPI_Finalize(ierr)
end program mpi_alltoallw_64bit
