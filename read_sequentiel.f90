program read_with_header_integer
  implicit none

  integer :: i, header_size, num_valeurs
  integer           :: dimGlob
  character(len=64) :: header
  integer, dimension(:), allocatable :: valeurs
  integer :: unit, file_size, valeurs_start
  
  ! Ouvrir le fichier en mode lecture
  open(newunit=unit, file='donnees.dat', status='old', access='stream', form='unformatted')
  
  ! Lire la taille du header (en supposant qu'elle est connue ou fixe)
  header_size = 256  ! Ajustez cette valeur si nécessaire
  
  ! Lire le header
  read(unit) header
  read(header(5:63), '(i10)') dimGlob  ! debut en position 5
  print '("dimGlob=",i3)',dimGlob

  
  
  ! Déterminer la taille du fichier
  inquire(unit=unit, size=file_size)
  print '()'
  print '("taille du fichier en octets=",i0)',file_size
 

  ! Allouer l'espace pour les données
  allocate(valeurs(dimGlob))
  read(unit)valeurs(1:dimGlob)
  
  ! Afficher les données
  print '("valeurs ",*(i4,1x))',valeurs(1:dimGlob) !> format norme fortran 2008

  ! Fermer le fichier
  close(unit)

  ! Libérer les tableaux
  deallocate(valeurs)

end program read_with_header_integer
