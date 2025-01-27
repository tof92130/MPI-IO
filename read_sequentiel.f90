program read_with_header_integer
  use iso_fortran_env
  implicit none
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer           :: dimGlob
  integer           :: unit, file_size
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  !> Ouverture du fichier en mode lecture
  !open(newunit=unit, file='donnees.dat', status='old', access='stream', form='unformatted',action='read')
  open(newunit=unit, file='donnees.dat', status='old', access='stream', form='unformatted',action='read',convert='BIG_ENDIAN'                                            )

  inquire(unit=unit, size=file_size)
  print '()'
  print '("taille du fichier en octets=",i0)',file_size 
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Lecture du header
  block 
    character(len=64) :: header
    read(unit) header ; print '("header: ",a)',header
    read(header(5:63), '(i10)') dimGlob  ! debut en position 5
    print '("dimGlob=",i0)',dimGlob
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Lecture des int32
  block 
    integer(int32), pointer :: valeurs(:)
    allocate(valeurs(dimGlob))
    read(unit)valeurs(1:dimGlob)
    print '("valeurs ",*(i6,1x))',valeurs(1:dimGlob) !> format norme fortran 2008
    deallocate(valeurs)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Lecture des real64
  block
    real(real64), pointer :: valeurs(:)
    allocate(valeurs(1:dimGlob))
    read(unit)valeurs(1:dimGlob)
    print '("valeurs ",*(f6.0,1x))',valeurs(1:dimGlob) !> format norme fortran 2008
    deallocate(valeurs)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Lecture des character(80)
  block
    character(80), pointer :: valeurs(:)
    allocate(valeurs(1:dimGlob))
    read(unit)valeurs(1:dimGlob)
    print '("valeurs ",*(a,/))',valeurs(1:dimGlob) !> format norme fortran 2008
    deallocate(valeurs)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Fermeture du fichier
  close(unit)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
end program read_with_header_integer