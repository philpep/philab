program runtime
  ! Programme principal.
  ! Il ne fait qu'analyser la ligne de commande
  ! lire les bonnes matrices puis executer
  ! la bonne fonction.


  ! Les modules qu'on va utiliser.
  use mod_mat_creuse
  use mod_normes
  use mod_utils
  use mod_gauss
  use mod_pw_iter

  implicit none

  ! Déclaration des variables

  ! Les arguments de la ligne de commande
  character (len=256), dimension(3) :: argv
  ! Les fichiers d'entrée.
  character (len=256), dimension(2) :: infile
  ! Type des fichiers d'entrée.
  character, dimension(2) :: T
  ! Les matrices creuses
  type(element), dimension(:), allocatable :: A,B
  ! Les matrices entières
  integer, dimension(:,:), allocatable :: C,D
  ! En fait le programme sait executer des fonctions
  ! sur deux matrices au maximum, donc avec le "jeux" de
  ! matrices A,B,C,D on peut tout faire.
  integer :: i


  ! On récupère les arguments de la ligne
  ! de commande. La subroutine getarg et la
  ! fonction iargc (qui renvoie le nombre d'arguments
  ! passées au programme) sont des
  ! extension GNU intégrée dans gfortran.
  ! Ce n'est pas très portable, mais c'est la
  ! seule manière que je connais.
  do i = 1,min(iargc(),3)
     call getarg(i, argv(i))
  end do

  ! On analyse la commande suivant le nombre
  ! d'arguments passés a la ligne de commande.
  select case (iargc())
  case(0,1)
     print*,'Liser le fichier README pour comprendre comment utiliser ce programme'
     stop
  case (2)
     if(argv(1) == 'produit' .or. argv(1) == 'somme' .or. argv(1) == 'gauss') then
        write(0,*) 'Pour utiliser',argv(1),', il faut deux opérandes'
        stop
     end if
     infile(1) = argv(2)
  case default
     infile(1) = argv(2)
     infile(2) = argv(3)
  end select


  ! On ouvre le premier fichier. status='OLD'
  ! permet de générer une érreur si le fichier
  ! n'existe pas.
  open(unit=15, status='OLD', file=infile(1))
  ! On lis le type de la première matrice.
  read(15,*) T(1)
  ! On ferme le fichier
  close(15)

  ! On alloue la matrice suivant le type
  if(T(1) == 'C') then
     call readmat(A,infile(1))
  elseif (T(1) == 'I') then
     call readmat(C,infile(1))
  else
     write(0,*) 'Format de fichier non valide : ', infile(1)
     stop
  end if

  ! On lis la deuxième matrice (sauf si
  ! la commande ne nécessite qu'une seule matrice,
  ! calcul de normes par exemple).

  if (iargc() > 2) then
     ! La methode est la même...
     open(unit=15, status='OLD', file=infile(2))
     read(15,*) T(2)
     close(15)
     if(T(2) == 'C') then
        call readmat(B,infile(2))
     elseif (T(2) == 'I') then
        call readmat(D,infile(2))
     else
        write(0,*) 'Format de fichier non valide : ', infile(2)
        stop
     end if
  end if



  ! argv(1) est la commande que l'on veut
  ! executer.
  ! On teste sur la commande et sur les types de
  ! matrices.
  select case (argv(1))
  case('norme1')
     if(T(1) == 'C') then
        print*,norme_1(A)
     else
        print*,norme_1(C)
     end if
  case('normeinf')
     if(T(1) == 'C') then
        print*,norme_inf(A)
     else
        print*,norme_inf(C)
     end if
  case('normefro')
     if(T(1) == 'C') then
        print*,frobenius(A)
     else
        print*,frobenius(C)
     end if
  case('print')
     if(T(1) == 'C') then
        call mprint(A)
     else
        call mprint(C)
     end if
  case('produit')
     if(T(1) == 'C' .and. T(2) == 'C') then
        call mprint(A*B)
     elseif (T(1) == 'C' .and. T(2) == 'I') then
        call mprint(A*D)
     elseif (T(1) == 'I' .and. T(2) == 'C') then
        call mprint(C*B)
     elseif (T(1) == 'I' .and. T(2) == 'I') then
        call mprint(matmul(C, D))
     else ! On est jamais trop prudent :)
        print*,'Il y a eu une erreur lors de la lecture de la matrice...'
     end if
  case('somme')
     if(T(1) == 'C' .and. T(2) == 'C') then
        call mprint(A+B)
     elseif (T(1) == 'C' .and. T(2) == 'I') then
        call mprint(A+D)
     elseif (T(1) == 'I' .and. T(2) == 'C') then
        call mprint(B+C)
     elseif (T(1) == 'I' .and. T(2) == 'I') then
        call mprint(C+D)
     else ! On est jamais trop prudent :)
        print*,'Il y a eu une erreur lors de la lecture de la matrice...'
     end if
  case('gauss')
     if(T(1) == 'C' .and. T(2) == 'C') then
        call mprint(gauss(mat2mat(A),mat2mat(B)))
     elseif (T(1) == 'C' .and. T(2) == 'I') then
        call mprint(gauss(mat2mat(A),D))
     elseif (T(1) == 'I' .and. T(2) == 'C') then
        call mprint(gauss(C,mat2mat(B)))
     elseif (T(1) == 'I' .and. T(2) == 'I') then
        call mprint(gauss(C,D))
     else ! On est jamais trop prudent :)
        print*,'Il y a eu une erreur lors de la lecture de la matrice...'
     end if
  case('pw_iter')
     if(T(1) == 'C' .and. T(2) == 'C') then
        print*,norme_inf(pw_iter(mat2mat(A),mat2mat(B)))
     elseif (T(1) == 'C' .and. T(2) == 'I') then
        print*,norme_inf(pw_iter(mat2mat(A),D))
     elseif (T(1) == 'I' .and. T(2) == 'C') then
        print*,norme_inf(pw_iter(C,mat2mat(B)))
     elseif (T(1) == 'I' .and. T(2) == 'I') then
        print*,norme_inf(pw_iter(C,D))
     else ! On est jamais trop prudent :)
        print*,'Il y a eu une erreur lors de la lecture de la matrice...'
     end if
  case default
     print*,'Commande inconnue : ', argv(1)
  end select


  ! On libère la memoire
  ! si on pouvait avoir une telle fonction en C :-)
  if(allocated(A)) then
     deallocate(A)
  end if
  if(allocated(B)) then
     deallocate(B)
  end if
  if(allocated(C)) then
     deallocate(C)
  end if
  if(allocated(D)) then
     deallocate(D)
  end if

end program runtime
