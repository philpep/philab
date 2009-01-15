! Divers outils d'affichage
! de matrices. Codes répétitifs
module mod_utils
  use mod_mat_creuse
  implicit none

  interface mprint
     module procedure mprinti, mprinte, mprintr
  end interface

  interface readmat
     module procedure readmat_i, readmat_e
  end interface

contains

  ! Ces subroutines servent a afficher les matrices
  ! a peu près proprement. càd lignes par lignes
  ! mprint c'est pour "matrix print"
  ! {{{ mprint
  subroutine mprinti (A)

    implicit none
    integer, dimension(:,:), intent(in) :: A
    integer :: i

    do i = 1, size(A,1)
       print*,A(i,:)
    end do

  end subroutine mprinti

  subroutine mprintr (A)

    implicit none
    real, dimension(:,:), intent(in) :: A
    integer :: i

    do i = 1, size(A,1)
       print*,A(i,:)
    end do

  end subroutine mprintr

  ! La fonction d'affichage pour les matrices
  ! creuses de type real
  subroutine mprinte (A)

    implicit none
    type(element), dimension(:), intent(in) :: A
    ! On crée un vecteur qui contiendra chaque ligne
    ! de A
    integer, dimension(maxval(A%ind_c)) :: V
    integer :: i, k

    ! On parcoure sur les lignes
    do i = 1,maxval(A%ind_l)
       ! On fixe toutes les valeurs de V à 0
       V = 0.
       ! On cherche les valeurs non nulles
       ! sur la ligne i
       do k = 1, size(A)

          if (i == A(k)%ind_l) then
             V(A(k)%ind_c) = A(k)%val
          end if

       end do
       ! On affiche le vecteur ligne
       print*,V
    end do

  end subroutine mprinte
  ! }}}

  ! Ces fonction permettent de lire une matrice
  ! NE PAS OUBLIER DE LIBÉRER LA MÉMOIRE
  ! Pour le type matrice creuse
  ! {{{ readmat
  subroutine readmat_e (A,fichier)

    type(element), dimension(:), allocatable, intent(inout) :: A
    character (len=256) :: fichier
    integer :: size_mat
    character :: c

    open(unit=15, status='OLD', file=fichier)

    read(15,*) c
    read(15,*) size_mat

    allocate(A(size_mat))

    read(15,*) A
    close(15)

  end subroutine readmat_e

  subroutine readmat_i (A,fichier)

    integer, dimension(:,:), allocatable, intent(inout) :: A
    character (len=256) :: fichier
    integer :: n,m,i
    character :: c

    open(unit=15, status='OLD', file=fichier)

    read(15,*) c
    read(15,*) n,m

    allocate(A(n,m))
    ! On lit ligne par ligne, sinon
    ! fortran range tout en colonne...
    do i = 1,n
       read(15,*) A(i,:)
    end do

    close(15)

  end subroutine readmat_i
  ! }}}

  ! Permet d'exporter une matrice dans le fichier
  ! de sortie
  ! {{{ export
  subroutine export(A, fichier)

  integer, dimension(:,:), intent(in) :: A
  character(len=256), intent(in) :: fichier
  integer :: i

  open(unit=15, file=fichier)
  write(15,*) 'I'
  write(15,*) size(A,1), size(A,2)
  ! On écris lignes par lignes
  do i=1,size(A,1)
     write(15,*) A(i,:)
  end do
  close(15)
  end subroutine export
  ! }}}

end module mod_utils
